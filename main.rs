// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

#![crate_type = "bin"]
#![crate_name = "lrs_build"]
#![feature(std_misc, fs_time, os, libc)]
#![allow(deprecated)]

extern crate libc;

use std::path::{AsPath};
use std::{env, mem, os, thread, cmp};
use std::io::{self, BufReader, BufRead, Write};
use std::rc::{Rc};
use std::cell::{RefCell};
use std::fs::{self, File};
use std::process::{Command};
use std::sync::mpsc::{channel, Receiver, Sender};

macro_rules! err {
    ($fmt:expr) => { err!(concat!($fmt, "{}"), "") };
    ($fmt:expr, $($arg:tt)*) => {{
        let mut stderr = io::stderr();
        let _ = writeln!(&mut stderr, $fmt, $($arg)*);
        unsafe { libc::_exit(1); }
    }};
}

#[derive(Debug)]
struct Build {
    start: Option<String>,
    objs: Vec<Rc<RefCell<Obj>>>,
}

/// Status of an obj that needs rebuilding
#[derive(Debug, PartialEq)]
enum BuildStatus {
    /// Build has not yet started
    Pending,
    /// Building right now
    Building,
    /// Done building
    Built,
}

#[derive(Debug)]
struct Obj {
    name: String,
    needs_rebuild: Option<bool>,
    obj_modified: Option<u64>,
    build_status: BuildStatus,
    deps: Vec<Rc<RefCell<Obj>>>,
}

impl Obj {
    fn from_name(n: String) -> Obj {
        Obj {
            name: n,
            needs_rebuild: None,
            obj_modified: None,
            build_status: BuildStatus::Pending,
            deps: vec!(),
        }
    }

    fn check_needs_rebuild(&mut self) -> bool {
        if self.needs_rebuild.is_some() {
            return self.needs_rebuild.unwrap();
        }

        let mut last_dep_built = 0;
        for dep in &self.deps {
            let mut dep = dep.borrow_mut();
            if dep.check_needs_rebuild() {
                self.needs_rebuild = Some(true);
            } else {
                last_dep_built = cmp::max(last_dep_built, dep.obj_modified.unwrap());
            }
        }

        if self.needs_rebuild == Some(true) {
            return true;
        }

        let full_name = match &self.name[..] {
            "linux" => "linux".to_string(),
            _ => format!("linux_{}", self.name),
        };
        let obj_path = format!("obj/lib{}.rlib", full_name);

        // check obj time
        let obj_modified = match fs::metadata(&obj_path) {
            Ok(m) => m.modified(),
            _ => {
                self.needs_rebuild = Some(true);
                return true;
            },
        };

        if obj_modified < last_dep_built {
            self.needs_rebuild = Some(true);
            return true;
        }

        // load files
        let mut dep_path = "obj".as_path().to_path_buf();
        dep_path.push(&format!("{}.d", full_name));
        let deps_file = match File::open(&dep_path) {
            Ok(f) => BufReader::new(f),
            _ => {
                self.needs_rebuild = Some(true);
                return true;
            },
        };
        let mut files: Option<Vec<String>> = None;
        for line in deps_file.lines() {
            let line = line.unwrap();
            if line.starts_with(&obj_path) {
                let line = &line[obj_path.len()+2..];
                files = Some(line.split(' ').map(|w| w.to_string()).collect());
                break;
            }
        }
        let files = match files {
            Some(f) => f,
            _ => {
                self.needs_rebuild = Some(true);
                return true;
            },
        };

        // check files
        for file in files {
            match fs::metadata(&file) {
                Ok(ref m) if m.modified() <= obj_modified => { },
                _ => {
                    self.needs_rebuild = Some(true);
                    return true;
                },
            }
        }

        self.needs_rebuild = Some(false);
        self.obj_modified = Some(obj_modified);
        false
    }
}

/// Gets the next sub-target of this object that can be built, or `Err(false)` if there
/// are no more subtargets to be built, or `Err(true)` if there are more subtargets to be
/// built but they cannot be built right now.
fn next_target(obj: &Rc<RefCell<Obj>>) -> Result<Rc<RefCell<Obj>>, bool> {
    let mut can_build = true;

    {
        let obj = obj.borrow();

        if !obj.needs_rebuild.unwrap() || obj.build_status == BuildStatus::Built {
            return Err(false);
        }

        if obj.build_status == BuildStatus::Building {
            return Err(true);
        }

        for dep in &obj.deps {
            match next_target(dep) {
                Ok(t) => return Ok(t),
                Err(true) => can_build = false,
                _ => { },
            }
        }
    }

    match can_build {
        true => Ok(obj.clone()),
        _ => Err(true),
    }
}

/// Find the LRSBuild directory and the second directory after it.
fn find_conf() -> (String, Option<String>) {
    let mut cwd = env::current_dir().unwrap();
    let mut last = None;
    let mut second_to_last = None;
    loop {
        cwd.push("LRSBuild");
        let exists = fs::metadata(&cwd).is_ok();
        cwd.pop();
        if exists {
            return (cwd.to_str().unwrap().to_string(), second_to_last);
        }
        mem::swap(&mut last, &mut second_to_last);
        last = cwd.file_name().map(|f| f.to_str().unwrap().to_string());
        if last.is_none() {
            err!("Cannot find LRSBuild");
        }
        cwd.pop();
    }
}

/// Parse the contents of LRSBuild
fn parse(build: &mut Build) {
    let mut preobjs: Vec<(String, Vec<String>)> = vec!();

    for line in BufReader::new(fs::File::open("LRSBuild").unwrap()).lines() {
        let line = line.unwrap();
        let words: Vec<&str> = line.trim().split(' ').map(|w| w.trim())
                                   .filter(|w| w.len() > 0).collect();
        if words.len() > 0 {
            match words[0] {
                "obj" => {
                    assert!(words.len() > 1);
                    preobjs.push((words[1].to_string(), words[2..].iter().map(|w| w.to_string()).collect()));
                },
                _ => err!("expected predicate"),
            }
        }
    }

    for preobj in preobjs {
        let mut obj = Obj::from_name(format!("{}", preobj.0));
        for dep in preobj.1 {
            let dep_name = format!("{}", dep);
            match build.objs.iter().find(|x| x.borrow().name == dep_name) {
                Some(x) => obj.deps.push(x.clone()),
                _ => err!("can't find dependency {} of {}", dep_name, obj.name),
            }
        }
        build.objs.push(Rc::new(RefCell::new(obj)));
    }
}

/// Gets the object we're interested in building
fn get_base_obj(build: &Build) -> Rc<RefCell<Obj>> {
    let obj = build.start.as_ref().and_then(|s| build.objs.iter().find(|o| &o.borrow().name == s));
    match obj {
        Some(o) => o.clone(),
        _ => {
            let mut obj = Obj::from_name("linux".to_string());
            obj.deps = build.objs.clone();
            Rc::new(RefCell::new(obj))
        },
    }
}

fn build_thread(num: usize, req_recv: Receiver<String>, res_send: Sender<(usize, bool)>) {
    for req in req_recv.iter() {
        let mut cmd = Command::new("rustc");
        cmd.arg("--emit=link,dep-info");
        cmd.arg("--out-dir=obj");
        cmd.arg("-L").arg("obj");
        cmd.arg(&format!("src/{}/lib.rs", req));
        let res = cmd.spawn().and_then(|mut p| p.wait()).map(|r| r.success()).unwrap_or(false);
        res_send.send((num, res)).unwrap();
    }
}

fn main() {
    env::set_var("RUST_BACKTRACE", "true");

    let (top_dir, start) = find_conf();
    env::set_current_dir(&top_dir).unwrap();
    let mut build = Build { start: start, objs: vec!() };
    parse(&mut build);
    let target = get_base_obj(&build);
    target.borrow_mut().check_needs_rebuild();

    let _ = fs::create_dir("obj");

    let (res_send, res_recv) = channel();
    let mut threads = vec!();
    for i in 0..os::num_cpus() {
        let (req_send, req_recv) = channel();
        let res_send = res_send.clone();
        thread::spawn(move || build_thread(i, req_recv, res_send));
        threads.push((req_send, None));
    }
    let mut num_building = 0;

    let mut cur_target = None;
    'outer: loop {
        if cur_target.is_none() {
            cur_target = match next_target(&target) {
                Ok(t) => Some(t),
                Err(false) if num_building == 0 => break,
                _ => None,
            };
        }

        if cur_target.is_some() && num_building < threads.len() {
            for thread in &mut threads {
                if thread.1.is_none() {
                    let cur_target = cur_target.take().unwrap();
                    {
                        let mut target = cur_target.borrow_mut();
                        target.build_status = BuildStatus::Building;
                        println!("  building {}", target.name);
                    }
                    thread.0.send(cur_target.borrow().name.clone()).unwrap();
                    thread.1 = Some(cur_target);
                    num_building += 1;
                    continue 'outer;
                }
            }
        }

        // Either there is currently no target or all threads are busy

        let (thread_num, res) = res_recv.recv().unwrap();
        let target = threads[thread_num].1.take().unwrap();
        if !res {
            err!("building {} failed", target.borrow().name);
        }
        num_building -= 1;
        target.borrow_mut().build_status = BuildStatus::Built;
    }
}
