// This program is subject to the terms of the GNU General Public
// License, version 2.0. If a copy of the GPL was not distributed with
// this program, You can obtain one at http://gnu.org.

#![crate_name = "lrs_build"]
#![feature(default_type_parameter_fallback)]

extern crate lrsb_types;
extern crate lrsb_lexer;
extern crate lrsb_parser;
extern crate lrsb_eval;
extern crate lrsb_funcs;

use std::file::{self, File};
use std::file::mode::{MODE_DIRECTORY};
use std::time::{self, Time};
use std::fmt::{Debug, Write};
use std::rc::{Arc};
use std::sync::{Queue, Mutex};
use std::{env, mem, cmp, thread};
use std::process::{self, WAIT_EXITED, ChildStatus};
use std::string::{CString, CPtrPtr, CStr, NoNullStr};
use std::io::{BufReader, BufRead};
use std::iter::{IteratorExt};
use std::getopt::{Getopt};
use std::hashmap::{HashMap};

/// Print an error to stderr and exit.
macro_rules! errexit {
    ($fmt:expr) => { errexit!(concat!($fmt, "{}"), "") };
    ($fmt:expr, $($arg:tt)*) => {{
        errln!($fmt, $($arg)*);
        process::exit(1);
    }};
}

macro_rules! tryerr {
    ($val:expr, $fmt:expr) => { tryerr!($val, concat!($fmt, "{}"), "") };
    ($val:expr, $fmt:expr, $($arg:tt)*) => {{
        match $val {
            Ok(x) => x,
            Err(e) => errexit!(concat!("lrs_build: ", $fmt, ": ({:?})"), $($arg)*, e),
        }
    }};
}

mod target;
mod term;
mod lrsb;

/// Status of an obj that needs rebuilding.
#[derive(Eq)]
enum BuildStatus {
    /// Build has not yet started
    Pending,

    /// Building right now
    Building,

    /// Done building
    Built,
}

struct Obj {
    crate_name: CString,
    path: CString,
    name: CString,
    deps: Vec<Arc<Mutex<Obj>>>,
    needs_rebuild: Option<bool>,
    obj_modified: Option<Time>,
    build_status: BuildStatus,
}

impl Debug for Obj {
    fn fmt<W: Write>(&self, mut w: &mut W) -> Result {
        write!(w, "{:?}", self.name)
    }
}

pub struct Build {
    target: Arc<Mutex<Obj>>,
    cfgs: Vec<CString>,
}

impl Obj {
    /// Checks whether this object has to be rebuilt. Returns the result.
    ///
    /// After this function returns, the needs_rebuild field is `Some` for this object
    /// and, recursively, all of its dependencies.
    fn check_needs_rebuild(&mut self, force_rebuild: bool, target: &str) -> bool {
        // The check was already run for this object. This can happen if this is a
        // dependency of multiple objects.
        if self.needs_rebuild.is_some() {
            return self.needs_rebuild.unwrap();
        }

        // Store the last time any of the objects dependencies was built.
        let mut last_dep_built = Time::seconds(0);
        for dep in &self.deps {
            let mut dep = dep.lock();
            if dep.check_needs_rebuild(force_rebuild, target) {
                // If a dependency has to be rebuilt, then this object also has to be
                // rebuilt. We don't exit the loop prematurely because of the guarantee in
                // the function documentation.
                self.needs_rebuild = Some(true);
            } else {
                // If we don't have to rebuild this dependency, then we might still have
                // to rebuild this object because the dependency was built after this
                // object was built.
                last_dep_built = cmp::max(last_dep_built, dep.obj_modified.unwrap());
            }
        }

        if force_rebuild {
            self.needs_rebuild = Some(true);
        }

        if self.needs_rebuild == Some(true) {
            return true;
        }

        // Generate the full name of the object.
        let obj_path = format!("obj/{}/lib{}.rlib", target, self.crate_name).unwrap();

        // Check when the compiled object was last modified.
        let obj_modified = match file::info(&obj_path) {
            Ok(m) => m.last_modification(),
            _ => {
                // If the object doesn't exist then we definitely have to rebuilt.
                self.needs_rebuild = Some(true);
                return true;
            },
        };

        // The object exists. We have to check if any of the dependencies were rebuilt
        // after this object was built.
        if obj_modified < last_dep_built {
            self.needs_rebuild = Some(true);
            return true;
        }

        // Load the ".d" file generated by rustc which contains the files that were used
        // to compile this object, e.g, "lib.rs".
        let dep_path = format!("obj/{}/{}.d", target, self.crate_name).unwrap();
        let mut deps_file = match File::open_read(&dep_path) {
            Ok(f) => BufReader::new(f, 4096).unwrap(),
            _ => {
                // For some reason the ".rlib" exists but the ".d" doesn't exist. In this
                // case we just rebuild the object.
                self.needs_rebuild = Some(true);
                return true;
            },
        };

        // The line that starts with "obj/libfull_name.rlib: " contains our dependencies.
        let mut line: Vec<_> = Vec::new();
        while deps_file.copy_until(&mut line, b'\n').unwrap() > 0 {
            if line.last() == Some(&b'\n') {
                line.pop();
            }
            if line.starts_with(obj_path.as_ref()) {
                break;
            }
            line.truncate(0);
        }

        let files = if line.starts_with(obj_path.as_ref()) {
            let line = &line[obj_path.len()+2..];
            line.split(|&b| b == b' ')
        } else {
            // For some reason the file didn't contain the correct line. We just
            // rebuild.
            self.needs_rebuild = Some(true);
            return true;
        };

        // Check the modification time of all the files that were used to build this
        // object.
        for file in files {
            match file::info(file) {
                Ok(ref m) if m.last_modification() <= obj_modified => { },
                _ => {
                    self.needs_rebuild = Some(true);
                    return true;
                },
            }
        }

        // If we don't rebuild this object, then we have to store the last time it was
        // built in obj_modified.
        self.needs_rebuild = Some(false);
        self.obj_modified = Some(obj_modified);
        false
    }
}

/// Gets the next sub-target of this object that can be built, or `Err(false)` if there
/// are no more subtargets to be built, or `Err(true)` if there are more subtargets to be
/// built but they cannot be built right now.
fn next_target(obj: &Arc<Mutex<Obj>>) -> Result<Arc<Mutex<Obj>>, bool> {
    let mut can_build = true;

    {
        let obj = obj.lock();

        // If this object doesn't need rebuilding, then none of the dependencies need
        // rebuilding either. If this object has already been built, then all dependencies
        // have already been built.
        if !obj.needs_rebuild.unwrap() || obj.build_status == BuildStatus::Built {
            return Err(false);
        }

        // If this object is being built right now, then all dependencies have been built
        // and there is nothing else to do. We return `true` because objects that depend
        // on this object can't be built yet.
        if obj.build_status == BuildStatus::Building {
            return Err(true);
        }

        // We check all dependencies. If there is a dependency that can be built, then we
        // built it. If there are dependencies that have not been built yet and cannot be
        // built right now, then we cannot built this object either.
        for dep in &obj.deps {
            match next_target(dep) {
                Ok(t) => return Ok(t),
                Err(true) => can_build = false,
                _ => { },
            }
        }
    }

    match can_build {
        true => Ok(obj.add_ref()),
        _ => Err(true),
    }
}

/// Find the LRSBuild directory and the second directory after it.
///
/// The directory layout is `conf_dir/src/file` so the second directory after it can be,
/// e.g., "file" or "core".
fn find_conf() -> (CString, CString) {
    let mut path = tryerr!(env::get_cwd(), "Couldn't get cwd");
    let original = tryerr!(path.try_to(), "");

    loop {
        path.push_file("LRSBuild");
        let exists = file::exists(&*path) == Ok(true);
        path.pop_file();
        if exists {
            return (path, original);
        }
        if path.len() == 0 {
            errexit!("Cannot find LRSBuild");
        }
        path.pop_file();
    }
}

/// Parses the contents of LRSBuild.
fn parse_config(path: &NoNullStr, original: &NoNullStr, cfgs: &[&CStr]) -> Build {
    let file = tryerr!(File::open_read("LRSBuild"), "Error opening LRSBuild");

    let mut line: Vec<_> = Vec::new();
    tryerr!(line.read_to_eof(file), "");

    tryerr!(lrsb::parse(path, original, cfgs, line), "Error parsing LRSBuild")
}

struct Task {
    obj: Arc<Mutex<Obj>>,
    time: Time,
}

impl Task {
    fn new(obj: Arc<Mutex<Obj>>) -> Task {
        Task {
            obj: obj,
            time: mem::zeroed(),
        }
    }
}

/// The function that calls `lrsc` and waits for it to exit.
fn build_thread(requests: &Queue<Task>, results: &Queue<Task>, config: &Config) {
    let mut args = CPtrPtr::new().unwrap();
    let obj_path = format!("obj/{}", config.target).unwrap();

    loop {
        let mut task = requests.pop_wait();
        let start = time::MONO.get_time().unwrap();

        args.clear();
        args.push("lrsc").unwrap();
        args.push("--emit=link,dep-info").unwrap();
        args.push("--out-dir").unwrap();
        args.push(&obj_path).unwrap();
        args.push("-L").unwrap();
        args.push(&obj_path).unwrap();
        if config.target != target::DEFAULT {
            args.push("--target").unwrap();
            args.push(config.target).unwrap();
        }
        for arg in &config.lrsc_args {
            args.push(arg);
        }
        args.push(&task.obj.lock().path).unwrap();
        let args = args.finish().unwrap();

        let pid = process::fork(|| {
            let res = process::exec("lrsc", args);
            tryerr!(res, "Could not exec rustc");
        });
        let pid = tryerr!(pid, "Could not fork");

        let res = process::wait_id(pid, WAIT_EXITED);
        let res = tryerr!(res, "Could not wait for child process");

        if res != ChildStatus::Exited(0) {
            errexit!("rustc did not exit successfully: {:?}", res);
        }

        let end = time::MONO.get_time().unwrap();
        task.time = end - start;

        results.push_wait(task);
    }
}

struct Args {
    times: bool,
    finishes: bool,
    graph: bool,
    rebuild: bool,
    makefile: bool,
    target: &'static str,
    cfgs: Vec<&'static CStr>,
    tail: Vec<&'static CStr>,
}

fn print_help(code: u8) -> ! {
    const HELP: &'static str = "\
Usage: lrs_build [options]*
Options:
  -r --rebuild          Rebuild everything
  -t --target <TARGET>  Build for <TARGET> instead of the default target 
     --times            Print compile times after a successful exit
  -f --finish           Print when a compile step finishes
  -g --graph            Print the dependency graph and exit
     --cfg <OPT>        Add a configuration option
     --makefile         Generate a makefile
  -h --help             Print this help and exit";
    println!("{}", HELP);
    process::exit(code);
}

fn print_makefile(build: &Build, config: &Config) {
    println!(".PHONY: all\n");
    println!("native_target := $(shell lrsc -V -v | grep host | cut -d' ' -f 2)");
    println!("target ?= $(native_target)\n");
    println!("all: obj/$(target)/lib{}.rlib\n", build.target.lock().crate_name);

    fn print_obj(obj: &Mutex<Obj>, config: &Config) {
        if obj.lock().build_status != BuildStatus::Pending {
            return;
        }
        obj.lock().build_status = BuildStatus::Building;

        println!("-include obj/$(target)/{}.d", obj.lock().crate_name);
        print!("obj/$(target)/lib{}.rlib: ", obj.lock().crate_name);
        for dep in &obj.lock().deps {
            print!("obj/$(target)/lib{}.rlib ", dep.lock().crate_name);
        }
        print!("\n\tlrsc --emit=link,dep-info --out-dir obj/$(target) --target $(target) ");
        for arg in &config.lrsc_args {
            print!("{} ", arg);
        }
        println!("{}\n", obj.lock().path);

        for dep in &obj.lock().deps {
            print_obj(dep, config);
        }
    }

    print_obj(&build.target, config);
}

fn short_target_to_target(t: &[u8]) -> Option<&'static str> {
    match t {
        b"arm"     => Some(target::ARM),
        b"aarch64" => Some(target::AARCH64),
        b"i686"    => Some(target::I686),
        b"x86_64"  => Some(target::X86_64),
        b"lkern-kernel" => Some(target::LKERN_KERNEL),
        _ => None,
    }
}

fn parse_args() -> Args {
    let mut times = false;
    let mut finishes = false;
    let mut graph = false;
    let mut rebuild = false;
    let mut makefile = false;
    let mut target = target::DEFAULT;
    let mut cfgs = vec!();

    let mut args = env::args();
    args.next(); // skip program name

    let params = &[(Some('t'), Some("target"), false),
                   (None, Some("cfg"), false)];

    let mut getopts = Getopt::new(args, params);
    for (arg, param) in &mut getopts {
        match AsRef::<[u8]>::as_ref(arg) {
            b"r" | b"rebuild" => rebuild = true,
            b"t" | b"target" => {
                let param = match param {
                    Some(p) => p,
                    _ => errexit!("missing argument `-t`, `--target`"),
                };
                target = match short_target_to_target(param) {
                    Some(t) => t,
                    _ => errexit!("invalid target: {:?}", param),
                };
            },
            b"times" => times = true,
            b"f" | b"finish" => finishes = true,
            b"g" | b"graph" => graph = true,
            b"h" | b"help" => print_help(0),
            b"cfg" => {
               let param = match param {
                   Some(p) => p,
                   _ => errexit!("missing argument `--cfg`"),
               };
               cfgs.push(param);
            },
            b"makefile" => makefile = true,
            _ => errexit!("invalid argument: {:?}", arg),
        }
    }

    Args {
        times: times,
        finishes: finishes,
        graph: graph,
        rebuild: rebuild,
        target: target,
        cfgs: cfgs,
        makefile: makefile,
        tail: {
            let mut vec = Vec::new();
            vec.extend(env::args().consume(getopts.used()+1));
            vec
        },
    }
}

fn print_times(tasks: &mut [Task]) {
    tasks.sort_by(|t1, t2| t2.time.cmp(&t1.time));
    if tasks.len() == 0 {
        return;
    }
    println!("");
    for task in tasks {
        let obj = task.obj.lock();
        println!("{:3}.{:03}  {}", task.time.seconds, task.time.nanoseconds / 1_000_000,
                                   obj.name);
    }
}

fn print_graph(build: &Build) {
    println!("digraph deps {{");

    fn print(obj: &Obj, map: &mut HashMap<usize, ()>) {
        if map.get(&mem::addr(obj)).is_some() {
            return;
        }
        map.set(mem::addr(obj), ());
        for dep in &obj.deps {
            let dep = dep.lock();
            println!("{} -> {};", obj.name, dep.name);
            print(&dep, map);
        }
    }

    let mut map = HashMap::new().unwrap();
    print(&build.target.lock(), &mut map);

    println!("}}");
}

struct Config {
    lrsc_args: Vec<CString>,
    target: &'static str,
}

fn main() {
    let (top_dir, original) = find_conf();
    let args = parse_args();
    env::set_cwd(&*top_dir).unwrap();
    let build = parse_config(&top_dir, &original, &args.cfgs);

    let mut lrsc_args = vec!();
    for cfg in &build.cfgs {
        let c: &NoNullStr = "--cfg".try_as_ref().unwrap();
        lrsc_args.push(c.try_to().unwrap());
        lrsc_args.push(cfg.try_to().unwrap());
    }
    for cfg in &args.tail {
        lrsc_args.push((***cfg).try_to().unwrap());
    }

    let config = Config {
        lrsc_args: lrsc_args,
        target: args.target,
    };

    if args.makefile {
        print_makefile(&build, &config);
        return;
    }

    if args.graph {
        print_graph(&build);
        return;
    }

    build.target.lock().check_needs_rebuild(args.rebuild, args.target);

    let _ = file::create_dir("obj", MODE_DIRECTORY);

    let num_builders = thread::cpu_count().unwrap();

    let results = Queue::new(num_builders).unwrap();
    let requests = Queue::new(num_builders).unwrap();

    let mut builders: Vec<_> = Vec::with_capacity(num_builders).unwrap();

    for _ in 0..num_builders {
        let thread = thread::scoped(|| build_thread(&requests, &results, &config));
        let thread = tryerr!(thread, "Could not spawn threads");
        builders.push(thread);
    }

    let mut num_building = 0;
    let mut finished_tasks: Vec<_> = Vec::new();
    let mut cur_target = None;

    'outer: loop {
        if cur_target.is_none() {
            // Find the next target that can be built. If nothing new can be built and
            // there are no compilations running, then we're done.
            cur_target = match next_target(&build.target) {
                Ok(t) => Some(t),
                Err(false) if num_building == 0 => break,
                _ => None,
            };
        }

        if cur_target.is_some() && num_building < builders.len() {
            let cur_target = cur_target.take().unwrap();
            {
                let mut target = cur_target.lock();
                target.build_status = BuildStatus::Building;
                println!("  building {}", target.name);
            }
            requests.push_wait(Task::new(cur_target));
            num_building += 1;
        } else {
            let task = results.pop_wait();
            num_building -= 1;
            task.obj.lock().build_status = BuildStatus::Built;
            if args.finishes {
                println!("    finished {}", task.obj.lock().name);
            }
            finished_tasks.push(task);
        }
    }

    if args.times {
        print_times(&mut finished_tasks);
    }

    // We have no way to tell the builders that we're done so the join handles would try
    // to join indefinitely.
    process::exit(0);
}
