// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use std::file::{self, File};
use std::file::mode::{MODE_DIRECTORY};
use std::time::{self, Time};
use std::clone::{MaybeClone};
use std::rc::{Arc};
use std::sync::{Queue, Mutex};
use std::{env, mem, cmp, thread};
use std::process::{self, WAIT_EXITED, ChildStatus};
use std::string::{NoNullString, ByteString, CPtrPtr, AsByteStr, CStr};
use std::io::{BufReader, BufRead};
use std::iter::{IteratorExt};
use std::getopt::{Getopt};

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
            Err(e) => errexit!(concat!("lrs_build: ", $fmt, ": ({:?}))"), $($arg)*, e),
        }
    }};
}

/// Object containing the state of the build process.
struct Build {
    start: Option<NoNullString>,
    objs: Vec<Arc<Mutex<Obj>>>,
}

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

/// A single object in our build tree.
struct Obj {
    /// Name of the object, e.g., "core" or "file".
    name: ByteString,

    /// Whether the object need rebuilding.
    needs_rebuild: Option<bool>,

    /// If needs_rebuild is false, then this contains the date the object was built.
    obj_modified: Option<Time>,

    /// If needs_rebuild is true, then this contains the status of the built.
    build_status: BuildStatus,

    /// The dependencies of this object.
    deps: Vec<Arc<Mutex<Obj>>>,
}

impl Obj {
    /// Creates a new, empty object from the given name.
    fn from_name(n: &[u8]) -> Obj {
        Obj {
            name: ByteString::from_vec(n.to_owned().unwrap()),
            needs_rebuild: None,
            obj_modified: None,
            build_status: BuildStatus::Pending,
            deps: Vec::new(),
        }
    }

    /// Checks whether this object has to be rebuilt. Returns the result.
    ///
    /// After this function returns, the needs_rebuild field is `Some` for this object
    /// and, recursively, all of its dependencies.
    fn check_needs_rebuild(&mut self, force_rebuild: bool) -> bool {
        // The check was already run for this object. This can happen if this is a
        // dependency of multiple objects.
        if self.needs_rebuild.is_some() {
            return self.needs_rebuild.unwrap();
        }

        // Store the last time any of the objects dependencies was built.
        let mut last_dep_built = Time::seconds(0);
        for dep in &self.deps {
            let mut dep = dep.lock();
            if dep.check_needs_rebuild(force_rebuild) {
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

        // Generate the full name of the object. If the name is "lrs", then it refers to
        // the final crate "lrs" which is already its full name. Otherwise, e.g. if it's
        // "core" or "file", the full name has the "lrs_" prefix.
        let full_name: ByteString = if &self.name == "lrs" {
            "lrs".as_byte_str().to_owned().unwrap()
        } else {
            format!("lrs_{}", self.name)
        };
        let obj_path: ByteString = format!("obj/lib{}.rlib", full_name);

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
        let mut dep_buf = [0; 4096];
        let dep_path: ByteString = format!("obj/{}.d", full_name);
        let mut deps_file = match File::open_read(&dep_path) {
            Ok(f) => BufReader::buffered(f, &mut dep_buf),
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
fn find_conf() -> (NoNullString, Option<NoNullString>) {
    let mut path = NoNullString::new();
    tryerr!(env::get_cwd(&mut path), "Couldn't get cwd");

    let mut last = None;
    let mut second_to_last = None;
    loop {
        path.push_file("LRSBuild");
        let exists = file::exists(&path) == Ok(true);
        path.pop_file();
        if exists {
            return (path, second_to_last);
        }
        if path.len() == 0 {
            errexit!("Cannot find LRSBuild");
        }
        mem::swap(&mut last, &mut second_to_last);
        last = Some(path.pop_file().to_owned().unwrap());
    }
}

/// Parses the contents of LRSBuild.
fn parse_config(build: &mut Build) {
    let file = tryerr!(File::open_read("LRSBuild"), "Error opening LRSBuild");

    let mut buf = [0; 4096];
    let mut reader = BufReader::buffered(file, &mut buf);
    let mut line: Vec<_> = Vec::new();

    'outer: loop {
        line.truncate(0);
        'inner: loop {
            tryerr!(reader.copy_until(&mut line, b'\n'), "Error reading LRSBuild");
            match line.last() {
                Some(&b'\n') => { line.pop(); },
                None => break 'outer,
                _ => { },
            }
            match line.last() {
                Some(&b'\\') => { line.pop(); },
                _ => break 'inner,
            }
        }

        let mut words = line.split(|&b| b == b' ').filter(|w| w.len() > 0);

        if let Some(cmd) = words.next() {
            match cmd {
                b"obj" => parse_obj(build, words),
                _ => errexit!("unexpected predicate: {:?}", cmd.as_byte_str()),
            }
        }
    }
}

fn parse_obj<'a, I>(build: &mut Build, mut words: I)
    where I: Iterator<Item = &'a [u8]>,
{
    let name = match words.next() {
        Some(n) => n,
        _ => errexit!("obj without object name"),
    };

    let mut obj = Obj::from_name(name);
    for dep in words {
        let dep = dep.as_byte_str();
        match build.objs.find(|x| &x.lock().name == dep) {
            Some(p) => obj.deps.push(build.objs[p].add_ref()),
            _ => errexit!("can't find dependency {:?} of {:?}", dep, obj.name),
        }
    }

    build.objs.push(Arc::new().unwrap().set(Mutex::new(obj)));
}

/// Gets the object we're interested in building.
///
/// This is either the "lrs" object or the object in whose directory tree we are right
/// now, e.g., if we are in `src/file` or any subdirectory, then we only want to build
/// `file` and its dependencies.
fn get_base_obj(build: &Build) -> Arc<Mutex<Obj>> {
    let pos = build.start.as_ref().chain(|s| build.objs.find(|o| &o.lock().name == s));

    match pos {
        Some(p) => build.objs[p].add_ref(),
        _ => {
            let mut obj = Obj::from_name(b"lrs");
            obj.deps = build.objs.maybe_clone().unwrap();
            Arc::new().unwrap().set(Mutex::new(obj))
        },
    }
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
    let mut args: CPtrPtr = CPtrPtr::new().unwrap();
    let mut src_path: Vec<u8> = Vec::new();

    loop {
        let mut task = requests.pop_wait();
        let start = time::MONO.get_time().unwrap();

        src_path.truncate(0);
        write!(src_path, "src/{}/lib.rs", task.obj.lock().name).unwrap();

        args.clear();
        args.push("lrsc").unwrap();
        args.push("--emit=link,dep-info").unwrap();
        args.push("--out-dir=obj").unwrap();
        args.push("-Lobj").unwrap();
        for arg in &config.tail {
            args.push(arg);
        }
        args.push(&src_path).unwrap();
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

struct Config {
    times: bool,
    finishes: bool,
    graph: bool,
    rebuild: bool,
    tail: Vec<&'static CStr>,
}

fn print_help(code: u8) -> ! {
    const HELP: &'static str = "\
Usage: lrs_build [options]*
Options:
  -r --rebuild Rebuild everything
  -t --times   Print compile times after a successful exit
  -f --finish  Print when a compile step finishes
  -g --graph   Print the dependency graph and exit
  -h --help    Print this help and exit";
    println!("{}", HELP);
    process::exit(code);
}

fn parse_args() -> Config {
    let mut times = false;
    let mut finishes = false;
    let mut graph = false;
    let mut rebuild = false;

    let mut args = env::args();
    args.next(); // skip program name

    let mut getopts = Getopt::new(args, &[]);
    for (arg, _) in &mut getopts {
        match <AsRef<[u8]>>::as_ref(&arg) { // arg.as_ref():&[u8] ???
            b"r" | b"rebuild" => rebuild = true,
            b"t" | b"times" => times = true,
            b"f" | b"finish" => finishes = true,
            b"g" | b"graph" => graph = true,
            b"h" | b"help" => print_help(0),
            _ => errexit!("invalid argument: {:?}", arg),
        }
    }

    Config {
        times: times,
        finishes: finishes,
        graph: graph,
        rebuild: rebuild,
        tail: env::args().consume(getopts.used()+1).collect(),
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
    for obj in &build.objs {
        let obj = obj.lock();
        for dep in &obj.deps {
            let dep = dep.lock();
            println!("{} -> {};", obj.name, dep.name);
        }
    }
    println!("}}");
}

fn main() {
    let (top_dir, start) = find_conf();
    let args = parse_args();
    env::set_cwd(&top_dir).unwrap();
    let mut build = Build { start: start, objs: vec!() };
    parse_config(&mut build);

    if args.graph {
        print_graph(&build);
        return;
    }

    let target = get_base_obj(&build);
    target.lock().check_needs_rebuild(args.rebuild);

    let _ = file::create_dir("obj", MODE_DIRECTORY);

    let num_builders = thread::cpu_count().unwrap();

    let results = Queue::new(num_builders).unwrap();
    let requests = Queue::new(num_builders).unwrap();

    let mut builders: Vec<_> = Vec::with_capacity(num_builders).unwrap();

    for _ in 0..num_builders {
        let thread = thread::scoped(|| build_thread(&requests, &results, &args));
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
            cur_target = match next_target(&target) {
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
