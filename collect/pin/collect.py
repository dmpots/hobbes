#!/usr/bin/env python3

import argparse
import datetime
import glob
import logging
import multiprocessing
import os
import os.path
import queue
import re
import subprocess
import sys
import shutil
import threading
import time

LOG_FORMAT="[{levelname}] {message}"
log = logging.getLogger()
PIN_LOG = 'pin.log'

class Log:
    @staticmethod
    def init(opts):
        lvl = logging.INFO
        if opts.debug:
            lvl = logging.DEBUG
        logging.basicConfig(format=LOG_FORMAT, style="{", level=lvl)

    @staticmethod
    def fatal_error(msg, *args):
        log.error(msg, *args)
        sys.exit(1)

    @staticmethod
    def debug(msg, *args):
        log.debug(msg, *args)

    @staticmethod
    def error(msg, *args):
        log.error(msg, *args)

    @staticmethod
    def info(msg, *args):
        log.info(msg, *args)
        
class FileCheck:
    @staticmethod
    def exists(filename, fatal=False):
        if not os.path.exists(filename):
            log = FileCheck.get_log(fatal)
            log("File %s does not exist", filename)
            return False
        return True

    @staticmethod
    def executable(filename, fatal=False):
        if not FileCheck.exists(filename, fatal):
            return False

        if not (os.path.isfile(filename) and os.access(filename, os.X_OK)):
            FileCheck.get_log(fatal)("File: " + filename + " is not executable")
            return False

        return True

    @staticmethod
    def get_log(fatal):
        if fatal:
            return Log.fatal_error
        else:
            return Log.error

class FileReader:
    @staticmethod
    def read_lines(filename):
        def non_comment(line):
            return line != "\n" and not line.startswith('#')

        lines = []
        with open(filename) as f:
            for line in f:
                if non_comment(line):
                    lines.append(line.rstrip('\n'))
        return lines

class Command:
    def __init__(self, fname, name, working_dir, exe, args, stdin, stdout, stderr):
        self.source = fname
        self.name = name
        self.dir  = working_dir
        self.exe  = exe
        self.args = args
        self.stdin = stdin
        self.stdout = stdout
        self.stderr = stderr
        
    @staticmethod
    def parse(fname, line):
        name,dir,command = line.split("|")
        args   = command.split()
        exe    = Command.make_abs(args.pop(0), dir)
        stdin  = Command.parse_redirect("<",  args, dir)
        stdout = Command.parse_redirect(">",  args, dir)
        stderr = Command.parse_redirect("2>", args, dir)

        c = Command(fname, name, dir, exe, args, stdin, stdout, stderr)
        Log.debug("Parsed Command: " + repr(c))
        return c

    @staticmethod
    def make_abs(path, wd):
        if not os.path.isabs(path):
            return os.path.join(wd, path)
        return path

    @staticmethod
    def parse_redirect(redir, args, working_dir):
        if not redir in args:
            return None

        # get redirected value and remove from list
        loc     = args.index(redir)
        _       = args.pop(loc) # >
        f_redir = args.pop(loc) # filename
        return Command.make_abs(f_redir, working_dir)

    def __str__(self):
        return self.name

    def __repr__(self):
        fields = [repr(f) for f in [self.name, self.dir, self.exe,
                                    self.args, self.stdin, self.stdout,
                                    self.stderr]]
        
        return "Command(" + ",".join(fields) + ")"

class PinError(Exception):
    def __init__(self, msg):
        self.message = msg
    pass

class PinTool:
    pin_dir   = os.environ['HOME'] + "/pin"
    tools_dir = pin_dir + "/source/tools/pin-tools";
    arch_dir  = "obj-intel64"
    dylib_ext = ".so"
    pin_exe   = os.path.join(pin_dir, "pin")
    
    def __init__(self, name):
        self.name = name
        self.tool_path = os.path.join(PinTool.tools_dir, name,
                                      PinTool.arch_dir,  name + PinTool.dylib_ext)

    def run(self, command):
        self.run_process(command)

    def run_process(self, command):
        outfile  = self.outfile_name(command)
        pin_args = [PinTool.pin_exe,
                    "-logfile", os.path.abspath(PIN_LOG),
                    "-t", self.tool_path,
                    "-o", outfile]
        cmd_args = [command.exe] + command.args
        full_args = pin_args + ["--"] + cmd_args

        (inh, outh, errh) = self.open_handles(command)

        try:
            p = subprocess.Popen(full_args,
                                 stdin=inh, stdout=outh, stderr=errh,
                                 cwd=command.dir, shell=False)
            p.wait()
            if p.returncode != 0:
                raise PinError("non-zero return: "+str(p.returncode))
            
        finally:
            if inh:  inh.close()
            if outh: outh.close()
            if errh: errh.close()

    def outfile_name(self,command):
        cwd   = os.path.abspath(os.path.curdir)
        pid   = str(os.getpid())
        tname = self.name
        
        return cwd + "/RESULTS/" + ".".join([command.name, tname, pid, "LOG"])

    def open_handles(self,command):
        def open_or_null(f, mode):
            if f:
                return open(f, mode)
            else:
                return open('/dev/null', mode)

        inh  = open_or_null(command.stdin,  'r')
        outh = open_or_null(command.stdout, 'w')
        errh = open_or_null(command.stderr, 'w')
        return (inh, outh, errh)
            
    
    def __str__(self):
        return self.name

class Task:
    def __init__(self, tool, command):
        self.tool    = tool
        self.command = command
        self.result  = "skipped"
        self.name    = self.tool.name +"@"+self.command.name

    def run(self):
        Log.info("Started:   => " + self.name)

        # make sure executable exists
        if not FileCheck.executable(self.command.exe):
            return self.set_failure()
        
        # make sure working directory exists
        if not FileCheck.exists(self.command.dir):
            return self.set_failure()
        
        try:
            self.tool.run(self.command)
        except PinError as e:
            Log.error("PinError: " + e.message)
            return self.set_failure()
        
        return self.set_success()

    def set_failure(self):
        self.result = "failed"
        Log.info("Failed:       "+self.name)
        return self

    def set_success(self):
        self.result = "passed"
        Log.info("Completed: <= "+self.name)
        return self

    def ok(self):
        return self.result == "passed"

    def destdir(self):
        base = re.sub('\.commands', '', os.path.basename(self.command.source))
        return 'RESULTS/'+base+"."+self.tool.name

    # Return a list of files created by this task
    def outfiles(self):
        # Some tools will add an extra paramater to the output file name to
        # handle collecting results from multiple threads. We explicitly look
        # for these file and return the full list of generated outputs
        filespec = self.tool.outfile_name(self.command)
        if os.path.exists(filespec):
            return [filespec]
        else:
            return glob.glob(filespec[0:-4] + "*.LOG")

class Config:
    def __init__(self, tools_file, commands_file):
        FileCheck.exists(tools_file, fatal=True)
        FileCheck.exists(commands_file, fatal=True)

        # Parse tools
        self.tools = [PinTool(t) for t in FileReader.read_lines(tools_file)]

        # Parse commands
        self.command_files = FileReader.read_lines(commands_file)
        self.commands = []
        for cfile in self.command_files:
            self.parse_commands_file(cfile)

        Log.info("Using %d tool(s):\n  %s", len(self.tools),
                 "\n  ".join(map(str, self.tools)))
        Log.info("Found %d commands in %d command files(s):\n  %s",
                  len(self.commands),  len(self.command_files),
                  "\n  ".join([str(c) for c in self.commands]))

    def parse_commands_file(self, cfile):
        lines = FileReader.read_lines(cfile)
        for line in lines:
            Log.debug("parsing line: %s", line)
            self.commands.append(Command.parse(cfile, line))

# Thread pool for runnin tasks concurrently
class Pool:
    def __init__(self, num_workers):
        self.num_workers = num_workers

    def run(self, tasks):
        inpq    = queue.Queue()
        statusq = queue.Queue()
        for task in tasks:
            inpq.put(task)

        for i in range(self.num_workers):
            threading.Thread(target=self.worker, args=(inpq,statusq)).start()

        status = threading.Thread(target=self.status, args=(statusq, len(tasks)))
        status.daemon = True
        status.start()
        
        inpq.join()

    def worker(self, q, statq):
        while not q.empty():
            task = q.get(block=True, timeout=1)
            task.run()
            q.task_done()
            statq.put_nowait(True) # counter of completed tasks

    def status(self, statq, total):
        # Only print status when the amount has changed
        finished = 0
        while finished != total:
            statq.get()
            finished += 1
            Log.info("%d of %d tasks completed (%6.2f%%) %s",
                     finished, total, 100 * (finished/total),
                     time.strftime("%H:%M:%S %A %m/%d/%Y"))

def parse_args(args):
    parser = argparse.ArgumentParser(description="Collect low-level measurements")
    parser.add_argument('-d', '--debug', action='store_true')
    parser.add_argument('-j', help="run tasks concurrently",
                        metavar='JOBS', type=int, default=1)
    return parser.parse_args(args)

def make_tasks():
    # Pars configurations
    conf = Config("tools.conf", "commands.conf")

    # Build tasks
    tasks = []
    for tool in conf.tools:
        for command in conf.commands:
            tasks.append(Task(tool, command))
    return tasks

def copy_results(task):
    dst = task.destdir()
    if not os.path.exists(dst):
        os.mkdir(dst)
        
    for src in task.outfiles():
        shutil.move(src, dst)
      
def main(args):
    start_time = time.time()
    opts = parse_args(args)
    Log.init(opts)

    tasks = make_tasks()

    # Run tasks
    Log.info("Running %d tasks in %d threads", len(tasks), opts.j)
    Pool(opts.j).run(tasks)

    # Move results into destination directories
    successes = 0
    for task in tasks:
        if task.ok():
            successes += 1
            copy_results(task)
    Log.info("%d of %d tasks completed successfully", successes, len(tasks))
    
    # Print how long we took
    end_time = time.time()
    duration = datetime.timedelta(seconds=int(end_time) - int(start_time))
    Log.info("Collection completed in %s", str(duration))
    
if __name__ == "__main__":
    args = sys.argv[1:]
    main(args)
