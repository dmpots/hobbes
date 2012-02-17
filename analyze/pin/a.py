#!/usr/bin/env python3
# Script to aggregate pin results to data files that can be fed to R

import os
import opcode
import sys
import bench

tools  = ['ibdetails','jumpmix','opcodemix']#['bblengthmix', 'ibdetails', 'jumpmix', 'opcodemix']
suites = ['fibon', 'spec']

def log(msg, *args):
    sys.stderr.write(msg.format(*args))
    sys.stderr.write("\n")

def main(args):
    if len(args) != 1:
        print('usage: ./a.py <RESULTS DIR>')
        sys.exit(1)

    root = args[0]
    for tool in tools:
        tool_files = []
        for suite in suites:
            result_dir = os.path.join(root, suite + '.' + tool)
            if os.path.exists(result_dir):
                for r in os.listdir(result_dir):
                    result_file = os.path.join(result_dir, r)
                    tool_files.append((suite, result_file))

        if len(tool_files):
            process(tool, tool_files)
        else:
            print("Found no tool files")

            
def process(tool, files):
    def process_with(p, files, outf):
        (suite, result_file) = files[0]
        files                = files[1:]
        p(outf, suite, result_file, header=True)
        for (suite, result_file) in files:
            p(outf, suite, result_file)

    outname = '{}.dat'.format(tool)
    outf = open(outname, 'w')
    print("writing {}".format(outname))
    #outf = sys.stdout

    if tool == 'jumpmix':
        process_with(jumpmix, files, outf)
    elif tool == 'opcodemix':
        process_with(opcodemix, files, outf)
    elif tool == 'ibdetails':
        process_with(ibdetails, files, outf)
    else:
        raise Exception('Unknown tool '+tool)

def jumpmix(outf, suite, result_file, header=False):
    benchmark = benchmark_name(suite, result_file)
    format_line = '{:10} {:10} {:20} {:20} {:>20}\n'
    Suite = suite.capitalize()

    if header:
        outf.write(format_line.format("Suite", "Group", "Benchmark", "Type", "Count"))
    for line in strip_comments(result_file):
        (n, jump, count, count_taken) = line.split()
        if (not jump.startswith('*')) and (not jump.startswith('syscall')):
            bmgroup = bench.group(benchmark)
            outf.write(format_line.format(Suite, bmgroup, benchmark, jump, count))

def opcodemix(outf, suite, result_file, header=False):
    benchmark = benchmark_name(suite, result_file)
    format_line = '{:10} {:10} {:15} {:10} {:10} {:>20}\n'
    Suite = suite.capitalize()

    if header:
        outf.write(format_line.format("Suite", "Group", "Benchmark",
                                      "Opcode", "Type", "Count"))
    for line in strip_comments(result_file):
        (n, op, c1, c2) = line.split()
        if not op.startswith('*'):
            opgroup = opcode.group(op)
            bmgroup = bench.group(benchmark)
            outf.write(format_line.format(Suite, bmgroup, benchmark,
                                          op, opgroup, int(c1)+int(c2)))

def ibdetails(outf, suite, result_file, header=False):
    benchmark = benchmark_name(suite, result_file)
    bmgroup   = bench.group(benchmark)
    Suite = suite.capitalize()
    format_line = "{:6} {:8} {:15} {:7} {:7} {:7} {:7} {:7} {:7}\n"
    header_line = 'Suite Group Benchmark Type Targets Sources Static Dynamic Locality'
    
    if header:
        outf.write(format_line.format(*header_line.split()))

    for line in strip_comments(result_file):
        if line.startswith("@"):
            branch_type = line[1:-2].upper()
            continue
        metrics = [m.strip() for m in line.split("|")]
        output = format_line.format(
            Suite, bmgroup, benchmark, branch_type,
            metrics[0], metrics[1], metrics[3], metrics[4], metrics[5])
        outf.write(output)

def benchmark_name(suite, result_file):
    components = os.path.basename(result_file).split('.')
    if suite == 'spec':
        name = "{}.{}".format(components[0], components[1])
    elif suite == 'fibon':
        dash = components[0].find('-')
        name = "{}".format(components[0][:dash])
    else:
        raise Exception('unknown suite ' + suite)
    return name
        
def strip_comments(result_file):
    with open(result_file, 'r') as f:
        for line in f:
            if line != '\n' and (not line.startswith('#')):
                yield(line)


if __name__ == "__main__":
    args = sys.argv[1:]
    main(args)


