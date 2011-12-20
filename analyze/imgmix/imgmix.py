#!/usr/bin/env python3

import os
import re
import sys


def find_logs(root):
    logs = []
    for (dirpath, _, filenames) in os.walk(root):
        for filename in filenames:
            if re.match(r".*[.]imgmix[.]\d+[.]LOG", filename):
                logs.append(os.path.join(dirpath, filename))
    return logs

def read_logs(logs):
    def clean(l):
        return os.path.basename(l.strip())
    libs = set()
    for log in logs:
        with open(log) as f:
            libs.update(map(clean, f.readlines()))
    return libs

def filter_libs(libs):
    for lib in libs:
        # Get rid of anything not a lib
        if not lib.startswith("lib"):
            continue

        # Get rid of Haskell runtime
        if lib.startswith("libHSrts"):
            continue

        # Allow libquantum for SPEC
        if lib.startswith("libquantum"):
            yield lib

        # Get rid of non-Haskell libraries
        if not lib.startswith("libHS"):
            continue

        yield lib

def normalize_libs(libs):
    def normalize(lib):
        dot = lib.find(".")
        dash= lib.find("-")

        if dot == -1 and dash == -1:
            return lib
        elif dot == -1:
            return lib[:dash]

        elif dash == -1:
            return lib[:dot]
        else:
            return lib[:min(dot,dash)]
        
    return map(normalize, libs)
            
    

def main(argv):
    if len(argv) != 1:
        print("usage: imgmix.py <rootdir>")
        sys.exit(1)

    root = argv[0]
    logs = find_logs(root)
    libs = read_logs(logs)
    libs = filter_libs(libs)
    libs = normalize_libs(libs)

    final_set = sorted(set(libs))
  
    print("  const char* AllowedNamesList[] = {", end="\n    ")
    print(",\n    ".join(map(lambda l: '"'+l+'"', final_set)))
    print("  };")

if __name__ == "__main__":
    main(sys.argv[1:])
    
