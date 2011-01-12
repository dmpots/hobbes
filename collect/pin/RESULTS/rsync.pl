#!/usr/bin/perl

$cmd = "rsync -avz haskell.cs.rice.edu:Research/darcs/pin-counter/pin-run/RESULTS/ .";
print $cmd, "\n";
print `$cmd`;
