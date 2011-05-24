#!/usr/bin/perl

$cmd = "rsync -avz haskell.cs.rice.edu:Research/git/hobbes/collect/pin/RESULTS/ .";
print $cmd, "\n";
print `$cmd`;
