#!/usr/bin/perl


@tools = qw(opcodemix jumpmix regmix);

print "CLEANING OLD FILES\n";
for $tool (@tools) {
  $cmd = "rm H.$tool/* C.$tool/*";
  print "$cmd\n";
  print `$cmd`;
}

print "COPYING FILES\n";
for $tool (@tools) {
  # Haskell
  $cmd = "find nofib.$tool/ shootout.ghc.$tool/ -type f -exec cp {} H.$tool \\;";
  print "$cmd\n";
  print `$cmd`;

  # C
  $cmd = "find spec.gcc.$tool/ spec.icc.$tool/ shootout.gcc.$tool/ -type f -exec cp {} C.$tool \\;";
  print "$cmd\n";
  print `$cmd`;
}



