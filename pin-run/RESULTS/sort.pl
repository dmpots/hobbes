#!/usr/bin/perl


@tools = qw(opcodemix bblengthmix);
@destDirs = qw(H C parallel.ghc);

print "CLEANING OLD FILES\n";
for $tool (@tools) {
  for $dir (@destDirs) {
    if( -e "$dir.$tool") {
      $cmd = "rm -f $dir.$tool/*";
      print "$cmd\n";
      print `$cmd`;

      $cmd = "rmdir $dir.$tool";
      print "$cmd\n";
      print `$cmd`;
    }
  }
}
print "\n\n";

print "CREATING DIRECTORIES\n";
for $tool (@tools) {
  for $dir (@destDirs) {
    $cmd = "mkdir $dir.$tool";
    print "$cmd\n";
    print `$cmd`;
  }
}
print "\n\n";

print "SORTING C AND HASKELL FILES\n";
for $tool (@tools) {
  # Haskell
  $cmd = "find nofib.$tool/ shootout.ghc.$tool/ nofibpar.$tool/ dph.$tool/ -type f -exec cp {} H.$tool \\;";
  print "$cmd\n";
  print `$cmd`;

  # C
  $cmd = "find spec.gcc.$tool/ spec.icc.$tool/ shootout.gcc.$tool/ -type f -exec cp {} C.$tool \\;";
  print "$cmd\n";
  print `$cmd`;
}
print "\n\n";

print "COPYING PARALLEL FILES\n";
for $tool (@tools) {
  $cmd = "find nofibpar.$tool/ dph.$tool/ -type f -exec cp {} parallel.ghc.$tool \\;";
  print "$cmd\n";
  print `$cmd`;

}
print "\n\n";
