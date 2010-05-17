#!/usr/bin/perl


#@tools = qw(opcodemix bblengthmix papi);
@tools = qw(papi);
@destDirs = qw(H C parallel.ghc parallel.ghc-llvm);
$cleanOnly = 0;
$parallelOnly = 0;

if (grep(/--clean/i, @ARGV)) {
  $cleanOnly = 1;
}

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

if($cleanOnly) {exit 0;}

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

  $cmd = "find nofibpar-llvm.$tool/ dph-llvm.$tool/ -type f -exec cp {} parallel.ghc-llvm.$tool \\;";
  print "$cmd\n";
  print `$cmd`;

  $cmd = "find nofibpar-viaC.$tool/ dph-viaC.$tool/ -type f -exec cp {} parallel.ghc-viaC.$tool \\;";
  print "$cmd\n";
  print `$cmd`;

}
print "\n\n";
