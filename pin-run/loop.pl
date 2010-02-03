#!/usr/bin/perl
use File::Basename;
$PinTool = "bblengthmix";

#@commands = qw(
#              ../nofib-pin/nofib.commands 
#              ../nofibpar-pin/nofibpar.commands 
#              ../spec2006-pin/spec.gcc.commands 
#              ../dph-pin/dph.commands 
#              ../shootout-pin/shootout.ghc.commands 
#              ../shootout-pin/shootout.gcc.commands 
#           );
@commands = qw(
              ../nofib-pin/nofib.commands 
              ../nofibpar-pin/nofibpar.commands 
              ../dph-pin/dph.commands 
              ../shootout-pin/shootout.ghc.commands 
  );
 
for my $cmdFile (@commands) {
  print "Running command file:          $cmdFile\n";
  if (! -e $cmdFile) {
    print "Missing command file $cmdFile\n";
    exit 1;
  }
  #open RUN, "./t.pl|" or die "error running pintool\n";
  open RUN, "./runPin.pl --$PinTool $cmdFile|" or die "error running pintool\n";
  while (<RUN>) {
    print;
    if (/killing myself/i){print "Error running pin tool\n"; exit 1}
  }
  $destDir = basename($cmdFile);
  $destDir =~ s/\.commands//;
  $destDir.= ".$PinTool";
  $destDir = "RESULTS/$destDir";
  print "Creating dest dir     $destDir\n";
  system("mkdir $destDir");
  print "Moving results to dir $destDir\n";
  system("mv RESULTS/*.LOG $destDir");
  if ($? != 0) {print "ERROR running pin tool\n"; exit 1;}
  print "Finished running command file: $cmdFile\n";
}
