#!/usr/bin/perl
#
#
$LoopSize   = 100;
@Tools      = qw(opcodemix jumpmix regmix);
@TrainSizes = qw(02 04 06 08 10);
@Cutoffs    = qw(0.00 0.01 0.02 0.03 0.04 0.05 0.10);
@ProgSets   = qw(SHOOTOUT HGCC HICC SPEC);
#@ProgSets  = qw(ALL HGCC HICC SPEC);
$Total   = @Tools * @TrainSizes * @Cutoffs * @ProgSets;
$Current = 0;
print "$Total\n"; 
#exit 0;

for my $spec      (@ProgSets)   {
for my $tool      (@Tools)      {
for my $trainSize (@TrainSizes) {
for my $cutoff    (@Cutoffs)    {
  $start = time();
  $Current++;
  $count   = "($Current/$Total)";
  $outFile = "RESULTS/svm.$tool.$trainSize.-$cutoff-.$spec";
  $cmd = "./runTraining.pl $LoopSize $trainSize $cutoff $spec $tool | tee -a $outFile";
  $mailCmd = "mail -s \"[TRAIN] finished\" dmp\@rice.edu < $outFile";

  print "$count\n";
  print "$cmd\n";
  system("echo \"$count\" > $outFile");
  system($cmd);
  if($? == -1) {
    print "Failed to execute: $!\n";
    exit 1;
  }
  elsif($? & 127) {
    printf "Child died with signal: %d\n", ($? & 127);
    exit 1;
  }
  else {
    $rc = $? >> 8;
    if ($rc != 0) {
        print "ERROR: $!";
        exit 1;
    }
  }

  print "$mailCmd\n";
  $rc = system($mailCmd);
  if($rc != 0) {
      print "ERROR: $!";
      exit 1;
  }
  $end = time();
  printf "Iteration $Current took %d hours %d minutes and %d seconds\n",(gmtime ($end-$start))[2,1,0];
}
}
}
}


