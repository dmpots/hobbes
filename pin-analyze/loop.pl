#!/usr/bin/perl
#
#
$LoopSize   = 100;
$Method     = "svm"; # kmeans or svm
#@Tools      = qw(opcodemix bblengthmix);
@Tools      = qw(papi);
@TrainSizes = qw(02 04 06 08);
#@Cutoffs    = qw(0.00 0.01 0.02 0.03 0.04 0.05 0.10);
@Cutoffs    = qw(0.00);
#@ProgSets   = qw(
#  NH:NV:NM
#  TH:TV:TM
#  NH:TH
#
#  SG:SL
#  TG:TL
#
#  NH:SG
#  TH:TG
#  AH:AC
#);
#@ProgSets = qw(NV:IG NV:FG NM:IL NM:FL TV:TG TM:TL);
@ProgSets = qw(NV:SG NM:SL TV:TG TM:TL);
$SendProgressMail   = 0;

if($Method eq "kmeans") {
  @TrainSizes = qw(0);
}

$Total   = @Tools * @TrainSizes * @Cutoffs * @ProgSets;
$Current = 0;
print "$Total\n"; 

for my $spec      (@ProgSets)   {
for my $tool      (@Tools)      {
for my $trainSize (@TrainSizes) {
for my $cutoff    (@Cutoffs)    {
  $start = time();
  $Current++;
  $count   = "($Current/$Total)";
  $cleanSpec = $spec;
  $cleanSpec  =~ s/:/_/g;
  $outFile = "RESULTS/$Method.$tool.$trainSize.-$cutoff-.$cleanSpec";
  if( $Method eq "kmeans") {
    $cmd = "./runKmeans.pl $LoopSize $cutoff $spec $tool";
  }
  else {
    $cmd = "./runTraining.pl $LoopSize $trainSize $cutoff $spec $tool";
  }
  $mailCmd = "mail -s \"[TRAIN] finished\" dmp\@rice.edu < $outFile";

  print "$count\n";
  print "$cmd\n";
  system("echo \"$count\" > $outFile");
  system("$cmd >> $outFile");
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
        print "ERROR: $rc $!\n";
        exit 1;
    }
  }
  if($SendProgressMail) {
    print "$mailCmd\n";
    $rc = system($mailCmd);
    if($rc != 0) {
        print "ERROR: $!";
        exit 1;
    }
  }
  $end = time();
  printf "Iteration $Current took %d hours %d minutes and %d seconds\n",(gmtime ($end-$start))[2,1,0];
}
}
}
}

$mailCmd = "mail -s \"[PINALYZE] finished\" dmp\@rice.edu < /dev/null";
system($mailCmd);

