#!/usr/bin/perl
#
# ProgSets Key = $Compiler$Suite$Compiler$Suite where
# Compilers
#  H = Ghc
#  G = Gcc
#  I = Icc
#
# Suites
#  S = spec
#  T = shootout
#  N = nofib
#  P = parallel
#  R = nofibpar
#  D = dph 


BEGIN {$| = 1;} #autoflush
$Pgm = "trainModel";
$chooseTrainingSets = 1;
$pinalyze           = 1;
$grid               = 1;
$svm_train          = 1;
$svm_predict        = 1;

if (grep(/--KMEANS/i, @ARGV) ) {
  $pinalyze           = 0;
  $grid               = 0;
  $svm_train          = 0;
  $svm_predict        = 0;
  print "--KMEANS detected. Only running chooseTrainingSets.pl\n";
}

# params
$TrainSize = 5;
$Threshold = 0.0;
$ProgSets  = 0;
$PinTool   = "opcodemix";
if (@ARGV > 0) {
  my $t = shift @ARGV;
  if ($t !~ /^\d+$/) {print "BAD TrainSize: $t\n"; exit 1;}
  $TrainSize = $t;
}
if (@ARGV > 0) {
  my $t = shift @ARGV;
  if ($t !~ /^\d+(\.\d+)?$/) {print "BAD Threshold: $t\n"; exit 1;}
  if ($t > 1.0)      {print "BAD Threshold: $t\n"; exit 1;}
  $Threshold = $t;
}
if (@ARGV > 0) {
  my $t = shift @ARGV;
  $ProgSets = $t;
}
if (@ARGV > 0) {
  my $t = shift @ARGV;
  if    ($t =~ /opcod/i){$PinTool = "opcodemix";}
  elsif ($t =~ /jump/i) {$PinTool = "jumpmix";}
  elsif ($t =~ /reg/i)  {$PinTool = "regmix";}
  else {print "BAD PinTool: $t"; exit 1;}
}

print "USING TrainSize = $TrainSize, Threshold = $Threshold, ProgSets = $ProgSets PinTool = $PinTool\n";

# choose training set
if($chooseTrainingSets) {
  print "Choosing training sets\n";
  my $Hprogs   = "HaskellProgram ../pin-run/RESULTS/H.$PinTool";
  my $Cprogs   = "CProgram       ../pin-run/RESULTS/C.$PinTool";

  my $nofibGhc    = "NofibGhc    ../pin-run/RESULTS/nofib.$PinTool";
  my $specGcc     = "SpecGcc     ../pin-run/RESULTS/spec.gcc.$PinTool";
  my $specIcc     = "SpecIcc     ../pin-run/RESULTS/spec.icc.$PinTool";
  my $shootGhc    = "ShootoutGhc ../pin-run/RESULTS/shootout.ghc.$PinTool";
  my $shootGcc    = "ShootoutGcc ../pin-run/RESULTS/shootout.gcc.$PinTool";
  my $parallelGhc = "ParallelGhc ../pin-run/RESULTS/parallel.ghc.$PinTool";
  my $setSize     = $TrainSize;
  my @classes     = ();
  if    ($ProgSets =~ /all/i) {@classes = ($Hprogs, $Cprogs);}
  elsif ($ProgSets =~ /indi/i){@classes = ($nofibGhc, $specGcc, $specIcc);}
  elsif ($ProgSets =~ /HNGS/i){@classes = ($nofibGhc, $specGcc);}
  elsif ($ProgSets =~ /HNIS/i){@classes = ($nofibGhc, $specIcc);}
  elsif ($ProgSets =~ /GSIS/i){@classes = ($specGcc,  $specIcc);}
  elsif ($ProgSets =~ /HTGT/i){@classes = ($shootGhc, $shootGcc);}
  elsif ($ProgSets =~ /GTGS/i){@classes = ($shootGcc, $specGcc);}
  elsif ($ProgSets =~ /HNHT/i){@classes = ($nofibGhc, $shootGhc);}
  elsif ($ProgSets =~ /HTGS/i){@classes = ($shootGhc, $specGcc);}
  elsif ($ProgSets =~ /HPGS/i){@classes = ($parallelGhc, $specGcc);}
  elsif ($ProgSets =~ /HPHN/i){@classes = ($parallelGhc, $nofibGhc);}
  else  {print "Unknown ProgSets: $ProgSets"; exit 1;}

  $rc = system("./chooseTrainingSets.pl $setSize @classes");
  check($rc, "Error choosing training sets");
}

# remove old svm data
if($pinalyze) {
  print "Removing old .svm data\n";
  for my $file (<*.svm>) {
      print "Deleting file $file\n";
      unlink $file;
  }

# generate svm formatted data
  print "Generating svm formatted data\n";
  $pinalyze = "./pinalyze";
  $threshold = $Threshold;
  $rc = system("$pinalyze -o TRAIN   -f $threshold -v TRAIN/*");
  check($rc, "Error generating svm TRAINING data");
}

# grid search for best params
if($grid) {
  print "Searching for svm parameters\n";
  $gridSearch = "libsvm-2.9/tools/grid.py -svmtrain libsvm-2.9/svm-train";
  $rc = system("$gridSearch TRAIN.svm > _T 2>/dev/null");
  check($rc, "Error searching for parameters");

  ($cParam, $gammaParam) = split(' ', `tail -1 _T`);
  unlink "TRAIN.svm.out";
  unlink "TRAIN.svm.png";
  unlink "_T";
}

# svm train
if($svm_train) {
    print "Training model into file TRAIN.svm.model\n";
    print "  using C = $cParam, Gamma = $gammaParam\n";
    $train = "./libsvm-2.9/svm-train -s 0 -t 2 -g $gammaParam -c $cParam";
    $rc = system("$train TRAIN.svm TRAIN.svm.model");
    check($rc, "Error training model");
}

# svm predict
if($svm_predict) {
  print "Predicting with model\n";
  unlink 'PREDICT.out';
  #$rc = system("$pinalyze -o PREDICT -f $threshold -v PREDICT/*");
  #check($rc, "Error generating svm PREDICTION data");
  #$predict = "./libsvm-2.9/svm-predict";
  #$rc = system("$predict PREDICT.svm TRAIN.svm.model PREDICT.out\n");
  $rc = system("./pinalyze -m TRAIN.svm.model -f $Threshold PREDICT/*\n");
  check($rc, "Error predicting");
}


sub check {
    my ($rc, $msg) = @_;
    if ($rc != 0) {
        print "$Pgm: $msg\n";
        exit $rc;
    }
}
