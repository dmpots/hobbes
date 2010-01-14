#!/usr/bin/perl

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
  if    ($t =~ /indi/i)  {$ProgSets = 1;}
  elsif ($t =~ /hgcc/i)  {$ProgSets = 2;}
  elsif ($t =~ /hicc/i)  {$ProgSets = 3;}
  elsif ($t =~ /spec/i)  {$ProgSets = 4;}
  elsif ($t =~ /shoot/i) {$ProgSets = 5;}
  elsif ($t =~ /ssgcc/i) {$ProgSets = 6;}
  elsif ($t =~ /hnhs/i)  {$ProgSets = 7;}
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

  my $nofibGhc = "NofibGhc    ../pin-run/RESULTS/nofib.$PinTool";
  my $specGcc  = "SpecGcc     ../pin-run/RESULTS/spec.gcc.$PinTool";
  my $specIcc  = "SpecIcc     ../pin-run/RESULTS/spec.icc.$PinTool";
  my $shootGhc = "ShootoutGhc ../pin-run/RESULTS/shootout.ghc.$PinTool";
  my $shootGcc = "ShootoutGcc ../pin-run/RESULTS/shootout.gcc.$PinTool";
  my $setSize        = $TrainSize;
  my @classes        = ();
  if ($ProgSets == 0) {push @classes, ($Hprogs, $Cprogs);}
  if ($ProgSets == 1) {push @classes, ($nofibGhc, $specGcc, $specIcc);
                       push @classes, ($shootGhc, $shootGcc);}
  if ($ProgSets == 2) {push @classes, ($nofibGhc, $specGcc);}
  if ($ProgSets == 3) {push @classes, ($nofibGhc, $specIcc);}
  if ($ProgSets == 4) {push @classes, ($specGcc,  $specIcc);}
  if ($ProgSets == 5) {push @classes, ($shootGhc, $shootGcc);}
  if ($ProgSets == 6) {push @classes, ($shootGcc, $specGcc);}
  if ($ProgSets == 7) {push @classes, ($nofibGhc, $shootGhc);}
  if ($ProgSets  > 7) {print "Unknown ProgSets: $ProgSets"; exit 1;}

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
  $rc = system("$pinalyze -o PREDICT -f $threshold -v PREDICT/*");
  check($rc, "Error generating svm PREDICTION data");
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
  $predict = "./libsvm-2.9/svm-predict";
  $rc = system("$predict PREDICT.svm TRAIN.svm.model PREDICT.out\n");
  check($rc, "Error predicting");
}


sub check {
    my ($rc, $msg) = @_;
    if ($rc != 0) {
        print "$Pgm: $msg\n";
        exit $rc;
    }
}
