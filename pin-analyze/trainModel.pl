#!/usr/bin/perl

BEGIN {$| = 1;} #autoflush
$Pgm = "trainModel";
$chooseTrainingSets = 1;
$pinalyze           = 1;
$grid               = 1;
$svm_train          = 1;
$svm_predict        = 1;

# params
$TrainSize = 5;
$Threshold = 0.0;
$SpecOnly  = 0;
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
  if ($t =~ /hgcc/i)  {$SpecOnly = 1;}
  if ($t =~ /hicc/i)  {$SpecOnly = 2;}
  if ($t =~ /spec/i)  {$SpecOnly = 3;}
}
if (@ARGV > 0) {
  my $t = shift @ARGV;
  if    ($t =~ /opcod/i){$PinTool = "opcodemix";}
  elsif ($t =~ /jump/i) {$PinTool = "jumpmix";}
  elsif ($t =~ /reg/i)  {$PinTool = "regmix";}
}

print "USING TrainSize = $TrainSize, Threshold = $Threshold, SpecOnly = $SpecOnly PinTool = $PinTool\n";

# choose training set
if($chooseTrainingSets) {
  print "Choosing training sets\n";
  my $haskellProgram = "HaskellProgram ../pin-run/RESULTS/nofib.$PinTool";
  my $specGcc        = "SpecGcc        ../pin-run/RESULTS/spec.gcc.$PinTool";
  my $specIcc        = "SpecIcc        ../pin-run/RESULTS/spec.icc.$PinTool";
  my $setSize        = $TrainSize;
  #my @classes        = ($haskellProgram, $specGcc, $specIcc);
  my @classes        = ();
  if ($SpecOnly == 0) {push @classes, ($haskellProgram, $specGcc, $specIcc);}
  if ($SpecOnly == 1) {push @classes, ($haskellProgram, $specGcc);}
  if ($SpecOnly == 2) {push @classes, ($haskellProgram, $specIcc);}
  if ($SpecOnly == 3) {push @classes, ($specGcc,        $specIcc);}

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
