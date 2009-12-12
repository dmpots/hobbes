#!/usr/bin/perl

$Pgm = "trainModel";
$chooseTrainingSets = 1;
$pinalyze           = 1;
$grid               = 1;
$svm_train          = 1;
$svm_predict        = 1;

# choose training set
if($chooseTrainingSets) {
  print "Choosing training sets\n";
  $haskellProgram = "HaskellProgram ../pin-run/RESULTS/nofib";
  $specGcc        = "SpecGcc        ../pin-run/RESULTS/spec.full";

  $rc = system("./chooseTrainingSets.pl 10 $haskellProgram $specGcc");
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
  $threshold = 0.0;
  $rc = system("$pinalyze -o TRAIN   -f $threshold -v TRAIN/*");
  check($rc, "Error generating svm TRAINING data");
  $rc = system("$pinalyze -o PREDICT -f $threshold -v PREDICT/*");
  check($rc, "Error generating svm PREDICTION data");
}

# grid search for best params
if($grid) {
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
