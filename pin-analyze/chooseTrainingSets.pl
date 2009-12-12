#!/usr/bin/perl
use List::Util qw(shuffle);
use File::Copy "cp";
use File::Basename;
require 'programClass.pl';

$Pgm="chooseTrainingSets";
$RESULTS = "../pin-run/RESULTS";
$Nofib="$RESULTS/nofib";
$Spec_full="$RESULTS/spec.full";

$TrainDir="TRAIN";
$PredictDir="PREDICT";

$trainSize = shift @ARGV;
die usage() unless $trainSize =~ m/^\d+$/;

%allSets = @ARGV;
for my $label (keys %allSets) {
    my $dir = $allSets{$label};
    chop $dir if ($dir  =~ m/\/$/);
    $allSets{$label} = $dir;
    if(not checkClass($label)) {
        print "ERROR: Invalid Class: $label\n";
        print "Valid Classes:\n$ProgramClassStrings\n";
        $Err++;
        $usage++;
    }
    if(-d $dir) {
        my @files = <$dir/*.LOG>;
        if(@files < $trainSize) {
            my $numFiles = scalar(@files);
            print "ERROR: Not enough files($numFiles) in dir $dir\n";
            $Err++;
        }
    }
    else {
        print "ERROR: Invalid Directory: $dir\n";
        $Err++;
        $usage++;
    }
}
print usage() if $usage; chkErr();

if (not -e $TrainDir) {
    print "Making training directory: $TrainDir\n";
    mkdir $TrainDir || mkErr("Error making directory: $TrainDir");
}

if (not -e $PredictDir) {
    print "Making training directory: $PredictDir\n";
    mkdir $PredictDir || mkErr("Error making directory: $PredictDir");
}
chkErr();

print "Cleaning up old files in $TrainDir and $PredictDir\n";
for my $dir ($TrainDir, $PredictDir) {
    $cnt = unlink <$dir/*>;
    print "Deleted $cnt files in $dir\n";
}
#exit(0);

#copy files
for my $label (keys %allSets) {
    print "$label: Preparing $TrainDir and $PredictDir\n";
    my $dir = $allSets{$label};
    my @files = shuffle(<$dir/*.LOG>);

    my @trainSet = @files[0 .. ($trainSize-1)];
    my @predictSet = @files[$trainSize .. $#files];
    my $predictSize = $#predictSet + 1;

    print "Copying $trainSize files from $dir to training set in $TrainDir\n";
    for my $train (@trainSet) {
        #print "$train", "\n";
        cp($train, $TrainDir);
    }

    print "Copying $predictSize files from $dir to prediction set in $PredictDir\n";
    for my $predict (@predictSet) {
        #print "$predict", "\n";
        cp($predict, $PredictDir);
    }
    my @copiedTrainSet   = map {$TrainDir.  "/".basename($_)} @trainSet;
    my @copiedPredictSet = map {$PredictDir."/".basename($_)} @predictSet;

    print "Splitting and labeling files\n";
    $rc = system ("./label.pl", $label, @copiedTrainSet, @copiedPredictSet);
    if($rc != 0) {
        $Err++;
        print "ERROR: problem labeling files\n";
        chkErr();
    }

    print "Removing .LOG and .STATIC.LOG files\n";
         @deleteFiles = (@copiedTrainSet, @copiedPredictSet);
    push @deleteFiles, (<$TrainDir/*.STATIC.LOG>, <$PredictDir/*.STATIC.LOG>);
    $cnt = unlink @deleteFiles;
    print "DELETED $cnt FILES\n";
}

sub usage {
    return "usage: $Pgm <num-sets> [Label1 Dir1] ... [LabelN DirN]\n";
}

sub mkErr {
    ($msg) = @_;
    print("$msg\n"); 
    $Err++;
}

sub chkErr {
    if ($Err) {
        print "$Pgm: Too many errors, exiting\n";
        exit($Err);
    }
}
