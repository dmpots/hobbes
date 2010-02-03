#!/usr/bin/perl
use Cwd;
use File::Basename;

$PinDir="$ENV{HOME}/pin";
$SimpleDir="$PinDir/source/tools/SimpleExamples/obj-intel64";
$InstMixPinTool="$SimpleDir/opcodemix.so";
$BBLenMixPinTool="$SimpleDir/bblengthmix.so";
$JumpMixPinTool="$SimpleDir/jumpmix.so";
$RegMixPinTool="$SimpleDir/regmix.so";
$DieOnNofibFailure=1;
$SanityCheckOnly=0;

$FullPathPinTool = $InstMixPinTool;
if(grep(/--opcodemix/i, @ARGV)) {
  $FullPathPinTool = $InstMixPinTool;
}
elsif(grep(/--bblengthmix|--bblenghtmix/i, @ARGV)) {
  $FullPathPinTool = $BBLenMixPinTool;
}
elsif(grep(/--jumpmix/i, @ARGV)) {
  $FullPathPinTool = $JumpMixPinTool;
}
elsif(grep(/--regmix/i, @ARGV)) {
  $FullPathPinTool = $RegMixPinTool;
}
@ARGV = grep(!/^--/, @ARGV); #remove switches
  

$Pintool=basename($FullPathPinTool);
$PinPrefix="setarch x86_64 -R $PinDir/pin -t $FullPathPinTool";

while(<>) {
    next if /^#/;
    chomp;
    my ($name,$dir,$cmd) = split(/\|/);
    runPin($name, $dir, $cmd);
}
print "runPin finished running successfully\n";

sub runPin {
    my ($name, $dir, $cmd) = @_;

    my $cwd = getcwd();
    my $pinCmd = "$PinPrefix -o $cwd/RESULTS/$name.$Pintool.$$.LOG -- $cmd\n";
    print "COMMAND: $pinCmd\n";
    chdir $dir or die "Can't cd to $dir: $!\n";
    SanityCheck($cmd);
    unless ($SanityCheckOnly) {
        system($pinCmd);
        if($? == -1) {
            print STDERR "Failed to execute: $!\n. Killing myself";
            exit 1;
        }
        elsif($? & 127) {
            printf STDERR "child died with signal %d\n. Killing myself", ($? & 127);
            exit 1;
        }
        else {
            my $exit_val  = $? >> 8;
            printf STDERR "child '$cmd' exited with value %d\n.", $exit_val;

            if ($exit_val != 0 && $DieOnNofibFailure) {
                print STDERR "Nonzero exit status. Killing myself\n";
                exit 1;
            }
        }
    }

    chdir $cwd or die "Can't cd to $cwd: $!\n";
}

sub SanityCheck {
    my ($cmd) = @_;
    my ($exe) = split(/\s+/, $cmd);
    if (not (-e $exe && -x $exe) ) {
        print STDERR "File $exe not executable\n";
        exit 1;
    }
    else {
        print STDERR "Checking $exe...ok\n";
    }
}

