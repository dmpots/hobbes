#!/usr/bin/perl
use Cwd;
use File::Basename;

$PinDir="$ENV{HOME}/pin";
$SimpleDir="$PinDir/source/tools/SimpleExamples/obj-intel64";
$InstMixPinTool="$SimpleDir/opcodemix.so";
$Pintool=basename($InstMixPinTool);
$PinPrefix="setarch x86_64 -R $PinDir/pin -t $InstMixPinTool";
$DieOnNofibFailure=1;
$SanityCheckOnly=0;

while(<>) {
    chomp;
    my ($name,$dir,$cmd) = split(/\|/);
    runPin($name, $dir, $cmd);
}

sub runPin {
    my ($name, $dir, $cmd) = @_;
    my ($exe) = split(/\s+/, $cmd);
    $exe =~ s#^\./##;

    my $cwd = getcwd();
    my $pinCmd = "$PinPrefix -o $cwd/RESULTS/$exe.$Pintool.$$.LOG -- $cmd\n";
    chdir $dir or die "Can't cd to $dir: $!\n";
    SanityCheck($cmd);
    unless ($SanityCheckOnly) {
        system($pinCmd);
        if($? == -1) {
            print STDERR "Failed to execute: $!\n";
            exit 1;
        }
        elsif($? & 127) {
            printf STDERR "child died with signal %d\n", ($? & 127);
            exit 1;
        }
        else {
            my $exit_val  = $? >> 8;
            printf STDERR "child '$cmd' exited with value %d\n", $exit_val;

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

