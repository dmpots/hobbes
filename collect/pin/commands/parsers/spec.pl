#!/usr/bin/perl

$Pgm       = "findCommands";
$inCmdSec  = "";
$BenchName = "";
$BenchCmd  = "";
$BenchDir  = "";
while(<>) {
    if($inCmdSec) {
        if(!/^\s+/) { #end of commands section
            print "$BenchName|$BenchDir|$BenchCmd\n";
            $inCmdSec = $BenchName = $BenchCmd = $BenchDir = "";
        }
    }

    if(/Running (\d+)\.(\w+)/) {
        #print "BM: $1.$2\n";
        $BenchName = "$1.$2";
    }

    if(/^Commands to run:/) {
        $inCmdSec = 1;
        next;
    }

    if($inCmdSec) {
        #print if $inCmdSec;

        if(/\s+-C (.*)/) {
            $BenchDir = $1;
        }
        else {
            $BenchCmd = ParseArgs(split);
        }
    }

}

exit $Status;

sub ParseArgs {
    @Args = @_;
    my $cmdLine = "";
    my $out = $in = $err = "";

    while(@Args) {
        my $arg    =  shift @Args;
        #print "A: $arg\n";
        if   ($arg =~ /-o(.*)/) {$out = grab_arg_arg('-o', $1);}
        elsif($arg =~ /-i(.*)/) {$in  = grab_arg_arg('-i', $1);}
        elsif($arg =~ /-e(.*)/) {$err = grab_arg_arg('-e', $1);}
        #elsif($arg =~ /^-(.*)/) {print STDERR "UNKNOWN ARG: $arg\n"; 
        #                         $Status++;}
        else { #assume rest are command line args
            $cmdLine .= " $arg ";
            $cmdLine .= join(' ', @Args);
            last;
        }
    }

    if($in) { $cmdLine .= " < $in "; }
    $cmdLine =~ s/^\s+//;
    return $cmdLine;
}

sub grab_arg_arg {
    my ($option, $rest_of_arg) = @_;
    #print "PA: $option, $rest_of_arg, @Args\n";
    if ($rest_of_arg ne "") {
	    return($rest_of_arg);
    } 
    elsif ($#Args>= 0) {
	    my ($temp) = $Args[0]; shift(@Args); 
	    return($temp);
    }  
    else {
	    print STDERR "$Pgm: no argument following $option option\n";
	    $Status++;
    }
}
