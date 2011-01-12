#!/usr/bin/perl
use File::Basename;

($Pgm = $0) =~ s|.*/||;
%Progs = ();
@CmdLines = ();
@Dirs = ();
$Verbose = 0;
$Status = 0;
$NoFibHome = "/home/dave/ghc-BUILD/ghc-HEAD-BUILD/nofib";
if ( $ENV{'TMPDIR'} ) { # where to make tmp file names
    $TmpPrefix = $ENV{'TMPDIR'};
} else {
    $TmpPrefix ="$DEFAULT_TMPDIR";
    $ENV{'TMPDIR'} = "$DEFAULT_TMPDIR"; # set the env var as well
}

$hit = 0;
$pwd = "";
while (<>) {
    if(/^PWD = (.*)$/) {$pwd = $1;}
    if(/==nofib== (.*)?: time to run \1 follows/) {
        unless ($Progs{$1}) {
            $hit = 1;
            $Progs{$1} = $1;
        }
        next;
    };
    if ($hit) {
        $hit = 0;
        ($cmd) = split(/;/);
        $cmd =~ s#/usr/bin/time|(\.\./)*/?runstdtest/runstdtest##g;
        $cmd =~ s#^\s+##;
        push @CmdLines, $cmd
    }
}

foreach $cmd (@CmdLines) {
    #print "$cmd\n";
    #$cmd = $CmdLines{$pwd};
    @PgmArgs = ();
    $PgmExitStatus = 0;
    $PgmStdinFile  = '/dev/null';
    @argv  = split /\s+/, $cmd;
    $ToRun = $argv[0]; shift(@argv);
    ParseArgs(@argv);

    #print "$ToRun $PgmStdinFile @PgmArgs\n";
    $find_name = $ToRun;
    $find_name =~ s#\./##;
    $dir = dirname(`find $NoFibHome -name $find_name -type f | grep -v '_darcs'`);
    $bmName = basename($dir);

    print "$bmName|$dir|";
    print "$ToRun @PgmArgs < $PgmStdinFile \n"
}

sub ParseArgs {
    @Args = @_;
    arg: while($#Args >= 0) {
        $_ = shift @Args;
        #print "$_ -- FDSFS\n";
        /^--$/	&& do { # let anything past after --
                push(@PgmArgs, @Args);
                last arg; };

        /^-v$/	       && do { next arg; };
        /^-accept-output-stderr$/ && do { next arg; };
        /^-accept-output-stdout$/ && do { next arg; };
        /^-accept-output$/        && do { next arg; };

        /^-stdout-binary/ && do { next arg; };
        /^-stderr-binary/ && do { next arg; };

        /^-O(.*)/	&& do { push(@PgmArgs, &grab_arg_arg('-O',$1)); next arg; };
        /^-i(.*)/	&& do { $PgmStdinFile = &grab_arg_arg('-i',$1); next arg; };
        /^-fail/    && do { next arg; };
        /^-x(.*)/	&& do { $PgmExitStatus = &grab_arg_arg('-x',$1); 
                            print STDERR "$Pgm: CAN NOT HANDLE -x\n";
                            exit 1;
                            next arg; };
        /^-o1(.*)/	&& do { &grab_arg_arg('-o1',$1); next arg; };
        /^-o2(.*)/	&& do { &grab_arg_arg('-o2',$1); next arg; };
        /^-prescript(.*)/  && do { $PreScript = &grab_arg_arg('-prescript',$1);
                                print STDERR "$Pgm: CAN NOT HANDLE prescript\n";
                                exit 1;
                                next arg; };
        /^-postscript(.*)/ && do { $PostScript = &grab_arg_arg('-postscript',$1);
                                print STDERR "$Pgm: CAN NOT HANDLE postscript\n";
                                exit 1;
                                next arg; };
        /^-script/ && do { print STDERR "$Pgm: -script argument is obsolete;\nUse -prescript and -postscript instead.\n";
                $Status++;
                next arg; };
        /^-(ghc|hbc)-timing$/ && do { next arg; };
        /^-cpu-counting-(.)$/ && do { next arg; };
        /^-cachegrind$/ && do { next arg };
        /^-t(.*)/	&& do { &grab_arg_arg('-t', $1); next arg; };

        # anything else is taken to be a pgm arg
        push(@PgmArgs, $_);
    }
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

#shell command
#$TimeCmd /bin/sh -c \'$CachegrindPrefix $ToRun $TimingMagic @PgmArgs < $PgmStdinFile 1> $TmpPrefix/runtest$$.1.raw 2> $TmpPrefix/runtest$$.2.raw 3> $TmpPrefix/ runtest$$.3.raw\'
#
