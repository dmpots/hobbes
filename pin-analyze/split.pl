#!/usr/bin/perl

use File::Basename;

$ProgramClass  = "HaskellProgram|CProgram|NofibGhc|SpecGcc|SpecIcc";
$ProgramClass .= "|ShootoutGhc|ShootoutGcc";

# Find program class for these files
$label = shift @ARGV;
if ($label !~ /$ProgramClass/) {
    print "ERROR: must specify program class as first argument\n";
    print "ProgramClass is one of\n";
    $cs = join("\n  ", split(/\|/, $ProgramClass));
    print "  $cs\n";
}

foreach $file (@ARGV) {
    open FH, '<', $file;
    $mode = '';
    header: while (<FH>) {
        if(/^#.*\$(static).*/) {$mode= uc $1;last header;}
    }
    ($base, $dir, $ext) = fileparse($file, ".LOG");
    $outf = $dir.$base.".".$mode.$ext;

    open FH_STATIC, '>', $outf
        or die "Unable to open file $outf: $!";
    print FH_STATIC "$label\n";
    static: while (<FH>) {
        if(/^#.*\$(dynamic).*/) {$mode= uc $1; last static;}
        if(/^#.*/) {next;}
        if(/^$/)   {next;}

        ($opcode, $opname, $count) = split ' ', $_;
        print FH_STATIC "($opcode, $opname, $count)\n" 
            if $opname !~ /^\s*\*/
    }
    $outf = $dir.$base.".".$mode.$ext;
    open FH_DYNAMIC, '>', $outf
        or die "Unable to open file $outf: $!";
    print FH_DYNAMIC "$label\n";
    dynamic: while (<FH>) {
        if(/^#.*/) {next;}
        if(/^$/)   {next;}

        ($opcode, $opname, $count) = split ' ', $_;
        print FH_DYNAMIC "($opcode, $opname, $count)\n"
            if $opname !~ /^\s*\*/
    }
    
    close FH; 
    close FH_STATIC;
    close FH_DYNAMIC;
}



