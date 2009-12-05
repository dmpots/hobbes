#!/usr/bin/perl

use File::Basename;

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
    static: while (<FH>) {
        if(/^#.*\$(dynamic).*/) {$mode= uc $1; last static;}
        if(/^#.*/) {next;}
        if(/^$/)   {next;}

        ($opcode, $opname, $count) = split ' ', $_;
        print FH_STATIC "($opcode, $opname, $count)\n" 
            if $opcode < 3000;
    }
    $outf = $dir.$base.".".$mode.$ext;
    open FH_DYNAMIC, '>', $outf
        or die "Unable to open file $outf: $!";
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



