#!/usr/bin/perl

use File::Basename;
#use strict;
require "programClass.pl";

# Find program class for these files
my $Pgm = "label.pl";
my $OPCODEMIX = "OpcodeMix";
my $label = shift @ARGV;
if(not checkClass($label)) {
    print "ERROR: must specify program class as first argument\n";
    print "ProgramClass is one of\n";
    my $cs = join("\n  ", @ProgramClasses);
    print "  $cs\n";
}

foreach $file (@ARGV) {

    my $fileType = testFile($file);
    if($fileType eq $OPCODEMIX) {
      parseOpcodeMix($file);
    }
    else {
      print "$Pgm: UNKNOWN file type for file: $file\n";
      exit 1;
    }
}

sub testFile {
  my $fileName = shift @_;
  my $fileType = "UNKNOWN";

  open(FH, '<', $fileName) || die "unable to open file $fileName";
  while(<FH>) {
    if (/^#(\s)*opcode/) {$fileType = $OPCODEMIX; last;}
  }
  close FH;
  return $fileType;
}


sub parseOpcodeMix {
    my $file = shift @_;

    open(FH, '<', $file) || die "unable to open file $file";
    my $mode = '';
    header: while (<FH>) {
        if(/^#.*\$(static).*/) {$mode= uc $1;last header;}
    }
    my ($base, $dir, $ext) = fileparse($file, ".LOG");
    my $outf = $dir.$base.".".$mode.$ext;

    open FH_STATIC, '>', $outf
        or die "Unable to open file $outf: $!";
    print FH_STATIC "$OPCODEMIX\n";
    print FH_STATIC "$label\n";
    static: while (<FH>) {
        if(/^#.*\$(dynamic).*/) {$mode= uc $1; last static;}
        if(/^#.*/) {next;}
        if(/^$/)   {next;}

        my ($opcode, $opname, $count) = split ' ', $_;
        print FH_STATIC "($opcode, $opname, $count)\n" 
            if $opname !~ /^\s*\*/
    }
    my $outf = $dir.$base.".".$mode.$ext;
    open FH_DYNAMIC, '>', $outf
        or die "Unable to open file $outf: $!";
    print FH_DYNAMIC "$OPCODEMIX\n";
    print FH_DYNAMIC "$label\n";
    dynamic: while (<FH>) {
        if(/^#.*/) {next;}
        if(/^$/)   {next;}

        my ($opcode, $opname, $count) = split ' ', $_;
        print FH_DYNAMIC "($opcode, $opname, $count)\n"
            if $opname !~ /^\s*\*/
    }
    
    close FH; 
    close FH_STATIC;
    close FH_DYNAMIC;
}
