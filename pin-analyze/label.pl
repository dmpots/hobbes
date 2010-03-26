#!/usr/bin/perl

use File::Basename;
#use strict;
require "programClass.pl";

# Find program class for these files
my $Pgm = "label.pl";
my $OPCODEMIX = "OpcodeMix";
my $JUMPMIX   = "JumpMix";
my $REGMIX    = "RegMix";
my $BLOCKMIX  = "BBLengthMix";
my $PAPIMIX   = "PapiMix";
my $label = shift @ARGV;
if(not checkClass($label)) {
    print "ERROR: must specify program class as first argument\n";
    print "ProgramClass is one of\n";
    my $cs = join("\n  ", @ProgramClasses);
    print "  $cs\n";
    exit 1;
}

foreach $file (@ARGV) {

    my $fileType = testFile($file);
    if($fileType eq $OPCODEMIX) {
      parseOpcodeMix($file);
    }
    elsif($fileType eq $JUMPMIX) {
      parseJumpMix($file);
    }
    elsif($fileType eq $REGMIX) {
      parseRegMix($file);
    }
    elsif($fileType eq $BLOCKMIX) {
      parseBlockMix($file);
    }
    elsif($fileType eq $PAPIMIX) {
      parsePapiMix($file);
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
    if (/^#(\s)*opcode/)      {$fileType = $OPCODEMIX;last;}
    if (/^#(\s)*JUMPMIX/)     {$fileType = $JUMPMIX;  last;}
    if (/^#(\s)*num(\s)+reg/) {$fileType = $REGMIX;  last;}
    if (/^#(\s)*block-length/){$fileType = $BLOCKMIX;  last;}
    if (/^(\s)*\(PAPI_/)      {$fileType = $PAPIMIX;  last;}
  }
  close FH;
  return $fileType;
}

sub parseRegMix {
  my $file = shift @_;
  my ($base, $dir, $ext) = fileparse($file, ".LOG");
  my $outf = $dir.$base.".REGMIX".$ext;

  open(FH, '<', $file) || die "unable to open file $file";
  open FH_DYNAMIC, '>', $outf
      or die "Unable to open file $outf: $!";
  print FH_DYNAMIC "$REGMIX\n";
  print FH_DYNAMIC "$label\n";

  header: while (<FH>) {
      if(/^#.*$/) {next header;}
      else        {last header;}
  }

  do {
    next if (/^#.*$/);

    if(/(\d+)\s+(r[\w]+)\s+(\d+)\s+(\d+)/) {
      my ($id, $name, $read, $written) = ($1, $2, $3, $4);
      $name = uc($name);

      print FH_DYNAMIC "(${name}_R, $read)\n";
      print FH_DYNAMIC "(${name}_W, $written)\n";
      #print "$id - $name - $read - $written\n";
    }

  } while(<FH>);

  close FH_DYNAMIC;
  close FH;

}

sub parseJumpMix {
  my $file = shift @_;
  my ($base, $dir, $ext) = fileparse($file, ".LOG");
  my $outf = $dir.$base.".JUMPMIX".$ext;

  open(FH, '<', $file) || die "unable to open file $file";
  open FH_DYNAMIC, '>', $outf
      or die "Unable to open file $outf: $!";
  print FH_DYNAMIC "$JUMPMIX\n";
  print FH_DYNAMIC "$label\n";

  header: while (<FH>) {
      if(/^#.*$/) {next header;}
      else        {last header;}
  }

  do {
    next if (/^#.*$/);

    if(/(\d+)\s+([\w-]+)\s+(\d+)\s+(\d+)/) {
      my ($id, $name, $seen, $taken) = ($1, $2, $3, $4);
      $name =~ s/indirect-(call|branch)/I\u\L$1/;
      $name = ucfirst($name);

      print FH_DYNAMIC "(${name}Seen,  $seen)\n";
      print FH_DYNAMIC "(${name}Taken, $taken)\n";
      #print "$id - $name - $seen - $taken\n";
    }

  } while(<FH>);

  close FH_DYNAMIC;
  close FH;
}


sub parseOpcodeMix {
    my $file = shift @_;

    open(FH, '<', $file) || die "unable to open file $file";
    my $mode = '';
    header: while (<FH>) {
        if(/^#.*\$(static).*/) {$mode= uc $1;last header;}
    }
    my ($base, $dir, $ext) = fileparse($file, ".LOG");
    my $outf = $dir.$base.".OPCODEMIX.".$mode.$ext;

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
    my $outf = $dir.$base.".OPCODEMIX.".$mode.$ext;
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

sub parseBlockMix {
  my $file = shift @_;
  my ($base, $dir, $ext) = fileparse($file, ".LOG");
  my $outf = $dir.$base.".BLOCKMIX".$ext;

  open(FH, '<', $file) || die "unable to open file $file";
  open FH_DYNAMIC, '>', $outf
      or die "Unable to open file $outf: $!";
  print FH_DYNAMIC "$BLOCKMIX\n";
  print FH_DYNAMIC "$label\n";

  header: while (<FH>) {
      if(/^#.*$/) {next header;}
      else        {last header;}
  }

  do {
    next if (/^#.*$/);

    if(/\s*(\d+)\s+(\d+)\s+\d+\.\d+/) {
      my ($size, $count) = ($1, $2);
      print FH_DYNAMIC "($size, $count)\n";
    }

  } while(<FH>);

  close FH_DYNAMIC;
  close FH;

}

sub parsePapiMix {
  my $file = shift @_;
  my ($base, $dir, $ext) = fileparse($file, ".LOG");
  my $outf = $dir.$base.".PAPIMIX".$ext;

  open(FH, '<', $file) || die "unable to open file $file";
  open FH_DYNAMIC, '>', $outf
      or die "Unable to open file $outf: $!";
  print FH_DYNAMIC "$PAPIMIX\n";
  print FH_DYNAMIC "$label\n";

  header: while (<FH>) {
      if(/^#.*$/) {next header;}
      else        {last header;}
  }

  do {
    next if (/^#.*$/);
    print FH_DYNAMIC;
  } while(<FH>);

  close FH_DYNAMIC;
  close FH;
}
