#!/usr/bin/perl
use File::Basename;
use File::Copy;

$SortDestDir = dirname(__FILE__);
while (@ARGV) {
  my $file = shift @ARGV;
  if(-d $file) {
    chop $file if $file =~ m/\/$/;
    print "DIRECTORY: $file\n";
    @files = <$file/*.LOG> ;
    while (@files){
      my $subFile = shift @files;
      #print "FILE: $subFile\n";
      moveFile($subFile);
    }
  }
  elsif(-f $file) {
    #print "FILE: $file\n";
    moveFile($subFile);
  }
}

sub moveFile {
  my ($source) = @_;
  my $basename = basename($source);
  my $intDest = "$SortDestDir/spec.int/$basename";
  my $fpDest  = "$SortDestDir/spec.fp/$basename";
  #print "SI: $SpecIntMatch\n";
  #print "SF: $SpecIntMatch\n";

  if($basename =~ m/$SpecIntMatch/) {
    print "SPECINT: $source\n";
    #print "copy($source, $intDest);\n";
    copy($source, $intDest) || die "Error copying file: $!";
  }
  elsif($basename =~ m/$SpecFPMatch/) {
    print "SPECFP: $source\n";
    #print "copy($source, $fpDest);\n"
    copy($source, $fpDest)  || die "Error copying file: $!";
  }
  else {
    print "UNKNOWN: $source";
  }
}

BEGIN {
@specFP = qw(
    410.bwaves
    416.gamess
    433.milc
    434.zeusmp
    435.gromacs
    436.cactusADM
    437.leslie3d
    444.namd
    447.dealII
    450.soplex
    453.povray
    454.calculix
    459.GemsFDTD
    465.tonto
    470.lbm
    481.wrf
    482.sphinx3
    998.specrand
);
$SpecFPMatch = join("|", @specFP);

@specInt = qw(
    400.perlbench
    401.bzip2
    403.gcc
    429.mcf
    445.gobmk
    456.hmmer
    458.sjeng
    462.libquantum
    464.h264ref
    471.omnetpp
    473.astar
    483.xalancbmk
    999.specrand
);
$SpecIntMatch = join("|", @specInt);
}
