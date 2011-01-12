#!/usr/bin/perl
use File::Basename;

if (@ARGV < 1) {
  print "usage: labelSpec.pl <LABEL> [file1]...[fileN]\n";
  exit 1;
}
$Label = shift @ARGV;
if($Label ne "icc" && $Label ne "gcc") {
  print "INVALID LABEL: $Label\n";
  print "usage: labelSpec.pl <LABEL> [file1]...[fileN]\n";
  exit 1;
}

while (@ARGV) {
  my ($file, $dir) = fileparse(shift @ARGV);
  my ($num,$name,@other) = split /\./, $file;
  my $rest = join(".", @other);
  my $newname = "$num.$name-$Label.$rest"; 
  print "$file --> $newname\n";
  rename("$dir/$file", "$dir/$newname") || die "rename failed: $!\n";

}

