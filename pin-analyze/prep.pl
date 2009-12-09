#!/usr/bin/perl

$class = shift @ARGV;
$dir   = shift @ARGV;
usage(1) unless -d $dir;
chop $dir if $dir[-1] == "/";

$cnt = unlink <$dir/*.DYNAMIC*.LOG>;
print "Deleted $cnt DYNAMIC files\n";
$cnt = unlink <$dir/*.STATIC*.LOG>;
print "Deleted $cnt STATIC files\n";

@files = <$dir/*.LOG>;
print "Splitting Files: \n";
print (join "  \n", @files); print "\n";
system ("./split.pl", $class, @files);

sub usage {
    my ($die) = @_;
    print "usage: prep <ProgramClass> <dir>\n";
    exit($die) if $die;
}

