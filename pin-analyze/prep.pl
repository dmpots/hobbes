#!/usr/bin/perl

$class = shift @ARGV;
@files = @ARGV;
print "Splitting Files: \n";
print (join "  \n", @files); print "\n";
system ("./label.pl", $class, @files);

sub usage {
    my ($die) = @_;
    print "usage: prep <ProgramClass> <dir>\n";
    exit($die) if $die;
}

