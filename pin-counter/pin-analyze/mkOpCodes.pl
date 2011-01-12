#!/usr/bin/perl

@Opcodes=();
while(<>) {
    ($opcode, $name)  = split;
    $Opcodes[$opcode] = $name;
}

$first = 1;
print "module Opcodes where\n\n";
print "data Opcode = \n";
for my $opcode (0..$#Opcodes) {
    $op = $Opcodes[$opcode];
    print "      $op\n" if     $first;
    print "    | $op\n" unless $first;
    $first = 0;
}
print "    deriving(Show, Read, Eq, Enum, Ord)\n";

#print "fromInt = \n";

