#!/usr/bin/env perl
use strict; use warnings;
use English;
use File::Basename;
my $dir = dirname(__FILE__) . '/..';
# Something to make sure we are recursing subroutines.
sub five() {
    5
}
my $file;
if (scalar @ARGV) {
    $file = shift @ARGV;
    die "Can't find file $file" unless -f $file;
} else {
    $file = __FILE__;
}
open FH, '<', $file or die $!;
local $INPUT_RECORD_SEPARATOR; # enable localized slurp mode
my $content = <FH>;
open STDERR, '>', '/dev/null' or die $!;
my $rc = system ($EXECUTABLE_NAME, "-I$dir", '-MO=Lines,-exec', '-e', 
		 $content);
unless (0 == $rc) {
    die "$file didn't parse\n";
}
