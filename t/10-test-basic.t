#!/usr/bin/env perl
use warnings; use strict;
use English;  use File::Spec;
use File::Basename;
use Test::More;

sub test_it($$$)
{
    my ($invocation, $expect_ary, $msg) = @_;
    $DEBUGGING=1;
    print $invocation, "\n" if $DEBUGGING;
    my @lines = `$invocation`;
    my $rc = $CHILD_ERROR >> 8;
    is($rc, 0, "$msg run successfully");
    map {chomp} @lines;
    is_deeply(\@lines, $expect_ary, "$msg output comparison:");
}

my $top_dir = File::Spec->catfile(dirname(__FILE__), '..');
# unless ($OSNAME eq 'MSWin32') {
#     my $test_prog = File::Spec->catfile($top_dir, qw(examples dolines.pl));
#     my $expect = [6, 7, 12, 13, 14, 15, 17, 19, 20, 21, 22, 23, 25, 26];
#     test_it("$EXECUTABLE_NAME $test_prog", $expect, 'file invocation');
# }

my $lib_dir = File::Spec->catfile($top_dir, 'lib');
my $code = '
# string exec form
your(\"Perl code\");
goes(\"here\");
     ';
test_it("$EXECUTABLE_NAME -I$lib_dir -MO=CodeLines,-exec -e \"$code\"",
	[3, 4], 'string invocation');

my $test_prog = File::Spec->catfile($top_dir, 'examples', 'three.pl');

test_it("$EXECUTABLE_NAME -I$lib_dir -MO=CodeLines $test_prog",
	["1", "1", "2", "3"], 'file invocation without counts');

test_it("$EXECUTABLE_NAME -I$lib_dir -MO=CodeLines,-c $test_prog",
	["1 2", "2 1", "3 1"], 'file invocation with counts');

# my $test_prog = File::Spec->catfile($top_dir, 'examples', 'five.pl');
# test_it("$EXECUTABLE_NAME -I$lib_dir -MO=CodeLines,-c $test_prog",
# 	["3 1", "5 1"], 'file invocation with subroutine');
done_testing;
