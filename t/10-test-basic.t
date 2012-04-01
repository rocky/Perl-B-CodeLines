#!/usr/bin/env perl
use warnings; use strict;
use English;  use File::Spec;
use File::Basename;
use Test::More 'no_plan';
my $test_prog = File::Spec->catfile(dirname(__FILE__),
				    qw(.. examples dolines.pl));
my @lines = `$EXECUTABLE_NAME $test_prog`;
my $rc = $CHILD_ERROR >> 8;
Test::More::is($rc, 0, 'dolines.pl run successfully');
map {chomp} @lines;
# print join(', ', @lines, "\n");
my @expect = (6, 7, 12, 13, 14, 15, 17, 19, 20, 21, 22, 23, 25, 26);
is_deeply(\@lines, \@expect, "statement boundaries match");
