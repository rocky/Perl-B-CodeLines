package B::CodeLines;
# Copyright (C) 2012, 2015 Rocky Bernstein. All rights reserved.
# This program is free software; you can redistribute and/or modify it
# under the same terms as Perl itself.

use strict; use warnings;

our $VERSION   = '1.2';

use B qw(class);
use B::Utils;

#  my $current_file;
use vars qw(@results);

@results = ();

sub gather($) {
    my $top_file = shift;

    my $callback = sub {
        my $op = $_[0];
        $op->codelines($_[1])
            if 'COP' eq B::class($op) and $top_file eq $op->file;
    };

    # Enbugger->stop;

    foreach (values %{B::Utils::all_roots()}) {
        eval { B::Utils::walkoptree_simple( $_, $callback ); }
    }

    foreach (values %{B::Utils::all_starts()}) {
        eval { B::Utils::walkoptree_simple( $_, $callback ); }
    }

    foreach (@{B::Utils::anon_subs()}) {
        eval { B::Utils::walkoptree_simple( $_, $callback ); }
    }

    # print "+++1 $current_file\n";
    # walksymtable(\%main::, 'print_subs', 1, 'B::Lines::');

}

sub gather_counts($)
{
    my $results = shift;
    my %counts = ();
    foreach my $tuple (@{$results}) {
	$counts{$tuple->[0]}++;
    }
    return \%counts;
}

sub main($) {
    my $opts = shift;
    my ($pkg, $top_file, $rest) = caller(2);
    gather($top_file);
    if ($opts->{show_count}) {
	my $counts = gather_counts(\@results);
	foreach my $key (sort {$a <=> $b} keys(%{$counts})) {
	    print "$key $counts->{$key}\n";
	}
    } else  {
	foreach my $tuple (@results) { print "$tuple->[0]\n"; }
    }
}

sub compile {
    my $opts = {
	show_count => 0,
    };
    while (my $arg = shift @_) {
	if ($arg eq "-c") {
	    $opts->{show_count} = 1;
	}
    }

    main($opts);
}

sub B::OP::codelines {
    my($op) = @_;
    if ('COP' eq class($op)) {
        # $current_file = $op->file;
        push @B::CodeLines::results, [$op->line, +$op];
    }
}

# Demo code
unless (caller) {
    # A line like the one below with more than one statement should
    # appear more than once;
    my $file = __FILE__; my $tuple;
    gather($file);
    foreach $tuple (@results) {
	printf "%02d 0x%x\n", $tuple->[0], $tuple->[1];
    }
    print '-' x 20, "\n";
    my $counts = gather_counts(\@results);
    foreach my $key (sort {$a <=> $b} keys(%{$counts})) {
	print "$key $counts->{$key}\n";
    }
};

1;
