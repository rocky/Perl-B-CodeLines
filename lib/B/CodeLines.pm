package B::CodeLines;
# Copyright (C) 2012 Rocky Bernstein. All rights reserved.
# This program is free software; you can redistribute and/or modify it
# under the same terms as Perl itself.

use strict; use warnings; 

our $VERSION   = '1.1';

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

    foreach (values B::Utils::all_roots) {
        eval { B::Utils::walkoptree_simple( $_, $callback ); }
    }

    foreach (values B::Utils::all_starts) {
        eval { B::Utils::walkoptree_simple( $_, $callback ); }
    }

    foreach (values B::Utils::anon_subs) {
        eval { B::Utils::walkoptree_simple( $_, $callback ); }
    }

    # print "+++1 $current_file\n";
    # walksymtable(\%main::, 'print_subs', 1, 'B::Lines::');

}

sub main() {
    my ($pkg, $top_file, $rest) = caller(2);
    gather($top_file);
    foreach my $tuple (@results) { print "$tuple->[0]\n"; }
}

sub compile {
    return \&main
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
    gather(__FILE__);
    foreach my $tuple (@results) { 
	printf "%02d %0x\n", $tuple->[0], $tuple->[1]; 
    }
};

1;
