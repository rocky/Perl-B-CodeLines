package B::CodeLines;
# Copyright (C) 2012 Rocky Bernstein. All rights reserved.
# This program is free software; you can redistribute and/or modify it
# under the same terms as Perl itself.

use strict; use warnings; 

our $VERSION   = '1.1';

use B qw(class);
use B::Utils;

#  my $current_file;

sub main() {
    my ($pkg, $top_file, $rest) = caller(2);

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

sub compile {
    return \&main
}

sub B::OP::codelines {
    my($op) = @_;
    if ('COP' eq class($op)) {
        # $current_file = $op->file;
        printf "%s\n", $op->line;
    }
}

1;
