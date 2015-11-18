package B::CodeLines;
# Copyright (C) 2012, 2015 Rocky Bernstein. All rights reserved.
# This program is free software; you can redistribute and/or modify it
# under the same terms as Perl itself.

use strict; use warnings;

our $VERSION   = '1.2';

use B qw(class);
use B::Utils;

#  my $current_file;
use vars qw(@prog_args);
@prog_args = ();

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    $self->{results} = ();
    $self->{output_format} = 'lines';
    while (my $arg = shift @_) {
    	if ($arg eq "-c") {
    	    $self->{output_format} = 'counts';
    	} elsif ($arg eq "-r") {
    	    $self->{output_format} = 'raw';
    	}
    }
    return $self;
}

=head3

gather($self, $top_file)
walks op tree accumulating COP addresses, i.e. those with
line numbers associated with them, provided the associated file is
equal to $top_file.

The resulting array of line number and opcode address
is stored in $self->{results}. And that is returned.

=cut
sub gather($$) {
    my ($self, $top_file) = @_;

    my $callback = sub {
        my $op = $_[0];
        push @{$self->{results}}, [$op->line, +$op]
	    if 'COP' eq B::class($op) and $top_file eq $op->file;
    };

    foreach (values %{B::Utils::all_roots()}) {
        eval { B::Utils::walkoptree_simple( $_, $callback ); }
    }

    foreach (values %{B::Utils::all_starts()}) {
        eval { B::Utils::walkoptree_simple( $_, $callback ); }
    }

    foreach (@{B::Utils::anon_subs()}) {
        eval { B::Utils::walkoptree_simple( $_, $callback ); }
    }
}

=head3

gather_counts($self)

Takes previously stored results in $self object and turns that into a
hash indexed by line number. The value is the number of times that
the line appears. The hash is returned.

=cut
sub gather_counts($)
{
    my $self = shift;
    my %counts = ();
    foreach my $tuple (@{$self->{results}}) {
	$counts{$tuple->[0]}++;
    }
    return \%counts;
}

=head3

gather_ops($self)

Takes previously stored results in $self object and turns that into a
hash indexed by line number. The value is the number are the opcodes
for that line.

=cut
sub gather_ops($)
{
    my $self = shift;
    my %counts = ();
    foreach my $tuple (@{$self->{results}}) {
	my ($key, $value) = @{$tuple};
	if (exists $counts{$key}) {
	    push @{$counts{$key}}, $value;
	} else {
	    @{$counts{$key}} = [$value];
	}
    }
    return \%counts;
}

sub main() {
    my $self = __PACKAGE__->new(@prog_args);
    my ($pkg, $top_file, $rest) = caller(2);
    $self->gather($top_file);
    my $output_format = $self->{output_format};
    if ($output_format eq 'counts') {
	my $counts = $self->gather_counts();
	foreach my $key (sort {$a <=> $b} keys(%{$counts})) {
	    print "$key $counts->{$key}\n";
	}
    } elsif ($output_format eq 'raw')  {
	foreach my $tuple (@{$self->{results}}) {
	    printf "%s 0x%x\n", $tuple->[0], $tuple->[1];
	}
    } else {
	foreach my $tuple (@{$self->{results}}) {
	    print $tuple->[0], "\n";
	}
    }
}

=head3

compile() this gets called automatically when you run
perl -MO=Codelines as part of Perl's "compilation" phase.

=cut
sub compile {
    @prog_args = @_;
    return \&main
}

# Demo code
unless (caller) {
    # A line like the one below with more than one statement should
    # appear more than once;
    my $file = __FILE__; my $tuple;

    my $cl = __PACKAGE__->new(@_);
    $cl->gather($file);
    print "line: address\n";
    foreach $tuple (@{$cl->{results}}) {
	printf "%4d: 0x%x\n", $tuple->[0], $tuple->[1];
    }
    print '-' x 20, "\n";
    print "line: counts\n";
    my $counts = $cl->gather_counts();
    foreach my $key (sort {$a <=> $b} keys(%{$counts})) {
	printf "%4d: %d\n", $key, $counts->{$key};
    }

    print '-' x 20, "\n";
    my $ops = $cl->gather_ops();
    print "line: addresses\n";
    foreach my $key (sort {$a <=> $b} keys(%{$ops})) {
	printf "%4d: [%s]\n", $key, join(', ', map sprintf("0x%x", $_), @{$ops->{$key}});
    }
};

1;
