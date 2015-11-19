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
    $self->{file} = {};
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
is stored in $self->{results}.

=cut

sub gather($;$)
{
    my ($self, $collect_file) = @_;

    my $callback = sub {
        my $op = $_[0];
	return unless 'COP' eq B::class($op);
	return if defined $collect_file && $collect_file ne $op->file;
	my $ary;
	if (exists $self->{file}{$op->file}) {
	    $ary = $self->{file}{$op->file};
	} else {
	    $ary = $self->{file}{$op->file} = [];
	}
	push @{$ary}, [$op->line, +$op];
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

    $self->{file};
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
    foreach my $file (keys %{$self->{file}}) {
	my @ary = @{$self->{file}{$file}};
	foreach my $tuple (@ary) {
	    my $line = $tuple->[0];
	    if (not exists $counts{$file}) {
		$counts{$file} = {};
	    }
	    $counts{$file}{$line}++;
	}
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
    foreach my $file (keys %{$self->{file}}) {
	my @ary = @{$self->{file}{$file}};
	foreach my $tuple (@ary) {
	    my ($key, $value) = @{$tuple};
	    if (not exists $counts{$file}) {
		$counts{$file} = {};
	    }
	    if (exists $counts{$file}{$key}) {
		push @{$counts{$file}{$key}}, $value;
	    } else {
		$counts{$file}{$key} = [$value];
	    }
	}
    }
    return \%counts;
}

sub main() {
    my $self = __PACKAGE__->new(@prog_args);
   my ($pkg, $top_file, $rest) = caller(2);
    print $top_file, "\n";
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
    my $ary = $cl->{file}{$file};
    foreach $tuple (@{$ary}) {
    	printf "%4d: 0x%x\n", $tuple->[0], $tuple->[1];
    }

    # Redo with everything;
    $cl = __PACKAGE__->new(@_);
    $cl->gather();
    print '-' x 20, "\n";
    print "line: counts\n";
    my $all_counts = $cl->gather_counts();
    foreach my $file (keys %{$all_counts}) {
	print "File $file\n";
	my $counts = $all_counts->{$file};
	foreach my $key (sort {$a <=> $b} keys(%{$counts})) {
	    printf "%4d: %d\n", $key, $counts->{$key};
	}
    }

    print '-' x 20, "\n";
    print "line: addresses\n";
    my $all_ops = $cl->gather_ops();
    foreach my $file (keys %{$all_ops}) {
	print "File $file\n";
	my $ops = $all_ops->{$file};
	foreach my $key (sort {$a <=> $b} keys(%{$ops})) {
	    printf "%4d: [%s]\n", $key, join(', ', map sprintf("0x%x", $_), @{$ops->{$key}});
	}
    }
};

1;
