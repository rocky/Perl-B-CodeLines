package B::CodeLines;
# Copyright (C) 2012, 2015 Rocky Bernstein. All rights reserved.
# This program is free software; you can redistribute and/or modify it
# under the same terms as Perl itself.

use strict; use warnings;

our $VERSION   = '1.2';

use B qw(class main_start main_root main_cv OPf_KIDS walksymtable);
use B::Utils;

my $current_self;

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

sub walk_topdown {
    my($op, $sub, $level) = @_;
    $sub->($op, $level);
    if ($op->flags & OPf_KIDS) {
	for (my $kid = $op->first; $$kid; $kid = $kid->sibling) {
	    walk_topdown($kid, $sub, $level + 1);
	}
    }
    elsif (class($op) eq "PMOP") {
	my $maybe_root = $op->pmreplroot;
	if (ref($maybe_root) and $maybe_root->isa("B::OP")) {
	    # It really is the root of the replacement, not something
	    # else stored here for lack of space elsewhere
	    walk_topdown($maybe_root, $sub, $level + 1);
	}
    }
}

# The structure of this routine is purposely modeled after op.c's peep()
sub sequence {
    my($op) = @_;
    my $oldop = 0;
    return if class($op) eq "NULL";
    for (; $$op; $op = $op->next) {
	my $name = $op->name;
	if ($name =~ /^(null|scalar|lineseq|scope)$/) {
	    next if $oldop and $ {$op->next};
	} else {
	    if (class($op) eq "LOGOP") {
		my $other = $op->other;
		$other = $other->next while $other->name eq "null";
		sequence($other);
	    } elsif (class($op) eq "LOOP") {
		my $redoop = $op->redoop;
		$redoop = $redoop->next while $redoop->name eq "null";
		sequence($redoop);
		my $nextop = $op->nextop;
		$nextop = $nextop->next while $nextop->name eq "null";
		sequence($nextop);
		my $lastop = $op->lastop;
		$lastop = $lastop->next while $lastop->name eq "null";
		sequence($lastop);
	    } elsif ($name eq "subst" and $ {$op->pmreplstart}) {
		my $replstart = $op->pmreplstart;
		$replstart = $replstart->next while $replstart->name eq "null";
		sequence($replstart);
	    }
	}
	$oldop = $op;
    }
}

sub B::OP::codelines {
    my($op) = @_;
    if ('COP' eq class($op)) {
	my $ary;
	if (!exists $current_self->{file}{$op->file}) {
	    $current_self->{file}{$op->file} = [];
	}
	$ary = $current_self->{file}{$op->file};
	push @{$ary}, [$op->line, +$op];
    }
}

sub B::GV::print_subs
  {
    my($gv) = @_;
    # Should bail if $gv->FILE ne $B::Lines::current_file.
    print $gv->NAME(), " ", $gv->FILE(), "\n";
    eval {
      walk_topdown($gv->CV->START,
		   sub { $_[0]->codelines($_[1]) }, 0)
    };
  };


sub main {
    my $self = __PACKAGE__->new(@prog_args);
    my ($pkg, $top_file, $rest) = caller(2);
    $current_self = $self;
    sequence(main_start);
    return if class(main_root) eq "NULL";
    walk_topdown(main_root,
		 sub { $_[0]->codelines($_[1]) }, 0);
    my $output_format = $self->{output_format};
    my $results = $current_self->{file}{$top_file};
    if ($output_format eq 'counts') {
	my $counts = $current_self->gather_counts();
	foreach my $key (sort {$a <=> $b} keys(%{$counts->{$top_file}})) {
	    print "$key $counts->{$top_file}{$key}\n";
	}
    } elsif ($output_format eq 'raw')  {
	foreach my $tuple (@{$results}) {
	    printf "%s 0x%x\n", $tuple->[0], $tuple->[1];
	}
    } else {
	foreach my $tuple (@{$results}) {
	    print $tuple->[0], "\n";
	}
    }
    # print "+++1 $current_file\n";
    # walksymtable(\%main::, 'print_subs', 1, 'B::Lines::');
}


=head3

compile() this gets called automatically when you run
perl -MO=Codelines as part of Perl's "compilation" phase.

=cut
sub compile {
    @prog_args = @_;
    return sub { main(); }
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
