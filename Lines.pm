package B::Lines;
use strict; use warnings; 

our $VERSION   = "0.78";

use B qw(class main_start main_root main_cv svref_2object OPf_KIDS);

my $curcv;

# output handle, used with all Concise-output printing
our $walkHandle;	# public for your convenience
BEGIN { $walkHandle = \*STDOUT }

# use Enbugger;
sub concise_main {
    sequence(main_start);
    # Enbugger->stop;
    $curcv = main_cv;
    return if class(main_root) eq "NULL";
    walk_topdown(main_root,
		 sub { $_[0]->concise($_[1]) }, 0);
}

sub compile {
    return sub {
	concise_main();
	return ();	# something
    }
}

my $lastnext;	# remembers op-chain, used to insert gotos

no warnings 'qw'; # "Possible attempt to put comments..."; use #7

my %sequence_num;
my $seq_max = 1;

sub reset_sequence {
    # reset the sequence
    %sequence_num = ();
    $seq_max = 1;
    $lastnext = 0;
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
    return if class($op) eq "NULL" or exists $sequence_num{$$op};
    for (; $$op; $op = $op->next) {
	last if exists $sequence_num{$$op};
	my $name = $op->name;
	if ($name =~ /^(null|scalar|lineseq|scope)$/) {
	    next if $oldop and $ {$op->next};
	} else {
	    $sequence_num{$$op} = $seq_max++;
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

sub concise_op {
    my ($op) = @_;
    if ('COP' eq class($op)) {
	return sprintf "Line: %s\n", $op->line;
    }
    return '';
}

sub B::OP::concise {
    my($op) = @_;
    $lastnext = $op->next;
    print $walkHandle concise_op($op);
}

# B::OP::terse (see Terse.pm) now just calls this
sub b_terse {
    my($op) = @_;

    # This isn't necessarily right, but there's no easy way to get
    # from an OP to the right CV. This is a limitation of the
    # ->terse() interface style, and there isn't much to do about
    # it. In particular, we can die in concise_op if the main pad
    # isn't long enough, or has the wrong kind of entries, compared to
    # the pad a sub was compiled with. The fix for that would be to
    # make a backwards compatible "terse" format that never even
    # looked at the pad, just like the old B::Terse. I don't think
    # that's worth the effort, though.
    $curcv = main_cv unless $curcv;

    $lastnext = $op->next;
    print concise_op($op);
}

1;
