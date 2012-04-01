package B::CodeLines;
use strict; use warnings; 

our $VERSION   = '1.0';

use B qw(class main_start main_root main_cv OPf_KIDS walksymtable);

my $current_file;

# use Enbugger;
sub concise_main {
    sequence(main_start);
    # Enbugger->stop;
    return if class(main_root) eq "NULL";
    walk_topdown(main_root,
		 sub { $_[0]->concise($_[1]) }, 0);
    # print "+++1 $current_file\n";
    # walksymtable(\%main::, 'print_subs', 1, 'B::Lines::');

}

sub compile {
    return sub { concise_main(); }
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

sub concise_op {
    my ($op) = @_;
    if ('COP' eq class($op)) {
	$current_file = $op->file;
	return sprintf "%s\n", $op->line;
    }
    return '';
}

sub B::OP::concise {
    my($op) = @_;
    print concise_op($op);
}

sub B::GV::print_subs
  {
    my($gv) = @_;
    # Should bail if $gv->FILE ne $B::Lines::current_file.
    print $gv->NAME(), " ", $gv->FILE(), "\n";
    eval {
      walk_topdown($gv->CV->START,
		   sub { $_[0]->concise($_[1]) }, 0) 
    };
  };


1;
