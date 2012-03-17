package B::Lines;
# Note: we need to keep track of how many use declarations/BEGIN
# blocks this module uses, so we can avoid printing them when user
# asks for the BEGIN blocks in her program. Update the comments and
# the count in concise_specials if you add or delete one. The
# -MO=Concise counts as use #1.

use strict; # use #2
use warnings; # uses #3 and #4, since warnings uses Carp

our $VERSION   = "0.78";

# use #6
use B qw(class main_start main_root main_cv cstring svref_2object
	 SVf_IOK SVf_NOK SVf_POK SVf_IVisUV SVf_FAKE OPf_KIDS OPf_SPECIAL
	 CVf_ANON PAD_FAKELEX_ANON PAD_FAKELEX_MULTI SVf_ROK);


# rendering mechanics:
# these 'formats' are the line-rendering templates
# they're updated from %style when $stylename changes

# lesser players:
my $do_main = 0;	# force printing of main routine

# another factor: can affect all styles!

my $curcv;

# output handle, used with all Concise-output printing
our $walkHandle;	# public for your convenience
BEGIN { $walkHandle = \*STDOUT }

sub concise_subref {
    my($coderef, $name) = @_;
    my $codeobj = svref_2object($coderef);

    return concise_stashref(@_)
	unless ref $codeobj eq 'B::CV';
    concise_cv_obj($codeobj, $name);
}

sub concise_stashref {
    my($h) = @_;
    local *s;
    foreach my $k (sort keys %$h) {
	next unless defined $h->{$k};
	*s = $h->{$k};
	my $coderef = *s{CODE} or next;
	reset_sequence();
	print "FUNC: ", *s, "\n";
	my $codeobj = svref_2object($coderef);
	next unless ref $codeobj eq 'B::CV';
	eval { concise_cv_obj($codeobj, $k) };
	warn "err $@ on $codeobj" if $@;
    }
}

# This should have been called concise_subref, but it was exported
# under this name in versions before 0.56
*concise_cv = \&concise_subref;

sub concise_cv_obj {
    my ($cv, $name) = @_;
    # name is either a string, or a CODE ref (copy of $cv arg??)

    $curcv = $cv;

    if (ref($cv->XSUBANY) =~ /B::(\w+)/) {
	print $walkHandle "$name is a constant sub, optimized to a $1\n";
	return;
    }
    if ($cv->XSUB) {
	print $walkHandle "$name is XS code\n";
	return;
    }
    if (class($cv->START) eq "NULL") {
	no strict 'refs';
	if (ref $name eq 'CODE') {
	    print $walkHandle "coderef $name has no START\n";
	}
	elsif (exists &$name) {
	    print $walkHandle "$name exists in stash, but has no START\n";
	}
	else {
	    print $walkHandle "$name not in symbol table\n";
	}
	return;
    }
    sequence($cv->START);
    my $root = $cv->ROOT;
    unless (ref $root eq 'B::NULL') {
	walk_topdown($root, sub { $_[0]->concise($_[1]) }, 0);
    } else {
	print $walkHandle "B::NULL encountered doing ROOT on $cv. avoiding disaster\n";
    }
}

# use Enbugger;
sub concise_main {
    sequence(main_start);
    # Enbugger->stop;
    $curcv = main_cv;
    return if class(main_root) eq "NULL";
    walk_topdown(main_root,
		 sub { $_[0]->concise($_[1]) }, 0);
}

sub concise_specials {
    my($name, @cv_s) = @_;
    my $i = 1;
    if ($name eq "BEGIN") {
	splice(@cv_s, 0, 8); # skip 7 BEGIN blocks in this file. NOW 8 ??
    } elsif ($name eq "CHECK") {
	pop @cv_s; # skip the CHECK block that calls us
    }
    for my $cv (@cv_s) {
	print $walkHandle "$name $i:\n";
	$i++;
	concise_cv_obj($cv, $name);
    }
}

my @render_packs; # collect -stash=<packages>

sub compile {
    my (@args) = ();
    return sub {
	my @newargs = ();
	warn "disregarding non-options: @newargs\n" if @newargs;

	for my $objname (@args) {
	    next unless $objname; # skip null args to avoid noisy responses

	    if ($objname eq "BEGIN") {
		concise_specials("BEGIN",
				 B::begin_av->isa("B::AV") ?
				 B::begin_av->ARRAY : ());
	    } elsif ($objname eq "INIT") {
		concise_specials("INIT", 
				 B::init_av->isa("B::AV") ?
				 B::init_av->ARRAY : ());
	    } elsif ($objname eq "CHECK") {
		concise_specials("CHECK",
				 B::check_av->isa("B::AV") ?
				 B::check_av->ARRAY : ());
	    } elsif ($objname eq "UNITCHECK") {
		concise_specials("UNITCHECK",
				 B::unitcheck_av->isa("B::AV") ?
				 B::unitcheck_av->ARRAY : ());
	    } elsif ($objname eq "END") {
		concise_specials("END",
				 B::end_av->isa("B::AV") ?
				 B::end_av->ARRAY : ());
	    }
	    else {
		# convert function names to subrefs
		my $objref;
		if (ref $objname) {
		    $objref = $objname;
		} else {
		    $objname = "main::" . $objname unless $objname =~ /::/;
		    print $walkHandle "$objname:\n";
		    no strict 'refs';
		    unless (exists &$objname) {
			print $walkHandle "err: unknown function ($objname)\n";
			return;
		    }
		    $objref = \&$objname;
		}
		concise_subref($objref, $objname);
	    }
	}
	for my $pkg (@render_packs) {
	    no strict 'refs';
	    concise_stashref(\%{$pkg.'::'});
	}

	if (!@args or $do_main or @render_packs) {
	    print $walkHandle "main program:\n" if $do_main;
	    concise_main();
	}
	return @args;	# something
    }
}

my $lastnext;	# remembers op-chain, used to insert gotos

no warnings 'qw'; # "Possible attempt to put comments..."; use #7

my $chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

my %sequence_num;
my $seq_max = 1;

sub reset_sequence {
    # reset the sequence
    %sequence_num = ();
    $seq_max = 1;
    $lastnext = 0;
}

sub seq {
    return '';
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

our %priv; # used to display each opcode's BASEOP.op_private values

sub concise_sv {
    my($sv, $hr, $preferpv) = @_;
    $hr->{svclass} = class($sv);
    $hr->{svclass} = "UV"
      if $hr->{svclass} eq "IV" and $sv->FLAGS & SVf_IVisUV;
    Carp::cluck("bad concise_sv: $sv") unless $sv and $$sv;
    $hr->{svaddr} = sprintf("%#x", $$sv);
    if ($hr->{svclass} eq "GV" && $sv->isGV_with_GP()) {
	my $gv = $sv;
	my $stash = $gv->STASH->NAME; if ($stash eq "main") {
	    $stash = "";
	} else {
	    $stash = $stash . "::";
	}
	$hr->{svval} = "*$stash" . $gv->SAFENAME;
	return "*$stash" . $gv->SAFENAME;
    } else {
	if ($] >= 5.011) {
	    while (class($sv) eq "IV" && $sv->FLAGS & SVf_ROK) {
		$hr->{svval} .= "\\";
		$sv = $sv->RV;
	    }
	} else {
	    while (class($sv) eq "RV") {
		$hr->{svval} .= "\\";
		$sv = $sv->RV;
	    }
	}
	if (class($sv) eq "SPECIAL") {
	    $hr->{svval} .= ["Null", "sv_undef", "sv_yes", "sv_no"]->[$$sv];
	} elsif ($preferpv && $sv->FLAGS & SVf_POK) {
	    $hr->{svval} .= cstring($sv->PV);
	} elsif ($sv->FLAGS & SVf_NOK) {
	    $hr->{svval} .= $sv->NV;
	} elsif ($sv->FLAGS & SVf_IOK) {
	    $hr->{svval} .= $sv->int_value;
	} elsif ($sv->FLAGS & SVf_POK) {
	    $hr->{svval} .= cstring($sv->PV);
	} elsif (class($sv) eq "HV") {
	    $hr->{svval} .= 'HASH';
	}

	$hr->{svval} = 'undef' unless defined $hr->{svval};
	my $out = $hr->{svclass};
	return $out .= " $hr->{svval}" ; 
    }
}

my %srclines;

sub concise_op {
    my ($op) = @_;
    if ('COP' eq class($op)) {
	print "Line: ", $op->line, "\n";
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
