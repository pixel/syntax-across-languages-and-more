use strict;

# scanning mode: one of "at_beginning", "anywhere", "all_options"
# kind: one of "noarg", "arg", "optional_arg"

my ($SHORT, $LONG, $KIND, $DESCR) = (0 .. 3);

sub usageInfo {
    my ($opt_descr) = @_;

    my @l = map { [ 
		   fmt_opt(0, $_->[$KIND], split('', $_->[$SHORT])),
		   fmt_opt(1, $_->[$KIND], split(' ', $_->[$LONG])),
		   $_->[$DESCR]
		  ] } @$opt_descr;

    my @max_sizes = map { maximum(map length, @$_) } unzip(@l);

    map {
	my $i;
	join('', map { "  $_" . " " x ($max_sizes[$i++] - length) } @$_);
    } @l;
}

sub fmt_opt {
    my ($is_long, $kind, @opts) = @_;

    my ($dash, $space, $equal) = $is_long ? ("--", "", "=") : ("-", " ", "");

    my @l = do {
	if ($kind =~ /^arg=(.*)/) {
	    map { "$dash$_$space$equal$1" } @opts;
	} elsif ($kind =~ /optional_arg=(.*)/) {
	    map { "$dash$_\[$equal$1\]" } @opts;
	} else {
	    map { "$dash$_" } @opts;
	}
    };
    join(", ", @l);
}

sub get_opt {
    my ($opt_descr, $scanning_mode, @args) = @_;
    
    my (@args_in_between, @pbs, @options);

    my $check_matches = sub {
	my ($opt, @matches) = @_;
	if (@matches == 1) {
	    $matches[0];
	} else {
	    push @pbs, @matches ? 
	      ("option `$opt' is ambiguous; could be one of:", usageInfo(\@matches)) :
	      "unrecognized option `$opt'";
	    '';
	}
    };

    my $handle_long = sub {
	my ($opt, $arg) = @_;

	my $match = $check_matches->($opt, grep { $_->[$LONG] =~ /\b\Q$opt/ } @$opt_descr) or return;
	my $kind = $match->[$KIND];
	
	if ($kind eq 'noarg' && $arg) {
	    push @pbs, "option `$opt' doesn't allow an argument";
	} elsif ($kind =~ /^arg=(.*)/ && !$arg) {
	    push @pbs, "option `$opt' requires an argument $1";
	} else {
	    push @options, [ $match->[$LONG], $arg ];
	}
    };
    my $handle_short = sub {
	my ($opts) = @_;

	while ($opts) {
	    (my $opt, $opts) = $opts =~ /(.)(.*)/;

	    my $match = $check_matches->($opt, grep { $_->[$SHORT] =~ /\Q$opt/ } @$opt_descr) or next;
	    my $kind = $match->[$KIND];

	    my $arg;
	    if ($kind eq 'noarg') {
	    } elsif ($opts) {
		($arg, $opts) = ($opts, '');
	    } elsif ($kind =~ /^arg=(.*)/) {
		$arg = shift @args
		  or push(@pbs, "option `$opt' requires an argument $1"), return;
	    }
	    push @options, [ $match->[$LONG], $arg ];
	}
    };

    while (@args) {
	local $_ = shift @args;
	if    (/^--$/)         { last } # if $scanning_mode ne 'all_options'
	elsif (/^--(.*)=(.*)/) { $handle_long->($1, $2) }
	elsif (/^--(.*)/)      { $handle_long->($1) }
	elsif (/^-(.+)/)       { $handle_short->($1) }
	else {
	    push @args_in_between, $_;
	    last if $scanning_mode eq 'at_beginning';
	}
    }
    @pbs and die join("\n", @pbs, '');

    if ($scanning_mode eq 'all_options') {
	push @options, [ 'Arg', $_ ] foreach @args_in_between;
	@args_in_between = '';
    }
    \@options, [ @args_in_between, @args ];
}

############################################-
# and here a small and hopefully enlightening example:

my @opt_descr = (
    [ 'v',  'verbose',         'noarg',             "verbosely list files" ],
    [ 'V?', 'version release', 'noarg',             "show version info" ],
    [ 'o',  'output',          'optional_arg=FILE', "use FILE for dump" ],
    [ 'n',  'name',            'arg=USER',          "only dump USER's files" ],
);

eval {
    my ($options, $args) = get_opt(\@opt_descr, at_beginning => qw(-?));
    use Data::Dumper;
    print Dumper($options, $args);
};
if ($@) {
    die join("\n", $@ . "Usage: foobar [OPTION...] files...", usageInfo(\@opt_descr), '');
}


sub maximum { my $n = shift; $_ > $n and $n = $_ foreach @_; $n }
sub unzip {
    my @r;
    foreach (@_) {
	my $i;
	push @{$r[$i++]}, $_ foreach @$_;
    }
    @r;
}
