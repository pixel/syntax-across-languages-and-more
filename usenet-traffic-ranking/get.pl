#!/usr/bin/perl

sub url {
    my ($group, $year, $month, $day1, $day2) = @_;
    $group = html_quote($group);
    'http://groups.google.fr/groups?as_q=&num=10&as_scoring=d&hl=fr&btnG=Recherche+Google&as_epq=&as_oq=&as_eq=&as_ugroup=' . $group .
      '&as_usubject=&as_uauthors=&as_umsgid=&lr=&as_qdr=&as_drrb=b&as_miny=' . $year . '&as_minm=' . $month . '&as_mind='. $day1 . '&as_maxy=' . $year . '&as_maxm=' . $month . '&as_maxd=' . $day2;
}

sub one_range {
    my ($url) = @_;
    local *F;
    sleep 1;
    open F, "/usr/bin/lynx -source '$url' |";
    foreach (<F>) {
	print LOG $_;
	/sur un total d'environ <b>(.*?)</ or next;
	my $nb = $1;
	$nb =~ s/,//g;
	return $nb;
    }
    0;
}

sub month {
    my ($group, $year, $month) = @_;
    return 0 if $year >= 2002 && $month >= 5;
    my $sum;
    my $step = 4;
    foreach my $day1 (1, 6, 11, 16, 21, 26) {
	my $day2 = $day1 + $step;
	my $nb = one_range(url($group, $year, $month, $day1, $day2));
#	$nb += one_range(url($group, $year, $month, $day1 + 3, $day2));
	$sum += $nb;
	my $s = sprintf "%4d-%02d-%02d %d\n", $year, $month, $day1, $nb;
	printf F $s;
	print STDERR $s;
    }
    $sum;
}

sub year {
    my ($group, $year) = @_;
    my @l = map { month($group, $year, $_) } (1 .. 12);
    grep {$_} @l ? @l : undef;
}

sub group {
    my ($grpname) = @_;
    local *F; local *LOG;
#    -e $grpname and die "already done";
    open F, ">> data/$grpname" or die;
    open LOG, "> data/.log.$grpname.html" or die;
    my $group = "comp.lang.$grpname*";
    foreach my $year (reverse (1990 .. 2002)) {
	year($group, $year) or last;
    }
}

#print url('comp.lang.java.misc*', 2002, 1, 21, 31), "\n";
#exit;
group($ARGV[0]);


sub html_quote {
    local $_ = shift;
    s/\*/%2A/g;
    s/\+/%2B/g;
    $_;
}
