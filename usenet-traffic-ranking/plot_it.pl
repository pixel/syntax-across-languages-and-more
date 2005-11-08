#!/usr/bin/perl

my %collected_means;
my $gnuplot;

my $terminal;
if ($ARGV[0] eq '-t') {
    shift @ARGV;
    $terminal = shift @ARGV;
}

@ARGV or die "usage: plot_it.pl <programming language name> ...\n";

if (@ARGV == 1) {
    my ($name) = @ARGV;
    my @l = compute_mean($name);

    open G, "> .tmp" or die;
    print G foreach @l;
    close G;

    $gnuplot = sprintf 'plot ".tmp" using 1:4 title "%s" with l, ".tmp" using 1:3 title "%s" with l, ".tmp" using 1:2 title "%s" with dots', $name, $name, $name;
} else {
    my @langs = @ARGV;

    compute_mean($_) foreach @langs;

    open G, "> .tmp" or die;
    print G "# ", join(" ", @langs), "\n";

    foreach my $year (sort keys %collected_means) {
	print G "$year ", join(" ", map { $collected_means{$year}{$_} || 0 } @langs), "\n";
    }
    close G;

    my $i = 2;
    $gnuplot = 'plot [1993:2002] ' . join(", ", map { sprintf('".tmp" using 1:%d smooth csplines title "%s" with l', $i++, $_) } @langs);
}

$terminal = 'postscript eps color' if $terminal eq 'eps';

open H, "> .gnuplot_it" or die;
print H "set size 0.6, 0.5\n" if $terminal =~ s/small-//;
print H "set terminal $terminal\n" if $terminal;
print H <<EOF;
set title "Usenet comp.lang traffic"
set yrange [0:]
set key left
set ylabel "number of posts per day"
$gnuplot
EOF
print H "pause -1\n" if !$terminal;
close H;
system("gnuplot .gnuplot_it");

#unlink ".tmp";


sub compute_mean {
    my ($name) = @_;
    open F, "sort -n data/$name |";

    my $nb_for_mean = 20;
    my @means = (0) x $nb_for_mean;

    my $nb_for_mean2 = 40;
    my @means2 = (0) x $nb_for_mean2;

    my $max_mean;

    my $cnt;
    local $_;
    while (<F>) {
	$cnt++;
	my ($year, $month, $day, $nb) = /(\d*)-(\d*)-(\d*) (\d*)/ or next;
	$nb < 10000 or next;
	my $y = $year + (30 * ($month - 1) + $day - 1) / (12 * 30);

	$nb /= 5;

	my $mean = shift(@means) / $nb_for_mean;
	push @means, 0;
	$_ += $nb foreach @means;
	$mean = 0 if $cnt < $nb_for_mean;

	my $mean2 = shift(@means2) / $nb_for_mean2;
	push @means2, 0;
	$_ += $nb foreach @means2;
	$mean2 = 0 if $cnt < $nb_for_mean2;

	$max_mean = $mean if $mean > $max_mean;

	$collected_means{sprintf("%.2f", $y - 2 / 3 * $nb_for_mean2 * 5 / (12 * 30))}{$name} = sprintf("%3.1f", $mean2);

	push @l, [ $nb, sprintf("%.2f %3.1f %.2f %.2f\n", $y, $nb, $mean, $mean2) ];
    }
    map { $_->[0] < $max_mean * 1.3 ? $_->[1] : () } @l;
}
