#!/usr/bin/perl -pi

if (/<body /) {

    my @langs = map { /(.*)\.listing/ } glob("*.listing");
    my $langs = join('',
		     map { sprintf qq(<li><a href="%s.html">%s</a> (<a href="%s.listing">raw source</a>)\n), $_, ucfirst, $_ } @langs);

    $_ .= <<EOF;
Here are small examples using continuations.
<p>
These examples are available in the following languages:
<ul>
$langs
</ul>
EOF
}
