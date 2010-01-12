my %l = (
'Abstract data types' => 'Simula 67 FOLDOC',
'Algebraic data types' => 'Hope (giving ML) FOLDOC',
'Assignment operator overloading' => 'C++',
'BNF (Backus-Naur Form)' => "used to describe Algol 60's syntax FOLDOC=Backus-Naur+Form",
'Block nesting with scope' => 'Algol 60',
'Chained comparisons' => 'BCPL http://dev.perl.org/rfc/25.html',
'Class' => 'Simula 67 FOLDOC=class',
'Closure' => 'Lisp FOLDOC',
'Comments' => 'Cobol FOLDOC',
'Compound statements (group statements into one)' => 'Algol 58',
'Continuations' => '?? (1972) FOLDOC=continuation+passing+style',
'Variable declaration anywhere in a block' => "Algol 68",
'Encapsulation' => '',
'Exception handling' => 'PL/I FOLDOC=exception',
'Explicit typing' => 'Algol 58',
'Garbage collection' => 'Lisp http://www.memorymanagement.org/glossary/g.html#garbage.collection',
'Heap allocation' => 'Lisp http://www.memorymanagement.org/glossary/h.html#heap.allocation',
'Higher order' => 'Algol 60 or maybe Lisp? FOLDOC=higher-order+function',
'Hygienic macros' => 'Scheme R4RS http://www.cs.indiana.edu/scheme-repository/R4RS/r4rs_12.html',
'Inheritance' => 'Simula 67 FOLDOC',
'Lazy evaluation' => 'ISWIM (giving Haskell) FOLDOC',
'List comprehension' => 'KRC (giving Haskell) FOLDOC',
'Macros' => 'Cobol FOLDOC',
'Modules' => 'Modula-2 FOLDOC',
'Monads' => 'Haskell FOLDOC',
'Multiple selection ("case" statement)' => 'Algol W FOLDOC=switch+statement',
'Object-oriented Programming' => 'Simula 67 FOLDOC',
'Operator overloading' => 'Algol 68 FOLDOC',
'Orthogonality' => 'Algol 68',
'Parametric Polymorphism' => 'ML FOLDOC=polymorphism',
'Pass by name' => 'Algol 60 FOLDOC=call-by-name',
'Pass by value/result' => 'Algol W FOLDOC=call-by-value-result',
'Pass by value' => 'Fortran FOLDOC=call-by-value',
'Pattern matching' => 'Hope (giving ML) FOLDOC',
'Pointer datatype' => 'PL/I FOLDOC=pointer',
'References' => 'Algol 68 FOLDOC',
'Separate compilation' => 'Fortran II (interest: hardware failures were very frequent => length of a program could not exceed 300/400 lines)',
'Stack allocation' => 'Algol 58 http://www.memorymanagement.org/glossary/s.html#stack.allocation',
'Stack dynamic variables' => 'Algol 60',
'Static allocation' => 'Fortran http://www.memorymanagement.org/glossary/s.html#static.allocation',
'Structures (records)' => 'Cobol FOLDOC=struct',
'Type classes' => 'Haskell FOLDOC=type+class',
'Type inference' => 'ML FOLDOC',
'User-defined data types' => 'Algol 68',
'Using C as portable assembler' => "C++ (Cfront)",
'Vertical layout (Indentation to show block structure)' => 'CPL, ISWIM (giving Haskell)',
);

# Lisp macros 1963 "MACRO Definitions for LISP" Timothy Hart

open STDOUT, "|m4";

print <<'EOF';
include(`../mirrors.m4')
<html>
  <head>
    <title>History of programming languages concepts</title>
  </head>

<body>
    MACRO_MIRRORS_LINK(index.html, ` to Pixel''`s programming languages study page', language-study/concepts-history.html)

Also have a look at the <a href="diagram.html">diagram of programming languages history</a>.

<table border=1 cellpadding=4>
EOF

my $foldoc = 'http://foldoc.doc.ic.ac.uk/foldoc/foldoc.cgi?query';

foreach (sort keys %l) {
    $l{$_} =~ s|FOLDOC=|$foldoc=|;
    $l{$_} =~ s|FOLDOC|"$foldoc=" . join('+', map { s/s$//; lc } split)|e;
    my ($concept, $lang) = $l{$_} =~ /(.*)\s+(http:.*)/ ?
      (sprintf('<a href="%s">%s</a>', $2, html_quote($_)), $1) :
      (html_quote($_), $l{$_});
    printf "<tr><td>%s</td><td>%s</td></tr>\n", $concept, html_quote($lang);
}

print <<'EOF';
</table>

<hr>
<address><a href="mailto:pixel@rigaux.org">Pixel</a></address>
This document is licensed under <a href="http://www.gnu.org/copyleft/fdl.html">GFDL</a> (GNU Free Documentation License).

</body>
</html>
EOF

sub html_quote {
    local ($_) = @_;
    s/!</!&lt;/g;
    s/!>/!&gt;/g;
    $_;
}
