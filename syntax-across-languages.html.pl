
use strict;
use vars qw(@static @dynamic @OO @functional @reflexive @typed @rare @has_lambda @various);

########################################################################################
# kinds ################################################################################
########################################################################################
my @OO_dynamic = 
  qw(E JavaScript Perl Perl6 MSH PHP Python Rebol Ruby Smalltalk VisualBasic YCP Tcl MzScheme Oz Matlab);
my @OO_static = 
  ('C#', qw(Ada Beta C++ Cecil Pike Objective-C Java Eiffel Sather Delphi-Kylix Pliant Simula));
my @functional_dynamic = 
  qw(Scheme Erlang FL Logo);
my @functional_static = 
  qw(Mercury SML Haskell);

@static = (@OO_static, @functional_static,
  qw(C Cobol Pascal Fortran merd Modula-2 Modula-3 OCaml));
@dynamic = (@functional_dynamic, @OO_dynamic,
  qw(Awk Basic Dylan Forth Maple Lua Icon XPath XSLT Pike PostScript Prolog BourneShell Oz EmacsLisp CommonLisp ClassicREXX Yorick));
@OO = (@OO_dynamic, @OO_static, 
  qw(OCaml CommonLisp  Dylan merd));
@functional = (@functional_dynamic, @functional_static, 
  qw(OCaml EmacsLisp CommonLisp Dylan merd Smalltalk));

@reflexive = (@dynamic, 
  qw(Java merd Pliant));
@typed = (@static, 
  qw(Dylan Smalltalk Perl6 VisualBasic YCP));
@has_lambda = (@functional,
  qw(Ruby Python Perl Perl6 MSH), 'C#2', 'C#3');

@various = qw(APL BCPL B PL/I MUMPS HTML TeX SGML XML YAML Assembler SQL92);

my %kind_dependencies = (
  reflexive => 'dynamic',
  typed => 'static',
  has_lambda => 'functional',
);

my %kind_descriptions = (
  static => 'Statically typed',
  dynamic => 'Dynamically typed',
  OO => 'Object Oriented',
  functional => 'Functional',
  reflexive => 'Reflexive',
  typed => 'Has types',
  has_lambda => 'Has anonymous functions',
);
my %kinds = map { $_ => \@{$::{$_}} } (qw(rare various), keys %kind_descriptions);

my %hierarchy = (
  'Fortran90' => ['Fortran'],
  'C99' => ['C'],
  'C#2' => ['C#'],
  'C#3' => ['C#2', 'C#'],
  'SQL99' => ['SQL92'],
  'Delphi-Kylix' => ['Pascal'],
  'Objective-C' => ['C'],
  'VisualBasic' => ['Basic'],
  'PHP5' => ['PHP'],
  'SML-NJ' => ['SML'],
  'Scheme-SRFI1' => ['Scheme'],
  'Scheme-SRFI28' => ['Scheme'],
  'Scheme-SRFI34' => ['Scheme'],
  'MzScheme' => ['Scheme-SRFI1', 'Scheme-SRFI28','Scheme'],
  'Squeak' => ['Smalltalk'],
  'XSLT' => ['XPath', 'XML'],
  'KornShell' => ['BourneShell'],
);

# complete kinds using the hierarchy of languages
# and fill_in rev_hierarchy
my %rev_hierarchy;
while (my ($l, $subls) = each %hierarchy) {
    foreach my $subl (@$subls) {
	push @{$rev_hierarchy{$subl}}, $l;
	next if @$subls > 1;
	foreach (values %kinds) {
	    push @$_, $l if member($subl, @$_);
	}
    }
}

my @langs = uniq(map { @$_ } values %kinds);


########################################################################################
# main #################################################################################
########################################################################################
my $all = [

'Various' => [

  'commenting' =>
  [
   'until end of line' =>
   [
    '#' => "Perl Perl6 Maple Ruby Python Tcl Icon Awk BourneShell PHP merd E Pliant YAML",
    '//' => "BCPL C99 C++ C# Java Dylan Pike PHP JavaScript YCP Yorick",
    '--' => "Cecil Eiffel Sather Simula Haskell Ada Lua SQL92",
    ';' => "EmacsLisp Logo CommonLisp Scheme Rebol Assembler MUMPS",
    '%' => "Prolog Mercury TeX Erlang PostScript Oz Matlab",
    'rem' => "Basic",
    "'" => "VisualBasic",
    '\\' => "Forth",
    '!' => "Fortran90 Assembler",
    'C or * in column 1' => "Fortran",
   ],

   'nestable' =>
   [
    '(* ... *)' => "OCaml Beta Pascal Modula-3 SML",
    '%( ... %)' => "Matlab",
    '/* ... */' => "Oz Dylan ClassicREXX SQL99",
    '{ ... }' => "Pascal Rebol",
    '{- ... -}' => "Haskell",
    '#| ... |#' => "CommonLisp",
    '#if 0 ... #endif' => "C",
    'comment { ... }' => "Rebol",
    'comment [ ... ]' => "Rebol",
    '[ ... ] (when unused)' => "Rebol",
    '--[[ ... ]]' => "Lua",
   ],

   'non nestable' =>
   [
    '" ... "' => "Smalltalk",
    '/* ... */' => "C C++ C# Java B PL/I Pike PHP JavaScript Mercury YCP Yorick",
    '<!-- ... -->' => "HTML XML",
    '( ... )' => "Forth",
   ],
  ],

  'documentation comment' =>
  [ { MLANG => "C ClassicREXX Logo MUMPS Prolog Maple" },
   'until end of line' =>
   [
    '///' => "C# Java",
    '-- |' => "Haskell",
    '-- ^' => "Haskell",
   ],

   'non nestable' =>
   [
    "/** ... */ (for C, it is not a standard convention, but it is the more widespread)" => "C C# Java E PHP",
   ],

   '' =>
   [
    '{-| ... -}' => "Haskell",
    '(** ... *)' => "OCaml",
    '/* DOCUMENT ... */' => "Yorick",
    'indexing
              identifier: "...";' => "Eiffel",

    'rebol [ Note: "..." ]' => "Rebol",
    'func ["..." arg] ...' => "Rebol",

    pre('class X:
"""...
"""

def x():
"""...
"""
') . " (any string literal would work)" => "Python",

    '(define (f para1 para2) "..." ...)' => "Scheme",
    '(defun f (para1 para2) "..." ...)' => "EmacsLisp CommonLisp",

    pre('=pod
...
=cut') . " (see also =head1, =head2, =over, etc)" => "Perl Perl6",

    pre('=begin
...
=end') => "Ruby",

    pre('function MYFUNCTION
%MYFUNCTION the very first comment line is displayed in the help table of contents
%
% the remaining lines are displayed when getting help for MYFUNCTION
%
') => "Matlab",


   ],
  ],

  'information about the current line and file' =>
  [ { MLANG => "OCaml Ada MUMPS Matlab Prolog" },
   '__LINE__ __FILE__' => "C C++ Perl Pike PHP Ruby",
   '$?LINE $?FILE' => "Perl6",
   'inspect.stack()[0][2] inspect.stack()[0][1]' => "Python",
   pre('(new System.Diagnostics.StackFrame(true)).GetFileLineNumber()
(new System.Diagnostics.StackFrame(true)).GetFileName()') => 'C#',
   'system/script/header/file (need "file: %script-header.r" in file header)' => "Rebol",
   'SOURCELINE() / parse source OS . SOURCENAME' => "ClassicREXX",
  ],

  'tokens' =>
  [ { ALL => 1 },
    'case-sensitivity (keywords, variable identifiers...)' =>
    [ # see http://www.swiss.ai.mit.edu/~jaffer/r5rs_4.html#SEC14 for Scheme, Guile is an exception
     'case-sensitive' => 'B C C++ C# Java Maple Prolog JavaScript Lua Matlab Pike Perl Perl6 Python Ruby XML YAML Tcl Smalltalk BourneShell OCaml Haskell merd Awk Modula-3 Pliant',
     'case-insensitive' => "PL/I Pascal Logo Rebol VisualBasic Eiffel Ada SGML HTML Scheme CommonLisp Forth Assembler ClassicREXX SQL92",
     'case-sensitive: variables<br>case-insensitive: keywords, functions, constants...' => "PHP",
     'case-sensitive: identifiers<br>case-insensitive: keywords' => "E",
     'case-sensitive: identifiers<br>case-insensitive: commands' => "MUMPS",
    ],

    'if case sensitive, what is the standard way for <a href="http://c2.com/cgi/wiki?CapitalizationRules">scrunching together multiple words</a>' =>
    [ { MLANG => 'MUMPS' },
     # see http://dotnet.di.unipi.it/EcmaSpec/CSharp/cont25.html C.1.4 for C#
     # perl -ne '$l{$_} = 1 foreach /(\w*[a-z][_]\w*)/g; $m{$_} = 1 foreach /(\w*[a-z][A-Z]\w*)/g; END { print join(" ", keys %l), "\n", join(" ", keys %m), "\n" }'
     'CamelCase' => "Haskell Java JavaScript Tcl Smalltalk C# E Pascal VisualBasic",
     'underscores' => 'merd',
     'dots' => "Logo",
     'hyphens' => "Rebol EmacsLisp CommonLisp",
     'underscores for functions, unclear for modules / types / constructors' => "OCaml",
     'UPPER_CASE' => "BourneShell",
     'lowercasenoseparator' => 'Matlab',
     'underscores, UPPER_CASE for class names' => "Eiffel",
     'CamelCase for classes, underscores for methods' => "Python",
     'CamelCase for types, underscores for functions, variables, ...' => "Pliant",
     'CamelCase for modules and classes, ALL_CAPS for constants, underscores for functions, variables, ...' => 'Ruby',
     'CamelCase for modules and classes, ALLCAPS for macros, underscores for methods, constants and variables' => 'Pike',
     'CamelCase for modules, ALL_CAPS for constants, unclear for functions / variables' => "Perl Perl6",
     'CamelCase for variables, underscores for predicates' => 'Prolog',
     'usually lowercase or underscores, ALL_CAPS for macros' => 'C',
     'usually underscores' => "C++",
     'Camel_Case' => "Ada",
    ],

    'variable identifier regexp' =>
    [
     '[a-zA-Z][a-zA-Z0-9]*' => "PL/I Smalltalk",
     "[a-zA-Z][_a-zA-Z0-9]*" => "Eiffel Matlab",
     "[a-zA-Z](_?[a-zA-Z0-9])*" => 'Ada',
     "[_a-zA-Z][_a-zA-Z0-9]*" => 'B C C++ C# E Maple Python Perl Perl6 PHP Tcl Awk BourneShell',
     "[_a-zA-Z][_a-zA-Z0-9]* or '[^']*'" => "Pliant",
     '[_a-zA-Z$][_a-zA-Z0-9$]*' => "Java JavaScript",
     '[a-zA-Z%][a-zA-Z0-9]*' => "MUMPS",
     "[_a-z][_a-zA-Z0-9]*" => 'Ruby',
     "[_a-z][_a-zA-Z0-9]*[!?']*" => 'merd',
     "[_a-z][_a-zA-Z0-9']*" => 'SML OCaml Haskell',
     "[_A-Z][_a-zA-Z0-9]*" => 'Prolog Mercury',
     "[_a-zA-Z!$%&*/:<=>?^][_a-zA-Z!$%&*/:<=>?^0-9.+-]*" => "Scheme", # cooked from http://www.cs.indiana.edu/scheme-repository/R4RS/r4rs_9.html
     "[a-zA-Z!?@#$_][a-zA-Z0-9!?@#$_]*" => "ClassicREXX",
     q([_a-zA-Z?!.'+*&|=~-][_a-zA-Z0-9?!.'+*&|=~-]* or 
<br>[^0-9[](){}":;/][^ \n\t[](){}":;/]*) => "Rebol",
     '\S+' => "Forth", # ??
    ],

    'function identifier regexp (if different from variable identifier regexp)' =>
    [ { KIND => 'rare' },
     "[_a-zA-Z][_a-zA-Z0-9]*[!?]?" => 'Ruby',
     "[_a-z][_a-zA-Z0-9]*" => 'Prolog Mercury',
     '[^ \t\n\r\f]+' => "Tcl",
    ],

    'keyword regexp (if different from variable identifier regexp)' =>
    [ { KIND => 'rare' },
      '[A-Z]+' => "Modula-3",
    ],

    'type regexp (if different from variable identifier regexp)' =>
    [ { KIND => 'rare' },
      "[_A-Z][_a-zA-Z0-9']*" => 'Haskell',
      "[_a-z][_a-zA-Z0-9']*" => 'OCaml Mercury',
    ],

    'constant regexp (if different from variable identifier regexp)' =>
    [ { KIND => 'rare' },
      "[A-Z][_a-zA-Z0-9]*" => 'Ruby',
      "[_A-Z][_a-zA-Z0-9']*" => 'OCaml Haskell',
      "[_a-z][_a-zA-Z0-9']*" => 'Mercury',
    ],
    
  ],

  'breaking lines (useful when end-of-line and/or indentation has a special meaning)' =>
  [ { MLANG => "Pliant MUMPS Prolog" },
   'nothing needed' => 'Ada B C C++ C# Maple Eiffel PostScript Rebol Java YCP JavaScript Pascal Perl Perl6 OCaml CommonLisp EmacsLisp Scheme SML Smalltalk XSLT Forth Oz',
   '\\' => "Awk Python Ruby Tcl E BourneShell",
   '_' => "VisualBasic",
   ',' => "ClassicREXX",
   '~' => "Logo",
   '...' => "Matlab",
  ],

  'variable assignment or declaration' =>
  [
   'assignment' =>
   [
    '=' => "B C C++ C# Java Perl Perl6 Matlab Pike PHP Basic Erlang Icon Oz JavaScript Lua BourneShell Awk YCP ClassicREXX Yorick",
    ':=' => "BCPL Ada Cecil Pascal Dylan Eiffel Maple Sather Modula-3 Simula Smalltalk E Pliant",
    '<-' => "OCaml",
    '_ (displayed <- with a special character)' => "Squeak",
    ':' => "BCPL Rebol",
    '-> (variable on the right)' => "Beta",
    'def' => "PostScript",
    'setq' => "EmacsLisp CommonLisp",
    'setf' => "CommonLisp",
    'set' => "Rebol CommonLisp",
    'SET v=...' => "MUMPS",
    'set!' => "Scheme",
    'is' => "Prolog",
    'make "v e' => "Logo",
   ],

   'declaration' =>
   [
    '=' => "Haskell SML Prolog Mercury",
    '<-' => "Haskell",
    ':-' => "Prolog",
    'let v = e in' => "OCaml",
    'let v = e' => "BCPL",
    'def v := e / var v := e' => "E",
    'my / our / local / use vars' => "Perl",
    'my / our / temp' => "Perl6",
    'define' => "Dylan Scheme",
    'let let*' => "Scheme CommonLisp",
    'letrec fluid-let' => "Scheme",
    'flet labels defun defmethod defvar defparameter defsetf ..' => "CommonLisp",
    'local V1 = e V2 = e2 in ... end' => "Oz",
    'global v1, v2' => "Python",
    'global v1 v2' => "Matlab",
    ':@' => "Beta",
    'NEW v' => "MUMPS",
    'v: t' => "Ada Pascal Eiffel",
    '| v1 v2 |' => 'Smalltalk',
    'auto v1, v2; extrn v3, v4;' => "B",
    'var' => "JavaScript Pliant",
    'gvar' => "Pliant",
    'variable v (the variable behaves like a pointer)' => "Forth",
    '<xsl:variable name="v" select="e"/>' => "XSLT",
   ], 

   'both' =>
   [
    '=' => "Python Ruby merd",
    ':=' => "merd",
    'set, variable' => "Tcl",
   ],
  ],

  'grouping expressions' =>
  [ { MLANG => 'EmacsLisp CommonLisp Scheme XSLT PostScript Forth' },
   '( ... )' => "Beta BCPL Logo Prolog B ClassicREXX Maple C C++ C# Matlab YCP Java Eiffel Rebol MUMPS MSH Pike Perl Perl6 Python Ruby Pascal Haskell OCaml Smalltalk SML merd E Tcl PHP JavaScript Lua Ada Awk Modula-3 Pliant XPath Oz SQL92 Yorick",
   '[ ... ]' => "Rebol",
   'indentation' => "merd",
   '$ ...' => "Haskell",
   'begin ... end' => "OCaml Ruby",
   'BEGIN ... END' => "Modula-3",
   'space (cf <a href="http://merd.net/choices_syntax.html#horizontal_layout">horizontal layout</a>)' => "merd",
  ],

  'block (grouping statements, especially when statements are not expressions)' =>
  [ { MLANG => 'OCaml Prolog Rebol SML XSLT CommonLisp Maple Oz PostScript' },
   '{ ... }' => "Pike PHP JavaScript Awk BourneShell Tcl Yorick",
   '{ ... } (introduce scope)' => "B C C++ C# Java Perl Perl6 E Haskell Modula-3 YCP",
   '( ... ) (introduce scope)' => "BourneShell",
   '[ ... ]' => "Logo",
   '[ x. y. ... ]' => "Smalltalk",
   'begin ... end (introduce scope)' => "Pascal Ada",
   'do ... end' => "ClassicREXX",
   'do ... end (introduce scope)' => "PL/I Lua",
   'indentation' => "Python Pliant MUMPS",
   'indentation (introduce scope)' => "Haskell merd",
   'foo ... end  where foo in { if, do, ... }' => "Modula-2 Ruby",
   'foo ... end  where foo in { if, for, while, ... }' => "Matlab",
   'foo ... end  where foo in { if, loop, ... }' => "Eiffel",
   'foo ... end foo  where foo in { if, do, ... }' => "Fortran90 Ada",
   '(* ... *) (ascii representation, original uses a special charset)' => "BCPL",
   '(# ... #)' => "Beta",
  ],

  'use a block as a return value (when statements are not expressions)' =>
  [ { KIND => 'rare' },
    'valof' => "BCPL",
    'do' => "Perl Perl6",
    "proc() .. end proc" => "Maple",
  ],

  'equality / inequality' =>
  [
   'shallow' =>
   [
    '== != ' => "B C C++ Java OCaml Pike Tcl Perl Perl6 Awk Yorick",
    '= /=' => "Fortran90 Eiffel",
    '= <>' => "Pliant Rebol Logo Maple Modula-2",
    '= # (in Modula-2, <> and # are synonyms)' => "Modula-2 Modula-3",
    "= !=" => "BourneShell",
    "== === != !== (=== and !== differ from == and != when the objects' type differ)" => "JavaScript PHP",
    "=== !==" => "PHP5",
    '== ~=' => "Lua",
    '== ~~' => "Smalltalk",
    '== ~==' => "Dylan",
    "= '=" => "MUMPS",
    '= ~= neqv (ascii representation, original uses a special charset)' => "BCPL",
    'is_equal (for objects)' => "Eiffel",
    'equal?' => "Scheme",
    'eq ne' => "EmacsLisp PostScript",
    'eq, eql' => "CommonLisp",
    'eq? eqv?' => "Scheme",
    '.EQ. .NE.' => "Fortran",
    'is / is not' => "Python",
   ],

   'deep' =>
   [
    '== !=' => "Awk C++ C# E Ruby merd Python YCP PHP5",
    '== <>' => "Python",
    '== /=' => "Haskell",
    '== \=' => "Oz",
    '== \==' => "ClassicREXX Prolog",
    '= /=' => "Ada",
    '= !=' => "XPath Maple",
    '= <>' => "VisualBasic OCaml Rebol SML Beta Pascal SQL92",
    '= ~=' => "Dylan Smalltalk",
    '== ~= eq ne isequal isequalwithequalnans' => "Matlab",
    '=@= \=@= / = \= / =:= =\= (normal / structural / unification / arithmetic)' => "Prolog",
    '.eq' => "Logo",
    'equal?' => "Scheme",
    'equals' => "Java",
    'equal' => "EmacsLisp CommonLisp Pike",
    'equalp' => "CommonLisp",
    'deep_is_equal' => "Eiffel",
    'isEqual' => "Objective-C",
   ],
  ],

  'comparison' =>
  [ { ALL => 1 },
   '' =>
   [
    '< > <= >=' => "Ada Awk Beta Maple Logo B C C++ C# YCP E Matlab Java Pascal Rebol Smalltalk VisualBasic Scheme Pike Perl Perl6 merd Tcl Haskell Ruby SML OCaml PHP Eiffel JavaScript EmacsLisp CommonLisp Dylan Lua Awk Modula-3 Python Pliant XPath ClassicREXX SQL92 Yorick",
    '< > =< >=' => "Mercury Oz",
    "< > '> '<" => "MUMPS",
    '<< >> <<= >>= (deep comparison)' => "ClassicREXX",
    '@< / @=< / @> / @>=' => "Prolog",
    'lt gt le ge' => "Perl Perl6 PostScript",
    '-lt -gt -le -ge' => "BourneShell MSH",
    '.LT. .GT. .LE. .GE.' => "Fortran",
   ],

   'returns 3 values (i.e. inferior, equal or superior)' =>
   [ { MLANG => 'B C C++ C# Ada Logo Awk Modula-3 Lua XSLT Rebol PostScript ClassicREXX Matlab' },
    'a <=> b' => "Ruby Perl Perl6 merd",
    'cmp' => "Perl Perl6 Python",
    'compare' => "OCaml Haskell Prolog Mercury Pliant Smalltalk",
    'strcmp' => "PHP",
    'three_way_comparison' => "Eiffel",
    'string compare' => "Tcl",
    'compareTo' => 'Java',
   ],

   'returns 4 values (i.e. inferior, equal, superior or not comparable)' =>
   [ { KIND => 'rare' },
    'compare' => "Pliant",
    'compareTo' => 'E',
   ],

   'min / max (binary or more)' =>
   [ { MLANG => 'Ruby B C C# Awk XSLT' },
    'min / max' => "Ada C++ Java Rebol Maple Lua Beta Prolog Pike Matlab Python Smalltalk E Eiffel merd CommonLisp Scheme OCaml Haskell Dylan Pliant SQL92",
    'min minstr / max maxstr (in List::Util)' => "Perl",
    'Min / Max' => "Oz",
    'MIN / MAX' => "Modula-3 ClassicREXX",
    'measure-object -min / measure-object -max' => "MSH",
   ],
  ],

  'runtime evaluation' =>
  [ { KIND => 'dynamic', MLANG => "XSLT" },
   'eval' => "Perl Perl6 Ruby Python Scheme EmacsLisp Tcl Matlab PHP JavaScript CommonLisp YCP",
   'dostring' => "Lua",
   'Compiler evaluate:' => "Smalltalk",
   'runtime_compile / compile + execute' => "Pliant",
   'Compiler.evalExpression or Compiler.parseOzVirtualString' => "Oz",
   'compile_string' => "Pike",
   'interpret' => "ClassicREXX",
   'run' => "Logo",
   'XECUTE' => "MUMPS",
   'do / reduce / compose / load' => "Rebol",
   '[...]' => "Tcl",
   '`...`' => "BourneShell",
   '=.. (Univ operator)' => "Prolog",
  ],

  'force garbage collection' =>
  [ { MLANG => 'Ada B C C++ CommonLisp Pascal XSLT ClassicREXX MUMPS Matlab', },
    'doGC' => "Beta",
    'GC.start' => "Ruby",
    'gc' => "Logo Pike Maple",
    'System.gc()' => "Java",
    'System.gcDo' => "Oz",
    'System.GC.Collect()' => "C#",
    'gc.collect()' => "Python",
    'full_collect' => "Eiffel",
    'garbage_collect' => "Mercury Prolog",
    'collectgarbage' => "Lua",
    'VM.garbageCollect()' => "JavaScript",
    'Gc.full_major()' => "OCaml",
    'Smalltalk garbageCollect' => "Smalltalk",
    'incremental garbage collection => not needed' => "Perl Perl6",
    'recycle' => "Rebol",
    'interp.gc()' => 'E',
    '(ext:gc)' => "CommonLisp",
  ],

],

'Functions' => [

  'function call' =>
  [ { ALL => 1 },

    '' => 
    [
     'f(a,b,...)' => "Awk B C C++ C# Maple Java YCP Matlab Erlang Pike Perl Perl6 Mercury merd Eiffel Python Ruby Pascal E PHP JavaScript Dylan Lua Ada Awk Modula-3 XPath Prolog Yorick",
     'f a b ...' => "SML Haskell Logo OCaml Rebol Matlab Tcl Pliant BourneShell MSH",
     'f(a,b,...f) or f[a,b,...] depending on the version' => "BCPL",
     '(f a b ...) (apply f l) ' => "Scheme EmacsLisp CommonLisp",
     '(funcall f a b ...)' => "EmacsLisp CommonLisp",
     '{f a b}' => "Oz",
     'f[a,b,...] or f.call(a,b,...)' => "Ruby",
     '&$f(a,b,...) or $f->(a,b,...)' => "Perl",
     '$f.(a,b,...)' => "Perl6",
     'f a, b, ...' => "Perl",
     'f, a, b, ... (procedure call)' => "Yorick",
     'v = f(a, b, ...) or call f a, b, ...' => "ClassicREXX",
     'a b ... f' => "PostScript Forth",
     'a f' => 'Smalltalk',
     'a f: b g: ... (the function is "f: g:")' => "Smalltalk",
     "(a,b,...)->&f or (a,b,...)->f" => "Beta",
     'f:a (in Pliant, special sugar for only one parameter)' => 'FL',
     '.. [ f, A, B, ...]' => "Prolog",
     pre('<xsl:call-template name="f">
    <xsl:with-param name="a" select=a/>
    <xsl:with-param name="b" select=b/>
</xsl:call-template>') => "XSLT",
    ],

    'with no parameter' =>
    [
     'f' => "Ada Eiffel Pascal PostScript Logo Matlab Rebol Ruby Perl MSH Perl6 Mercury Haskell Tcl Pliant BourneShell Prolog", # Haskell egs: monads
     'f()' => "Awk Pike Maple Erlang Python C C++ C# YCP Java E Lua Perl JavaScript merd OCaml SML",
     '(f)' => "Scheme EmacsLisp CommonLisp",
     '(funcall f)' => "EmacsLisp CommonLisp",
     '{f}' => "Oz",
     'f[] or f.call' => "Ruby",
     '&$f or $f->()' => "Perl",
     '$f.()' => "Perl6",
     'v = f()' => "ClassicREXX",
     'call f' => "ClassicREXX Fortran",
     'f value (f is a block)' => "Smalltalk",
     '<xsl:call-template name="f">/' => "XSLT",
    ],
  ],

  '<a href="http://www.haskell.org/hawiki/PartialApplication">partial application</a> (in the examples below, a normal call is "f(a,b)")' =>
  [ { KIND => 'functional', ALL => 1, MLANG => "Smalltalk Maple CommonLisp Scheme Erlang Matlab" },

    'give the first argument' =>
    [
     'f a' => "SML Haskell OCaml",
     'f(a)' => "Mercury",
     'f(a,)' => "merd",
     '&f.assuming(var_name => a)' => "Perl6",
     'functools.partial(f, a)' => "Python",
    ],

    'give the second argument' =>
    [ { MLANG => 'SML Haskell OCaml' },
     'f(,b)' => "merd",
     '&f.assuming(b => b)' => "Perl6",
    ],

    'give the first argument to operator ">"' =>
    [
     '(a >)' => "Haskell merd",
     '(>) a' => "OCaml",
    ],

    'give the second argument to operator ">"' =>
    [ { MLANG => "OCaml" },
     '(> a)' => "Haskell merd",
    ],
  ],

  'function definition' =>
  [
   '' =>
   [
    'sub f { ... }' => "Perl Perl6",
    'sub f($para1, $para2, ...) { ... }' => "Perl6",
    'def f(para1, para2, ...): ...' => "Python",
    'def f(para1, para2, ...) ... end' => "Ruby",
    'def f(para1, para2, ...) ... { ... }' => "E",
    'f para1 para2 = ...' => "Haskell",
    'let f para1 para2 = ...' => "OCaml",
    'f(para1, para2, ...) = valof $( ... $)' => "BCPL",
    'f(para1, para2, ...) = ...' => "merd",
    'f ...  or  f: para1 ...' => "Smalltalk",
    'f: func [para1 para2 ...] ...' => "Rebol",
    '/f { ... } def' => "PostScript",
    'f := (para1, para2, ...) -> ...' => "Maple",
    'func f(a, b, c...) { ... }' => "Yorick",
    'typ0 f(typ1 para1, typ2 para2, ...) { ... }' => "C C++ C# Pike YCP",
    'function f(para1, para2) { ... }' => "Awk JavaScript",
    'function f(para1, para2) ... code ... end' => "Lua",
    'function f { ... }' => "KornShell",
    'function f { param(para1, [typ2]para2, ...) ... }' => "MSH",
    '(define (f para1 para2) ...)' => "Scheme",
    '(defun f (para1 para2) ...)' => "EmacsLisp CommonLisp",
    'fun { F Para1 Para2 } ... end' => "Oz",
    'proc f {para1 para2} { ... }' => "Tcl",
    pre('function retval = f(para1, para2)
retval = ...') => "Matlab",
    pre(':- func f(typ1, typ2, ...) = typ0.
f(Para1, Para2, ...) = ...') => "Mercury",
    pre('function f(para1 : type1; para2 : typ2; ...) return retval is
begin
   ...
end f;') => "Ada",
    pre('function f para1 para2 -> retval
  arg typ1 para1; arg typ2 para2; arg rettyp retval;
  ...') => "Pliant",

    pre('function f(para1 : typ1, para2 : typ2, ...) : retval;
var retval : typ0;
begin
  ...
end') => 'Pascal',

    pre('f (para1 : typ1; para2, para3 : typ2; ...) : rettyp is
do
  ...
end') => "Eiffel",

    pre('<xsl:template name="f">
    <xsl:param name="para1"/>
    <xsl:param name="para2"/>

    ...
</xsl:template>') => "XSLT",

    'Function f(para1, para2)
...
End Function' => "VisualBasic",

    ': f ... ;' => "Forth",

    'f() { ... }' => 'BourneShell',

    pre('f : procedure
  ...
return retval') => "ClassicREXX",

    pre('to f :para1 :para2
   ...
end') => "Logo",

   ],

   'procedures' =>
   [
    pre('procedure f(para1 : typ1, para2 : typ2);
begin
  ...
end') => 'Pascal',

    pre('f (para1 : typ1; para2, para3 : typ2; ...) is
do
  ...
end') => "Eiffel",

    pre('procedure f(para1 : typ1; para2 : MODE type2; ...) is
begin
   ...
end f;

MODE ::= | OUT | IN OUT') => "Ada",

    'void f(typ1 para1, typ2 para2, ...) { ... }' => "C C++ C# Pike",
    'let f(para1, para2, ...) be $( ... $)' => "BCPL",
    'proc { F Para1 Para2 } ... end' => "Oz",
    'f := proc(para1, para2, ...) ... end proc' => "Maple",

    'Sub f(para1, para2)
...
End Sub' => "VisualBasic",

    pre('function f(para1, para2)
...') => "Matlab",

    pre('f : procedure
  ...
return') => "ClassicREXX",
   ],

   'predicates' => 
   [
    'f(Para1, Para2, ....) :- ... .' => "Prolog",
   ],
  ],

  'anonymous function' =>
  [ { KIND => 'has_lambda' },
   'sub { my ($a, $b) = @_; ... }' => "Perl",
   '{ my ($a, $b) = @_; ... } (when callee has special "&" prototype)' => "Perl",
   '{ ... } (arguments are in the stack' => "PostScript",
   '[ ... ]' => "Logo",
   '{ param(para1, [typ2]para2, ...) ... }' => "MSH",
   "{|a, b| ... } (this is a block, not precisely a function, but it's alike)" => "Ruby",
   '[:a :b| ... ]' => 'Smalltalk',
   'lambda a, b: ...' => "Python",
   'lambda(typ1 para1, typ2, para2, ...) { ... };' => "Pike",
   '(a, b) => ...' => "C#3",
   "(a, b) -> ..." => "Maple",
   'a, b -> ...' => "merd",
   '-> $a, $b { ... }' => "Perl6",
   '\a b -> ...' => "Haskell",
   'fn (a, b) => ...' => "SML",
   'fun a b -> ...' => "OCaml",
   '(func(A, B) = C :- ...)' => "Mercury",
   'function(a, b) ...' => "JavaScript",
   'function(a, b) ... end' => "Lua",
   'fun(a, b) -> ... end' => "Erlang",
   'fun {$ A B} ... end (also works for procedures: proc {$ A B} ... end)' => "Oz",
   'func [a b ...] ...' => "Rebol",
   'def _(para1, para2, ...) ... { ... }' => "E",
   'proc {|a, b| ...}' => "Ruby",
   'lambda {|a, b| ...}' => "Ruby",
   '(lambda (a b) ...)', "Scheme EmacsLisp CommonLisp",
   "inline('...x...y...') (x, y, z are the parameters)" => "Matlab",
   'method(a, b) ... end method (method is optional)' => "Dylan",
   "create_function('$a,$b','...')" => "PHP",
   'delegate(ta a, tb b) { ... }' => "C#2",
  ],

  'function return value' => 
  [ { MLANG => 'Prolog PostScript' },
   'breaks the control flow' => ($::return_a_value =
   [
    q(return (in Lua, "return xxx" can only appear before a block end. in Matlab, only in inline('...'))) => "Awk B BCPL CommonLisp C Maple C++ C# ClassicREXX Java E Pike YCP Perl Perl6 Ruby Rebol Python Tcl Ada PHP Pliant JavaScript BourneShell Yorick Lua Matlab",
    'Return' => "VisualBasic",
    'RETURN' => "Modula-3",
    'resultis ("return" is used when there is no value to return)' => "BCPL",
    'return from xxx' => "CommonLisp",
    '^' => "Smalltalk",
    'Exit Function / Exit Sub' => "VisualBasic",
    'output' => "Logo",
   ]),

   'function body is the result' => 
   [
    'no syntax needed (in Matlab, only for anonymous function)' => "Maple Haskell OCaml Erlang SML EmacsLisp Rebol CommonLisp Scheme Perl Perl6 Ruby Dylan Oz Matlab",
   ],

   'setting the result' => 
   [
    'Result := val' => "Eiffel",
    '<function name> = val' => "VisualBasic",
    '<function name> := val' => "Pascal",
    '<retvar name> = val;' => "Matlab",
   ],
   
  ],

  'function called when a function is not defined (in dynamic languages)' =>
  [ { KIND => 'dynamic', MLANG => 'Logo Oz Rebol ClassicREXX Matlab', }, # in Oz: the function call is postponed until the function gets defined.  So the current thread will block, waiting for another thread to define the function. (mechanism of dataflow variables)
   'AUTOLOAD' => "Perl",
   'AUTOSCALAR, AUTOMETH, AUTOLOAD...' => "Perl6",
   '__getattr__' => "Python",
   'method_missing' => "Ruby",
   'doesNotUnderstand' => "Smalltalk",
   'unknown' => "Tcl",
   'no-applicable-method' => "CommonLisp",
   'doesNotRecognizeSelector' => "Objective-C",
   'match [name, args] { ... }' => "E",
   'the predicate fail' => "Prolog",
  ],

  'runtime inspecting the caller information' =>
  [ { KIND => 'dynamic', MLANG => 'E Logo' },
   'caller' => "Perl Perl6 Ruby",
   'inspect.stack()[1]' => "Python",
   'backtrace' => "Pike",
   "trace 'I'" => "ClassicREXX",
   "evalin('caller', expression)" => "Matlab",
   'current_predicate' => "Prolog",
   'where(2)' => "Maple",
  ],

  'function composition' =>
  [ { KIND => 'functional', MLANG => 'OCaml Smalltalk Matlab Logo', },
   '.' => "Haskell",
   '~' => "merd",
   'o' => "SML",
   '@' => "Maple",
   'compose' => 'Dylan',
  ],

  'identity function' =>
  [ { KIND => 'functional', MLANG => 'OCaml Erlang Logo', },
    'id' => "Haskell",
    'identity' => "CommonLisp",
    'yourself' => "Smalltalk",
  ],

],

'Control Flow' => [
  { MLANG => 'Haskell XSLT' },

  'sequence' =>
  [ { MLANG => "Forth PostScript Oz" },
   ',' => "C C++ Perl Matlab Pike JavaScript Prolog",
   '.' => "Smalltalk",
   ';' => "Awk Maple Beta PL/I B C C++ C# Java Matlab YCP Pike Pascal Python Ruby Perl Perl6 OCaml SML merd Tcl E PHP JavaScript Ada BourneShell Haskell Modula-3 Pliant",
   ':' => "Maple",
   'nothing, optionally ;' => "Lua ClassicREXX",
   'space' => "Eiffel Rebol",
   'end-of-line' => "Awk Ruby Python Lua merd Basic Tcl E Matlab Fortran Assembler JavaScript BourneShell Haskell Pliant",
   '(begin ...)' => "Scheme",
   '(progn ...) (prog1 ...) (prog2 ...) ' => "EmacsLisp CommonLisp",
   '>>' => "Haskell",
  ],

  'if_then' =>
  [ { MLANG => "SML Prolog Haskell" },
   'if c then b' => "OCaml merd Pascal",
   'if c then b end' => "Ruby Eiffel Lua Oz",
   'if c then b end if' => "Ada Maple",
   'if c then b fi' => "Maple",
   'if c; then b; fi' => "BourneShell",
   'if (c) then b end' => "Dylan",
   'if c do b' => "BCPL",
   'IF c THEN b END' => "Modula-2 Modula-3",
   'if (c) b' => "Awk B C C# C++ Java Pike PHP JavaScript YCP",
   'if c: b' => "Python",
   'if c b' => "Pliant Rebol Tcl",
   'if (c): b endif' => "PHP",
   'if c {b}' => "Perl6",
   'if c [b]' => "Logo",
   'if (c) {b}' => "Perl E Yorick",
   'IF c b' => "MUMPS",
   'c -> b' => 'FL',
   'c b if' => "PostScript",
   'b if c' => "Perl Ruby",
   'c if b1 then' => "Forth",
   '(if c b)' => "CommonLisp Scheme",
   '(when c b)' => "EmacsLisp",
   'c ifTrue: b' => "Smalltalk",
   '<xsl:if test="c">b</xsl:if>' => "XSLT",

   'If c Then b' => "VisualBasic",
   pre('If c
  b
End If') => "VisualBasic",

   'if c; b end' => "Ruby",
   'if c, b, end' => "Matlab",

   pre('if c
  b
end') => "Ruby Matlab",

   pre('if c then ; b

if c then
  b

if c then do
  b
  ...
end') => "ClassicREXX",
  ],

  'if_then_else' =>
  [
   'if c then b1 else b2' => "SML OCaml Haskell merd",
   'if c then b1 else b2 end' => "Ruby Eiffel Lua",
   'if c then b1 elsif c2 then b2 else b3 end if' => "Ada",
   'if c then b1 elseif c2 then b2 else b3 end' => "Eiffel Oz",
   'if (c) then b1 elseif (c2) then b2 else b3 end' => "Dylan",
   'IF c THEN b1 ELSIF c2 THEN b2 ELSE b3 END' => "Modula-3",
   'If c Then b1 ElseIf c2 Then b2 Else b3 End If' => "Modula-2",
   'if (c) b1 else b2' => "Awk B C C# C++ Pike Java JavaScript YCP",
   'if c ?then? b1 elsif c2 ?then? b2 ?else? b3' => "Tcl",
   'if c then begin b1 end else begin b2 end' => "Pascal",
   'if c b1 eif c2 b2 else b3' => "Pliant",
   'if c then b1 elif c2 then b2 else b3 end if' => "Maple",
   'if c; then b1; elif c2; then b2; else b3; fi' => "BourneShell",
   'if c1, b1, elseif c2, b2, else, b3, end' => "Matlab",
   'if (c) b1 elseif (c2) b2 else b3' => "PHP",
   'if (c): b1 elseif (c2): b2 else: b3 endif' => "PHP",
   'if (c) {b1} elsif (c2) {b2} else {b3}' => "Perl",
   'if (c) {b1} else {b2}' => "E Yorick",
   '(if c b1 b2)' => "CommonLisp Scheme",
   '(if c then b1 else b2)' => "Mercury",
   '(c -> b1 ; c2 -> b2 ; b3)' => "Mercury",
   'c -> b1 ; b2' => "FL",
   'c ifTrue: b1 ifFalse: b2' => "Smalltalk",
   'ifelse c [b1] [b2]' => "Logo",
   'shunt c b1 c2 b2 b3' => "Pliant",
   'either c b1 b2 / if/else c b1 b2' => "Rebol",
   '(cond (c b1) (c2 b2) (t b3))' => "EmacsLisp CommonLisp",
   '(cond (c b1) (c2 b2) (else b3))' => "Scheme",
   'c -> b1 ; c2 -> b2 ; b3' => "Prolog",
   'case when c; b1 when c2; b2 else b3 end' => "Ruby",
   'test c then b1 or b2' => "BCPL",
   'e | c = b1 | c2 = b2 | otherwise = b3 (the result goes to "e")' => "Haskell",
   'c b1 b2 ifelse' => "PostScript",
   'c if b1 else b2 then' => "Forth",
   'c ? b1 : b2' => "Awk B C C++ C# Java Perl Ruby PHP JavaScript YCP Yorick",
   'c ?? b1 :: b2' => "Perl6",
   'b1 if c else b2' => "Python",
   '$SELECT(c:b1,c2:b2,1:b3)' => "MUMPS",
   'c -> b1, b2' => "BCPL",
   '(if c then b1 else b2 fi)' => "Beta",

  pre('<xsl:choose>
    <xsl:when test="c"> b1 </xsl:when>
    <xsl:when test="c2"> b2 </xsl:when>
    <xsl:otherwise> b3 </xsl:otherwise>
</xsl:choose>') => "XSLT",

   'If c Then b1 Else b2' => "VisualBasic",  

   pre('If c
  b1
Else
  b2
End If') => "VisualBasic",

   pre('if c: 
  b1 
elif c2:
  b2 
else: 
  b3') => "Python",

   pre('if c
  b1
elsif c2
  b2
else
  b3
end') => "Ruby",

   pre('if c
  b1
elseif c2
  b2
else
  b3
end') => "Matlab",

   pre('if c then ; b1 ; else ; b2

if c then
  b1
else
  b2

if c then do
  b1
  ...
end
else do
  b2
  ...
end') => "ClassicREXX",

   pre('IF c b
ELSE b') => "MUMPS",

  ],

  'ifnot_then (unless)' => 
  [ { KIND => 'rare' },
   'unless' => "EmacsLisp Perl",
  ],

  'multiple selection (switch)' =>
  [ { MLANG => 'Awk Ruby Python Maple PostScript Lua Pliant Smalltalk XSLT' },
   pre('switch (val) { 
   case v1: expr1; break; 
   case v2: case v3: expr23; break; 
   default: expr_else;
 }') => "C C++ Pike Java PHP JavaScript",

   pre('switch val { 
   case v1: expr1; goto done;
   case v2: case v3: expr23; goto done; 
 }
 expr_else;
 done:
 ') => "B",

   pre('switch (val) { 
   case v1: expr1; break; 
   case v2: case v3: expr23; break; 
   default: expr_else; break;
 }') . ' ("break"s are mandatory, even for "default"!)' => "C#",

   pre('switch (val) { 
   match v1 { expr1 } 
   match v2 { expr2 } 
   match _ { expr_else }
}') => "E",

   pre('switchon val  
   case v1: expr1
   case v2: expr2
   default: expr_else') => "BCPL",

    pre('switch val
  case v1
    expr1
  case {v2,v3}
    expr2
  otherwise
    expr3
end') => "Matlab",

   pre('case val of
   v1 : expr1; 
   v2, v3 : expr23
   else expr_else
 end') => "Pascal",

   pre('case val in
   v1) statement1 ;;
   v2|v3) statement23 ;;
   *) statement_else ;;
esac') => "BourneShell",

   pre('(if val
    // v1 then expr1
    // v2 then expr2 
    else expr_else
    if)') => "Beta",

   pre('match val with
 | v1 -> expr1
 | v2 | v3 -> expr23
 | _ -> expr_else') => "OCaml",

   pre('case val of
   v1 => expr1
 | v2 => expr2
 | _ => expr_else') => "SML",

   pre('CASE val OF
   v1 => expr1
 | v2 => expr2
 ELSE => expr_else END') => "Modula-3",

   pre('case val of
   v1 -> expr1
   v2 -> expr2
   _ -> expr_else') => "Haskell",

   pre('val.
   v1 -> expr1
   v2 -> expr2
   _ -> expr_else') => "merd",

   pre('(case val
   ((v1) expr1)
   ((v2) expr2)
   (otherwise expr_else))') => "CommonLisp",

   pre('(case val
   ((v1) expr1)
   ((v2) expr2)
   (else expr_else))') => "Scheme",

   pre('case val is
   when v1 => expr1
   when v2 | v3 => expr23
   when others => expr_else
 end case;') => "Ada",

   pre('case val
   when v1; expr1
   when v2, v3; expr23
   else expr_else
 end') => "Ruby",

   pre('inspect val
   when v1 then statement1
   when v2, v3 => statement23
   else statement_else
 end') => "Eiffel",

   pre('select (val);
   when (v1) statement1;
   when (v2, v3) statement23;
   otherwise statement_else;
 end;') => "PL/I",

   pre('X = val,
(X = v1, expr1 ; X = v2, expr2 ; expr_else)') => "Prolog Mercury",

   pre('my %case = (
    v1 => sub { expr1 },
    v2 => sub { expr2 },
); 
if ($case{val}) { $case{val}->() } else { expr_else }') => "Perl",

   pre('use Switch;
switch ($val) {
    case v1 { expr1 }
    case v2 { expr2 }
    else expr_else
})') . ' (Perl >= 5.8.0)' => "Perl",

   pre('given $val {
    when v1 { expr1 }
    when v2 { expr2 }
    default { expr_else }
}') => "Perl6",

   pre('Select val
    Case v1
	expr1
    Case v2, v3
	expr2
    Case Else
	expr_else
End Select') => "VisualBasic",

  pre('switch (val) {
    v1 { expr1 }
    v2 { expr2 }
    default { expr_else }
  }') => "MSH",

  pre('switch val [
    v1 [expr1]
    v2 [expr2]
]

switch/default [
    v1 [expr1]
    v2 [expr2]
][expr_else]') => "Rebol",

   'val caseOf: {[v1]->[expr1]. [v2]->[expr2]} otherwise: expr_else' => "Squeak",

   pre('select
  when v1 expr1
  when v2 | v3 expr23
  otherwise expr_else
end') => "ClassicREXX",

   pre('CASE val
    WHEN v1 THEN expr1
    WHEN v2 THEN expr2
    ELSE expr_else
END') => "SQL92",

  ],

  'loop' => 
  [ { ALL => 1 },
    'forever loop' =>
    [ { MLANG => 'Awk B Maple C C++ C# Java Prolog E Lua Matlab Pascal JavaScript Haskell Perl Perl6 Python OCaml Smalltalk SML Tcl Eiffel Pliant' }, # Haskell would be: loop f = f >> loop f
     'loop' => "Ruby merd PostScript",
     'loop expr end loop' => "Ada",
     'LOOP expr END' => "Modula-3",
     '(loop do expr)' => "CommonLisp",
     "cycle (# do ... #) " => "Beta",
     'repeat' => "Squeak",
     'forever' => "Rebol Logo",
     pre('Do
    expr
Loop') => "VisualBasic",
     pre('do forever
  ...
end') => "ClassicREXX",

   ],

   'while condition do something' =>
   [ { MLANG => 'Prolog' },
    'while (cond) expr' => "Awk B C C++ C# Java E Pike Perl Perl6 Ruby PHP JavaScript YCP Yorick",
    'while cond expr' => "Tcl",
    'while cond loop expr end loop' => "Ada",
    'while cond do expr' => "BCPL SML Pascal",
    'while cond do expr done' => "OCaml",
    'while cond do expr end do' => "Maple",
    'WHILE cond DO expr end' => "Lua",
    'while cond: expr' => "Python",
    'while cond; do expr; done' => "BourneShell",
    'while cond, expr, end' => "Matlab",
    'while [cond][expr]' => "Rebol",
    'while cond [expr]' => "Logo",
    'do.while [expr] cond' => "Logo",
    'cond whileTrue: expr' => "Smalltalk",
    '(loop while cond do expr)' => "CommonLisp",
    'loop (# while ::< (# do cond -> value #) do expr #) ' => "Beta",
    'begin cond while expr repeat' => "Forth",
    'from until not cond loop expr end' => "Eiffel",
    pre(q(while cond
    expr)), => "Pliant",
    pre('Do While cond 
    expr
Loop') => "VisualBasic",
    pre('do while cond
  ...
end') => "ClassicREXX",
   ],

   'do something until condition' =>
   [ { MLANG => 'Python Maple OCaml SML Eiffel Matlab Pliant Smalltalk Tcl' },
    'do expr until cond' => "Perl6",
    'do {expr} until cond' => "Perl",
    'do {expr} while (!cond) ' => "C C++ C# Java Awk Pike JavaScript Yorick",
    'begin expr end until cond' => "Ruby",
    'REPEAT expr UNTIL cond' => "Modula-3", 
    'loop (# until ::< (# do cond -> value #) do expr #) ' => "Beta",
    'loop expr exit when cond end loop' => "Ada",
    '(loop do expr until cond)' => "CommonLisp",
    'expr repeatuntil cond' => "BCPL",
    'repeat expr until cond' => "Lua Pascal",
    'repeat expr until (cond) ' => "YCP",
    'repeat, expr, cond' => "Prolog",
    'until [expr cond]' => "Rebol",
    'until cond [expr]' => "Logo",
    'do.while [expr] cond' => "Logo",
    '[expr . cond] whileFalse' => "Squeak",
    pre('Do 
expr
Loop Until cond') => "VisualBasic",
   ],

   'for each value in a numeric range, 1 increment (see also the entries about ranges)' =>
   [ { MLANG => 'Eiffel Prolog' },
    'for (int i = 1; i <= 10; i++) expr' => "C C#",
    'foreach my $i (1 .. 10) { expr }' => "Perl",
    'foreach ($i in 1..10) { expr }' => "MSH",
    'for (1 .. 10) -> $i { expr }' => "Perl6",
    'for i = 1:10, expr, end' => "Matlab",
    'for i := 1 to 10 do expr' => "Pascal",
    'for i = 1 to 10 do expr done' => "OCaml",
    'For i = 1 To 10 expr Next' => "VisualBasic",
    'for i in 1 .. 10 loop ... end loop' => "Ada",
    'for i in range(1, 11)' => "Python",
    'for i in `seq 1 10`; do expr; done' => "BourneShell",
    'FOR I=1:1:10 expr' => "MUMPS",
    'for i from 1 to 10 do expr end do' => "Maple",
    'for [i 1 10 +1] [expr]' => "Logo",
    '1 1 10 expr for' => "PostScript",
    '(1..10).each {|i| expr }' => "Ruby",
    '1.upto(10) {|i| expr }' => "Ruby",
    pre('do i = 1 for 10
  ...
end') => "ClassicREXX",

   ],

   'for each value in a numeric range, 1 decrement' =>
   [ { MLANG => 'Eiffel Prolog' },
    'for X := 10 downto 1 do expr' => "Pascal",
    'for i = 10 downto 1 do expr done' => "OCaml",
    'for i in reverse 1 .. 10 loop ... end loop' => "Ada",
    'for (int i = 10; i >= 1; i--) expr' => "C C#",
    'for (my $i = 10; $i >= 1; $i--) { expr }' => "Perl",
    'loop (my $i = 10; $i >= 1; $i--) { expr }' => "Perl6",
    'from i := 10 until i < 1 loop expr i := i - 1 end' => "Eiffel",
    'for i = 10:-1:1, expr, end' => "Matlab",
    'for i in range(10, 0, -1)' => "Python",
    'for i in `seq 10 -1 1`; do expr; done' => "BourneShell",
    'for i from 10 to 1 by -1 do expr end do' => "Maple",
    'for [i 1 10 -1] [expr]' => "Logo",
    'FOR I=10:-1:1 expr' => "MUMPS",
    '10 -1 1 expr for' => "PostScript",
    '10.downto(1) {|i| expr }' => "Ruby",
    pre('do i = 10 to 1 by -1
  ...
end') => "ClassicREXX",
   ],

   'for each value in a numeric range, free increment' =>
   [ { MLANG => 'OCaml Eiffel Prolog' },
    'for (int i = 1; i <= 10; i += 2) expr' => "C C# Pike",
    'for (i = 1; i <= 10; i += 2) expr' => "Awk",
    'for (my $i = 1; $i <= 10; $i += 2) { expr }' => "Perl",
    'loop (my $i = 1; $i <= 10; $i += 2) { expr }' => "Perl6",
    'from i := 1 until i > 10 loop expr i := i + 2 end' => "Eiffel",
    'for i = 1:3:10, expr, end' => "Matlab",
    'for i in range(1, 11, 2)' => "Python",
    'for ((x = 1; x <= 10; x += 2)); do ...; done' => "BourneShell",
    'for i from 1 to 10 by 2 do expr end do' => "Maple",
    'for [i 1 10 2] [expr]' => "Logo",
    'FOR I=1:2:10 expr' => "MUMPS",
    '1 2 10 expr for' => "PostScript",
    '(1..10).step(2) {|i| expr }' => "Ruby",
    pre('do i = 1 to 10 by 2
  ...
end') => "ClassicREXX",
   ],

   'for "a la C" (while + initialisation)' =>
   [ { MLANG => 'Ada B Pascal Maple Python Prolog Rebol Matlab OCaml Smalltalk SML CommonLisp Ruby Pliant ClassicREXX' },
    'for' => "Awk C C++ C# Java Perl Pike Tcl PHP MSH JavaScript Yorick",
    'loop' => "Perl6",
    'for ((x = 0; x < 10; x++)); do ...; done' => "BourneShell",
    'from init_code until cond loop ... incr_statement end' => "Eiffel",
     # 'for var in range loop exp end loop' => "Ada",
   ],

  ],

  'breaking control flow' =>
  [ { ALL => 1, MLANG => "OCaml SML Beta Prolog PostScript Eiffel Oz", },
   'returning a value' => $::return_a_value,

   'goto (unconditional jump)' =>
   [ { MLANG => 'Awk Prolog Ruby Maple merd Beta E Matlab Python Lua PostScript Smalltalk Java Pliant JavaScript' },
    'goto' => "BCPL B C C++ C# Perl Basic Logo MUMPS Pascal Fortran Cobol Ada Yorick",
    'go throw' => "CommonLisp",
    'signal' => "ClassicREXX",
   ],

   'continue / break' =>
   [ { MLANG => 'B Prolog Smalltalk' },
    'continue / break' => "Awk C C++ C# Java E Matlab Pike JavaScript Python PHP YCP Yorick",
    'next / last' => "Perl Perl6",
    'next / break (in Ruby, see also catch/throw)' => "Maple Ruby",
    '/ break' => "BCPL Lua",
    '/ break/return' => "Rebol",
    '/ exit' => "Ada PostScript",
    '/ stop' => "Logo",
    "restart / leave" => "Beta Pliant",
    '/ Exit Do, Exit For' => "VisualBasic",
    '/ return from xxx  or  return' => "CommonLisp",
    'iterate / leave' => "ClassicREXX",
   ],

   'redo / retry' =>
   [ { KIND => 'rare' },
    'redo/' => "Perl Perl6",
    'redo / retry' => "Ruby",
   ],

  ],

  'exception' =>
  [ { ALL => 1, MLANG => 'Awk B C XSLT Pascal' },
   'throwing' =>
   [ { MLANG => 'Scheme' },
    'raise' => "Python SML OCaml Scheme-SRFI34 merd Ruby Eiffel Ada",
    'RAISE' => "Modula-3",
    'raise ... end' => "Oz",
    'throw' => "C# C++ Java Prolog Pike Logo E Erlang Rebol JavaScript Haskell",
    'throw/name' => "Rebol",
    'die' => "Perl Perl6",
    'return -code' => "Tcl",
    'error' => "EmacsLisp Maple CommonLisp Dylan Matlab Lua Pliant Lua",
    'signal' => "CommonLisp Dylan Smalltalk",
    'signal predefined_condition_name' => "ClassicREXX",
    'cerror warn' => "CommonLisp",
    '[NSException raise:name ...]' => "Objective-C",
   ],

   'catching' =>
   [ { MLANG => 'Scheme' },
    'try: a except exn: b' => "Python",
    'try a with exn -> b' => "OCaml",
    'try a catch (exn) b' => "C++ C# Java JavaScript",
    'try a catch exn then b end' => "Oz",
    'try a catch exn: b end try' => "Maple",
    'try { a CATCH exn { b } }' => "Perl6",
    'TRY a EXCEPT exn => b END' => "Modula-3",
    'a handle exn => b' => "SML",
    'a on: exception_name do: [:exn | b]' => "Smalltalk",
    'ifCurtailed' => "Smalltalk",
    'rescue' => "Eiffel Ruby",
    'eval {a}; if ($@) b' => "Perl",
    'exception when exception_name =>' => "Ada",
    'catch a (\exn -> b) ' => "Haskell",
    'catch' => "Erlang Prolog Logo Rebol Tcl",
    'catch/name' => "Rebol",
    'catch(expr) or catch { ... };' => "Pike",
    'pcall' => "Lua",
    'with-exception-handler or guard' => "Scheme-SRFI34",
    'block a exception(exn) b end' => "Dylan",
    '?, shy, safe' => "Pliant",
    'handler-bind handler-case ignore-errors' => "CommonLisp",
    'NS_DURING a NS_HANDLER b NS_ENDHANDLER' => "Objective-C",
     pre('try
  a
catch
  b
end') => "Matlab",

    pre('signal on predefined_condition_name
...
predefined_condition_name :
  ...
') => "ClassicREXX",
   ],

   'cleanup: code executed before leaving' =>
   [ { KIND => 'rare' },
    'ensure' => "Smalltalk Ruby", # ensure is there in Eiffel but is meant for postconditions, and is turned down when optimising
    'finally' => "C# Java Maple Python",
    'FINALLY' => "Modula-3",
    'unwind-protect' => "EmacsLisp CommonLisp",
    'cleanup' => "Dylan",
    'dynamic-wind' => "Scheme",
   ],

   'retrying: after catching an exception, tell the snippet to be re-run' =>
   [ { KIND => 'rare' },
    'retry' => "Eiffel Smalltalk Ruby",
    'restart' => "Dylan",
    'continue' => "Python",
   ],

   'resume execution where the exception took place' =>
   [ { KIND => 'rare' },
    'resume' => "Smalltalk",
   ],
  ],

  'call-with-current-continuation' =>
  [ { KIND => 'rare' },
   'call-with-current-continuation (often provided in the abbreviated form call/cc)' => 'Scheme',
   'callcc' => 'Ruby SML-NJ',
  ],

],

'Types' => [
  { KIND => 'typed' },

  'declaration' =>
  [ { MLANG => 'Smalltalk' },
   'typedef t n' => "C C++ Pike",
   'type n is t' => "Ada",
   'type n ...' => "Pliant",
   'type n = t' => "OCaml Haskell SML Pascal",
   'TYPE n = t' => 'Modula-3',
   'using n = ...' => "C#",
   'data n = t' => "Haskell",
   'datatype n = t' => "SML",
   'newtype n = t' => "Haskell",
   'n = t' => "merd",
   'n : t ' => "Beta",
   "(deftype n () 't)" => "CommonLisp",
  ],

# datatype declaration
#
# Haskell: data T = C1 t u ... | C2 v ...
#          newtype T = C1 t u ...
# SML:     datatype T = C1 of t u ... | C2 of v ...

  'annotation (or variable declaration)' =>
  [ { MLANG => 'Smalltalk' },
   ':' => "SML Pascal OCaml Eiffel E Ada Modula-3",
   '::' => "Haskell Mercury Dylan",
   '!!' => "merd",
   't v' => "C C++ C# Java Pike Pliant Perl6 YCP",
   '(declare (v t))' => "CommonLisp",
   'v :@ t' => "Beta",
   'typeset' => "BourneShell",
  ],

  'cast' => 
  [ { MLANG => "Smalltalk SML" },

    'upcast' =>
    [
     '(t) e' => "C C++ C# Java PHP",
     't(e)' => "Pascal Ada",
     '[t] e' => "Pike",
     'static_cast<t>(e)' => "C++",
     'e :> t' => "OCaml",
     'CAST(e as t)' => "SQL92", 
     'typecast(e,t)' => "Matlab",
    ],

    'downcast (need runtime checking)' =>
    [
     '(t) e' => "Java",
     't(e)' => "Ada",
     'e : t' => "E",
     '[t] e' => "Pike",
     'dynamic_cast<t>(e)' => "C++",
     'e as t' => "C#",
     'v ?= e (expression "e" is cast to the type of "v")' => "Eiffel",
     'NARROW(e, t)' => "Modula-3",
     'typecast(e,t)' => "Matlab",
    ],

    'computed conversion (calls an internal or a user-defined function)' =>
    [     
     '(t) e' => "C++ Pike",
     '[t] e' => "MSH",
     't(e)' => "C++ Matlab",
     'e : t' => "E",
     'cast e t' => "Pliant",
     'expr cast t' => "Pliant",
     'make t e / to t e' => "Rebol",
    ],
  ],

  'mutability, constness' =>
  [ { ALL => 1, MLANG => "Smalltalk Eiffel" },
   'type of a mutable value' =>
   [
    'it is the default' => "Ada C C++ Java Matlab C#",
    'val x: T' => "Pascal",
    'T ref' => "SML OCaml",
    'STRef a T' => "Haskell",
   ],

   'type of a constant value' =>
   [ { MLANG => "C C# Matlab" },
    'const T' => "C99 C++",
    'constant T' => "Ada",
    'const x: T' => "Pascal",
    'it is the default' => "SML OCaml Haskell",
   ],

   'special cases' =>
   [ { KIND => 'rare' },
    '"readonly" fields (quite bad: only the reference is non-mutable whereas the object is still mutable)' => "C#",
    '"final" fields, parameters, local variables (quite bad: only the reference is non-mutable whereas the object is still mutable)' => "Java",
   ],
  ],
],

'Object Oriented & Reflexivity' => [
  { KIND => 'OO' },
  'method invocation' =>
  [ { ALL => 1 },
    '' =>
    [
     'object.method(para)' => "C++ C# Java E MSH Python Perl6 Beta Cecil Delphi-Kylix Eiffel Sather Modula-3 Ruby VisualBasic Icon merd JavaScript",
     'object#method para' => "OCaml",
     'object:method(para)' => "Lua",
     'object method para' => "Tcl Pliant",
     'object method: para1 method_continuation: para2' => "Smalltalk",
     'object <- method(para) (eventual send)' => 'E',
     '[ object method: para ]' => "Objective-C",
     'object->method(para)' => "Pike Perl PHP C++",
     'object["method"](para)' => "Pike",
     'object/method para' => "Rebol",
     'method object para' => "Haskell Mercury",
     '(method object para)' => "CommonLisp",
     'method(object, para)' => "Dylan Ada Matlab",
     'para->method' => "Beta",
     '(send object method para)' => "MzScheme",
    ],

    'with no parameter' =>
    [
     'object.method' => "merd Ruby Eiffel Perl6",
     'object.property (properties are something alike attributes, but really are methods)' => "C#",
     'object.method()' => "C# C++ Java E Python JavaScript",
     'object#method' => "OCaml",
     'object:method' => "Pliant",
     'object->method' => 'Perl',
     'object->method()' => "Pike",
     'object/method' => "Rebol",
     'object["method"]()' => "Pike",
     'object method' => "Smalltalk",
     '[ object method ]' => "Objective-C",
     'method object' => "Haskell Mercury",
     '(method object)' => "CommonLisp",
     'method(object)' => "Dylan Ada Matlab",
     '(send object method)' => "MzScheme",
    ],
  ],

  'object creation' =>
  [
   'new' => "Ada Simula PHP Pliant",
   'new class_name(...)' => "C++ C# Java Perl JavaScript VisualBasic",
   'new class_name ...' => "OCaml",
   'class_name.new(...)' => "Ruby Perl6",
   'class_name new' => "Smalltalk",
   'class_name(...)' => "Pike Python Matlab",
   'class_name v(...)' => "C++",
   'class_name.Create' => "Delphi-Kylix",
   '!class_name!constructor_name(...)' => "Eiffel",
   '&'  => "Beta",
   'make-object' => "MzScheme",
   '(make-instance class_name ...)' => "CommonLisp",
   '[class_name alloc]' => "Objective-C",
   'make class_name! ...' => "Rebol",
   'def object_name { ... }' => "E",
  ],

  'object cloning' =>
  [ { MLANG => 'E' },
   'o.clone' => "Perl6",
   'o.clone (one level depth)' => "Ruby Eiffel",
   'o.deep_clone' => "Eiffel",
   'o.clone()' => "Java",
   'o.Clone()' => "C#",
   'clone / copy or deepCopy' => "Smalltalk",
   'dclone' => "Perl",
   '[o copy]' => "Objective-C",
   'copy.copy(o) (general deep copy function)' => "Python",
   'purecopy' => "EmacsLisp",
   '{< >}  or  Oo.copy o' => "OCaml",
   'o2 = o (object cloning is the default, uses the copy constructor in C++)' => "C++ PHP Matlab",
   'o2.all := o.all' => "Ada",
   'make o []' => "Rebol",
  ],

  "manually call an object's destructor" =>
  [ { MLANG => 'OCaml Eiffel E Smalltalk Matlab Ruby Java' },
   'delete' => "C++ JavaScript",
   'destroy' => "Pike",
   'DESTROY' => "Perl",
   'dealloc' => "Objective-C",
   'Dispose' => "C#",
   '__del__' => "Python",
   'Requires instantiation of Ada.Unchecked_Deallocation' => "Ada",
  ],

  'class declaration' =>
  [ { MLANG => "Perl E" },
   'class' => "C# C++ Java Perl6 Matlab Pike Ruby Python OCaml Haskell PHP MzScheme",
   'class c inherit p1 p2 ... feature decl decl ... end' => "Eiffel",
   'defclass defstruct' => "CommonLisp",
   'subclass' => 'Smalltalk',
   'type' => "Pliant",
   'type c is tagged record ... end record' => 'Ada',
   '@interface c { ... } ... @end' => "Objective-C",
   ':' => "Beta"
  ],

  'testing class membership' =>
  [ { MLANG => 'OCaml' },
   'isa' => "Perl Matlab",
   'is_a? kind_of?' => "Ruby",
   'o.meta.isa' => "Perl6",
   'isKindOf' => "Smalltalk",
   'isKindOfClass' => "Objective-C",
   'dynamic_cast' => "C++",
   'instanceof' => "Java JavaScript",
   'isinstance' => "Python",
   'in' => "Ada",
   'is' => "C#",
   'is_a' => "PHP",
   'Program.inherits or Program.implements' => "Pike",
   'entry_type' => "Pliant",
   'typep' => "CommonLisp",
   'ISTYPE' => 'Modula-3',
   'object## < classname##' => "Beta",
   'type.accepts(object) / object =~ v : type' => "E",
   'var ?= val (assignment attempt)' => "Eiffel",
  ],

  'get the type/class corresponding to an object/instance/value' =>
  [ { KIND => 'reflexive' },
    'class' => "Ruby Smalltalk Matlab Objective-C",
    '__class__' => "Python",
    'getClass' => "Java",
    'typeid' => "C++",
    'typeof' => "C# JavaScript",
    'type-of' => "CommonLisp",
    'ref' => "Perl",
    'generator' => "Eiffel",
    'meta' => "Perl6",
    'object_program' => "Pike",
    'getAllegedType' => "E",
  ],

  'methods available' =>
  [ { KIND => 'reflexive', MLANG => "Perl Pliant" },
   'methods' => "Ruby Matlab",
   'get_class_methods' => "PHP",
   'getMethods' => "Java",
   'get-member' => "MSH",
   'indices' => "Pike",
   'o.meta.getmethods' => "Perl6",
   'o.__class__.__dict__ (does not work on builtin types)' => "Python",
   'o class selectors / o class allSelectors' => "Smalltalk",
   'o.__getAllegedType().getMessageTypes()' => "E",
  ],

  'inheritance' => 
  [
   "child :< parent " => "Beta",
   "class child : parent" => "C# C++",
   'class child < parent end' => "Ruby",
   'class child is parent { ... }' => "Perl6",
   "class child extends parent" => "Java",
   "class child(parent):" => "Python",
   'class child inherit parent end' => "Eiffel",
   'parent subclass: child' => "Smalltalk",
   'make parent ...' => "Rebol",
   'inherit' => "Pike OCaml",
   'def child extends makeSuperObject(parent, ...) { ... }' => "E",
   'type child is new parent with record ... end record' => "Ada",
   '(defclass child (parent) ...)' => "CommonLisp",
   '@interface child : parent { ... } ... @end' => "Objective-C",
   '@ISA = qw(parent1 parent2)' => "Perl",
  ],
				     
  'has the method' =>
  [ { KIND => 'reflexive dynamic' },
    'can' => "Perl Perl6",
    'respond_to?' => "Ruby",
    'respondsTo' => "E Smalltalk",
    'respondsToSelector' => "Objective-C",
    'hasattr(obj, "meth") (see also callable(obj.meth) for <a href="http://www.python.org/doc/current/ref/types.html">unbound methods</a>)' => "Python",
    'object->method' => "Pike",
    "all [in object 'method function? get in object 'method]" => "Rebol",
    'find-method' => "CommonLisp",
    'ismethod' => "Matlab",
  ],

  'current instance' =>
  [ { MLANG => 'OCaml' }, # OCaml can access current instance by binding it to a name
   'this' => "C++ C# Java JavaScript Pike PHP Beta",
   'THIS' => "Simula",
   'self' => "Smalltalk Rebol Ruby Objective-C",
   'object_name if defined as: def object_name { ... }' => "E",
   'Current' => 'Eiffel',
   'first parameter (in Python, usually called self)' => "Perl Python Matlab Pliant",
   'dispatching parameter' => "Ada CommonLisp",
   'Me' => "VisualBasic",
   '.' => "Perl6",
  ],

  'accessing parent method' =>
  [ { MLANG => 'Python C++ OCaml' },
   'super' => "Java E Smalltalk Ruby Objective-C",
   'super(Class, self).meth(args)' => "Python",    
   'base' => "C#",
   'Precursor' => "Eiffel",
   '$o.SUPER::method(...)' => "Perl6",
   '$o->SUPER::method(...)' => "Perl",
   'method(parent(dispatching-parameter))' => "Ada",
   'call-next-method' => "CommonLisp",
  ],

  'accessing child method' =>
  [ { KIND => 'rare' },
   "inner" => "Beta",
  ],

],

'Package, Module' => [
  { MLANG => 'C Awk Eiffel Pliant EmacsLisp Logo Smalltalk ClassicREXX' },

  'package scope' =>
  [
   '.' => "C# Java Python Ruby Modula-3 SML OCaml Pascal E Ada Haskell",
   ':' => "XML",
   '::' => "C++ Perl merd Tcl YCP",
   ': :: (":" is for external symbols only, recommended)' => "CommonLisp",
   ':-' => "Maple",
   "'" => "Perl",
   '__' => "Mercury",
   '/' => "Matlab",
  ],

  # many info taken from http://www.cs.odu.edu/~zeil/cs355/Lectures/3modoop/modules_summary.pdf

  'declare' =>
  [
   '' =>
   [
    'package p;' => "Perl Java",
    'namespace p { ... }' => "C++ C#",
    'module p where ...' => "Haskell",
    'module P ... end' => "Ruby",
    'module P = struct ... end' => "OCaml",
    '{ module "p"; ... }' => "YCP",
    ':- module(p)' => "Prolog",
    'p = module() ... end module' => "Maple",
    "(defpackage p ...)" => "CommonLisp",
    'automatically done based on the file name' => "Python OCaml",
    'package declare (directory name is package name)' => "Matlab",
    '<node xmlns="namespace"> ... </node>' => "XML",
    pre('package p is
   -- Declare public package members here
private
   -- Declare private package members here
end p;

package body p is
 ... -- Define package implementation here
end p;') => "Ada",
   ],
   
   'selective export' =>
   [
    'module p (name1, name2, ...) where ...' => "Haskell",
    '@ISA = qw(Exporter); @EXPORT = qw(name1 name2 ...);' => "Perl",
    'package p is ... end; package body p is ... end;' => "Ada",
    'p = module() export name1, name2, ...; ... end module' => "Maple",
    "(export 'name1 'name2)" => "CommonLisp",
    'attached to each name (public, private...) ' => "Java Pike",
    'append_features' => "Ruby",
    pre(q(module type PType = sig val name1 : type1 ... end
module P : PType  = struct ... end)) => "OCaml",
    'all files in package directory are exported. files in /private sub-directory are not exported, but can be used by the package itself' => "Matlab",
   ],
  ],

  'import' =>
  [
   'everything into current namespace' => 
   [
    'use p (if names are exported using @EXPORT)' => "Perl",
    'uses p' => "Pascal",
    'using p' => "C#",
    'using namespace p;' => "C++",
    "(use-package 'p)" => "CommonLisp",
    'open p' => "OCaml",
    'import' => "Pike",
    'import p' => "Haskell",
    'IMPORT p;' => "Modula-2",
    'import p.*' => "Java",
    'import "p"' => "YCP",
    'from p import *' => "Python",
    'with p; use p;' => "Ada",
    'inherit c export {NONE} all end' => "Eiffel",
    'include or even extend' => "Ruby",
    'do' => "Rebol",
    'addpath' => "Matlab",
    '. p' => "BourneShell",
    'source p' => "BourneShell",
   ],

   'selectively' =>
   [
    'import p (name1, name2, ...) ' => "Haskell",
    'import p.name1; import p.name2' => "Java",
    "(import '(p:name1 p:name2))" => "CommonLisp",
    'use p qw(name1 name2 ...)' => "Perl",
    'from p import name1, name2, ...' => "Python",
    'FROM p IMPORT name1, name2, ...;' => "Modula-2",
    'using p::name1; using p::name2; ...' => "C++",
    'with p; use type p.type1; ...' => "Ada",
    'with(p[name1, name2,])' => "Maple",
    'def name := <import:p.name>' => "E",
    ':- use_module(name1, name2, ...)' => "Prolog",
   ],

   'package (ie. load the package)' =>
   [
    'import p' => "Python",
    'use p; (if names are not exported or are exported using @EXPORT_OK)' => "Perl",
    'require p' => "Perl",
    'require "p"' => "Ruby",
    'require, "p"' => "Yorick",
    "(require 'p) (deprecated in ANSI Common Lisp, but used in ASDF)" => "CommonLisp",
    'with p;' => "Ada",
    'with(p)' => "Maple",
    'automatically done (using a correspondance from the package name to the file name)' => "OCaml Java",
   ],
  ],
],

'Strings' => [

  'type name' =>
  [ { KIND => 'typed' },
   'char[]' => "C",
   'char const[]' => "C++",
   'string' => "C++ C# Maple OCaml Pike Pascal YCP",
   'string!' => "Rebol",
   'String' => "C# merd Haskell Java JavaScript VisualBasic Smalltalk Ada",
   'STRING' => "Eiffel",
   'str' => "YAML",
   'Str' => "Perl6 Pliant",
   'NSString *' => "Objective-C",
   'CHAR, VARCHAR(size)' => 'SQL92',
  ],

  'character type name' =>
  [ { KIND => 'typed', MLANG => "Maple Pike Pliant VisualBasic" },
   'char' => "C C++ C# OCaml",
   'char!' => "Rebol",
   'Char' => "merd Haskell Perl6",
   'Character' => "Smalltalk Ada",
   'CHARACTER' => "Eiffel",
  ],

  'character "z"' =>
  [ { MLANG => "Awk Perl Perl6 Python JavaScript Prolog" },
   "'z'" => "B C C++ C# E OCaml Matlab Haskell Pike Pascal Eiffel Ada ClassicREXX",
   '"z"' => "Maple merd BourneShell ClassicREXX",
   '"z' => "Logo",
   '$z' => "Smalltalk",
   '#\z' => "Scheme CommonLisp",
   '#"z"' => "Rebol",
   '&z' => "Oz",
   '?z' => "Ruby EmacsLisp",
  ],

  'strings' =>
  [ { ALL => 1 },
   'verbatim' =>
   [
    "'...'" => "Perl Perl6 XPath YAML Python Matlab Ruby PHP Lua JavaScript Pascal Smalltalk BourneShell Beta Prolog ClassicREXX SQL92",
    '"..."' => "C C++ C# Maple XPath Java YCP YAML E Prolog Rebol Pike Python MUMPS EmacsLisp Scheme CommonLisp OCaml Ada Haskell SML Eiffel JavaScript Dylan Lua Awk Modula-3 Pliant FL Oz ClassicREXX",
    '"...' => "Logo",
    q('''...''', """...""") => "Python",
    '[[ ... ]]' => "Lua",
    "<<'MARK' ... MARK" => "BourneShell Perl Ruby",
    '{...{...}...}' => "Tcl",
    '(...)' => "PostScript",
    'q(...(...)...), q[...], q{...}, q<...>, q/.../' => "Perl Perl6",
    '%q(...(...)...), %q[...], %q{...}, %q<...>, %q/.../' => "Ruby",
    'q(...(...)...)' => "merd",
    '@"...""..."' => "C#",
    '@"..."' => "Objective-C",
   ],

   'with interpolation' =>
   [ { MLANG => "Haskell Beta Maple Logo Prolog C# ClassicREXX Eiffel" },
    '"... $v ..."' => "Perl Perl6 Tcl PHP BourneShell",
    '"... {v} ..."' => "merd",
    '"... #{v} ..." "... #$v ..." "... #@v ..." "... #@@v ..."' => "Ruby",
    '<<MARK ... $v ... MARK' => "BourneShell Perl",
    '<<MARK ... #{v} ... MARK' => "Ruby",
    '<<<MARK ... $v ... MARK' => "PHP",
    '[ subst {...{... $v ...}...} ]' => "Tcl",
    'qq(...(... $v ...)...), qq[...], qq{...}, qq<...>, qq/.../' => "Perl Perl6",
    '%Q(...(... #{v} ...)...), %Q[...], %Q{...}, %Q<...>, %Q/.../' => "Ruby",
    'qq(...(... {v} ...)...) ' => "merd",
    '"... %(v)s ..." % vars()' => "Python",
   ],

   'end-of-line (without writing the real CR or LF character)' =>
   [ { MLANG => "ClassicREXX Matlab Logo" },
    '"\n"' => "C C++ C# Perl Perl6 Maple Lua OCaml Python Ruby YCP Pike Java JavaScript Yorick",
    '"*n"' => "B BCPL",
    '"%N"' => "Eiffel",
    '"^/"' => "Rebol",
    '"~%" (when using format)' => "CommonLisp",
    '"[lf]"' => "Pliant",
    'vb_nl' => "VisualBasic",
   ],  
  ],

  'multi-line' =>
  [ { MLANG => "C Eiffel Prolog" },
   'all strings allow multi-line strings' => "E Maple Perl Perl6 OCaml Ruby Scheme Pascal CommonLisp EmacsLisp YCP",
   '@"..."' => "C#",
    q('''...''', """...""") => "Python",
    '[[ ... ]]' => "Lua",
    '{...}' => "Rebol",
    pre(q("...\n"
"...\n")) => "C",
    pre(q(... "...\n\\
    \\...\n")) => "Haskell",
    pre(q("...",
"...")) => "ClassicREXX",
    pre(q("...%N%
%...%N")) => "Eiffel",
  ],

  'convert something to a string (see also string interpolation)' =>
  [ { MLANG => 'Awk B C C++ Beta Prolog OCaml Logo SML BourneShell ClassicREXX' }, #- everything is a string in BourneShell
   'show' => "Haskell",
   'to_s, to_str, inspect, String()' => "Ruby",
   'to_string' => "merd Pliant",
   'tostring' => "Lua YCP",
   'toString' => "Java JavaScript",
   'ToString' => "C#",
   'String' => "JavaScript",
   'perl' => "Perl6",
   'Dumper' => "Perl",
   '"" . e' => "Perl",
   '"" ~ e' => "Perl6",
   '"" + e' => "Java E JavaScript",
   'string' => "Pliant",
   'str, `e`, repr' => "Python",
   'out' => "Eiffel",
   'cvs' => "PostScript",
   "'Image" => "Ada",
   'asString printString' => "Smalltalk",
   'as(<string>, e)' => "Dylan",
   '(string) e' => "Pike",
   'convert(e,string)' => "Maple",
   "(coerce e 'string)" => "CommonLisp",
   'prin1-to-string' => "EmacsLisp",
   'to string! / to-string / to ""' => "Rebol",
   'description' => "Objective-C",
  ],

  'serialize (marshaling)' =>
  [ { MLANG => 'Prolog Logo' },
   'export-clixml' => "MSH",
   'Marshal.to_string' => "OCaml",
   'Marshal.dump' => "Ruby",
  ],

  'unserialize (un-marshaling)' =>
  [ { MLANG => 'Prolog Logo' },
   'import-clixml' => "MSH",
   'Marshal.from_string' => "OCaml",
   'Marshal.load' => "Ruby",
  ],

  'sprintf-like' =>
  [ { MLANG => 'B Ada Haskell SML Eiffel Java JavaScript Scheme Smalltalk ClassicREXX' },
   'sprintf' => "Awk Maple C C++ Pike Matlab Perl Perl6 Ruby OCaml merd PHP",
   '%' => "Python Ruby",
   'format (but not using the C-like %-syntax)' => "Scheme-SRFI28 CommonLisp Erlang",
   'Format' => "C#",
   "putFormat" => "Beta",
   'stringWithFormat' => "Objective-C",
  ],

  'simple print' =>
  [
   'on strings' =>
   [
    'puts' => "C Dylan",
    'print' => "Awk Java Maple merd Basic SML PHP",
    'write' => "Pascal Pike JavaScript Yorick",
    'putStr' => "Haskell",
    'print_string' => "OCaml",
    'console' => "Pliant",
    'writeln' => "Pascal JavaScript",
    'write-string' => "CommonLisp",
    'putStrLn' => "Haskell",
    'Put_Line' => "Ada",
    'display' => "Cobol",
    'message' => "EmacsLisp",
    'put_string' => "Eiffel",
    'show' => "Smalltalk",
    'print_endline (adding an end-of-line)' => "OCaml",
    'println (adding an end-of-line)' => "Java merd",
    'put_chars' => "Erlang",
    'echo (in BourneShell, adding an end-of-line)' => "PHP BourneShell",
    'emit' => "Forth",
    "putText" => "Beta",
    'say' => "ClassicREXX",
   ],

   'on simple objects' =>
   [ { MLANG => "Beta"},
    'print' => "Perl Perl6",
    'say (adding an end-of-line)' => "Perl6",
    'puts (adding an end-of-line)' => "Tcl",
   ],

   'on any objects' =>
   [
    'print' => "Ruby Logo",
    'print (adding an end-of-line)' => "Python Haskell Rebol Dylan",
    'prin' => "Rebol",
    'Put' => "Ada",
    'p (adding an end-of-line)' => "Ruby",
    'puts (adding an end-of-line unless already newline-terminated)' => "Ruby",
    'display' => "Scheme",
    'write' => "Prolog Scheme CommonLisp",
    'print' => "CommonLisp",
    'princ prin1' => "CommonLisp EmacsLisp",
    'WriteLine' => "C#",
    'nothing - just remove ";" at the end of the expression, and it will print it' => "Matlab",
    'disp' => "Matlab",
   ],

   'printf-like' =>
   [
    'printf' => "C C++ Maple Matlab Perl Ruby OCaml merd Awk PHP",
    'write' => "Pike",
    'WriteLine' => "C#",
    'putFormat' => "Beta",
    'format (but not using the C-like %-syntax)' => "CommonLisp Prolog",
   ],
  ],

  'string equality & inequality' =>
  [
   'eq ne' => "Perl Perl6 Tcl",
   'strcmp' => "C Matlab",
   '== !=' => "Pike JavaScript",
   'isEqualToString (faster than isEqual)' => "Objective-C",
   '= \=' => "Prolog",

   # below are deep equality/inequality entries
   '== !=' => "Awk C++ C# E Ruby merd Python YCP",
   '== /=' => "Haskell",
   '== \=' => "Oz",
   '= !=' => "XPath BourneShell Maple",
   '= /=' => "Ada",
   '= \=' => "ClassicREXX",
   '= <>' => "OCaml VisualBasic SML Beta Pliant",
   '= ~=' => "Dylan Smalltalk",
   '== \== or = <> \=' => "ClassicREXX",
   '== ~=' => "Matlab",
   'equal?' => "Ruby Scheme",
   'equals' => "Java",
   'equal, equalp' => "CommonLisp",
   'is_equal' => "Eiffel",
   'isEqual' => "Objective-C",
  ],

  'string size' =>
  [
   'length' => "Awk C++ Maple Perl Prolog Ruby Haskell PostScript Matlab CommonLisp OCaml Java JavaScript Beta Eiffel Objective-C",
   'LENGTH' => "ClassicREXX",
   "'Length" => "Ada",
   'length?' => "Rebol",
   'size' => "C++ Ruby YCP SML Smalltalk E",
   'Length' => "C# Modula-3 Pascal Oz",
   'len' => "Python Pliant VisualBasic",
   'strlen' => "C PHP",
   'string-length' => "Scheme XPath",
   'sizeof' => "Pike",
   'count' => "Eiffel Logo",
   'bytes chars' => "Perl6",
   'CHARACTER_LENGTH' => "SQL92",
   'atom_length' => "Prolog",
  ],

  'string concatenation' =>
  [
  '+' => "Ruby Pike Python YCP Java C++ C# merd Pascal E Eiffel JavaScript Pliant MSH",
  '.' => "Perl PHP",
  ',' => "Smalltalk",
  '..' => "Lua",
  '~' => "Perl6",
  '&' => "Ada Modula-3 VisualBasic",
  '^' => "SML OCaml",
  '_' => "MUMPS",
  '||' => "PL/I Cecil Icon Maple ClassicREXX SQL92",
  '++' => "Haskell",
  '$a$b' => "Tcl BourneShell",
  'concatenate' => "Dylan CommonLisp",
  'string-append' => "Scheme",
  'cat' => "Maple",
  'Cat' => "Modula-3",
  'strcat' => "C",
  'concat' => "XPath",
  "append" => "Beta Rebol Prolog",
  'stringByAppendingString' => "Objective-C",
  ' ' => "Awk ClassicREXX",
  '[string1 string2]' => "Matlab",
  'word' => "Logo",
 ],

 'duplicate n times' =>
 [ { MLANG => 'C C++ C# Java Prolog Logo VisualBasic JavaScript Haskell Eiffel Beta OCaml SML Smalltalk' }, # Haskell is "concat (replicate n str)"
  '*' => "Ruby Pike Python E Ada",
  'x' => "Perl Perl6",
  'times' => "merd",
  'repeat' => "Pliant",
  'str_repeat' => "PHP",
  'string repeat' => "Tcl",
  'strrep' => "Lua",
  'repmat' => "Matlab",
  'insert/dup' => "Rebol",
  'COPIES' => "ClassicREXX",
  'cat(s$n)' => "Maple",
 ],

 'upper / lower case character' =>
 [
  'upcase / downcase' => "Ruby EmacsLisp",
  'uc / lc' => "Perl Perl6",
  'upper / lower (in Lua >= 5.0)' => "Python Pliant Matlab Lua",
  'toUpper / toLower' => "Haskell",
  'to_upper / to_lower' => "Eiffel",
  'To_Upper / To_Lower' => "Ada",
  'toUpperCase / toLowerCase' => "Java E JavaScript",
  'upper_case / lower_case' => "Pike",
  'uppercase / lowercase' => "OCaml Logo",
  'strupper / strlower' => "Lua",
  'ToUpper / ToLower' => "Oz C#",
  'toupper / tolower' => 'Awk C C++',
  'string toupper / string tolower' => "Tcl",
  'asLowercase / asUppercase' => "Smalltalk",
  "upCase / lowCase" => "Beta",
  'uppercase form / lowercase form' => "Rebol",
  'char-upcase / char-downcase' => "CommonLisp Scheme",
  'char_type(C_, to_upper(C)), char_type(C_, to_lower(C))' => "Prolog",
 ],

 'uppercase / lowercase / capitalized string' =>
 [ { MLANG => 'C C++ Haskell' }, # Haskell is "map toUpper / map toLower"
  'upcase / downcase' => "Ruby EmacsLisp",
  'upper / lower' => "Python SQL92 Matlab",
  'uppercase/lowercase' => "OCaml Rebol Logo",
  'upcase_atom/downcase_atom' => "Prolog",
  'toUpperCase / toLowerCase' => "Java E JavaScript",
  'ToUpper / ToLower' => "C#",
  'to_upper / to_lower' => "Eiffel Ada",
  'toupper / tolower' => 'Awk YCP',
  'uc / lc' => "Perl Perl6",
  'UpperCase / LowerCase' => "Pascal",
  'StringTools[UpperCase] / StringTools[LowerCase] / StringTools[Capitalize]' => "Maple",
  'uppercaseString / lowercaseString / capitalizedString' => "Objective-C",
  'UCase / LCase' => "VisualBasic",
  'strtoupper / strtolower' => "PHP",
  'strupper / strlower' => "Lua",
  'string toupper / string tolower' => "Tcl",
  'string-upcase / string-downcase' => "CommonLisp Scheme",
  'asLowercase / asUppercase' => "Smalltalk",
  'upcase_atom / downcase_atom' => "Prolog",
  "makeLC / makeUC" => "Beta",
  'parse upper var in_var out_var / parse lower var in_var out_var' => "ClassicREXX",
 ],

 'ascii to character' =>
 [ { MLANG => "Eiffel" },
  'chr' => "Ruby Perl Perl6 Python SML OCaml Haskell Pascal PHP",
  'chr$' => "VisualBasic",
  'char' => "Matlab",
  'format %c $c' => "Tcl",
  'toChar' => "E",
  'strchar' => "Lua",
  'from_integer' => "Eiffel",
  'fromCharCode' => "JavaScript",
  'character' => "Pliant",
  'Character value: c' => "Smalltalk",
  'code-char' => "CommonLisp",
  'integer->char' => "Scheme",
  "'Val" => "Ada",
  '(char) c' => 'C C++ C# Java',
  'to char! / to-char' => "Rebol",
  'X2C, D2C' => "ClassicREXX",
  '$CHAR(s)' => "MUMPS",
  'char_code' => "Prolog",
  'ascii' => "Logo",
  'StringTools[Char]' => "Maple",
 ],

 'character to ascii' =>
 [
  'ord' => "Perl Perl6 Python Haskell SML Pascal PHP",
  'asc' => "VisualBasic",
  'getNumericValue' => "Java",
  'charCodeAt' => "JavaScript",
  'asciiValue' => "Smalltalk",
  'code' => "OCaml Eiffel",
  'char-code' => "CommonLisp",
  'char->integer' => "Scheme",
  's[0]' => "Ruby",
  's 0 get' => "PostScript",
  'scan $s %c' => "Tcl",
  'strbyte' => "Lua",
  'toInteger' => "E",
  "'Pos" => "Ada",
  'number' => "Pliant",
  '(int) c' => "C C++ C# Java",
  'to integer! / to-integer' => "Rebol",
  'C2X, C2D' => "ClassicREXX",
  '$ASCII(s)' => "MUMPS",
  '(done automatically when applying mathematical operations on char, such as +)' => "Matlab",
  'char' => "Logo",
  'char_code' => "Prolog",
  'StringTools[Ord]' => "Maple",
 ],

 'accessing n-th character' =>
 [ { MLANG => 'Perl Perl6 ClassicREXX VisualBasic' },
  's[n]' => "Maple C C++ C# E Pike Python PHP Ruby",
  's(n)' => "Ada Matlab",
  's:n' => "Pliant",
  's.[n]' => "OCaml",
  's !! n' => "Haskell",
  's @ n' => "Eiffel",
  's/:n' => "Rebol",
  'string index s n' => "Tcl",
  'sub' => "SML",
  'char, aref, schar, svref' => "CommonLisp",
  'GetChar' => "Modula-3",
  'at (in C++, is range-checked whereas a[i] is not)' => "C++ Smalltalk",
  'aref' => "CommonLisp",
  'char(s, i)' => "B",
  'charAt' => "Java JavaScript",
  'characterAtIndex' => "Objective-C",
  "n -> s.inxGet" => "Beta",
  'string-ref' => "Scheme",
  '$EXTRACT(s, n)' => "MUMPS",
  'item' => "Logo",
 ],

 'extract a substring' =>
 [ { MLANG => "Haskell Logo" }, # Haskell is "take len (drop n s)"
  's[n..m]' => "Pike Maple Ruby",
  's(n..m)' => "Ada",
  's(n:m)' => "Matlab",
  's(n,m+1)' => "E",
  's[n:m+1]' => "Python",
  's[n,len]' => "Ruby",
  's n len' => "Pliant",
  'strndup(s + n, len)' => "C",
  'substring' => "Java Scheme YCP SML Eiffel XPath",
  'Substring' => "C#",
  'substr' => "Perl Perl6 C++ PHP",
  'SUBSTR' => "ClassicREXX",
  'sub' => "OCaml",
  'SUB' => "Modula-3",
  'subseq' => "CommonLisp",
  'slice' => "JavaScript",
  'mid$' => "JavaScript",
  'range' => "Tcl",
  'copy/part at s n len' => "Rebol",
  'copy/part at s n at s m' => "Rebol",
  's copyFrom: n to: m' => "Smalltalk",
  "(n,m)->s.sub" => "Beta",
  '[s substringWithRange:NSMakeRange(n, len)]' => "Objective-C",
  'SUBSTRING(s FROM n len)' => "SQL92",
  '$EXTRACT(s, n, m)' => "MUMPS",
  'sub_string / sub_atom' => "Prolog",
 ],

 'locate a substring' => 
 [ { MLANG => "OCaml" },
  'index' => "C Ruby Perl Ada Perl6 Python",
  'indexOf' => "JavaScript Java",
  'IndexOf' => "C#",
  'startOf' => "E",
  'search' => "Pike CommonLisp PostScript",
  'StringTools[Search]' => "Maple",
  'strstr strchr' => "C",
  'find' => "Rebol YCP Logo Python",
  'strfind' => "Matlab",
  '$FIND' => "MUMPS",
  'index_non_blank / find_token' => "Ada",
  'substring_index' => "Eiffel",
  'rangeOfString' => "Objective-C",
  'POS' => "ClassicREXX",
  'POSITION(needle IN s)' => "SQL92",
  'sub_string / sub_atom' => "Prolog",
 ],

 'locate a substring (starting at the end)' => 
 [ { MLANG => "Prolog" },
  'rindex' => "C Ruby Perl Perl6 OCaml Python",
  'rfind' => "C++ Python",
  'find/last' => "Rebol",
  'strrchr' => "C",
  'index(Going => Backward)' => "Ada",
  'lastStartOf' => "E",
  'lastIndexOf' => "JavaScript Java",
  'last_index_of (ESI dialect)' => "Eiffel",
  'LastIndexOf' => "C#",
  '(search substring string :from-end t)' => "CommonLisp",
  '[string rangeOfString:substring options:NSBackwardsSearch]' => "Objective-C",
  'LASTPOS' => "ClassicREXX",
  't=strfind(s,p), t(end)' => "Matlab",
  'StringTools[SearchAll](s,p)[-1]' => "Maple",
 ],

],

'Booleans' => [

 'type name' =>
 [ { KIND => 'typed', MLANG => 'C Pike' },
  'Bool' => "Perl6 Haskell Pliant",
  'bool' => "C# C++ C99 OCaml Python YAML",
  'Boolean' => "Smalltalk Lua Pascal VisualBasic Ada",
  'boolean' => "Java CommonLisp YCP Maple",
  'BOOLEAN' => "Eiffel",
  'logic!' => "Rebol",
  'logical' => "Matlab",
 ],

 'false value' =>
 [
   'false' => "C99 C++ BCPL C# OCaml Logo SML Pascal YCP Smalltalk PostScript Java E Ada Beta Pliant FL Oz Tcl Maple JavaScript Rebol Matlab BourneShell PHP Ruby Lua YAML",
   'False' => "VisualBasic Python Haskell merd Eiffel",
   'FALSE' => "Modula-3 SQL92",
   'false()' => "XPath",
   '#f' => "Dylan Scheme",

   'n' => "YAML",
   'nil' => "EmacsLisp CommonLisp Lua Ruby Lua",
   'no' => "Tcl YAML",
   'No' => "Prolog",
   'none' => "Rebol",
   'None' => "Python",
   'null' => "JavaScript",
   'NULL' => "C99 C++ C",
   'off' => "Tcl YAML",
   'undef' => "Perl Perl6",
   'undefined' => "JavaScript",

   'fail' => "Prolog",
   'FAIL' => "Maple",
   'array containing at least one false value' => "Matlab",
   'exit status different from 0' => "BourneShell",

   '0 (beware of 0.0 which is true in Pike!)' => "B VisualBasic C99 C++ Tcl Python JavaScript ClassicREXX MUMPS Awk Perl Perl6 Matlab XPath C PHP Pike",
   '0.0' => "Matlab",
   'NaN' => "JavaScript XPath",

   '""' => "Python JavaScript Awk Perl Perl6 XPath PHP",
   '"0"' => "Awk Perl Perl6",
   "''" => "Matlab",
   "'\0'" => "C99 C++ C",

   '()' => "Python Perl Perl6",
   '[]' => "Python Matlab",
   '{}' => "Python Matlab",
 ],

 'true value' =>
 [
  'TRUE' => "Modula-3 SQL92",
  'True' => "Haskell merd Eiffel VisualBasic Python",
  'true' => "BCPL C# OCaml Maple SML Pascal Logo Smalltalk PostScript Java E Ada PHP Beta Pliant FL Oz YCP Tcl BourneShell YAML Prolog Rebol",
  'true()' => "XPath",
  't' => "EmacsLisp CommonLisp",
  '#t' => "Scheme Dylan",
  'y' => "YAML",
  'yes' => "Tcl YAML",
  'Yes' => "Prolog",

  'on' => "Tcl YAML",

  'exit status 0' => "BourneShell",
  'anything not false' => "Awk Perl Perl6 B C Pike Matlab MUMPS XPath EmacsLisp CommonLisp Python Scheme Dylan Rebol",

  '1' => "ClassicREXX MUMPS",
  'non zero number' => "Tcl",
  'non-zero-numbers' => "VisualBasic",
 ],

 'logical not' =>
 [
  '!' => "Awk B C C++ C# E Java Pike Perl Perl6 Ruby YCP Tcl PHP JavaScript",
  'not' => "OCaml Logo SML Rebol Maple Pascal PostScript Ruby Scheme Haskell Perl Perl6 XPath Python Smalltalk merd Eiffel Lua EmacsLisp CommonLisp Ada Beta Pliant Forth Prolog",
  'Not' => "Oz VisualBasic",
  'NOT' => "Modula-3",
  '~' => "PL/I BCPL Dylan Matlab",
  '^' => "PL/I",
  "'" => "MUMPS",
  "\\" => "ClassicREXX",
 ],

 'logical or / and' =>
 [
  'short circuit' =>
  [
   '|| / &&' => "Awk C C++ C# Java Matlab Pike Perl Perl6 YCP Ruby OCaml Haskell merd Tcl E PHP JavaScript",
   '| / &' => "B BCPL Dylan",
   'or / and' => "Perl Perl6 Logo Ruby Python Modula-2 PHP Smalltalk EmacsLisp CommonLisp Scheme Lua Pliant",
   'OR / AND' => "Modula-3",
   'or / & (in Modula-2, "&" and "and" are synonyms)' => "OCaml",
   'any / all' => "Rebol",
   'orelse / andalso' => "SML",
   'orelse / andthen' => "Oz",
   'or else / and then' => "Ada Eiffel",
   '; / ,' => "Prolog",
   '& / !' => "MUMPS",
  ],

  'non short circuit (always evaluates both arguments)' =>
  [
   '| / &' => "C# Java Smalltalk ClassicREXX Matlab",
   'or / and' => "Maple Pascal PostScript Rebol SML Ada Eiffel Beta XPath Forth",
   'Or / And (in Oz, simple functions, not operators)' => "VisualBasic Oz",
   '\/ / /\ (ascii representation, original uses a special charset)' => "BCPL",
   '?| / ' => "Perl6",
  ],

 ],
],

'Bags and Lists' => [
 { MLANG => "C Cobol Fortran Pascal Basic Ada BourneShell ClassicREXX" },

 'type name' =>
 [ { KIND => 'typed' },
   'seq' => "YAML",
   'a list' => "OCaml",
   '[a]' => 'Haskell',
   'a[]' => "C#",
   'list' => "Maple",
   'List' => "Pliant",
   'Array or List' => "Perl6",
   'ARRAY or LINKED_LIST' => "Eiffel",
   'cell' => "Matlab",
 ],

 'list concatenation' =>
 [ { MLANG => 'C++' }, # C++ has the typical OO disease, it does everything in-place
  '+' => "Ruby Eiffel Pike Python merd E",
  ',' => "Smalltalk Maple Matlab Perl",
  '@' => "SML OCaml",
  '++' => "Haskell",
  '|||' => "Icon",
  'array_merge' => "PHP",
  'merge' => "YCP",
  'concat' => "Tcl JavaScript",
  'concatenate' => "Dylan",
  'nconc' => "EmacsLisp CommonLisp",
  "append" => "Beta Rebol Prolog Scheme EmacsLisp CommonLisp",
  'Append' => "Oz",
  'arrayByAddingObjectsFromArray' => "Objective-C",
  'sentence' => "Logo",
 ],

 'list flattening' =>
 [ { MLANG => 'Perl Matlab Logo Python C++ Smalltalk JavaScript' },
  'one level depth' =>
  [
   'concat' => "Haskell Mercury SML",
   'flatten' => "OCaml merd YCP Prolog",
   'Flatten' => "Oz",
   'eval concat' => "Tcl",
   'ListTools[FlattenOnce]' => "Maple",
  ],

  'recursive' =>
  [
   'flatten' => "Pike Ruby",
   'ListTools[Flatten]' => "Maple",
  ],
 ],

 'list constructor' =>
 [ { MLANG => "Beta C++" },
  '[ a, b, c ]' => "Haskell Maple Ruby Python Matlab YCP JavaScript SML YAML Perl Perl6 Prolog merd PostScript E",
  '( a, b, c )' => "Perl Perl6",
  '{ a, b, c } (restricted to initialisation of a local variable in C and C++)' => "Lua C C++",
  "#(a, b, c)" => "Dylan",
  '#(a b c) (a b c must be constants)' => "Smalltalk",
  '{ a. b. c }' => "Squeak",
  '[ a ; b ; c ]' => "OCaml",
  '[ a b c ]' => "Rebol Oz Logo",
  '({ a, b, c })' => "Pike",
  "'(a b c)" => "EmacsLisp CommonLisp Scheme",
  '<< a, b, c >>' => "Eiffel",
  'list' => "Tcl Dylan EmacsLisp CommonLisp Scheme",
  'new t[] { a, b, c }' => 'C#',
  'new[] { a, b, c }' => 'C#3',
  'new List<t> { a, b, c}' => "C#3",
  'Array(a, b, c) (beware, if you give only one integer argument, it is the size!)' => "JavaScript",
  '[NSArray arrayWithObjects:a, b, c, nil]' => "Objective-C",
  pre('  - a
  - b
  - c') => "YAML",
 ],

 'list/array indexing' =>
 [ { MLANG => "Beta" }, # eiffeil favors a.put(v, i) for write access
  'a[i]' => "B C C++ C# Java Pike BourneShell Maple Ruby Python merd Pascal E PHP Perl Perl6 Dylan Lua JavaScript Modula-3 MSH",
  'a*[i] or a!i or a*(i) depending on the version' => "BCPL",
  'a[i]:default' => "YCP",
  'a(i)' => "Ada Matlab",
  'a:i' => "Pliant",
  'a/:i' => "Rebol",
  'a.(i)' => "OCaml",
  'a !! i' => "Haskell Mercury",
  'a @ i' => "Eiffel",
  'a i get (for write access: a i o put)' => "PostScript",
  'at (in C++, it is range-checked whereas a[i] is not. in Smalltalk, for write access: a :at i :put o)' => "C++ Smalltalk",
  'lindex' => "Tcl",
  'nth' => "OCaml EmacsLisp CommonLisp",
  'Nth' => "Oz",
  'aref' => "EmacsLisp CommonLisp",
  'nth0 / nth1' => "Prolog",
  'list-ref / vector-ref' => "Scheme",
  'element' => "Dylan",
  'slice' => "Ruby",
  'node[i]' => "XPath",
  'objectAtIndex' => "Objective-C",
  'item' => "Logo",
 ],

 'adding an element at the beginning (list cons)' =>
 [ { MLANG => 'Python Objective-C' },
  'return the new list (no side-effect)' =>
  [
   ':' => "Haskell merd",
   '::' => "SML OCaml",
   '|' => "Oz",
   '[ e | l ]' => "Prolog Erlang",
   '[e l[]]' => "Maple",
   '[e l]' => "Matlab",
   'cons' => "EmacsLisp CommonLisp Scheme",
   'pair' => "Dylan",
   'fput' => "Logo",
  ],

  'side-effect' =>
  [ { MLANG => "Beta Maple" }, # Beta: (obj, list.head) -> list.insertBefore
   'unshift' => "Perl Perl6 Ruby JavaScript",
   'prepend' => "YCP",
   'push_front' => "C++",
   'addFirst' => "Smalltalk",
   'insert' => "Rebol",
   'put_first' => "Eiffel",
   'push' => "CommonLisp",
   'array_unshift' => "PHP",
  ],
 ],

 'adding an element at index' =>
 [ { MLANG => 'Eiffel OCaml Logo Prolog Matlab Maple' },
  'return the new list (no side-effect)' =>
  [
  ],

  'side-effect' =>
  [ {},
   '[a insertObject:e atIndex:i]' => "Objective-C",
   'a.insert(i, e)' => "Ruby",
  ],
 ],

 'adding an element at the end' =>
 [ { MLANG => 'CommonLisp Haskell Beta Prolog Maple OCaml SML' },
  'return the new list (no side-effect)' =>
  [
   'push' => "merd",
   'arrayByAddingObject' => "Objective-C",
   'lput' => "Logo",
  ],

  'side-effect' =>
  [
   'push' => "Perl Perl6 Ruby JavaScript",
   'push_back' => "C++",
   'append' => "Python Pliant Rebol",
   '+=' => "Pliant",
   'add' => "Java Smalltalk YCP",
   'put_last' => "Eiffel",
   'array_push' => "PHP",
   'addObject' => "Objective-C",
  ],
 ],

 'first element' =>
 [ { MLANG => "Beta Perl Python Ruby JavaScript Maple Matlab" },
  '' =>
  [
   'head' => "Haskell",
   'hd' => "OCaml",
   'car' => "Scheme EmacsLisp CommonLisp",
   'first' => "Eiffel Pliant Rebol Smalltalk Logo",
  ],

  'iterator' =>
  [ 
   'head' => "Beta",
   'begin' => "C++",
  ],
 ],

 'all but the first element' =>
 [ { MLANG => 'Perl Python C++ Ruby JavaScript Maple Eiffel' },
  'tail' => "Haskell",
  'tl' => "OCaml",
  'cdr' => "Scheme EmacsLisp CommonLisp",
  'butfirst' => "Logo",
  'allButFirst' => "Smalltalk",
  'a(2:end)' => "Matlab",
  'L = [_|ButFirst]' => "Prolog",
 ],

 'last element' =>
 [ { MLANG => 'C++ OCaml JavaScript Maple' },
  '' =>
  [
   'last' => "Eiffel Rebol Logo Haskell Prolog Pliant Smalltalk E Scheme",
   'Last' => "Oz",
   'lastObject' => "Objective-C",
   'a[-1]' => "Python Perl Pike Ruby",
   'a(end)' => "Matlab",
   'node[last()]' => "XPath",
   '(car (last lst))' => "CommonLisp EmacsLisp",
  ],

  'iterator' =>
  [
  ],
 ],

 'get the first element and remove it' =>
 [ { MLANG => 'Haskell Matlab Python C++ OCaml Maple Eiffel' },
  'shift' => "Perl Perl6 Ruby JavaScript",
  'shift!' => "merd",
  'pop' => "CommonLisp Logo",
  'removeFirst' => "Smalltalk",
  'array_shift' => "PHP",
 ],

 'get the last element and remove it' =>
 [ { MLANG => 'Haskell C++ OCaml Maple Eiffel Matlab' },
  'pop' => "E Perl Perl6 Ruby Python JavaScript",
  'pop!' => "merd",
  'array_pop' => "PHP",
  'removeLast' => "Smalltalk",
  'dequeue' => 'Logo',
 ],

 'for each element do something' =>
 [ { MLANG => "XSLT Matlab" }, # could be KIND has_lambda, but many languages has special cases for this
  'each' => "Ruby merd Pliant",
  'for v in l ...' => "Python Ruby E Maple",
  'for v in l; do ...; done' => "BourneShell",
  'for (v in l) ...' => "Awk Dylan",
  'for (var v in l) { ... }' => "JavaScript",
  'For Each v in l
...
Next' => "VisualBasic",
  'for v in range loop .. end loop' => "Ada",
  'for' => "Perl",
  'foreach' => "Perl Pike Rebol Logo Tcl Lua PHP",
  'foreach (t v in l) ...' => "C#",
  'foreach (v in l) ...' => "C#3",
  'foreach ($v in l) ...' => "MSH",
  'foreach(t v, l, { ... })' => "YCP",
  'for_each' => "C++",
  'for-each' => "Scheme",
  'forall' => "PostScript Prolog",
  'ForAll' => "Oz",
  'iter' => "OCaml",
  'do' => "Smalltalk",
  'do_all' => "Eiffel",
  'app' => "SML",
  'mapc' => "EmacsLisp",
  'mapM_' => "Haskell",
  '(dolist (v l) ...)  (loop for v in l do ...)  mapc' => "CommonLisp",
  "list.iterate (# do current ... #) " => "Beta",
 ],

 'transform a list (or bag) in another one' =>
 [ { KIND => 'has_lambda' },
  'map' => "Perl Pike Maple Ruby SML OCaml Haskell Mercury Scheme Python merd Dylan",
  'Map' => "Oz",
  'mapcar' => "EmacsLisp CommonLisp",
  'maplist' => "YCP Prolog",
  'sublist' => "Prolog",
  'map / map.se' => "Logo",
  'for-each' => "XSLT",
  'foreach or selected' => "MSH",
  'collect' => "Ruby Smalltalk",
  'transform' => "C++",
  'array_map' => "PHP",
  '[ f x | x <- l ] (list comprehension)' => "Haskell",
  '[ f(x) for x in l ] (list comprehension)' => "Python",
  'magical: sin(x) computes sin on each element' => "Matlab",
 ],

 'transform two lists in parallel' =>
 [ { KIND => 'has_lambda', MLANG => "Perl Ruby" },
  'map2' => "OCaml",
  'zipWith' => "Haskell",
  'Zip' => "Maple Oz",
  'map' => "Scheme Python Dylan Logo",
  'map.se' => "Logo",
  'mapcar' => "CommonLisp",
  'maplist2' => "Prolog",
  'l1 with: l2 collect: ...' => "Smalltalk",
  'transform' => "C++",
  'ListPair.map' => "SML",
  'magical: a binary function or operator is appliied on each element' => "Matlab",
 ],

 'find an element' =>
 [ { KIND => 'has_lambda', MLANG => "Python Perl Scheme" },
  'find' => "C++ CommonLisp Ruby Rebol SML OCaml Haskell Scheme-SRFI1 merd YCP",
  'find_if' => "C++",
  'find-if' => "CommonLisp",
  'first (in List::Util)' => "Perl",
  'detect' => "Ruby Smalltalk",
  'search' => "Pike",
  'ListTools[Search]' => "Maple",
  'lsearch' => "Tcl",
  'indexOfObject, indexOfObjectIdenticalTo' => "Objective-C",
  'find(a == 3)' => "Matlab",
 ],

 'keep elements matching' =>
 [ { KIND => 'has_lambda', MLANG => 'Scheme', },
  'find_all' => "Ruby OCaml",
  'filter' => "Pike OCaml SML Haskell Mercury Scheme-SRFI1 Python merd YCP",
  'filter!' => "Scheme-SRFI1",
  'Filter' => "Oz",
  'grep' => "Perl Perl6",
  'where' => "MSH",
  'select' => "Ruby Maple Smalltalk",
  'remove-if delete-if' => "CommonLisp",
#  'remove-each' => "Rebol", # unpure
  'reject' => "Ruby",
  'choose' => "Dylan",
  '[ x | x <- l, p x ] (list comprehension)' => "Haskell",
  '[ x for x in l if p(x) ] (list comprehension)' => "Python",
  'a(a == 3)' => "Matlab",
 ],

 'f(... f(f(init, e1), e2) ..., en)' =>
 [ { KIND => 'has_lambda', MLANG => "Perl Scheme Matlab" },
  'foldl' => "Haskell Maple SML Mercury merd",
  'FoldL' => "Oz",
  'fold_left' => "OCaml",
  'fold' => "Scheme-SRFI1",
  'reduce (in Perl, in List::Util)' => "Pike Python CommonLisp Dylan Perl Perl6",
  'inject' => "Ruby",
  'inject into' => 'Smalltalk',
 ],

 'f(e1, f(e2, ... f(en, init) ...))' =>
 [ { KIND => 'functional', MLANG => 'Smalltalk Scheme' },
  'foldr' => "Haskell Maple SML Mercury merd",
  'FoldR' => "Oz",
  'fold-right' => "Scheme-SRFI1",
  'fold_right' => "OCaml",
  'rreduce' => "Pike",
  "(reduce f '(e1 e2 ... en) :from-right t :initial-value init)" => "CommonLisp",
 ],

 'split a list in 2 based on a predicate' =>
 [ { KIND => 'functional', MLANG => 'Smalltalk Scheme Matlab' },
  'partition' => "OCaml Ruby SML Haskell Scheme-SRFI1 merd",
  'partition!' => "Scheme-SRFI1",
  'Partition' => "Oz",
  'split-sequence (not in standard)' => "CommonLisp",
  'ListTools[Split]' => "Maple",
 ],

 'is an element in the list' =>
 [ { MLANG => "C++ Perl Maple" },
  'member?' => "Ruby merd Dylan",
  'include?' => "Ruby",
  'mem' => "OCaml",
  'member' => "CommonLisp Prolog Scheme",
  'Member' => "Oz",
  'memq memv' => "Scheme",
  'memberp / member?' => "Logo",
  'contains' => "E YCP",
  'containsObject' => "Objective-C",
  'in' => "Python JavaScript SQL92",
  'in_array' => "PHP",
  'includes' => "Smalltalk",
  'elem' => "Haskell Mercury",
  'has' => "Eiffel",
  'has_value' => "Pike",
  'ismember' => "Matlab",
 ],

 'is the predicate true for an element' =>
 [ { KIND => 'has_lambda', MLANG => "Perl Scheme" },
  'any' => "Haskell Mercury Matlab Scheme-SRFI1 Python",
  'any?' => "Ruby Dylan",
  'anySatisfy' => "Smalltalk",
  'exists' => "OCaml SML",
  'exists?' => "merd",
  'some' => "CommonLisp",
  'Some' => "Oz",
  'ormap' => "Maple",
 ],

 'is the predicate true for every element' =>
 [ { KIND => 'has_lambda', MLANG => "Perl Scheme" },
  'all' => "Haskell SML Mercury Python",
  'All' => "Oz",
  'all?' => "Ruby merd",
  'allSatisfy' => "Smalltalk",
  'every' => "Scheme-SRFI1 CommonLisp",
  'every?' => "Dylan",
  'for_all' => "OCaml",
  'andmap' => "Maple",
 ],

 'smallest / biggest element' =>
 # accepting n-ary functions if a "*" tupling function exists		     
 [ { MLANG => 'OCaml Logo JavaScript' },
  'min / max' => "Eiffel Maple Perl6 Pike Matlab Ruby Scheme Prolog Python CommonLisp Smalltalk Java",
  'minimum / maximum' => "Haskell Mercury merd",
  'minimum-of / maximum-of' => "Rebol",
  'min minstr / max maxstr (in List::Util)' => "Perl",
  'min_element / max_element' => "C++",
 ],

 'join a list of strings in a string using a glue string' =>
 [ { MLANG => "Eiffel Logo" },
  'join' => "Perl Perl6 Python Rebol Ruby JavaScript PHP",
  'Join' => "C#",
  'ListTools[Join]' => "Maple",
  'rjoin' => "E",
  'concat' => "OCaml",
  'strcat' => "Matlab",
  'concat_atom' => "Prolog",
  'l * glue' => "Pike Ruby",
  "(mapconcat 'identity l glue)" => "EmacsLisp",
  'componentsJoinedByString' => "Objective-C",
 ],

 'list size' =>
 [
  'size' => "Ruby merd E Matlab Dylan Java C++ YCP Pliant Smalltalk",
  'sizeof' => "Pike",
  'length' => "C# Ruby Matlab SML Haskell Mercury OCaml Scheme PostScript Java JavaScript EmacsLisp CommonLisp Prolog",
  'Length' => "Oz",
  'length?' => "Rebol",
  'len' => "Python",
  'llength' => "Tcl",
  '$LENGTH' => "MUMPS",
  'elems' => "Perl6",
  'getn' => "Lua",
  'count' => "PHP Eiffel XPath Objective-C SQL92",
  'numel' => "Matlab",
  'scalar @l' => "Perl",
  'nops' => "Maple",
 ],

 'iterate with index' =>
 [ { MLANG => 'Haskell Maple Logo Matlab Prolog Perl Python C++ OCaml Smalltalk JavaScript' }, # Haskell is "zipWith (\i v -> expr) [1..] l"
  'each_with_index' => "Ruby merd",
  'foreach($l as $i => $v)' => "PHP",
  'for i => v in l' => "E",
  'for (v in l, i from 0) ... end' => "Dylan",
  'forAllInd' => "Oz",
  'foreachi' => "Lua",
  'foreach(l; typ0 i; typ1 v) { ... }' => "Pike",
  'withIndexDo' => "Squeak",
  '(loop for v in l as i upfrom 0 do ...)' => "CommonLisp",
 ],

 'remove duplicates' =>
 [ { MLANG => "Perl Python OCaml Smalltalk JavaScript Scheme" },
  'uniq' => "Ruby Perl6 Pike merd",
  'uniq2' => "Pike",
  'unique (in C++, it is done in place)' => "Rebol Matlab C++",
  'nub' => "Haskell",
  'array_unique' => "PHP",
  'ListTools[MakeUnique]' => "Maple",
  'delete-duplicates' => "Scheme-SRFI1 CommonLisp",
  'delete-duplicates!' => "Scheme-SRFI1",
  'remove-duplicates' => "Dylan CommonLisp",
  'lsort -unique' => "Tcl",
  'toset' => "YCP",
  'distinct' => "SQL92",
 ],

 'sort' =>
 [ { MLANG => 'Logo' },
  'sort (in Scheme, not standard, but nearly standard)' => "Eiffel Pike Prolog Matlab Maple Rebol Ruby C++ C# OCaml XSLT Haskell Java JavaScript CommonLisp Python Perl Perl6 merd E PHP Lua YCP Scheme",
  'sorted' => "Python",
  'Sort' => "Oz",
  'sort_by' => 'merd Ruby',
  'sortBy' => "Haskell Smalltalk",
  'order by' => "SQL92",
  'lsort' => "Tcl",
  'asort' => "Awk",
  'sort-object' => "MSH",
  'sortedArrayUsingSelector, sortUsingSelector' => "Objective-C",
  'predsort / keysort / mergesort' => "Prolog",
 ],

 'reverse' =>
 [
  'reverse' => "Pike Rebol Logo Ruby Haskell Prolog Perl Perl6 Java JavaScript Mercury Scheme Python merd Dylan EmacsLisp CommonLisp C++",
  'Reverse' => "C# Oz",
  'reversed' => "Smalltalk",
  'reverse_copy' => "C++",
  'rev' => "OCaml SML",
  'array_reverse' => "PHP",
  'ListTools[Reverse]' => "Maple",
  'fliplr flipud...' => "Matlab",
 ],

 'list of couples from 2 lists' =>
 [ { KIND => 'functional', MLANG => 'Smalltalk Scheme' },
  'combine' => "OCaml",
  'zip' => "Haskell Maple SML Scheme-SRFI1 Ruby Python Perl6 merd",  
  'pairlis (the result is not guaranteed to be the same as the order in the input)' => "CommonLisp",
  'transpose' => "Ruby",
  '[a b]' => "Matlab",
 ],

 '2 lists from a list of couples' =>
 [ { KIND => 'functional', MLANG => 'Smalltalk CommonLisp Scheme' },
  'split' => "OCaml",
  'unzip' => "Haskell SML merd",
  'unzip2' => "Scheme-SRFI1",
  'transpose' => "Ruby",
  'zip(*l)' => "Python",
  'a(1,:), a(2,:)' => "Matlab",
 ],

 'lookup an element in a association list' =>
 [ { KIND => 'functional', MLANG => 'Smalltalk' },
  'lookup' => "Haskell",
  'assoc' => "Ruby OCaml CommonLisp EmacsLisp Scheme",
  'assq' => "EmacsLisp Scheme",
  'assv' => "Scheme",
  'get_assoc' => "Prolog",
  'select' => "Rebol",
  'a.(e)' => "Matlab",
  'a[e]' => "Maple",
  'gprop' => "Logo",
 ],

 'list out of a bag' =>
 [ { KIND => 'rare' },
  'to_a' => "Ruby",
  'toArray' => "Java",
  'asArray' => "Smalltalk",
  'to_list' => "merd",
  'map-as(<list>, bag)' => "Dylan",
  '[a.(:)]' => "Matlab",
 ],

],

'Various Data Types' => [

 'tuple type' =>
 [ { KIND => 'typed', MLANG => 'C C++ C# Maple Eiffel Pascal Java Smalltalk JavaScript Ada' },
   't1 * ... * tn' => "OCaml",
   '(t1, ..., tn)' => "Haskell",
   't1, ..., tn' => "merd",
   'tuple!' => "Rebol",
   'Tuple[T1, T2, T3]' => "E",
 ],

 'tuple constructor' =>
 [ { MLANG => 'C C++ C# Eiffel Java JavaScript ClassicREXX' },
   'a, b, c' => "OCaml Ruby Python Lua merd",
   '( a, b, c )' => "SML Haskell Prolog Ada Perl",
   '{ a. b. c }' => 'Smalltalk',
   '{ a, b, c }' => 'Matlab',
   '[ a, b, c ]' => "E",
   'a . b . c' => "Rebol",
   '(cons a b)' => "CommonLisp",
 ],

 'computable tuple (these are a kind of immutable lists playing a special role in parameter passing)' =>
 [ { ALL => 1, KIND => 'dynamic', MLANG => "ClassicREXX Logo" },
   # tagged "dynamic" because in statically typed languages, this needs staged evaluation (like macros)
  'empty tuple' =>
  [
   # () in MLs has no characteristic of a tuple, so not put here
   '()' => "Python merd Perl Perl6",
   '[]' => "Ruby",
   '{}' => "Matlab Smalltalk",
   '#()' => "Smalltalk",
   'Nothing' => "Prolog",
  ],

  '1-uple' =>
  [
   'a or [a]' => "Ruby",
   'a,' => "Python Perl6",
   'tuple([a])' => "Python",
   '(a)' => "Perl", # Perl's lists are a kind of tuple. There is a distinction between ("foo") x 4 and "foo" x 4
   '((a))' => "merd",
   '{a}' => 'Smalltalk',
  ],
 
  'using a tuple for a function call' =>
  [ { MLANG => 'Smalltalk' },
   't' => "merd Perl",
   '*t' => "Ruby Python",
   't{:}' => "Matlab",
   'L =.. [ F | Args ], call(L)' => "Prolog",
  ],
 ],

 'reference (pointer)' =>
 [ { ALL => 1, MLANG => 'Ruby Python CommonLisp Matlab Scheme Smalltalk Eiffel Java ClassicREXX' },
  'creation' =>
  [
   '&' => "B C C++ C#",
   '\\' => "Perl",
   'AddressOf' => "VisualBasic",
   'addr (Borland Pascal extension)' => "Pascal",
   '@ (Borland Pascal extension)' => "Pascal",
   'lv' => "BCPL",
   'ref' => "C# SML OCaml",
   'newSTRef' => "Haskell",
   'NewCell' => "Oz",
   "'access" => "Ada",
   ":> :>>" => "Pliant",
   "''" => "Maple",
  ],

  'dereference' =>
  [
   '* (prefix)' => "B C C++ C#",
   '$ @ % & (prefix)' => "Perl",
   '->[...] ->{...} ->(...) (postfix)' => "Perl",
   '-> (infix)' => "C C++",
   '^ (postfix)' => "Modula-3 Pascal",
   '! (prefix)' => "OCaml SML",
   'rv' => "BCPL",
   'readSTRef' => "Haskell",
   'Access' => "Oz",
   '.[all]' => "Ada",
   '@' => "Forth",
   'eval' => "Maple",
  ],

  "assigning (when dereferencing doesn't give a lvalue)" =>
  [ { KIND => 'rare' },
   'writeSTRef' => "Haskell",
   'Assign' => "Oz",
   ':=' => "OCaml SML",
   '!' => "Forth",
  ],

 ],

 'optional value' => 
 [ { ALL => 1, MLANG => "ClassicREXX Prolog" },
  'null value' =>
  [
   '0' => 'C++',
   'NULL' => 'C SQL92 Maple',
   'nil' => "Ruby EmacsLisp CommonLisp Lua Objective-C",
   'null' => "C# JavaScript Java Smalltalk",
   'Null (only for "access" types)' => "Ada",
   'undef' => "Perl",
   'None' => "Python OCaml",
   'Nothing' => "Haskell",
   'Void' => "Eiffel",
   '#f ()' => "EmacsLisp",
   '(empty) / ~ / null' => "YAML",
  ],

  'value' =>
  [
   'v' => "Ada C C++ C# Eiffel Java Perl JavaScript Smalltalk Python Perl Ruby EmacsLisp CommonLisp Lua Scheme",
   'Just v' => "Haskell",
   'Some v' => "OCaml",
  ],

  'type name' =>
  [ { KIND => 'functional typed', MLANG => "Smalltalk" },
    'option' => "OCaml",
    'Maybe' => "Haskell",
  ],

 ],

 'record selector' =>
 [ { MLANG => "Perl Prolog Logo ClassicREXX" },
  '.' => "C C++ C# Ruby OCaml Matlab Ada Beta Pascal Python E Eiffel Java Modula-2 Modula-3 JavaScript Lua Oz",
  '::' => "XPath",
  '%' => "Fortran90",
  "' (attribute selector)" => "Ada",
  '^' => "Mercury",
  'r { field }' => "merd",
  'r:field' => "Pliant",
  'field r' => "Haskell",
  '->' => "C C++",
  'r[field]' => "Maple",
  'r["field"]' => "JavaScript",
  '#field r' => "SML",
  'normal function call' => "Haskell CommonLisp Dylan Smalltalk",
 ],

 'dictionary' =>
 [ { ALL => 1, MLANG => 'C Ada Haskell Logo Smalltalk ClassicREXX Prolog' },

  'type name' =>
  [ { KIND => 'typed' },
    'map' => "YAML",
    '(k, v) Hashtbl.t' => "OCaml",
    'dict' => "Python",
    'Dictionary' => "Pliant Smalltalk",
    'Hash' => "Perl6",
    'HASH_TABLE' => "Eiffel",
    'struct' => "Matlab",
    'table' => "Maple",
  ],

  'constructor' =>
  [ { MLANG => 'C++ C# OCaml Java' },
   '[ a => b, c => d ]' => "E",
   'array( a => b, c => d )' => "PHP",
   '{ a => b, c => d }' => "Ruby Perl Perl6",
   '{ a, b, c, d }' => "Ruby Perl",
   '{ a: b, c: d }' => "Python JavaScript YAML",
   '$[ a: b, c: d ]' => "YCP",
   '{ a->b. c->d }' => "Squeak",
   '{ a = b, c = d }' => "Lua",
   '@{ a = b; c = d }' => "MSH",
   '([ a:b, c:d ])' => "Pike",
   '<< a b c d >>' => "PostScript",
   "struct(a, b, c, d)" => "Matlab",
   'Hash[ a, b, c, d ]' => "Ruby",
   'table([a=b, c=d])' => "Maple",
   'define table foo a => b; c => d end' => "Dylan",
   '[NSDictionary dictionaryWithObjectsAndKeys:b, a, d, c, nil]' => "Objective-C",
   pre('  a: b
  c: d') => "YAML",
  ],

  'access' =>
  [
   'read/write' =>
   [
    'h[k]' => "Awk Maple Ruby Python E PHP MSH Dylan Lua JavaScript C++ C#",
    '$h{k}' => "Perl",
    '%h{k} or %h<s>' => "Perl6",
    '$h(k)' => "Tcl",
    'h.k' => "Lua JavaScript Matlab",
    'h:k' => "Pliant",
    'h["k"] or h->k' => "Pike",
    '(gethash k h)' => "CommonLisp",
   ],
   
   'read' =>
   [
    'h k get' => "PostScript",
    'find' => "OCaml",
    'fetch' => "Ruby",
    'get' => "Java",
    'at' => "Smalltalk",
    'h@k or h.at(k)' => "Eiffel",
    'h[k]:default' => "YCP",
    'h.get(k, returned_value_when_k_unfound)' => "Python",
    'objectForKey' => "Objective-C",
   ],

   'write' =>
   [
    'h k o put' => 'PostScript',
    'put' => "Eiffel Java",
    'add, replace' => "OCaml",
    'store' => "Ruby",
    'h[k]' => "YCP",
    'h at: k put: o' => "Smalltalk",
    '[h setObject:o forKey:k]' => "Objective-C",
   ],
  ],

  'has the key ?' =>
  [ { MLANG => "Maple CommonLisp Objective-C" },
   'exists $h{k}' => "Perl",
   'exists' => "Pliant Perl6",
   'has' => "Eiffel",
   'haskey' => "YCP",
   'has_key' => "Python",
   'has_key?, include?, key?, member?' => "Ruby",
   'Contains' => "C#",
   'containsKey' => "Java",
   'includesKey' => "Smalltalk",
   'k in h' => "Python",
   'k not in h' => "Python",
   'in' => "Awk",
   'mem' => "OCaml",
   'isfield' => "Matlab",
   'find (returns an iterator)' => "C++",
   'h[k]' => "Pike",
   '(gethash k h)' => "CommonLisp",
   'maps' => "E",
   'known' => "PostScript",
  ],

  'remove by key' =>
  [
   'delete $h{k}' => "Perl",
   'del h[k]' => "Python",
   'remove' => "Eiffel Java OCaml YCP",
   'Remove' => "C#",
   'removeKey' => "E Smalltalk",
   'remhash' => "CommonLisp",
   'delete' => "Ruby JavaScript Perl6",
   'erase' => "C++",
   'm_delete' => "Pike",
   'removeObjectForKey' => "Objective-C",
   'undef' => "PostScript",
   'rmfield' => "Matlab",
  ],

  'list of keys' =>
  [ { MLANG => 'C++ OCaml CommonLisp' },
   'keys' => 'Perl Perl6 Maple Python MSH Ruby Smalltalk',
   'keySet' => "Java",
   'allKeys' => "Objective-C",
   'AllKeys' => "C#",
   'indices' => "Pike",
   'current_keys' => "Eiffel",
   'getKeys' => "E",
   'array_keys' => "PHP",
   'fieldnames' => "Matlab",
   'findall(Key, item(Key, _), Keys)' => 
  ],

  'list of values' =>
  [ { MLANG => 'C# C++ OCaml CommonLisp' },
   'values' => "Perl Perl6 Java Pike Python Ruby Smalltalk",
   'getValues' => "E",
   'content' => "Eiffel",
   'array_values' => "PHP",
   'struct2cell' => "Matlab",
   'entries' => "Maple",
  ],
 ],

 'range' => 
 [ { MLANG => 'C C++ Maple OCaml Java ClassicREXX Eiffel' },
  'inclusive .. inclusive' =>
  [
   'a .. b' => "Ruby Perl Pascal merd E Ada MSH",
   'a:b' => "Matlab",
   '[ a .. b ]' => "Haskell",
   'to' => "Smalltalk",
   'seq a b / jot - a b (jot on BSD)' => "BourneShell",
   'range' => "PHP",
   'range(from: a, to: b, by: step)' => "Dylan",
   'List.number A B Step' => "Oz",
   'numlist / between' => "Prolog",
   'iseq' => "Logo",
  ],

  'inclusive .. exclusive' =>
  [
   'a ... b' => "Ruby",
   'a ..! b' => "E",
   'range' => "Python",
  ],
 ],

],

'Mathematics' => [

 'type name' =>
 [ { KIND => 'typed' },
  'integers' =>
  [
   'short, int, long' => "C C#",
   'int' => "YAML OCaml",
   'Int' => "Perl6",
   'Int, uInt, Int8, Int16...' => "Pliant",
   'integer' => "Maple",
   'INTEGER, INT, SMALLINT' => "SQL92",
   'INTEGER, INTEGER_8, NATURAL_8...' => "Eiffel",
   'int8,uint8,int16,uint16,...64' => "Matlab",
  ],

  'decimal' =>
  [
   'float, double' => "C C#",
   'float' => "YAML OCaml Maple",
   'Float, Float32, Float64' => "Pliant",
   'NUMERIC, DECIMAL, DOUBLE PRECISION' => "SQL92",
   'Rat' => "Perl6",
   'DOUBLE, REAL' => "Eiffel",
   'single, double' => "Matlab",
  ],
 ],


 'numbers syntax' =>
 [ { ALL => 1 },
   'integers' =>
   [
    '1000' => 'Awk Ada C++ B Maple Prolog Logo Rebol C C# Haskell E Java JavaScript Pascal Pliant Pike Python BourneShell Tcl Scheme Smalltalk Perl Perl6 Ruby Eiffel OCaml merd Oz SQL92',
    '1000, 1000.' => 'CommonLisp EmacsLisp',
    '1000, 1000., 1000.0' => 'Awk Matlab',
    "1000, '1000'D" => "ClassicREXX",
   ],

   'integers in base 2, octal and hexadecimal' =>
   [ { MLANG => "Matlab Prolog Logo" },
    '0b1, 07, 0xf' => 'Ruby Pike Perl Oz',
    '0b1, 0o7, 0xf' => 'OCaml Perl6',
    '07, 0xf' => "C C++ Python Tcl Awk JavaScript",
    '0xf' => "C# E",
    '07' => "B",
    '0o7, 0xf' => "Haskell",
    '1b' => "Eiffel",
    '2#1#, 8#7#, 16#f#' => "Ada",
    '2#{1}, #{F}' => "Rebol",
    '#b1, #o7, #xf' => "CommonLisp EmacsLisp Scheme",
    '#2r1, #8r7, #16rf' => "CommonLisp EmacsLisp",
    '1b, Fh' => "Pliant",
    "'1'B, 'F'X" => "ClassicREXX",
    "B'1', X'F'" => "SQL92",
   ],

   'integer thousand-seperator' => 
   [ { MLANG => "Awk C C++ C# Logo Maple Python Prolog Haskell Oz JavaScript CommonLisp ClassicREXX SQL92 Matlab" },
    '1_000, 10_00, 100_0' => 'E Perl Perl6 Ruby Eiffel OCaml',
    "1'000, 10'00, 100'0" => 'Rebol',
    '1_000' => 'merd Ada',
   ],

   'decimals' =>
   [ { MLANG => 'BourneShell' },
    '1000., 1E3' => 'C C++ E Logo Maple OCaml Java Python Scheme Tcl JavaScript Ada ClassicREXX SQL92',
    '1000., 1E3, 1,0' => 'Rebol',
    '1000., 1.E3' => 'Oz Eiffel',
    '1000.0, 1E3' => 'C# Pike Ruby Prolog CommonLisp EmacsLisp Smalltalk',
    '1000.0, 1.0E3' => 'Haskell',
    '1000, 1000.0, 1E3 (integers are decimals)' => 'Awk Perl Perl6 merd',
   ],
 ],

 'addition / subtraction / multiplication / division' =>
 [
  '+ / - / * / /' => "C C++ Java Eiffel Maple C# Logo Perl Perl6 Matlab MUMPS Python Pliant Ruby BourneShell merd Tcl Haskell Scheme CommonLisp EmacsLisp Smalltalk ClassicREXX SQL92 Prolog",
  '+ +. / - -. / * *. / / /. (with mathematical priorities)' => "OCaml",
  'sum / difference / product / quotient' => "Logo",
  'add / sub / mul / idiv div' => "PostScript",
 ],

 'exponentiation (power)' =>
 [ { MLANG => 'Pascal' },
  '**' => 'PL/I Perl Perl6 Rebol Ruby Python OCaml E Ada merd Fortran Prolog ClassicREXX',
  '^' => 'Eiffel Awk Dylan Lua Matlab Pliant',
  '* (APL uses a real multiplication sign for multiplication from a special character set)' => 'APL',
  '**, ^' => 'Maple',
  '**, ^ and ^^ (for each various types)' => 'Haskell',
  'pow' => 'C C++ Pike Python SML Tcl Java PHP JavaScript',
  'Pow' => "C# Oz",
  'power' => 'Delphi-Kylix Logo Rebol',
  'exp' => "PostScript",
  'expt' => "EmacsLisp CommonLisp Scheme",
  'raisedTo' => "Smalltalk",
 ],

 'negation' =>
 [
  '-' => "B BCPL Awk Maple Logo C++ C C# E Java Matlab Rebol Pike MUMPS Pliant Haskell Python BourneShell Tcl Scheme Smalltalk Perl Perl6 Ruby Eiffel merd EmacsLisp CommonLisp JavaScript Ada Prolog ClassicREXX",
  '- -.' => "OCaml",
  '~' => "SML Oz",
  'neg' => "PostScript",
  'negate' => "Rebol",
  'minus' => "Logo",
 ],

 'random' =>
 [ { ALL => 1 },
   'random number' =>
   [
    'rand' => 'Perl Perl6 Maple Ruby Matlab',
    'random' => "Prolog Logo",
    '$RANDOM' => "MUMPS",
    'Random.int' => "OCaml",
  pre('r: RANDOM
create r.make
r.start
r.item') => "Eiffel",    
   ],

   'seed the pseudo random generator' =>
   [ { MLANG => "Prolog" },
    'srand' => 'Perl Perl6 Ruby',
    'set_seed' => "Eiffel",
    'Random.init, Random.self_init' => "OCaml",
    "rand('state',...)" => "Matlab",
    'rerandom' => "Logo",
    'RandomTools[MersenneTwister][SetState]' => "Maple",
   ],
 ],

 'operator priorities and associativities' =>
 [ { MLANG => 'Scheme CommonLisp EmacsLisp' },
  'addition vs multiplication' =>
  [
   'mathematical' => "C C++ Maple Java C# Eiffel Perl Perl6 Python Ruby BourneShell merd Matlab Tcl Haskell ClassicREXX",
   'same priorities' => "Smalltalk MUMPS",
  ],

  'exponentiation vs negation (is -3^2 equal to 9 or -9)' =>
  [ { MLANG => 'C C++ MUMPS' },
   'mathematical' => "Perl Perl6 Maple Eiffel Ruby Python Haskell Matlab ClassicREXX",
   'negation first' => "OCaml",
  ],
 ],

 'square root / e-exponential / absolute value' =>
 [
  'sqrt / exp / abs' => 'C C++ Eiffel Maple Prolog Ruby Smalltalk SML Python Perl Perl6 Pascal Tcl Java E PHP JavaScript Lua Ada Haskell OCaml EmacsLisp CommonLisp Scheme',
  'sqrt realsqrt / exp / abs' => "Matlab",
  'sqrt / exp /' => "Awk Logo",
  'Sqrt / Exp / Abs' => 'C# Oz',
  'sqrt / / abs' => "PostScript",
  'Sqrt / / ABS' => "Modula-3",
  '/ exp / abs' => "Pliant",
  'sqrt / /' => "Pike",
  'square-root / exp / abs or absolute' => "Rebol",
  'Sqrt / Exp / ABS' => "ClassicREXX",
 ],

 'trigonometry' =>
 [ { ALL => 1 },
   'basic' => 
   [ # perl's tan is in Math::Trig
    'sin / cos / tan' => "C C++ Pike Maple Prolog Ruby Matlab Python Smalltalk SML Pascal Perl Perl6 Tcl Java E PHP JavaScript EmacsLisp CommonLisp Scheme Lua Ada Haskell OCaml Pliant",
    'Sin / Cos / Tan' => "C# Oz ClassicREXX",
    'sin / cos /' => "Awk PostScript",
    'sine / cosine / tangent' => "Eiffel Rebol",
    'radsin / radcos / radtan' => "Logo",
   ],

   'inverse' =>
   [ # perl's are in Math::Trig, JavaScript are in Math
    'asin / acos / atan (Ruby >= 1.7)' => "C C++ Pike Prolog Pliant CommonLisp Scheme Python OCaml Perl Perl6 Ruby JavaScript Ada Matlab",
    'Asin / Acos / Atan' => "C# Oz",
    'ASin / ACos / ATan' => "ClassicREXX",
    'arcsin / arccos / arctan' => "Maple",
    'arcSin / arcCos / arcTan' => "Smalltalk",
    'arcsine / arccosine / arctangent' => "Rebol",
    'arc_sine / arc_cosine / arc_tangent' => "Eiffel",
    ' / / atan' => "PostScript",
    ' / / radarctan' => "Logo", 
   ],
 ],

 'logarithm' =>
 [ { ALL => 1 },
   'base e' =>
   [
    'ln' => "Pascal Maple Delphi-Kylix Logo PostScript SML Smalltalk",
    'log' => "C C++ Eiffel Perl Maple Prolog Perl6 Ruby Python Tcl PHP Lua OCaml Pliant Matlab Awk Pike Java E JavaScript EmacsLisp Scheme CommonLisp",
    'Log' => "C# ClassicREXX Oz Ada",
    'log 10' => "Haskell",
    'log-e' => "Rebol",
   ],
   'base 10' =>
   [
    'log10' => "C C++ Eiffel Perl Prolog Perl6 Ruby Python Tcl PHP Lua OCaml Pliant Matlab Delphi-Kylix Logo",
    'Log10' => "C# ClassicREXX",
    'log' => "PostScript SML",
    'log: 10' => "Smalltalk",
    'log-10' => "Rebol",
    'log[10]' => "Maple",
    'logBase 10' => "Haskell",
    'Log(X => val, Base => 10.0)' => "Ada",
    '(log x 10) ' => "CommonLisp",
   ],
   'base 2' =>
   [ { KIND => 'rare' },
    'log2' => "Matlab",
    'log-10 / log-2' => "Rebol",
    'Log(X => val, Base => 2.0)' => "Ada",
   ],

 ],

 'euclidian division (both quotient and modulo)' =>
 [ { KIND => 'rare' },
  'divmod' => "Ruby Python",
  'divMod' => "Haskell", # and quotRem
  'div ldiv lldiv' => "C",
  'IntInf.quotrem' => "SML",
  'floor' => "Dylan CommonLisp",
 ],

 'modulo' =>
 [
  'modulo of -3 / 2 is 1' =>
  [
   '%' => "Ruby Pike Python Tcl Perl Perl6 ClassicREXX",
   '%%' => "E",
   '\\\\' => "Smalltalk",
   'mod' => "SML EmacsLisp CommonLisp Matlab Ada Haskell Prolog",
   'MOD' => "Modula-3",
   'modulo' => "Ruby Dylan Logo",
   'rem' => "Prolog",
  ],

  'modulo of -3 / 2 is -1' =>
  [
   '%' => "Awk B C C++ C# Java E PHP JavaScript Pliant",
   '#' => "MUMPS",
   'mod' => "Pascal PostScript Prolog Lua OCaml XPath Oz",
   'remainder' => "Ruby Scheme Logo",
   'rem' => "BCPL Ada Smalltalk Matlab",
   '//' => "Rebol ClassicREXX",
   '\\\\' => "Eiffel",
  ],
 ],

 'truncate / round / floor / ceil' =>
 [
  'trunc / round / floor / ceil' => "C C++ Maple Matlab",
  'truncate / round / floor / ceiling' => "CommonLisp Scheme PostScript Prolog Perl6",
  'int / round / floor / ceil' => "Pike JavaScript Python",
  'int / round / /' => "Logo",
  'to_i, Integer() / round / floor / ceil' => "Ruby",
  'TRUNC / FORMAT / Floor / Ceil' => "ClassicREXX",
  '/ round / floor / ceil' => "SML Tcl Java E PHP Lua",
  '/ Round / Floor / Ceiling' => "C#",
  '/ Round / Floor / Ceil' => "Oz",
  '/ round / floor / ceiling' => "PostScript Dylan EmacsLisp Haskell XPath",
  '/ ROUND / FLOOR / CEILING' => "Modula-3",
  '/ rounded / floor / ceiling' => "Smalltalk Eiffel",
  'int / / floor / ceil' => "Perl",
  'int_of_float / / floor / ceil' => "OCaml",
  ' / / floor / ceil' => "Lua",
  ' / Rounding / Floor / Ceiling' => "Ada",
  'to-integer / / /' => "Rebol",
 ],

 'bitwise operators' =>
 [
  'and / or / xor' =>
  [
   '& / | / ^' => "C C++ C# E Eiffel Pike Ruby Python Perl Java JavaScript",
   '& / |' => "YCP",
   '+& / +| / +^' => "Perl6",
   'and / or / xor' => "Rebol PostScript",
   'land / lor / lxor' => "OCaml",
   'logand / logior / logxor (see also bit-and / bit-or / bit-xor)' => "CommonLisp",
   'bitand / bitor / bitxor' => "Matlab Logo",
   'BITAND / BITOR / BITXOR' => "ClassicREXX",
   '/\ / \/ / xor' => "Prolog",
  ],

  'negation' => 
  [
   '~' => "C C++ C# Pike Ruby Python Perl Java JavaScript YCP",
   'not' => 'Eiffel PostScript',
   'lnot' => "OCaml",
   'lognot (see also bit-not)' => "CommonLisp",
   'bitnot' => "Eiffel Logo",
   'complement' => "Rebol",
   'bitcmp' => "Matlab",
   '\\' => "Prolog",
  ],

  'left shift / right shift / unsigned right shift' => 
  [
   '<< / >> / >>>' => "Java JavaScript",
   '<< / >>' => "C C++ C# Pike Prolog Ruby Python Perl YCP",
   '|<< / |>>' => "Eiffel",
   'lsl / lsr or asr' => "OCaml",
   'bitshift' => "PostScript Matlab",
   'ashift lshift' => "Logo",
   '(ash x positive-integer) / (ash x negative-integer) / ' => "CommonLisp",
  ],
 ],

],

'Threads' => 
 [ { KIND => 'rare' },

  'thread definition' =>
  [
   'task task_name is [entry entry_name[(parameter ...)]...] end task_name' => "Ada",
   'task type task_type_name is [entry entry_name[(parameter ...)]...] end task_type_name' => "Ada",
   'class class_name extends Thread {[override run method] }' => "Java",
   '... fork' => "Smalltalk",
   'thread ...' => "Pliant",
   pre(q(parallel [threads nb_threads] [mini mini_threshold] [maxi maxi_threshold] [active]
   ...
   task
     parallel_instructions
   [post
     sequential_instructions]
   ...)) => "Pliant",
   '[NSThread detachNewThreadSelector:mainFunction toTarget:target withObject:obj]' => "Objective-C",
  ],

  'thread creation' =>
  [
   'object t=Thread.Thread(f)' => "Pike",
  ],
  
  'thread object creation' =>
  [
   'MyTask : task_type_name;' => "Ada",
   'class_name MyThread = new class_name()' => "Java",
  ],

  'starting / stopping threads' =>
  [
   'start() / stop() ("stop" is now deprecated)' => "Java",
   'resume / suspend / terminate' => "Smalltalk",
   'Tasks are started when created / call Stop entry or "abort task-object-name"' => "Ada",
  ],
  
  'passing data directly between threads' =>
  [
   'call an entry with paramters' => "Ada",
   'call any public method' => "Java",
   'common variables are copied at thread creation, in abscence of a "share" statement' => "Pliant",
  ],
  
  'terminating thread communication due to a time-out' =>
  [
   'select task_entry_call; or delay timeout_limit; end select;' => "Ada",
  ],
  
  'Thread Synchronization' =>
  [
   'Defining a Synchronized Shared Resource' =>
   [
    pre('protected Object_Name is [entry entry_name(Parameter : [in out] is type [...]);
procedure procedure_name(Parameter : [in out] is type [...]);
function function_name return type;
private
shared data declaration
end Object_Name;') => "Ada",
    'synchronize (this){ ... }' => "Java",
   ],
	  
   'Synchronized Writing to a shared resource' =>
   [
    'Object_Name.Entry_Name(Parms)<br>Object_Name.Procedure_Name(Parms)' => "Ada",
    'Object_Name.SetMethod(Parms)' => "Java",
   ],
	  
   'Synchronized Reading of a Shared Resource' =>
   [
    'Object_Name.Function_Name' => "Ada",
    'Object_Name.GetMethod()' => "Java",
   ],
	  
   'Monitor Syntax' =>
   [ { MLANG => 'Java' },
    'Objectg_Name.Entry_Name(Parms)' => "Ada",
   ],
  ],
  
  'Joining Another Thread' =>
  [
   'Suspending a thread until another thread completes' =>
   [
    'Call task entry serviced just before task termination' => "Ada",
    'OtherThread.join();' => "Java",
   ],
	 
   'Suspending a Thread Until Another Thread Establishes An Internal State' =>
   [ { MLANG => 'Java',},
    'Call a task entry on the other thread' => "Ada",
   ],
  ],
  
  'Thread Prioritization' =>
  [
   'Selecting a Prioritization Model' =>
   [ { MLANG => 'Java' },
    'pragma Locking_Policy(Ceiling_Locking);' => "Ada",
   ],
	 
   'Establishing a base thread priority' =>
   [ { MLANG => 'Java' },
    'pragma Priority(expression);' => "Ada",
   ],
	 
   'Changing Thead Priority' =>
   [
    'Set_Priority(Priority_Value);' => "Ada",
    'setPriority(newPriority);' => "Java",
   ],
  ],
  
  'Thread-safe sharing of data without synchronization' =>
  [
   'Ensuring access is atomic' =>
   [ { MLANG => 'Java' },
    'pragma Atomic(Object_Name);' => "Ada",
   ],

   'Ensuring access is always via a local copy of the shared data' =>
   [ { MLANG => 'Java' },
    'pragma Volatile(Object_Name);' => "Ada",
   ],
  ],

 ],

];

########################################################################################
# normalize, using annotations to know missing entries #################################
########################################################################################
my %missing;
my %proximity;
my %count;

sub merge_options {
    my ($h1, $h2) = @_;

    $h1->{MLANG} = join(' ', grep {$_} $h1->{MLANG}, $h2->{MLANG});
    $h1->{KIND} = join(' ', grep {$_} $h1->{KIND}, $h2->{KIND});
    $h1->{ALL} ||= $h2->{ALL};
}

foreach (group_by2(@$all)) {
    my ($category, $l) = @$_;

    my %options;
    if (ref $l->[0]) {
	merge_options(\%options, shift @$l);
    }

    foreach (group_by2(@$l)) {
	my ($label, $l) = @$_;

	my %options = %options;
	if (ref $l->[0] eq 'HASH') {
	    merge_options(\%options, shift @$l);
	}

	# if no subcat, fake one
	if (ref $l->[1] ne 'ARRAY') {
	    @$l = ('' => [ @$l ]);
	}

	my @flattened;
	my @g = map {
	    my ($subcat, $l) = @$_;

	    my %options = %options;
	    if (ref $l->[0] eq 'HASH') {
		my $h = shift @$l;
		$h->{KIND} && !$options{ALL} and die "KIND not accepted here\n";
		merge_options(\%options, $h);
	    }
	    my @subcats = 
	      ref $l->[1] eq 'ARRAY' ?
		map { [ "$subcat: $_->[0]", $_->[1] ] } group_by2(@$l) :
		      [ "$subcat" => $l ];
	    push @flattened, map { @$_ } @subcats;

	    my @provided_langs = map { split(" ", $_->[1]) } group_by2(map { @{$_->[1]} } @subcats);
	    push @provided_langs, split(' ', $options{MLANG}) if $options{MLANG};

	    my @wanted_langs = @langs;
	    if ($options{KIND}) {
		@wanted_langs = intersection(\@wanted_langs, $kinds{$_}) foreach split(' ', $options{KIND});
	    }
	    [ $subcat => \@provided_langs, \@wanted_langs ];
	} group_by2(@$l);

	@$l = @flattened;

	if (!$options{ALL}) {
	    @g = [ '' => [ map { @{$_->[1]} } @g ], $g[0][2] ];
	}
	
	foreach (@g) {
	    my ($subcat, $provided_langs, $wanted_langs) = @$_;
	    my $provided_langs_ = [ map { $_, @{$rev_hierarchy{$_} || []} } @$provided_langs ];
	    push @{$missing{$_}{$category}}, $label . ($subcat && " ($subcat)") foreach difference2($wanted_langs, $provided_langs_);
	}
    }
}

# @various don't have missing things
delete @missing{@various};


########################################################################################
# building #############################################################################
########################################################################################
my ($comments, %comments, %langs);

$\ = "\n";

system("rm -rf syntax-across-languages");
mkdir "syntax-across-languages";

unlink "syntax-across-languages.html";
open STDOUT, ">syntax-across-languages.html" or die '';
open SHORT, ">syntax-across-languages/index.html" or die '';
chmod 0444, "syntax-across-languages.html";

my $print_both = sub { print @_; print SHORT @_ };

print intro();
print SHORT intro("../");

print qq(<h2>Light Version</h2>);
print qq(Same content split into <a href="syntax-across-languages/">multiple html files</a>);

print SHORT qq(<h2>One Big Page</h2>);
print SHORT qq(Same content in <a href="../syntax-across-languages.html">one big page</a>);

$print_both->("<h2>Categories</h2>");
$print_both->("<ul>");
my $cache = mangle_category_init();
foreach (group_by2(@$all)) {
    my ($category, $l) = @$_;
    my $mangled_category = mangle_category($category, '', $cache);
    print qq(<li><a href="#$mangled_category">$category</a>);
    print SHORT qq(<li><a href="$mangled_category.html">$category</a>);

    $print_both->("<ul>");
    foreach (group_by2(@$l)) {
	my ($label, $l) = @$_;

	$label = simplify_category($label);
	my $s = mangle_category($label, $mangled_category, $cache);
	print qq(<li><a href="#$s">$label</a>);
	print SHORT qq(<li><a href="$mangled_category.html#$s">$label</a>);
    }
    $print_both->("</ul>");
}
$print_both->("</ul><hr>");

sub table {
    my ($l, $category, $label, $subcat, $F) = @_;

    print $F $subcat;
    print $F "<table border=1 cellpadding=3>";

    my $i = 0;
    foreach (group_by2(@$l)) {
        my ($name, $langs) = @$_;

	my $more = '';
        if (my ($n, $comment) = $name =~ m/(.*) \s+ \( ( [^)]+ (?: \( [^)]* \) )* [^)]* ) \)$/sx) {
	    my $nb = $comments{$comment} ||= ++$comments;
	    $name = $n;
	    $more = qq(<a href="#$nb">($nb)</a>); #)
	    $l->[$i] = $name;
        } else {
	    $name =~ s/\s+$//;
	}

        printf $F "<tr><td><tt>%s</tt></td>", html_quote("$name$more");
	my @langs = sort { lc $a cmp lc $b } split(" ", $langs);
        print $F "<td>", html_quote(join(", ", @langs)), "</td></tr>";
	$i += 2;

	foreach my $a (@langs) {
	    $count{$a}++;
	    foreach my $b (@langs) {
		$proximity{$a}{$b}++ if $a ne $b;
	    }
	}
    }
    print $F "</table><p>";
}

foreach (group_by2(@$all)) {
    my ($category, $l) = @$_;
    my $mangled_category = mangle_category($category, '');

    local *CAT;
    open CAT, "> syntax-across-languages/$mangled_category.html" or die;
    print CAT "<html><head><title>syntax across languages per category</title></head><body>";

    my $print_both = sub { print @_; print CAT @_ };

    $print_both->(qq(<h2><a name="$mangled_category">$category</a></h2>));
    $print_both->("<ul>");
    foreach (group_by2(@$l)) {
	my ($label, $l) = @$_;

	my $s = mangle_category($label, $mangled_category);
	$print_both->(qq(<li><a name="$s">$label</a><p>));

	foreach (group_by2(@$l)) {
	    my ($subcat, $l) = @$_;
	    table($l, $category, $label, $subcat, $_) foreach *STDOUT, *CAT;

	    foreach (group_by2(@$l)) {
		my ($name, $langs) = @$_;
		push @{$langs{$_}{$category}{$label . ($subcat && " ($subcat)")}}, $name foreach split(" ", $langs);
	    }

	}
    }
    $print_both->("</ul><hr>");

    print CAT comments();
    print CAT end();
}

print comments();
$print_both->(similar_pages());
$print_both->(credits());
$print_both->(end());

sub comments {
    $comments or return '';
    my %c = reverse %comments;
    join("\n",
	 "<h2>Remarks</h2>", "<ul>",
	 (map { qq(<li><a name="$_">($_)</a> $c{$_}) } (1 .. $comments)),
	 "</ul>");
}
sub similar_pages {
    <<'EOF';
<p>
<h2>Similar Pages</h2>
<ul>
<li><a href="http://www.gavilan.edu/csis/languages/history.html">Comments and Literals in programming languages</a>
<li><a href="http://web.cs.mun.ca/~ulf/pld/surface.html">The Surface of Programming Languages</a>
<li><a href="http://www.cpcug.org/user/clemenzi/technical/Languages/">Delphi, VisualBasic, C++, Java</a>
<li><a href="http://www.ps.uni-sb.de/~rossberg/SMLvsOcaml.html">SML, OCaml</a>
<li><a href="http://www.chimu.com/publications/JavaSmalltalkSyntax.html">Java, Smalltalk</a>
<li><a href="http://www.cc.gatech.edu/classes/AY2000/cs2803ab_fall/SqueakBasics.html">Java, C++, Smalltalk</a>
<li><a href="http://www.soften.ktu.lt/~mockus/gmcsharp/csharp/c-sharp-vs-java.html">Java, C#</a>
<li><a href="http://www.javaworld.com/javaworld/jw-11-2000/jw-1122-csharp1_p.html">Java, C#</a>
<li><a href="http://www.dur.ac.uk/~dcl0bjc/Java/a.taste.of.csharp/onefile/index.htm">Java, C#</a>
<li><a href="http://renaud.waldura.com/doc/ruby/idioms.shtml">Ruby, Java, Perl</a>
<li><a href="http://mail.python.org/pipermail/python-list/1999-August/009692.html">Python, Perl</a>
<li><a href="http://www.norvig.com/python-lisp.html">Python, Lisp</a>
<li><a href="http://www.uni-muenster.de/ZIV/Mitarbeiter/EberhardSturm/PL1andC.html">PL/I, C</a>
<li><a href="http://www.cs.wcupa.edu/~rkline/perl2php/">Perl, PHP</a>
<li><a href="http://obsidianrook.com/devnotes/elisp-for-perl-programmers.html">EmacsLisp, Perl</a>
</ul>
<h2>References</h2>
<ul>
<li><a href="http://cm.bell-labs.com/cm/cs/who/dmr/bcpl.html">BCPL</a>
<li><a href="http://msdn.microsoft.com/library/en-us/vblr7/html/vaorivblangkeywordsall.asp">VisualBasic</a>
<li><a href="http://www.gobosoft.com/eiffel/syntax/">Eiffel</a>
<li><a href="http://sqlzoo.net/">SQL</a>
<li><a href="http://www.research.compaq.com/SRC/m3defn/html/m3.html">Modula-3</a>
</ul>
EOF
}
sub credits {
    <<'EOF';
<p>
<h2>Credits</h2>
<ul>
<li>Yoann 'Pad' Padioleau (Haskell additions, Beta, various fixes)
<li>Jakub Travnik (Pascal, Delphi-Kylix)
<li>Robert Feldt (Ruby additions)
<li>Pascal Terjan (PHP)
<li>Carlos 'angus' (PostScript)
<li>Axel Kittenberger (various)
<li>Guido van Rossum (block vs scoping)
<li>Jeffrey Hobbs (Tcl)
<li>Mark-Jason Dominus (SML, various)
<li>Ash Searle (Java, PHP, JavaScript)
<li>Mark S. Miller (<a href="http://www.erights.org/">E</a>)
<li><a href="http://www.cs.man.ac.uk/~pjj/babel/">Pete Jinks</a> (various)
<li>Steve Tolkin (various)
<li>Franck Arnaud (Eiffel)
<li>Tom Murphy (SML)
<li>Guy Steele (Fortran, and many various)
<li>Carl Gay (Dylan, CommonLisp)
<li>Jay nop@nop.com (Lua)
<li>Philippe Lhoste (Lua, JavaScript)
<li>Jim Rogers (Ada, Java, Threads section)
<li>Ketil Z. Malde (Haskell)
<li>Mark Carroll (<a href="http://m3.polymtl.ca/m3/pkg/pm3/language/modula3/src/m3defn/m3.html">Modula-3</a>)
<li>Keith Wansbrough (Haskell and a few SML)
<li>Remi Vanicat (OCaml)
<li>Matthieu Villeneuve (CommonLisp)
<li>Joachim Durchholz (Eiffel)
<li>Walter Vannini (C, "breaking lines" idea)
<li>Peter Lewerin (Tcl)
<li>Patrice Ossona de Mendez (Pliant)
<li>Bert Freudenberg (Smalltalk & Squeak additions corrections)
<li>Dennis Haney (Perl, C#)
<li>Fergus Henderson (Mercury)
<li>Ralph Becket (Mercury)
<li>Bill Thornton (Java)
<li>Nik Crabtree (C#)
<li>Neal Holtz (Python)
<li>Donald Chai (Python)
<li>Fred Spiessens (Oz)
<li>Martin Nilsson (Pike)
<li>Theodore Eastman (VisualBasic)
<li>George Herson (Eiffel)
<li>Lee Denison (Tcl)
<li>Anton Rolls (Rebol)
<li>Pedro (Lua)
<li>Nathan Sharfi (C99, C#, C++)
<li>Dirk Gerrits (Common Lisp, Scheme, Emacs Lisp)
<li>Tabitha Arrowny (Ruby, Python, Perl, ...)
<li>Péter Varga (BourneShell, Common Lisp, ...)
<li>Ian Henderson (Objective C)
<li>Anthony Borla (Classic REXX, Prolog, Logo)
<li>Paul McJones (Modula-3 fixes)
<li>Uwe Kolb (Smalltalk fixes)
<li>Ciaran McNulty (PHP)
<li>David B. Nagle (Yorick)
<li>Michael Schaufelberger (Eiffel)
<li>Samuel Charron (Erlang)
<li>Kyle Ross (OCaml)
<li>Damien Krotkine (Ruby)
<li>Guillaume Cottenceau (Java)
<li>David.Whitten (MUMPS)
<li>Phil Howard (various)
<li>Andrzej Zawadzki (Ruby)
<li>Stuart Brady (VisualBasic)
<li>Joris Gillis (Maple)
</ul>
EOF
}
sub intro {
    my ($ref) = @_;
    <<EOF;
<html>
  <head>
    <title>syntax across languages</title>
  </head>

<body>

<h2>Introduction</h2>

What's this about?
<p>
<ul>

<li>Language Designers:
<p>Looking for operator or function names? Well have a look at the
following and remember using existing one may ease the transition :)

<p>
<li>Language Users:
<p>You know one language and want to find the corresponding operator or
function in another language

<p>
<li>Language lovers:
<p>Want to know the various ways people invented for commenting/assigning/...?

</ul>

<p>This is of course <a href="${ref}syntax-across-languages-per-language/what-is-missing.html">incomplete</a>. I welcome <a href="mailto:pixel\@rigaux.org">contributions</a>!
<p>
You may also have a look at this information <a href="${ref}syntax-across-languages-per-language/">sorted by languages</a>.
EOF
}
sub end {
    <<'EOF' . "\n</body></html>";
<hr><address><a href="mailto:pixel@rigaux.org">Pixel</a></address>
This document is licensed under <a href="http://www.gnu.org/copyleft/fdl.html">GFDL</a> (GNU Free Documentation License).
<br> $Id$
EOF
}

########################################################################################
# per-language #########################################################################
########################################################################################
system("rm -rf syntax-across-languages-per-language");
mkdir "syntax-across-languages-per-language";

open STDOUT, ">syntax-across-languages-per-language/categories.html" or die '';
print "<html><head><title>language categories</title></head><body>";

print qq(Programming languages are categorized. The goal is to tag the various
entries as belonging to some categories, so that the number of "Missing:"s is
reduced. For example, the "object creation" entry only means something in the
category "Object Oriented".<p>);

print "<ul>";
print "<li>$_" foreach values %kind_descriptions;
print "</ul>";

print "<p>";
print "Note that:";
print "<ul>";
while (my ($k, $v) = each %kind_dependencies) {
    print qq(<li>"$kind_descriptions{$v}" automatically implies "$kind_descriptions{$k}");
}
print "</ul>";
print end();


my @langs_ = sort { lc $a cmp lc $b } keys %langs;
print STDERR "unused lang $_" foreach difference2(\@langs, \@langs_);
print STDERR "non described lang $_" foreach difference2(\@langs_, \@langs);


open STDOUT, ">syntax-across-languages-per-language/what-is-missing.html" or die '';
print "<html><head><title>Missing's in syntax across languages</title></head><body>";
print "<table border=1>";
foreach (@langs_) {
    my $nb = map { @$_ } values %{$missing{$_} || {}};
    my $nb_text =
      sprintf("<font color=#%02x%02x00>", boundit(8 * $nb), boundit(0xff - 3 * ($nb - 16))) .
	($nb == 0 ? 'all done' : "$nb Missings") . "</font>";
    printf qq(<tr><td><a href="%s.html">%s</a></td><td>%s</td></tr>\n), url_quote($_), html_quote($_), $nb_text;
}
print "</table>";
print end();


open STDOUT, ">syntax-across-languages/proximity.html" or die '';
print "<html><head><title>Proximity between languages</title></head><body>";
print "<table border=1>";
foreach my $lang (@langs_) {
    my $h = $proximity{$lang};
    my @l = sort { $b->[1] <=> $a->[1] } map {
	[ $_, $h->{$_} / ($count{$lang} + $count{$_}) * 2 ];
    } grep { $_ ne $lang && min($count{$lang}, $count{$_}) > 10 } keys %$h;
    @l = grep { $_->[1] > 0.25 } @l or next;
    
    my $nb_text = join(' ', map {
	my $nb = boundit(0xff - ($_->[1] * 1.6 - 0.2) * 0xff);
	sprintf("<font color=#%02x%02x%02x>%s (%d/%d)</font>", $nb, $nb, $nb, $_->[0], $h->{$_->[0]}, $count{$_->[0]});
    } @l);
    printf qq(<tr><td><a href="../syntax-across-languages-per-language/%s.html">%s</a> (%d)</td><td>%s</td></tr>\n), url_quote($lang), html_quote($lang), $count{$lang}, $nb_text;
}
print "</table>";
print end();


open STDOUT, ">syntax-across-languages-per-language/index.html" or die '';

print "<html><head><title>syntax across languages per language</title></head><body>";
print "<ul>";
printf qq(<li><a href="%s.html">%s</a>\n), url_quote($_), html_quote($_) foreach @langs_;
print "</ul>";

print comments();
print credits();
print end();

foreach my $lang (@langs_) {
    open STDOUT, ">syntax-across-languages-per-language/" . file_quote($lang) . ".html" or die "bad lang $lang\n";
    
    print "<html><head><title>syntax in ", html_quote($lang)," </title></head><body>";
    print q(
<p>
The "Missing:"s below indicate that an entry is incomplete.
<ul>
<li>either the entry exist in the language, and <a href="mailto:pixel@rigaux.org">please tell</a>.
<li>either the entry doesn't exist in the language, and <a href="mailto:pixel@rigaux.org">please tell so</a>.
The entry will be marked as such and won't appear as missing anymore.
</ul>
);

    printf qq(<hr>);

    foreach my $prev (@{$hierarchy{$lang} || []}) {
	printf qq(<p>See the <a href="%s.html">various entries for %s</a><p>\n), url_quote($prev), html_quote($prev);
    }

    print "<ul>";

    if (my @has_kinds = grep { member($lang, @{$kinds{$_}} ) } keys %kind_descriptions) {
	my %l; @l{@has_kinds} = ();
	foreach (keys %kind_dependencies) {
	    exists $l{$kind_dependencies{$_}} and delete $l{$_};
	}
	print qq(<li><a href="categories.html">Category</a>: ), join(", ", map { $kind_descriptions{$_}} sort keys %l), "<p>";
    }

    foreach my $category (map { $_->[0] } group_by2(@$all)) {
	my $l = $langs{$lang}{$category};
	my $misses = $missing{$lang}{$category};

	$l || $misses or next;

	my $mangled_category = mangle_category($category, '');
	print qq(<li><a href="../syntax-across-languages/$mangled_category.html">$category</a><p>);

	if ($l) {
	    print "<table border=1 cellpadding=6>";
	    foreach my $label (sort keys %$l) {
		($label, my @names) = map { html_quote($_) } $label, @{$l->{$label}};
		print "<tr><td><tt>$_</tt></td><td>$label</td></tr>" foreach @names;
	    }
	    print "</table><p>";
	}
	if ($misses) {
	    print "Missing: <blockquote>", join(" ", map { "<br>$_" } @$misses);
	    print "</blockquote>";
	}
    }
    print "</ul>";
    print comments();
    print end();
}

sub pre {
    "<pre>" . html_quote($_[0]) . "</pre>";
}
sub group_by2 {
    my @l;
    local $::i;
    for ($::i = 0; $::i < @_ ; $::i += 2) {
	push @l, [ $_[$::i], $_[$::i+1] ];
    }
    @l;
}
sub html_quote {
    local $_ = $_[0];
    s/CommonLisp/Common Lisp/;
    s/EmacsLisp/Emacs Lisp/;
    s/VisualBasic/Visual Basic/;
    s/ClassicREXX/Classic REXX/;
    if (!/<a/ && !/<pre>/) {
	s/</&lt;/g;
	s/>/&gt;/g;
	s/&lt;br&gt;/<br>/g; # put back <br>
    }
    $_;
}
sub file_quote {
    local $_ = $_[0];
    s|/|_|g;
    $_;
}
sub url_quote {
    local $_ = $_[0];
    s/#/%23/g;
    file_quote($_);
}
sub simplify_category {
    my ($s) = @_;
    $s =~ s!<a href=.*?>(.*?)</a>!$1!g;
    $s;
}
sub mangle_category_init {
    { '' => 1 };
}
sub mangle_category {
    my ($s, $have, $seen) = @_;

    $s = simplify_category($s);
    $s = 'power' if $s eq 'exponentiation (power)';

    my @l = map { ucfirst } grep { !/^(the|a)$/i } split(/[^a-z0-9]+/i, $s);
    my $nb = @l == 1 ? 5 : @l == 2 ? 4 : @l < 6 ? 3 : 1;

    length($_) > 4 and s/[aeiouy]//ig foreach @l;

    my $s_ = $have . join('', map { substr($_, 0, $nb) } @l);
    if ($seen) {
	$seen->{$s_} and die "name conflict @l for $s_ ($have$s vs $seen->{$s_})\n";
	$seen->{$s_} = $s;
    }
    $s_;
}
sub member { my $e = shift; foreach (@_) { $e eq $_ and return 1 } 0 }
sub uniq { my %l; $l{$_} = 1 foreach @_; grep { delete $l{$_} } @_ }
sub difference2 { my %l; @l{@{$_[1]}} = (); grep { !exists $l{$_} } @{$_[0]} }
sub intersection { my (%l, @m); @l{@{shift @_}} = (); foreach (@_) { @m = grep { exists $l{$_} } @$_; %l = (); @l{@m} = (); } keys %l }
sub boundit { $_[0] < 0 ? 0 : $_[0] > 0xff ? 0xff : $_[0] }
sub min  { my $n = shift; $_ < $n and $n = $_ foreach @_; $n }
sub if_ {
    my $b = shift;
    $b or return ();
    wantarray || @_ <= 1 or die("if_ called in scalar context with more than one argument " . join(":", caller()));
    wantarray ? @_ : $_[0];
}


# Lua about dictionary contructors
# 
# Given keys k1 and k2, and values v1 and v2, the general lua dictionary
# constructor is:
# 
#   {[k1]=v1, [k2]=v2}
# 
# To retrieve the value associated with key k1:
# 
#   a[k1]
# 
# So if k1 and k2 are the strings "foo" and "bar", this is written:
# 
#   {["foo"]=v1, ["bar"]=v2}
# 
# and 
# 
#   a["foo"]
# 
# Across languages that I use, it seems that the majority of dictionary
# constructors, and most dictionary accessors, are through constant
# strings.  Lua's shorthand for the above is:
# 
#   {foo=v1, bar=v2}
# 
# and
#  
#   a.foo
# 
# which are friendlier to type and read.  So I misunderstood what your
# form was asking for when I said Lua's dictionary constructor was
# merely {a=b, c=d}.


# CommonLisp todo
# - control flow: CASE, ECASE, TYPECASE, PROG1,...
# - loops: DO, DO* and LOOP (*very* powerful), DOLIST, DOTIMES.
# - breaking control flow: RETURN, RETURN-FROM
# - exceptions: THROW, CATCH
# 
# - strings: all the items under "missing" are not missing (except the
# repetition). Look at PRINT, PRINC, PRIN1, FORMAT, LENGTH, SUBSEQ, AREF,
# CHAR-CODE, CODE-CHAR, UPPER-CASE-P and others, CHAR-UPCASE and others,
# STRING-UPCASE and others, misc functions on strings.
# 
# - lists: same as above.
# 
# - various data types: tuples can be represented by any kind of sequence,
# or by structures.
