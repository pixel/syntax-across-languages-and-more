
use strict;
use vars qw(@static @dynamic @OO @functional @reflexive @typed @rare @has_lambda @various);

########################################################################################
# kinds ################################################################################
########################################################################################
my @OO_dynamic = 
  qw(E JavaScript Perl Perl6 MSH PHP Python Rebol Ruby Smalltalk Io VisualBasic YCP Tcl MzScheme Oz Matlab);
my @OO_static = 
  ('C#', qw(Ada Beta C++ Cecil Pike Objective-C Java Eiffel Sather Delphi-Kylix Pliant Simula));
my @functional_dynamic = 
  qw(Scheme Erlang FL Logo);
my @functional_static = 
  qw(Mercury SML Haskell);

@static = (@OO_static, @functional_static,
  qw(C Cobol Pascal Fortran merd Modula-2 Modula-3 OCaml), 'F#');
@dynamic = (@functional_dynamic, @OO_dynamic,
  qw(Awk Basic Dylan Forth Maple Lua Icon XPath XSLT Pike PostScript Prolog BourneShell FishShell Oz EmacsLisp CommonLisp ClassicREXX Yorick));
@OO = (@OO_dynamic, @OO_static, 
  qw(OCaml CommonLisp Dylan merd));
@functional = (@functional_dynamic, @functional_static, 
  qw(OCaml EmacsLisp CommonLisp Dylan merd Smalltalk));

@reflexive = (@dynamic, 
  qw(Java merd Pliant));
@typed = (@static, 
  qw(Dylan Smalltalk Perl6 VisualBasic YCP));
@has_lambda = (@functional,
  qw(Ruby Python Perl Perl6 MSH Mathematica), 'C#2', 'C#3', 'Tcl');

@various = qw(APL BCPL B J PL/I MUMPS HTML CSS TeX SGML XML YAML Assembler SQL92);

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
  'Tcl8.5' => ['Tcl'],
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
    '#' => "Perl Io Perl6 Maple Ruby Python Tcl Icon Awk FishShell BourneShell PHP merd E Pliant YAML",
    '//' => "BCPL Io C99 C++ C# F# Java Dylan Pike PHP JavaScript YCP Yorick",
    '--' => "Cecil Eiffel Sather Simula Haskell Ada Lua SQL92",
    ';' => "EmacsLisp Logo CommonLisp Scheme Rebol Assembler MUMPS",
    '%' => "Prolog Mercury TeX Erlang PostScript Oz Matlab",
    'rem' => "Basic",
    "'" => "VisualBasic",
    '\\' => "Forth",
    '!' => "Fortran90 Assembler",
    'NB.' => 'J',
    'C or * in column 1' => "Fortran",
   ],

   'nestable' =>
   [
    '(* ... *)' => "OCaml Beta F# Mathematica Pascal Modula-3 SML",
    '%( ... %)' => "Matlab",
    '/* ... */' => "Oz Io Dylan ClassicREXX SQL99",
    '{ ... }' => "Pascal Rebol",
    '{- ... -}' => "Haskell",
    '#| ... |#' => "CommonLisp",
    '#[ ... ]' => "Perl6",
    '#if 0 ... #endif' => "C",
    'comment { ... }' => "Rebol",
    'comment [ ... ]' => "Rebol",
    '[ ... ] (when unused)' => "Rebol",
    '--[[ ... ]]' => "Lua",
   ],

   'non nestable' =>
   [
    '" ... "' => "Smalltalk",
    '/* ... */' => "C C++ C# Java B PL/I Pike CSS PHP JavaScript Mercury YCP Yorick",
    '<!-- ... -->' => "HTML XML",
    '( ... )' => "Forth",
   ],
  ],

  'documentation comment' =>
  [ { MLANG => "C Tcl ClassicREXX Mathematica Logo MUMPS Prolog Maple" },
   'until end of line' =>
   [
    '///' => "C# F# Java",
    '-- |' => "Haskell",
    '-- ^' => "Haskell",
   ],

   'non nestable' =>
   [
    "/** ... */ (for C, it is not a standard convention, but it is the more widespread)" => "C C# Java E PHP",
    "(** ... *)" => "F#",
   ],

   '' =>
   [
    '{-| ... -}' => "Haskell",
    '(** ... *)' => "OCaml",
    '/* DOCUMENT ... */' => "Yorick",
    'indexing
              identifier: "...";' => "Eiffel",

    "someClass comment: '...'" => "Smalltalk",

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
  [ { MLANG => "OCaml Io Haskell Ada MUMPS CommonLisp Matlab Prolog Tcl" },
   '__LINE__ __FILE__' => "C C++ Perl Pike PHP Ruby",
   '__LINE__ __SOURCE_FILE__' => "F#",
   '$?LINE $?FILE' => "Perl6",
   'inspect.stack()[0][2] inspect.stack()[0][1]' => "Python",
   pre('(new System.Diagnostics.StackFrame(true)).GetFileLineNumber()
(new System.Diagnostics.StackFrame(true)).GetFileName()') => 'C#',
   'system/script/header/file (need "file: %script-header.r" in file header)' => "Rebol",
   'SOURCELINE() / parse source OS . SOURCENAME' => "ClassicREXX",
   'info frame 0' => "Tcl8.5",
   'thisContext lineNumber / thisContext method source' => "Smalltalk",
  ],

  'tokens' =>
  [ { ALL => 1 },
    'case-sensitivity (keywords, variable identifiers...)' =>
    [ # see http://www.swiss.ai.mit.edu/~jaffer/r5rs_4.html#SEC14 for Scheme, Guile is an exception
     'case-sensitive' => 'B C C++ C# F# Io Java Maple Mathematica Prolog JavaScript Lua Matlab Pike Perl Perl6 Python Ruby XML YAML Tcl Smalltalk BourneShell FishShell OCaml Haskell merd Awk Modula-3 Pliant Yorick',
     'case-insensitive' => "PL/I CSS Pascal Logo Rebol VisualBasic Eiffel Ada SGML HTML Scheme CommonLisp Forth Assembler ClassicREXX SQL92",
     'case-sensitive: variables<br>case-insensitive: keywords, functions, constants...' => "PHP",
     'case-sensitive: identifiers<br>case-insensitive: keywords' => "E",
     'case-sensitive: identifiers<br>case-insensitive: commands' => "MUMPS",
    ],

    'what is the standard way for <a href="http://c2.com/cgi/wiki?CapitalizationRules">scrunching together multiple words</a>' =>
    [ { MLANG => 'MUMPS' },
     # see http://dotnet.di.unipi.it/EcmaSpec/CSharp/cont25.html C.1.4 for C#
     # perl -ne '$l{$_} = 1 foreach /(\w*[a-z][_]\w*)/g; $m{$_} = 1 foreach /(\w*[a-z][A-Z]\w*)/g; END { print join(" ", keys %l), "\n", join(" ", keys %m), "\n" }'
     'camelCase' => "JavaScript",
     'CamelCase or camelCase' => "Haskell Io Java JavaScript Tcl Smalltalk C# E Pascal Mathematica VisualBasic",
     'underscores' => 'merd FishShell',
     'dots' => "Logo",
     'hyphens' => "Rebol EmacsLisp CommonLisp",
     'underscores for functions, unclear for modules / types / constructors' => "OCaml",
     'UPPER_CASE' => "BourneShell",
     'lowercasenoseparator' => 'Matlab',
     'underscores, UPPER_CASE for class names' => "Eiffel",
     'CamelCase for classes, underscores for methods' => "Python",
     'CamelCase for types, underscores for functions, variables, ...' => "Pliant",
     'CamelCase for methods, types and modules, underscore for functions' => "F#",
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
     '[a-zA-Z][a-zA-Z0-9]*' => "PL/I Smalltalk FishShell Mathematica",
     "[a-zA-Z][_a-zA-Z0-9]*" => "Eiffel Matlab",
     "[a-zA-Z](_?[a-zA-Z0-9])*" => 'Ada',
     "[_a-zA-Z][_a-zA-Z0-9]*" => 'B C C++ C# E Maple Python PHP Tcl Awk',
     '[_a-zA-Z0-9]+' => "BourneShell Perl Perl6",
     '[a-zA-Z0-9]+' => "FishShell",
     "[_a-zA-Z][_a-zA-Z0-9]* or '[^']*'" => "Pliant",
     '[_a-zA-Z$][_a-zA-Z0-9$]*' => "Java JavaScript",
     '[a-zA-Z%][a-zA-Z0-9]*' => "MUMPS",
     "[_a-z][_a-zA-Z0-9]*" => 'Ruby',
     "[_a-z][_a-zA-Z0-9]*[!?']*" => 'merd',
     "[_a-z][_a-zA-Z0-9']*" => 'SML OCaml Haskell',
     "[_a-zA-Z][_a-zA-Z0-9']*" => 'F#',
     "[_A-Z][_a-zA-Z0-9]*" => 'Prolog Mercury',
     "[_a-zA-Z!$%&*/:<=>?^][_a-zA-Z!$%&*/:<=>?^0-9.+-]*" => "Scheme", # cooked from http://www.cs.indiana.edu/scheme-repository/R4RS/r4rs_9.html
     "[a-zA-Z!?@#$_][a-zA-Z0-9!?@#$_]*" => "ClassicREXX",
     q([_a-zA-Z?!.'+*&|=~-][_a-zA-Z0-9?!.'+*&|=~-]* or 
<br>[^0-9[](){}":;/][^ \n\t[](){}":;/]*) => "Rebol",
     'anything without a space and is not a number' => "CommonLisp Forth",
    ],

    'function identifier regexp (if different from variable identifier regexp)' =>
    [ { KIND => 'rare' },
     "[_a-zA-Z][_a-zA-Z0-9]*[!?]?" => 'Ruby',
     "[_a-z][_a-zA-Z0-9]*" => 'Prolog Mercury',
     '[^ \t\n\r\f]+' => "Tcl",
     '[^ \t\n\r\f/]+' => "FishShell",
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
   'nothing needed' => 'Ada B C C++ C# F# Maple Mathematica Eiffel PostScript Rebol Java YCP JavaScript Pascal Perl Perl6 OCaml CommonLisp EmacsLisp Scheme SML Smalltalk XSLT Forth Oz',
   '\\' => "Awk Io C Python Ruby Tcl E FishShell BourneShell",
   '_' => "VisualBasic",
   ',' => "ClassicREXX",
   '~' => "Logo",
   '...' => "Matlab",
  ],

  'variable assignment or declaration' =>
  [
   'assignment' =>
   [
    '=' => "B C C++ C# Io Mathematica Java Perl Perl6 Matlab Pike PHP Basic Erlang Icon Oz JavaScript Lua BourneShell Awk YCP ClassicREXX Yorick",
    ':=' => "BCPL Ada SML Cecil Pascal Mathematica Dylan Eiffel Maple Sather Modula-3 Simula Smalltalk E Pliant",
    '<-' => "OCaml F#",
    '_ (displayed <- with a special character)' => "Squeak",
    ':' => "BCPL Rebol",
    '-> (variable on the right)' => "Beta",
    'def' => "PostScript",
    'setq' => "EmacsLisp CommonLisp",
    'setf' => "CommonLisp",
    'set' => "Rebol CommonLisp FishShell",
    'SET v=...' => "MUMPS",
    'set!' => "Scheme",
    'is' => "Prolog",
    'make "v e' => "Logo",
   ],

   'declaration' =>
   [
    '=' => "Haskell Prolog Mercury",
    '<-' => "Haskell",
    ':-' => "Prolog",
    ':=' => "Io",
    'let v = e in' => "F# OCaml",
    'let val v = e in' => "SML",
    'let v = e (F#: with indentation)' => "F# BCPL",
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
    't v' => "C C++ C# Java",
    '| v1 v2 |' => 'Smalltalk',
    'auto v1, v2; extrn v3, v4;' => "B",
    'var' => "JavaScript Pliant",
    'gvar' => "Pliant",
    'variable v (the variable behaves like a pointer)' => "Forth",
    'Module[{x, y = v},  ... ]' => 'Mathematica',
    'Block[{x, y = v}, ... ]' => 'Mathematica',
    'With[{c1 = v1, c2 = v2, ... }, ...]' => 'Mathematica',
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
   '( ... )' => "Beta BCPL Io Logo Mathematica Prolog B ClassicREXX Maple C C++ C# Matlab YCP Java Eiffel Rebol MUMPS MSH Pike Perl Perl6 Python Ruby Pascal Haskell OCaml F# Smalltalk SML merd E Tcl PHP JavaScript Lua Ada Awk Modula-3 Pliant XPath Oz SQL92 Yorick",
   '[ ... ]' => "Rebol",
   'indentation' => "merd",
   '$ ...' => "Haskell",
   'begin ... end' => "OCaml F# Ruby FishShell",
   'space (cf <a href="http://merd.net/choices_syntax.html#horizontal_layout">horizontal layout</a>)' => "merd",
  ],

  'block (grouping statements, especially when statements are not expressions)' =>
  [ { MLANG => 'OCaml Prolog Rebol SML XSLT Mathematica CommonLisp Maple Oz PostScript' },
   '{ ... }' => "Pike PHP JavaScript Awk BourneShell Tcl Yorick",
   '{ ... } (introduce scope)' => "B C C++ C# Java Perl Perl6 E Haskell Modula-3 YCP",
   '( ... ) (introduce scope)' => "BourneShell",
   '[ ... ]' => "Logo",
   '[ x. y. ... ]' => "Smalltalk",
   '"..."' => "Tcl",
   'begin ... end (introduce scope)' => "Pascal Ada",
   'BEGIN ... END' => "Modula-3",
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
    '== != ' => "B C C++ Io Java OCaml F# Pike Perl Perl6 Awk Yorick",
    '= /=' => "Fortran90 Eiffel",
    '= <>' => "Pliant Rebol Logo Maple Modula-2",
    '= # (in Modula-2, <> and # are synonyms)' => "Modula-2 Modula-3",
    "= !=" => "BourneShell FishShell",
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
    '== !=' => "Awk C++ C# E Ruby Tcl merd Python YCP PHP5",
    '== <>' => "Python",
    '== /=' => "Haskell",
    '== \=' => "Oz",
    '== \==' => "ClassicREXX Prolog",
    '= /=' => "Ada",
    '= !=' => "XPath Maple",
    '= <>' => "VisualBasic OCaml F# Rebol SML Beta Pascal SQL92",
    '= ~=' => "Dylan Smalltalk",
    '== ~= eq ne isequal isequalwithequalnans' => "Matlab",
    '=@= \=@= / = \= / =:= =\= (normal / structural / unification / arithmetic)' => "Prolog",
    '=== =!= / == != (structural / mathematical)' => "Mathematica",
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
    '< > <= >=' => "Ada Awk Io Beta Maple Mathematica Logo B C C++ C# YCP E Matlab Java Pascal Rebol Smalltalk VisualBasic Scheme Pike Perl Perl6 merd Tcl Haskell Ruby SML OCaml F# PHP Eiffel JavaScript EmacsLisp CommonLisp Dylan Lua Awk Modula-3 Python Pliant XPath ClassicREXX SQL92 Yorick",
    '< > =< >=' => "Mercury Oz",
    "< > '> '<" => "MUMPS",
    '<< >> <<= >>= (deep comparison)' => "ClassicREXX",
    '@< / @=< / @> / @>=' => "Prolog",
    'lt gt le ge' => "Perl Perl6 PostScript",
    '-lt -gt -le -ge' => "BourneShell FishShell MSH",
    '.LT. .GT. .LE. .GE.' => "Fortran",
   ],

   'returns 3 values (i.e. inferior, equal or superior)' =>
   [ { MLANG => 'B C C++ C# Ada Io Logo CommonLisp Awk Modula-3 Lua XSLT Rebol PostScript ClassicREXX Matlab' },
    'a <=> b' => "Ruby Perl Perl6 merd",
    'cmp' => "Perl Perl6 Python",
    'compare' => "OCaml F# Haskell Prolog Mercury Pliant Smalltalk",
    'strcmp' => "PHP C",
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
   [ { MLANG => 'Ruby B C C# Awk XSLT Tcl' },
    'min / max' => "Ada Io C++ Java Tcl8.5 Rebol Maple Lua Beta Prolog Pike Matlab Python Smalltalk E Eiffel merd CommonLisp Scheme OCaml F# Haskell Dylan Pliant SQL92 Yorick",
    'min minstr / max maxstr (in List::Util)' => "Perl",
    'Min / Max' => "Oz Mathematica",
    'MIN / MAX' => "Modula-3 ClassicREXX",
    'measure-object -min / measure-object -max' => "MSH",
   ],
  ],

  'runtime evaluation' =>
  [ { KIND => 'dynamic', MLANG => "XSLT" },
   'eval' => "Perl Perl6 Ruby Python BourneShell FishShell Scheme EmacsLisp Tcl Matlab PHP JavaScript CommonLisp YCP",
   'exec' => "Python",
   'dostring' => "Lua",
   'doString' => "Io",
   'Compiler evaluate:' => "Smalltalk",
   'runtime_compile / compile + execute' => "Pliant",
   'Compiler.evalExpression or Compiler.parseOzVirtualString' => "Oz",
   'compile_string' => "Pike",
   'interpret' => "ClassicREXX",
   'ToExpression' => "Mathematica",
   'run' => "Logo",
   'XECUTE' => "MUMPS",
   'do / reduce / compose / load' => "Rebol",
   '[...]' => "Tcl",
   '=.. (Univ operator)' => "Prolog",
  ],

  'force garbage collection' =>
  [ { MLANG => 'Ada B C C++ Tcl CommonLisp Mathematica Pascal XSLT ClassicREXX MUMPS Matlab FishShell', },
    'doGC' => "Beta",
    'GC.start' => "Ruby",
    'gc' => "Logo Pike Maple",
    'System.gc()' => "Java",
    'System.gcDo' => "Oz",
    'System.GC.Collect()' => "C# F#",
    'gc.collect()' => "Python",
    'full_collect' => "Eiffel",
    'garbage_collect' => "Mercury Prolog",
    'collectgarbage' => "Lua",
    'Collector collect' => "Io",
    'VM.garbageCollect()' => "JavaScript",
    'Gc.full_major()' => "OCaml",
    'Smalltalk garbageCollect' => "Smalltalk",
    'System.Mem.performGC' => 'Haskell',
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
     'f(a,b,...)' => "Awk B C C++ C# Io CSS Maple Java YCP Matlab Erlang Pike Perl Perl6 Mercury merd Eiffel Python Ruby Pascal E PHP JavaScript Dylan Lua Ada Awk Modula-3 XPath Prolog Yorick",
     'f a b ...' => "SML Haskell Logo F# OCaml Rebol Matlab Tcl Pliant BourneShell FishShell MSH",
     'f(a,b,...f) or f[a,b,...] depending on the version' => "BCPL",
     '(f a b ...) (apply f l) ' => "Scheme EmacsLisp CommonLisp",
     '(funcall f a b ...)' => "EmacsLisp CommonLisp",
     '{f a b}' => "Oz",
     '[apply f a b]' => "Tcl8.5",
     'f[a,b,...]' => "Mathematica",
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
     'f@a (only for one parameter)' => "Mathematica",
     'a // f (only for one parameter)' => "Mathematica",
     'a ~ f ~ b (only for two parameters)' => "Mathematica",
     '.. [ f, A, B, ...]' => "Prolog",
     pre('<xsl:call-template name="f">
    <xsl:with-param name="a" select=a/>
    <xsl:with-param name="b" select=b/>
</xsl:call-template>') => "XSLT",
    ],

    'with no parameter' =>
    [
     'f' => "Ada Eiffel Io Pascal PostScript Logo Matlab Rebol Ruby Perl MSH Perl6 Mercury Haskell Tcl Pliant BourneShell Prolog Yorick", # Haskell egs: monads
     'f()' => "Awk PHP Pike Maple Erlang Python C C++ C# YCP Java E Lua Perl JavaScript merd OCaml F# SML",
     '(f)' => "Scheme EmacsLisp CommonLisp",
     '(funcall f)' => "EmacsLisp CommonLisp",
     '{f}' => "Oz",
     'f[]' => "Mathematica",
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
  [ { KIND => 'functional', ALL => 1, MLANG => "Smalltalk Io Maple Mathematica CommonLisp Scheme Erlang Matlab" },

    'give the first argument' =>
    [
     'f a' => "SML Haskell OCaml F#",
     'f(a)' => "Mercury",
     'f(a,)' => "merd",
     '&f.assuming(var_name => a)' => "Perl6",
     'functools.partial(f, a) (Python >= 2.5)' => "Python",
     'interp alias {} f_a {} f a' => "Tcl",
    ],

    'give the second argument' =>
      [ { MLANG => 'SML Haskell OCaml F#' },
     'f(,b)' => "merd",
     '&f.assuming(b => b)' => "Perl6",
     'flip f b (it does not scale to 3rd argument)' => "Haskell",
    ],

    'give the first argument to operator ">"' =>
    [
     '(a >)' => "Haskell merd",
     '(>) a' => "OCaml F#",
    ],

    'give the second argument to operator ">"' =>
    [ { MLANG => "OCaml F#" },
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
    'let f para1 para2 = ...' => "OCaml F#",
    'f(para1, para2, ...) = valof $( ... $)' => "BCPL",
    'f(para1, para2, ...) = ...' => "merd",
    'f[para1_, para2_, ...] := ... para1 ...' => "Mathematica",
    'f ...  or  f: para1 ...' => "Smalltalk",
    'f: func [para1 para2 ...] ...' => "Rebol",
    '/f { ... } def' => "PostScript",
    'f := (para1, para2, ...) -> ...' => "Maple",
    'f := method(para1, para2, ..., code)' => "Io",
    'func f(a, b, c...) { ... }' => "Yorick",
    'typ0 f(typ1 para1, typ2 para2, ...) { ... }' => "C C++ C# Pike YCP",
    'function f(para1, para2) { ... }' => "Awk JavaScript",
    'function f(para1, para2) ... code ... end' => "Lua",
    'function f; ...; end' => "FishShell",
    'function f { ... }' => "KornShell",
    'function f { param(para1, [typ2]para2, ...) ... }' => "MSH",
    '(define (f para1 para2) ...)' => "Scheme",
    '(defun f (para1 para2) ...)' => "EmacsLisp CommonLisp",
    'fun { F Para1 Para2 } ... end' => "Oz",
    'fun f para1 para2 = ...' => 'SML',
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

    'f() { ... }' => 'BourneShell KornShell',

    pre('f : procedure
  ...
return retval') => "ClassicREXX",

    pre('to f :para1 :para2
   ...
end') => "Logo",

   ],

   'procedures' =>
   [
    pre('procedure f(para1 : typ1; para2, para3 : typ2);
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

    'variable number of arguments' =>
    [
      'one can use overloading on different number of arguments' => "C++ Java",
      'sub f { ... @_ }' => 'Perl',
      'sub f; ... $argv; end' => 'FishShell',
      'f() { ... $@ }' => "BourneShell",
      pre('function f(varargin)
for i=1:nargin
 ...(varargin{i})
end') => 'Matlab',
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
   '[list {a b} {...}]' => "Tcl8.5",
   'lambda a, b: ...' => "Python",
   'lambda(typ1 para1, typ2, para2, ...) { ... };' => "Pike",
   '(a, b) => ...' => "C#3",
   "(a, b) -> ..." => "Maple",
   'a, b -> ...' => "merd",
   '-> $a, $b { ... }' => "Perl6",
   '\a b -> ...' => "Haskell",
   'fn (a, b) => ...' => "SML",
   'fun a b -> ...' => "OCaml F#",
   '(func(A, B) = C :- ...)' => "Mercury",
   'function(a, b) ...' => "JavaScript",
   'function(a, b) ... end' => "Lua",
   'Function[{a, b}, ....] (see also: #1 + #2 &)' => "Mathematica",
   'fun(a, b) -> ... end' => "Erlang",
   'fun {$ A B} ... end (also works for procedures: proc {$ A B} ... end)' => "Oz",
   'func [a b ...] ...' => "Rebol",
   'def _(para1, para2, ...) ... { ... }' => "E",
   'proc {|a, b| ...}' => "Ruby",
   'lambda {|a, b| ...}' => "Ruby",
   '(lambda (a b) ...)', "Scheme EmacsLisp CommonLisp",
   "inline('...x...y...') (x, y, z are the parameters)" => "Matlab",
   'method(a, b, ...)' => "Io",
   'method(a, b) ... end method (method is optional)' => "Dylan",
   "create_function('$a,$b','...')" => "PHP",
   'delegate(ta a, tb b) { ... }' => "C#2",
  ],

  'function return value' => 
  [ { MLANG => 'Prolog PostScript' },
   'breaks the control flow' => ($::return_a_value =
   [
    q(return (in Lua, "return xxx" can only appear before a block end. in Matlab, only in inline('...'))) => "Awk Io B BCPL CommonLisp C Maple C++ C# ClassicREXX FishShell Java E Pike YCP Perl Perl6 Ruby Rebol Python Tcl Ada PHP Pliant JavaScript BourneShell Yorick Lua Matlab",
    'Return' => "VisualBasic Mathematica",
    'RETURN' => "Modula-3",
    'resultis ("return" is used when there is no value to return)' => "BCPL",
    'return from xxx' => "CommonLisp",
    '^' => "Smalltalk",
    'Exit Function / Exit Sub' => "VisualBasic",
    'output' => "Logo",
   ]),

   'function body is the result' => 
   [
    'no syntax needed (in Matlab, only for anonymous function)' => "Maple Io Mathematica Haskell OCaml F# Erlang SML EmacsLisp Tcl Rebol CommonLisp Scheme Perl Perl6 Ruby Dylan Oz Matlab",
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
   'forward' => "Io",
  ],

  'runtime inspecting the caller information' =>
  [ { KIND => 'dynamic', MLANG => 'E CommonLisp Logo' },
   'caller' => "Perl Perl6 Ruby",
   'call' => "Io",
   'inspect.stack()[1]' => "Python",
   'backtrace' => "Pike",
   "trace 'I'" => "ClassicREXX",
   "evalin('caller', ...)" => "Matlab",
   'current_predicate' => "Prolog",
   'thisContext sender' => "Smalltalk",
   'where(2)' => "Maple",
   'info level' => "Tcl",
  ],

  'function composition' =>
  [ { KIND => 'functional', MLANG => 'OCaml Io CommonLisp Smalltalk Matlab Logo', },
   '.' => "Haskell",
   '~' => "merd",
   'o' => "SML",
   '@' => "Maple",
   'compose' => 'Dylan',
   'Composition' => "Mathematica",
   '<<' => "F#",
   '>>' => "F#",
  ],

  'identity function' =>
  [ { KIND => 'functional', MLANG => 'OCaml F# Erlang Logo', },
    'id' => "Haskell",
    'identity' => "CommonLisp",
    'Identity' => "Mathematica",
    'yourself' => "Smalltalk",
  ],

],

'Control Flow' => [
  { MLANG => 'Haskell XSLT' },

  'sequence' =>
  [ { MLANG => "Forth PostScript Oz" },
   ',' => "C C++ Perl Matlab Pike JavaScript Prolog",
   '.' => "Smalltalk",
   ';' => "Awk Maple Mathematica Beta Io PL/I B C C++ C# Java Matlab YCP Pike Pascal Python Ruby Perl Perl6 OCaml F# SML merd Tcl E PHP JavaScript Ada BourneShell FishShell Haskell Modula-3 Pliant",
   ':' => "Maple",
   'nothing, optionally ;' => "Lua ClassicREXX",
   'space' => "Eiffel Rebol",
   'end-of-line' => "Awk Ruby F# Io Python Lua merd Basic Tcl E Matlab Fortran Assembler JavaScript BourneShell FishShell Haskell Pliant",
   '(begin ...)' => "Scheme",
   '(progn ...) (prog1 ...) (prog2 ...) ' => "EmacsLisp CommonLisp",
   '>>' => "Haskell",
  ],

  'if_then' =>
  [ { MLANG => "SML Prolog Haskell" },
   'if c then ...' => "OCaml F# Tcl merd Pascal",
   'if c then ... end' => "Ruby Eiffel Lua Oz",
   'if c then ... end if' => "Ada Maple",
   'if c then ... fi' => "Maple BourneShell",
   'if (c) then ... end' => "Dylan",
   'if c do ...' => "BCPL",
   'IF c THEN ... END' => "Modula-2 Modula-3",
   'if (c) ...' => "Awk B C C# C++ Java Pike PHP JavaScript YCP",
   'if c: ...' => "Python",
   'if c ...' => "Pliant Rebol Tcl",
   'if (c): ... endif' => "PHP",
   'if c {...}' => "Perl6",
   'if c [...]' => "Logo",
   'if (c) {...}' => "Perl E Yorick",
   'IF c ...' => "MUMPS",
   'c -> ...' => 'FL',
   'c ... if' => "PostScript",
   '... if c' => "Perl Ruby",
   'c if b1 then' => "Forth",
   '(if c ...)' => "CommonLisp Scheme",
   '(when c ...)' => "EmacsLisp",
   'c and ...' => "Perl Ruby",
   'if(c, ...)' => "Io",
   'If[c, ...]' => "Mathematica",
   'if(c) then(...)' => "Io",
   'c ifTrue(...)' => "Io",
   'c ifTrue: ...' => "Smalltalk",
   '<xsl:if test="c">...</xsl:if>' => "XSLT",

   'If c Then ...' => "VisualBasic",
   pre('If c
  ...
End If') => "VisualBasic",

   'if c; ... end' => "Ruby",
   'if c; ...; end' => "FishShell",
   'if c, ..., end' => "Matlab",

   pre('if c
  ...
end') => "Ruby Matlab",

   pre('if c then ; ...

if c then
  ...

if c then do
  ...
end') => "ClassicREXX",
  ],

  'if_then_else' =>
  [
   'if c then b1 else b2' => "SML OCaml F# Haskell merd",
   'if c then b1 else b2 end' => "Ruby Eiffel Lua",
   'if c then b1 elsif c2 then b2 else b3 end if' => "Ada",
   'if c then b1 elseif c2 then b2 else b3 end' => "Eiffel Oz",
   'if (c) then b1 elseif (c2) then b2 else b3 end' => "Dylan",
   'IF c THEN b1 ELSIF c2 THEN b2 ELSE b3 END' => "Modula-3",
   'If c Then b1 ElseIf c2 Then b2 Else b3 End If' => "Modula-2",
   'if (c) b1 else b2' => "Awk B C C# C++ Pike Java JavaScript YCP",
   'if c b1 elsif c2 b2 b3' => "Tcl",
   'if c then b1 elsif c2 then b2 else b3' => "Tcl",
   'if c then begin b1 end else begin b2 end' => "Pascal",
   'if c b1 eif c2 b2 else b3' => "Pliant",
   'if c then b1 elif c2 then b2 else b3 end if' => "Maple",
   'if c; then b1; elif c2; then b2; else b3; fi' => "BourneShell",
   'if c; b1; else b2; end' => "FishShell",
   'if c1, b1, elseif c2, b2, else, b3, end' => "Matlab",
   'if (c) b1 elseif (c2) b2 else b3' => "PHP",
   'if (c): b1 elseif (c2): b2 else: b3 endif' => "PHP",
   'if (c) {b1} elsif (c2) {b2} else {b3}' => "Perl",
   'if (c) {b1} else {b2}' => "E Yorick",
   '(if c b1 b2)' => "CommonLisp Scheme",
   '(if c then b1 else b2)' => "Mercury",
   '(c -> b1 ; c2 -> b2 ; b3)' => "Mercury",
   'c -> b1 ; b2' => "FL",
   'if(c, b1, b2)' => "Io",
   'If[c, b1, b2]' => "Mathematica",
   'if(c) then(b1) else(b2)' => "Io",
   'c ifTrue: b1 ifFalse: b2' => "Smalltalk",
   'ifelse c [b1] [b2]' => "Logo",
   'shunt c b1 c2 b2 b3' => "Pliant",
   'either c b1 b2 / if/else c b1 b2' => "Rebol",
   '(cond (c b1) (c2 b2) (t b3))' => "EmacsLisp CommonLisp",
   '(cond (c b1) (c2 b2) (else b3))' => "Scheme",
   'Which[c, b1, c2, b2]' => "Mathematica",
   'c -> b1 ; c2 -> b2 ; b3' => "Prolog",
   'case when c; b1 when c2; b2 else b3 end' => "Ruby",
   'test c then b1 or b2' => "BCPL",
   'e | c = b1 | c2 = b2 | otherwise = b3 (the result goes to "e")' => "Haskell",
   'c b1 b2 ifelse' => "PostScript",
   'c if b1 else b2 then' => "Forth",
   'c ? b1 : b2' => "Awk B C C++ C# Java Perl Ruby PHP JavaScript YCP Yorick",
   'c ?? b1 !! b2' => "Perl6",
   'b1 if c else b2 (Python >= 2.5)' => "Python",
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

   pre('IF c ...
ELSE ...') => "MUMPS",

  ],

  'ifnot_then (unless)' => 
  [ { KIND => 'rare' },
   'unless' => "EmacsLisp Perl",
   'ifFalse' => "Smalltalk",
   'if(c) not then(...)' => "Io",
  ],

  'multiple selection (switch)' =>
  [ { MLANG => 'Awk Ruby Python Maple PostScript Lua Pliant Smalltalk XSLT' },
   pre('switch (val) { 
   case v1: ...; break; 
   case v2: case v3: ...; break; 
   default: ...;
 }') => "C C++ Pike Java PHP JavaScript",

   pre('switch val { 
   case v1: ...; goto done;
   case v2: case v3: ...; goto done; 
 }
 ...;
 done:
 ') => "B",

   pre('switch (val) { 
   case v1: ...; break; 
   case v2: case v3: ...; break; 
   default: ...; break;
 }') . ' ("break"s are mandatory, even for "default"!)' => "C#",

   pre('switch (val) { 
   match v1 { ... } 
   match v2 { ... } 
   match _ { ... }
}') => "E",

   pre('switchon val  
   case v1: ...
   case v2: ...
   default: ...') => "BCPL",

    pre("switch val
  case v1
    ...
  case v2 v3
    ...
  case '*'
    ...
end") => "FishShell",

    pre('switch val
  case v1
    ...
  case {v2,v3}
    ...
  otherwise
    ...
end') => "Matlab",

   pre('case val of
   v1 : ...; 
   v2, v3 : ...
   else ...
 end') => "Pascal",

    pre('switch val {
    v1 {...} 
    v2 - v3 {...}
    default {...}
}') => 'Tcl',

   pre('case val in
   v1) statement1 ;;
   v2|v3) statement23 ;;
   *) statement_else ;;
esac') => "BourneShell",

   pre('(if val
    // v1 then ...
    // v2 then ... 
    else ...
    if)') => "Beta",

   pre('match val with
 | v1 -> ...
 | v2 | v3 -> ...
 | _ -> ...') => "OCaml F#",

   pre('case val of
   v1 => ...
 | v2 => ...
 | _ => ...') => "SML",

   pre('CASE val OF
   v1 => ...
 | v2 => ...
 ELSE => ... END') => "Modula-3",

   pre('case val of
   v1 -> ...
   v2 -> ...
   _ -> ...') => "Haskell",

   pre('val.
   v1 -> ...
   v2 -> ...
   _ -> ...') => "merd",

   pre('(case val
   ((v1) ...)
   ((v2) ...)
   (otherwise ...))') => "CommonLisp",

   pre('(case val
   ((v1) ...)
   ((v2) ...)
   (else ...))') => "Scheme",
    
   pre('case val is
   when v1 => ...
   when v2 | v3 => ...
   when others => ...
 end case;') => "Ada",

   pre('case val
   when v1; ...
   when v2, v3; ...
   else ...
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
(X = v1, ... ; X = v2, ... ; ...)') => "Prolog Mercury",

   pre('my %case = (
    v1 => sub { ... },
    v2 => sub { ... },
); 
if ($case{val}) { $case{val}->() } else { ... }') => "Perl",

   pre('use Switch;
switch ($val) {
    case v1 { ... }
    case v2 { ... }
    else ...
})') . ' (Perl >= 5.8.0)' => "Perl",

   pre('given $val {
    when v1 { ... }
    when v2 { ... }
    default { ... }
}') => "Perl6",

   pre('Select val
    Case v1
	...
    Case v2, v3
	...
    Case Else
	...
End Select') => "VisualBasic",

  pre('switch (val) {
    v1 { ... }
    v2 { ... }
    default { ... }
  }') => "MSH",

  pre('switch val [
    v1 [...]
    v2 [...]
]

switch/default [
    v1 [...]
    v2 [...]
][...]') => "Rebol",

   'val caseOf: {[v1]->[...]. [v2]->[...]} otherwise: ...' => "Squeak",

  'Switch[val, v1, ..., v2, ..., _, ...]' => "Mathematica",

   pre('select
  when v1 ...
  when v2 | v3 ...
  otherwise ...
end') => "ClassicREXX",

   pre('CASE val
    WHEN v1 THEN ...
    WHEN v2 THEN ...
    ELSE ...
END') => "SQL92",

  ],

  'loop' => 
  [ { ALL => 1 },
    'forever loop' =>
    [ { MLANG => 'Awk B Maple C C++ C# Tcl Java Prolog E Lua Matlab Pascal JavaScript Haskell Perl Python OCaml F# FishShell Smalltalk SML Tcl Eiffel Pliant' }, # Haskell would be: loop f = f >> loop f
     'loop' => "Ruby merd PostScript Perl6",
     'loop(...)' => "Io",
     'loop ... end loop' => "Ada",
     'LOOP ... END' => "Modula-3",
     '(loop do ...)' => "CommonLisp",
     "cycle (# do ... #) " => "Beta",
     'repeat' => "Squeak",
     'forever' => "Rebol Logo",
     pre('Do
    ...
Loop') => "VisualBasic",
     pre('do forever
  ...
end') => "ClassicREXX",

   ],

   'while condition do something' =>
   [ { MLANG => 'Prolog' },
    'while (c) ...' => "Awk B C C++ C# Java E Pike Perl Ruby PHP JavaScript YCP Yorick",
    'while c ...' => "Tcl Perl6",
    'while c loop ... end loop' => "Ada",
    'while c do ...' => "BCPL SML Pascal",
    'while c do ... done' => "OCaml F#",
    'while c do ... end do' => "Maple",
    'while c do ... end' => "Lua",
    'WHILE c DO ... END' => "Modula-3",
    'while c: ...' => "Python",
    'while c; do ...; done' => "BourneShell",
    'while c; ...; end' => "FishShell",
    'while c, ..., end' => "Matlab",
    'while [c][...]' => "Rebol",
    'while c [...]' => "Logo",
    'while(c, ...)' => "Io",
    'While[c, ...]' => "Mathematica",
    'do.while [...] c' => "Logo",
    'c whileTrue: ...' => "Smalltalk",
    '(loop while c do ...)' => "CommonLisp",
    'loop (# while ::< (# do c -> value #) do ... #) ' => "Beta",
    'begin c while ... repeat' => "Forth",
    'from until not c loop ... end' => "Eiffel",
    pre(q(while c
    ...)), => "Pliant",
    pre(q(while c do
    ...)), => "F#",
    pre('Do While c 
    ...
Loop') => "VisualBasic",
    pre('do while c
  ...
end') => "ClassicREXX",
   ],

   'do something until condition' =>
   [ { MLANG => 'Python Maple Io OCaml F# SML Eiffel Matlab Pliant Smalltalk Tcl' },
    'do ... until c' => "Perl6",
    'do {...} until c' => "Perl",
    'do ... while (!c) ' => "C C++ C# Java Awk Pike JavaScript Yorick",
    'begin ... end until c' => "Ruby",
    'REPEAT ... UNTIL c' => "Modula-3", 
    'loop (# until ::< (# do c -> value #) do ... #) ' => "Beta",
    'loop ... exit when c end loop' => "Ada",
    '(loop do ... until c)' => "CommonLisp",
    '... repeatuntil c' => "BCPL",
    'repeat ... until c' => "Lua Pascal",
    'repeat ... until (c) ' => "YCP",
    'repeat, ..., c' => "Prolog",
    'until [... c]' => "Rebol",
    'until c [...]' => "Logo",
    'do.while [...] c' => "Logo",
    'While[...; c]' => "Mathematica",
    '[... . c] whileFalse' => "Smalltalk",
    pre('Do 
...
Loop Until c') => "VisualBasic",
   ],

   'for each value in a numeric range, 1 increment (see also the entries about ranges)' =>
   [ { MLANG => 'SML Eiffel Prolog' },
    'for (int i = 1; i <= 10; i++) ...' => "C C#",
    'for (i = 1; i <= 10; i++) ...' => "Awk JavaScript",
    'for ($i = 1; $i <= 10; $i++) ...' => "PHP",
    'foreach my $i (1 .. 10) { ... }' => "Perl",
    'foreach ($i in 1..10) { ... }' => "MSH",
    'for (1 .. 10) -> $i { ... }' => "Perl6",
    'for i = 1:10, ..., end' => "Matlab",
    'for i = 1, 10 do ... end' => "Lua",
    'for i := 1 to 10 do ...' => "Pascal",
    'for i = 1 to 10 do ... done' => "OCaml F#",
    'For i = 1 To 10 ... Next' => "VisualBasic",
    'for i in 1 .. 10 loop ... end loop' => "Ada",
    'for i in 1 .. 10 do ... done' => "F#",
    'for i in xrange(1, 11)' => "Python",
    'for i in (seq 10); ...; end' => "FishShell",
#    'for i in `seq 1 10`; do ...; done' => "BourneShell", seq is non standard
    'FOR I=1:1:10 ...' => "MUMPS",
    'for i from 1 to 10 do ... end do' => "Maple",
    'for [i 1 10 +1] [...]' => "Logo",
    'for {set i 1} {$i <= 10} {incr i} {...}' => "Tcl",
    '1 1 10 ... for' => "PostScript",
    '(1..10).each {|i| ... }' => "Ruby",
    '1.upto(10) {|i| ... }' => "Ruby",
    '1 to(10) foreach(...)' => "Io",
    '1 to: 10 do: [...]' => "Smalltalk",
    '(loop for i from 1 to 10 do ...)' => "CommonLisp",
    'Do[..., {i, 1, 10}] (1 is optional in this example, since min is 1 by default)' => 'Mathematica',
    pre('do i = 1 for 10
  ...
end') => "ClassicREXX",

   ],

   'for each value in a numeric range, 1 decrement' =>
   [ { MLANG => 'SML Eiffel Prolog' },
    'for X := 10 downto 1 do ...' => "Pascal",
    'for i = 10 downto 1 do ... done' => "OCaml F#",
    'for i in reverse 1 .. 10 loop ... end loop' => "Ada",
    'for i in 10 .. -1 .. 1 do ... done' => "F#",
    'for (int i = 10; i >= 1; i--) ...' => "C C#",
    'for (my $i = 10; $i >= 1; $i--) { ... }' => "Perl",
    'loop (my $i = 10; $i >= 1; $i--) { ... }' => "Perl6",
    'for (i = 1; i <= 10; i--) ...' => "Awk JavaScript",
    'for ($i = 1; $i <= 10; $i--) ...' => "PHP",
    'from i := 10 until i < 1 loop ... i := i - 1 end' => "Eiffel",
    'for i = 10:-1:1, ..., end' => "Matlab",
    'for i = 10, 1, -1 do ... end' => "Lua",
    'For i = 10 To 1 Step -1 ... Next' => "VisualBasic",
    'for i in xrange(10, 0, -1)' => "Python",
    'for i in `seq 10 -1 1`; do ...; done' => "BourneShell",
    'for i in (seq 10 -1 1); ...; end' => "FishShell",
    'for i from 10 to 1 by -1 do ... end do' => "Maple",
    'for [i 1 10 -1] [...]' => "Logo",
    'FOR I=10:-1:1 ...' => "MUMPS",
    'for {set i 10} {$i >= 1} {incr i -1} {...}' => "Tcl",
    '10 -1 1 ... for' => "PostScript",
    '1 to: 10 by: -1 do: [...]' => "Smalltalk",
    '10 to(1) foreach(...)' => "Io",
    '10.downto(1) {|i| ... }' => "Ruby",
    '(loop for i from 1 to 10 by -1 do ...)' => "CommonLisp",
    'Do[..., {i, 10, 1, -1}]' => 'Mathematica',
    pre('do i = 10 to 1 by -1
  ...
end') => "ClassicREXX",
   ],

   'for each value in a numeric range, free increment' =>
   [ { MLANG => 'SML OCaml Eiffel Prolog' },
    'for (int i = 1; i <= 10; i += 2) ...' => "C C# Pike",
    'for (i = 1; i <= 10; i += 2) ...' => "Awk JavaScript",
    'for ($i = 1; $i <= 10; $i += 2) ...' => "PHP",
    'for (my $i = 1; $i <= 10; $i += 2) { ... }' => "Perl",
    'loop (my $i = 1; $i <= 10; $i += 2) { ... }' => "Perl6",
    'from i := 1 until i > 10 loop ... i := i + 2 end' => "Eiffel",
    'for i = 1:3:10, ..., end' => "Matlab",
    'for i = 1, 10, 2 do ... end' => "Lua",
    'For i = 1 To 10 Step 2 ... Next' => "VisualBasic",
    'for i in 1 .. 2 .. 10 do ... done' => "F#",
    'for i in xrange(1, 11, 2)' => "Python",
#    'for ((x = 1; x <= 10; x += 2)); do ...; done' => "BourneShell",  it must be bash
    'for i in (seq 1 2 10); ...; end' => "FishShell",
    'for i from 1 to 10 by 2 do ... end do' => "Maple",
    'for [i 1 10 2] [...]' => "Logo",
    'FOR I=1:2:10 ...' => "MUMPS",
    'for {set i 0} {$i <= 10} {incr i 2} {...}' => "Tcl",
    '1 2 10 ... for' => "PostScript",
    '1 to: 10 by: 2 do: [...]' => "Smalltalk",
    '(1..10).step(2) {|i| ... }' => "Ruby",
    '1 to (9,2) foreach(...)' => "Io",
    '(loop for i from 1 to 10 by 2 do ...)' => "CommonLisp",
    'Do[..., {i, 1, 10, 2}]' => 'Mathematica',
    pre('do i = 1 to 10 by 2
  ...
end') => "ClassicREXX",
   ],

   'for "a la C" (while + initialisation)' =>
   [ { MLANG => 'Ada B Pascal Maple Python Prolog Rebol Matlab OCaml F# Smalltalk SML CommonLisp Ruby Pliant ClassicREXX FishShell' },
    'for' => "Awk C C++ C# Java Perl Pike Tcl Mathematica PHP MSH JavaScript Yorick",
    'loop' => "Perl6",
    'for ((x = 0; x < 10; x++)); do ...; done' => "BourneShell",
    'from init_code until c loop ... incr_statement end' => "Eiffel",
     # 'for var in range loop exp end loop' => "Ada",
   ],

  ],

  'breaking control flow' =>
  [ { ALL => 1, MLANG => "OCaml F# SML Beta Prolog PostScript Eiffel Oz", },
   'returning a value' => $::return_a_value,

   'goto (unconditional jump)' =>
   [ { MLANG => 'Awk Tcl Prolog Ruby Maple merd Beta E Matlab Python Lua PostScript Smalltalk Java Pliant JavaScript FishShell' },
    'goto' => "BCPL B C C++ C# Perl Basic Logo MUMPS Pascal Fortran Cobol Ada Yorick",
    'Goto' => "Mathematica",
    'go throw' => "CommonLisp",
    'signal' => "ClassicREXX",
   ],

   'continue / break' =>
   [ { MLANG => 'B Prolog Smalltalk' },
    'continue / break' => "Awk C Tcl Io C++ C# Java E Matlab Pike JavaScript Python PHP YCP Yorick FishShell",
    'Continue / Break' => "Mathematica",,
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
    'redo / retry' => "Ruby Io",
   ],

  ],

  'exception' =>
  [ { ALL => 1, MLANG => 'Awk B C XSLT Pascal FishShell' },
   'throwing' =>
   [ { MLANG => 'Scheme' },
    'raise' => "Python SML OCaml F# Scheme-SRFI34 merd Ruby Eiffel Ada",
    'RAISE' => "Modula-3",
    'raise ... end' => "Oz",
    'Exception raise' => "Io",
    'throw' => "C# C++ Java Prolog Pike Logo E Erlang Rebol JavaScript Haskell",
    'Throw' => "Mathematica",
    'throw/name' => "Rebol",
    'throw new exception' => "PHP5",
    'die' => "Perl Perl6",
    'return -code' => "Tcl",
    'error' => "EmacsLisp Maple CommonLisp Dylan Matlab Lua Pliant Lua Yorick",
    'signal' => "CommonLisp Dylan Smalltalk",
    'signal predefined_condition_name' => "ClassicREXX",
    'cerror warn' => "CommonLisp",
    '[NSException raise:name ...]' => "Objective-C",
   ],

   'catching' =>
   [ { MLANG => 'Scheme' },
    'try: a except exn: ...' => "Python",
    'try a with exn -> ...' => "OCaml F#",
    'try a catch (exn) ...' => "C++ C# Java JavaScript PHP5",
    'try a catch exn then ... end' => "Oz",
    'try a catch exn: ... end try' => "Maple",
    'try(a) ; catch(...)' => "Io",
    'try { a CATCH exn { ... } }' => "Perl6",
    'TRY a EXCEPT exn => ... END' => "Modula-3",
    'a handle exn => ...' => "SML",
    'a on: exception_name do: [:exn | ...]' => "Smalltalk",
    'ifCurtailed' => "Smalltalk",
    'rescue' => "Eiffel Ruby",
    'eval {a}; if ($@) ...' => "Perl",
    'exception when exception_name =>' => "Ada",
    'catch a (\exn -> ...) ' => "Haskell",
    'catch' => "Erlang Prolog Logo Rebol Tcl",
    'Catch' => "Mathematica",
    'catch/name' => "Rebol",
    'catch(...) or catch { ... };' => "Pike",
     'if (catch(exn)) { ... } a' => "Yorick",
    'pcall' => "Lua",
    'with-exception-handler or guard' => "Scheme-SRFI34",
    'block a exception(exn) ... end' => "Dylan",
    '?, shy, safe' => "Pliant",
    'handler-bind handler-case ignore-errors' => "CommonLisp",
    'NS_DURING a NS_HANDLER ... NS_ENDHANDLER' => "Objective-C",
     pre('try
  a
catch
  ...
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
    'finally' => "C# F# Java Maple Python",
    'FINALLY' => "Modula-3",
    'unwind-protect' => "EmacsLisp CommonLisp",
    'cleanup' => "Dylan",
    'dynamic-wind' => "Scheme",
   ],

   'retrying: after catching an exception, tell the snippet to be re-run' =>
   [ { KIND => 'rare' },
    'retry' => "Eiffel Smalltalk Ruby",
    'restart' => "Dylan",
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
   'type n = t' => "OCaml F# Haskell SML Pascal",
   'TYPE n = t' => 'Modula-3',
   'using n = ...' => "C#",
   'data n = t' => "Haskell",
   'datatype n = t' => "SML",
   'newtype n = t' => "Haskell",
   'n = t' => "merd Python",
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
   ':' => "SML Pascal OCaml F# Eiffel E Ada Modula-3",
   '::' => "Haskell Mercury Dylan",
   '!!' => "merd",
   't v' => "C C++ C# Java Pike Pliant Perl6 YCP",
   '(declare (t v))' => "CommonLisp",
   'v :@ t' => "Beta",
   'typeset' => "BourneShell",
   '_t (matches an expression of type t)' => "Mathematica",
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
     'e : t'=> "F#",
     'upcast e' => "F#",
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
     'e :?> t' => "F#",
     'downcast e (type is infered)' => "F#",
     'v ?= e (expression "e" is cast to the type of "v")' => "Eiffel",
     'NARROW(e, t)' => "Modula-3",
     'typecast(e,t)' => "Matlab",
    ],

    'computed conversion (calls an internal or a user-defined function)' =>
    [     
     '(t) e' => "C++ Pike",
     '[t] e' => "MSH",
     't(e)' => "Python C++ Matlab",
     't e' => "F#",
     'e : t' => "E",
     'e :: t' => "Haskell", #- eg: 11111111111111111111 :: Int   is allowed even if overflow
     'cast e t' => "Pliant",
     '... cast t' => "Pliant",
     'make t e / to t e' => "Rebol",
    ],
  ],

  'mutability, constness' =>
  [ { ALL => 1, MLANG => "Smalltalk Eiffel" },
   'type of a mutable value' =>
   [
    'mutability is the default' => "Ada C C++ Java Matlab C#",
    'val x: T' => "Pascal",
    'T ref' => "SML OCaml F#",
    'STRef a T' => "Haskell",
   ],

   'type of a constant value' =>
   [ { MLANG => "C C# Matlab" },
    'const T' => "C99 C++",
    'constant T' => "Ada",
    'const x: T' => "Pascal",
    'constness is the default' => "SML OCaml F# Haskell",
   ],

   'special cases' =>
   [ { KIND => 'rare' },
    '"readonly" fields (quite bad: only the reference is non-mutable whereas the object is still mutable)' => "C#",
    '"final" fields, parameters, local variables (quite bad: only the reference is non-mutable whereas the object is still mutable)' => "Java",
   ],
  ],
],

'Object Oriented & Reflexivity' => [
  { KIND => 'OO', MLANG => 'Tcl Mathematica', },
  'method invocation' =>
  [ { ALL => 1 },
    '' =>
    [
     'object.method(para)' => "C++ C# Java E MSH Python Perl6 Beta Cecil Delphi-Kylix Eiffel Sather Modula-3 Ruby VisualBasic Icon merd JavaScript",
     'object.method para' => "F#",
     'object#method para' => "OCaml",
     'object:method(para)' => "Lua",
     'object method(para)' => "Io",
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
     'object.method' => "merd Ruby Eiffel F# Perl6",
     'object.property (properties are something alike attributes, but really are methods)' => "C#",
     'object.method()' => "C# C++ Java E Python JavaScript",
     'object#method' => "OCaml",
     'object:method' => "Pliant",
     'object->method' => 'Perl',
     'object->method()' => "Pike",
     'object/method' => "Rebol",
     'object["method"]()' => "Pike",
     'object method' => "Smalltalk Io",
     '[ object method ]' => "Objective-C",
     'method object' => "Haskell Mercury",
     '(method object)' => "CommonLisp",
     'method(object)' => "Dylan Ada Matlab",
     '(send object method)' => "MzScheme",
    ],
  ],

  'object creation' =>
  [ { MLANG => 'Io' },
   'new' => "Ada Simula PHP Pliant",
   'new class_name(...)' => "C++ C# F# Java Perl JavaScript VisualBasic PHP",
   'new class_name ...' => "OCaml",
   'class_name.new(...)' => "Ruby Perl6",
   'class_name new' => "Smalltalk",
   'class_name(...)' => "Pike Python F# Matlab",
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
  [ { MLANG => 'E CommonLisp' },
   'o.clone' => "Perl6",
   'o.clone (one level depth)' => "Ruby Eiffel",
   'o.deep_clone' => "Eiffel",
   'o.clone()' => "Java",
   'o.Clone()' => "C#",
   'clone $o' => "PHP5",
   'o clone' => "Io",
   'clone / copy or deepCopy' => "Smalltalk",
   'dclone' => "Perl",
   '[o copy]' => "Objective-C",
   'copy.copy(o) (general deep copy function)' => "Python",
   'purecopy' => "EmacsLisp",
   '{< >}  or  Oo.copy o' => "OCaml",
   'o2 = o (object cloning is the default, uses the copy constructor in C++)' => "C++ PHP Matlab",
   '$o2 = $o' => "PHP",
   'o2.all := o.all' => "Ada",
   'make o []' => "Rebol",
  ],

  "manually call an object's destructor" =>
  [ { MLANG => 'OCaml Tcl Io Eiffel E CommonLisp Smalltalk Matlab Ruby Java' },
   'delete' => "C++ JavaScript",
   'destroy' => "Pike",
   'DESTROY' => "Perl",
   'dealloc' => "Objective-C",
   'Dispose' => "C# F#",
   'del, __del__' => "Python",
   'Requires instantiation of Ada.Unchecked_Deallocation' => "Ada",
  ],

  'class declaration' =>
  [ { MLANG => "Perl Io E" },
   'class' => "C# C++ Java Perl6 Matlab Pike Ruby Python OCaml Haskell PHP MzScheme",
   'class c inherit p1 p2 ... feature decl decl ... end' => "Eiffel",
   'defclass defstruct' => "CommonLisp",
   'subclass' => 'Smalltalk',
   'type' => "Pliant",
   'type c is tagged record ... end record' => 'Ada',
   '@interface c { ... } ... @end' => "Objective-C",
   ':' => "Beta",
   'type c() = class ... end' => "F#",
   pre('type c() =
  ...') => "F#",

  ],

  'testing class membership' =>
  [ { MLANG => 'OCaml Io' },
   'isa' => "Perl Matlab",
   'is_a? kind_of?' => "Ruby",
   'o.meta.isa' => "Perl6",
   'isKindOf (see also isMemberOf)' => "Smalltalk",
   'isKindOfClass' => "Objective-C",
   'dynamic_cast' => "C++",
   'instanceof' => "Java JavaScript",
   'isinstance' => "Python",
   'in' => "Ada",
   'is' => "C#",
   'is_a' => "PHP",
   ':?' => "F#",
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
    'GetType' => "F#",
    'typeid' => "C++",
    'typeof' => "C# JavaScript",
    'type-of' => "CommonLisp",
    'type' => "Io",
    'ref' => "Perl",
    'generator' => "Eiffel",
    'meta' => "Perl6",
    'object_program' => "Pike",
    'getAllegedType' => "E",
  ],

  'methods available' =>
  [ { KIND => 'reflexive', MLANG => "Perl CommonLisp Pliant" },
   'methods' => "Ruby Matlab",
   'get_class_methods' => "PHP",
   'getMethods' => "Java",
   'get-member' => "MSH",
   'indices' => "Pike",
   'o.meta.getmethods' => "Perl6",
   'dir' => "Python",
   'slotNames' => "Io",
   'o.GetType().GetMethods()' => "F#",
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
   pre('type child =
  inherit parent
  ...') => "F#",

   '(defclass child (parent) ...)' => "CommonLisp",
   '@interface child : parent { ... } ... @end' => "Objective-C",
   '@ISA = qw(parent1 parent2)' => "Perl",
   'clone , setProtos, setProto, prependProto, appendProto' => "Io",
   'instance Parent Child where ...' => "Haskell",
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
    'hasSlot' => "Io",
    'try obj.GetType().GetMethod("meth") with ...' => "F#",
  ],

  'current instance' =>
  [ { MLANG => 'OCaml' }, # OCaml can access current instance by binding it to a name
   'this' => "C++ C# Java JavaScript Pike PHP Beta",
   'THIS' => "Simula",
   'self' => "Smalltalk Io Rebol Ruby Objective-C",
   'object_name if defined as: def object_name { ... }' => "E",
   'Current' => 'Eiffel',
   'first parameter (in Python, usually called self)' => "Perl Python Matlab Pliant",
   'the object variable' => "F#",
   'dispatching parameter' => "Ada CommonLisp",
   'Me' => "VisualBasic",
   '.' => "Perl6",
  ],

  'accessing parent method' =>
  [ { MLANG => 'Python C++ OCaml' },
   'super' => "Java E Smalltalk Ruby Objective-C",
   'super(Class, self).meth(args)' => "Python",    
   'base' => "C#",
   'resend' => "Io",
   'Precursor' => "Eiffel",
   '$o.SUPER::method(...)' => "Perl6",
   '$o->SUPER::method(...)' => "Perl",
   'method(parent(dispatching-parameter))' => "Ada",
   'call-next-method' => "CommonLisp",
    pre('type foo2 =
  inherit foo as parent
  ...
  member ... = ... parent.meth') => "F#",
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
   '.' => "C# Java F# Python Ruby Modula-3 SML OCaml Pascal E Ada Haskell Squeak",
   ':' => "XML",
   '::' => "C++ Ruby Perl merd Tcl YCP Squeak",
   ': :: (":" is for external symbols only, recommended)' => "CommonLisp",
   ':-' => "Maple",
   "'" => "Perl",
   '`' => "Mathematica",
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
    'namespace p ...' => "F#",
    'module p where ...' => "Haskell",
    'module P ... end' => "Ruby",
    'module P = struct ... end' => "OCaml",
    '{ module "p"; ... }' => "YCP",
    ':- module(p)' => "Prolog",
    'p = module() ... end module' => "Maple",
    "(defpackage p ...)" => "CommonLisp",
    'automatically done based on the file name' => "Python OCaml Tcl8.5",
    'package provide p 1.0' => "Tcl",
    'package declare (directory name is package name)' => "Matlab",
    'Begin["p`"] ... End[]' => "Mathematica",
    'BeginPackage["p`"] ... EndPackage[]' => "Mathematica",
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
    'namespace export name1' => "Tcl",
    pre('namespace p
  val name1 : type1
  ...') => "F#",
    'append_features' => "Ruby",
    pre(q(module type PType = sig val name1 : type1 ... end
module P : PType  = struct ... end)) => "OCaml",
    'all files in package directory are exported. files in /private sub-directory are not exported, but can be used by the package itself' => "Matlab",
    '__all__ = [ ... ]' => "Python",
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
    'open p' => "OCaml F#",
    'import' => "Pike",
    'import p' => "Haskell",
    'IMPORT p;' => "Modula-2",
    'import p.*' => "Java",
    'import "p"' => "YCP",
    'from p import *' => "Python",
    'with p; use p;' => "Ada",
    'namespace import p *' => "Tcl",
    'inherit c export {NONE} all end' => "Eiffel",
    'include or even extend' => "Ruby",
    'do' => "Rebol",
    'addpath' => "Matlab",
    '. p' => "BourneShell",
    'source p' => "BourneShell",
    'builtin -f /path/to/lib.so' => 'KornShell',
    '<< p`' => "Mathematica",
    'Get["p`"]' => "Mathematica",
    'Needs["p`"]' => "Mathematica",
   ],

   'selectively' =>
   [
    'import p (name1, name2, ...) ' => "Haskell",
    'import p.name1; import p.name2' => "Java",
    "(import '(p:name1 p:name2))" => "CommonLisp",
    'use p qw(name1 name2 ...)' => "Perl",
    'from p import name1, name2, ...' => "Python",
    'FROM p IMPORT name1, name2, ...;' => "Modula-2",
    'namespace import p name1' => "Tcl",
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
    'package require p' => "Tcl",
    'automatically done (using a correspondance from the package name to the file name)' => "OCaml Java",
   ],
  ],
],

'Strings' => [

  'type name' =>
  [ { KIND => 'typed' },
   'char[]' => "C",
   'char const[]' => "C++",
   'string' => "C++ C# F# Maple SML OCaml Pike Pascal YCP",
   'string!' => "Rebol",
   'String' => "C# merd Ruby Haskell Java JavaScript VisualBasic Smalltalk Ada",
   'STRING' => "Eiffel",
   'str' => "Python YAML",
   'Str' => "Perl6 Pliant",
   'NSString *' => "Objective-C",
   'CHAR, VARCHAR(size)' => 'SQL92',
   'Sequence' => "Io",
  ],

  'character type name' =>
  [ { KIND => 'typed', MLANG => "Maple Pike Io Pliant VisualBasic" },
   'char' => "C C++ C# F# OCaml SML",
   'char!' => "Rebol",
   'Char' => "merd Haskell Perl6",
   'Character' => "Smalltalk Ada",
   'CHARACTER' => "Eiffel",
  ],

  'character "z"' =>
  [ { MLANG => "Awk Tcl Lua Perl Perl6 Python Io JavaScript Prolog" },
   "'z'" => "B C C++ C# F# E OCaml Matlab Haskell Pike Pascal Eiffel Ada ClassicREXX",
   '"z"' => "Maple merd BourneShell ClassicREXX",
   '"z' => "Logo",
   '$z' => "Smalltalk",
   '#\z' => "Scheme CommonLisp",
   '#"z"' => "Rebol SML",
   '&z' => "Oz",
   '?z' => "Ruby EmacsLisp",
  ],

  'strings' =>
  [ { ALL => 1 },
   'with no interpolation of variables' =>
   [
    "'...'" => "Perl Perl6 CSS XPath YAML Python Matlab Ruby PHP Lua JavaScript Pascal Smalltalk BourneShell FishShell Beta Prolog ClassicREXX SQL92",
    '"..."' => "C C++ C# F# CSS Io Maple Mathematica XPath Java YCP YAML E Prolog Rebol Pike Python MUMPS EmacsLisp Scheme CommonLisp OCaml Ada Haskell SML Eiffel JavaScript Dylan Lua Awk Modula-3 Pliant FL Oz ClassicREXX",
    '"...' => "Logo",
    q('''...''') => "Python",
    q("""...""") => "Python Io",
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

   'with interpolation of variables' =>
   [ { MLANG => "SML Haskell CommonLisp Beta Maple Logo Prolog C C# ClassicREXX Eiffel" },
    '... (no spaces)' => "Tcl",
    '"... $v ..."' => "Perl Perl6 Tcl PHP BourneShell FishShell",
    '"... {v} ..."' => "merd",
    '"... #{v} ..." "... #$v ..." "... #@v ..." "... #@@v ..."' => "Ruby",
    '<<MARK ... $v ... MARK' => "BourneShell Perl",
    '<<MARK ... #{v} ... MARK' => "Ruby",
    '<<<MARK ... $v ... MARK' => "PHP",
    '[ subst {...{... $v ...}...} ]' => "Tcl",
    'qq(...(... $v ...)...), qq[...], qq{...}, qq<...>, qq/.../' => "Perl Perl6",
    '%Q(...(... #{v} ...)...), %Q[...], %Q{...}, %Q<...>, %Q/.../' => "Ruby",
    'qq(...(... {v} ...)...) ' => "merd",
    '"... #{v} ..." interpolate' => "Io",
    '"... %(v)s ..." % vars()' => "Python",
   ],

   'end-of-line (without writing the real CR or LF character)' =>
   [ { MLANG => "ClassicREXX Matlab Logo" },
    '\n' => "Tcl",
    '"\n"' => "C C++ C# Perl Io Perl6 Maple Mathematica Lua Haskell OCaml Python Ruby YCP Pike Java JavaScript Yorick FishShell",
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
   'all strings allow multi-line strings' => "E Maple F# Mathematica Smalltalk Perl Io Perl6 OCaml Ruby Scheme Pascal CommonLisp EmacsLisp YCP FishShell",
   '"...", {...}' => "Tcl",
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
  [ { MLANG => 'Awk B C C++ Beta Tcl Prolog OCaml Logo SML BourneShell ClassicREXX' }, #- everything is a string in BourneShell
   'show' => "Haskell",
   'to_s, to_str, inspect, String()' => "Ruby",
   'to_string' => "merd Pliant",
   'tostring' => "Lua YCP",
   'toString' => "Java JavaScript",
   'ToString' => "C# F# Mathematica",
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
   'asString' => "Smalltalk Io",
   'printString' => "Smalltalk",
   'as(<string>, e)' => "Dylan",
   '(string) e' => "Pike",
   'convert(e,string)' => "Maple",
   "(coerce e 'string)" => "CommonLisp",
   'prin1-to-string' => "EmacsLisp",
   'to string! / to-string / to ""' => "Rebol",
   'description' => "Objective-C",
   'pr1' => "Yorick",
  ],

  'serialize (marshalling)' =>
  [ { MLANG => 'C Prolog Logo Tcl' },
   'export-clixml' => "MSH",
   'serialize' => "PHP Io",
   'Marshal.to_string' => "OCaml",
   'Marshal.dump' => "Ruby",
   'Data.Binary.encode' => "Haskell",
   'BinaryFormatter.Serialize' => "F#",
   'storeBinaryOn' => "Smalltalk",
   'pickle.dump (see also cPickle)' => "Python",
   '(with-standard-io-syntax (write obj stream))' => "CommonLisp",
  ],

  'unserialize (un-marshalling)' =>
  [ { MLANG => 'C Prolog Logo Tcl' },
   'import-clixml' => "MSH",
   'unserialize' => "PHP",
   'Marshal.from_string' => "OCaml",
   'Marshal.load' => "Ruby",
   'Data.Binary.decode' => "Haskell",
   'BinaryFormatter.Deserialize' => "F#",
   'readBinaryFrom' => "Smalltalk",
   'pickle.load' => "Python",
   '(with-standard-io-syntax (read obj stream))' => "CommonLisp",
   'doString' => "Io",
  ],

  'sprintf-like' =>
  [ { MLANG => 'B Ada Io Haskell SML Eiffel JavaScript Scheme Smalltalk ClassicREXX' },
   'sprintf' => "Awk Maple C C++ F# Pike Matlab Perl Perl6 Ruby OCaml merd PHP",
   'printf' => "Haskell",
   '%' => "Python Ruby",
   'format' => "Tcl Java Lua",
   'format (but not using the C-like %-syntax)' => "Scheme-SRFI28 CommonLisp Erlang",
   'Format' => "C# F#",
   "putFormat" => "Beta",
   'stringWithFormat' => "Objective-C",
   'expandMacrosWith (but not using the C-like %-syntax)' => "Smalltalk",
  ],

  'simple print' =>
  [
   'on strings' =>
   [
    'puts' => "C Dylan",
    'print' => "Awk Java Maple merd Basic SML PHP",
    'write' => "Pascal Pike JavaScript Yorick",
    'putStr' => "Haskell",
    'print_string' => "OCaml F#",
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
    'echo (in BourneShell, adding an end-of-line)' => "PHP BourneShell FishShell",
    'emit' => "Forth",
    "putText" => "Beta",
    'say' => "ClassicREXX",
   ],

   'on simple objects' =>
   [ { MLANG => "Beta"},
    'print' => "Perl Perl6",
    'say (adding an end-of-line)' => "Perl6",
    'puts (adding an end-of-line)' => "Tcl",
    'puts -nonewline' => "Tcl",
   ],

   'on any objects' =>
   [
    'print' => "Ruby Io Logo Lua",
    'print (adding an end-of-line)' => "Python Haskell Rebol Dylan",
    'Print' => "Mathematica",
    'print e,' => "Python",
    'println (adding an end-of-line)' => "Io",
    'prin' => "Rebol",
    'Put' => "Ada",
    'p (adding an end-of-line)' => "Ruby",
    'puts (adding an end-of-line unless already newline-terminated)' => "Ruby",
    'display' => "Scheme",
    'write' => "Prolog Io Scheme CommonLisp",
    'writeln (adding an end-of-line)' => "Io",
    'print' => "CommonLisp",
    'printOn' => "Smalltalk",
    'princ prin1' => "CommonLisp EmacsLisp",
    'print_any' => "F#",
    'WriteLine' => "C# F#",
    'nothing - just remove ";" at the end of the expression, and it will print it' => "Matlab",
    'disp' => "Matlab",
   ],

   'printf-like' =>
   [
    'printf' => "C C++ F# Haskell KornShell Maple Matlab Perl Ruby OCaml merd Awk PHP",
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
   '== ~=' => "Lua",
   '= \=' => "Prolog",
   'isEqualToString (faster than isEqual)' => "Objective-C",

   # below are deep equality/inequality entries
   '== !=' => "Awk C++ C# E Io Ruby merd Python YCP",
   '== <>' => "Python",
   '== /=' => "Haskell",
   '== \=' => "Oz",
   '= !=' => "XPath BourneShell FishShell Maple",
   '= /=' => "Ada",
   '= \=' => "ClassicREXX",
   '= <>' => "OCaml F# VisualBasic SML Beta Pliant",
   '= ~=' => "Dylan Smalltalk",
   '== \== or = <> \=' => "ClassicREXX",
   '=== =!= / == != (structural / mathematical)' => "Mathematica",
   '== ~=' => "Matlab",
   'equal?' => "Ruby Scheme",
   'equals' => "Java",
   'equal, equalp' => "CommonLisp",
   'is_equal' => "Eiffel",
   'isEqual' => "Objective-C",
  ],

  'string size' =>
  [
   'length' => "Awk C++ F# Maple Perl Prolog Ruby Haskell PostScript Matlab CommonLisp OCaml Java JavaScript Beta Eiffel Objective-C",
   'LENGTH' => "ClassicREXX",
   "'Length" => "Ada",
   'length?' => "Rebol",
   'size' => "C++ Ruby Io YCP SML Smalltalk E",
   'Length' => "C# F# Modula-3 Pascal Oz",
   'len' => "Python Pliant VisualBasic",
   'strlen' => "C PHP",
   'string length' => "Tcl",
   'string-length' => "Scheme XPath",
   'StringLength' => "Mathematica",
   'sizeof' => "Pike",
   'count' => "Eiffel Logo",
   'bytes chars' => "Perl6",
   'CHARACTER_LENGTH' => "SQL92",
   'atom_length' => "Prolog",
   'wc -c' => "FishShell",
   '#' => "Lua",
  ],

  'string concatenation' =>
  [
  '+' => "Ruby Pike Python YCP Java C++ C# F# merd Pascal E Eiffel JavaScript Pliant MSH",
  '.' => "Perl PHP",
  '..' => "Io Lua",
  ',' => "Smalltalk",
  '~' => "Perl6",
  '&' => "Ada Modula-3 VisualBasic",
  '^' => "SML OCaml F#",
  '_' => "MUMPS",
  '||' => "PL/I Cecil Icon Maple ClassicREXX SQL92",
  '++' => "Haskell",
  '$a$b' => "Tcl BourneShell FishShell",
  'concatenate' => "Dylan CommonLisp",
  'string-append' => "Scheme",
  'StringJoin' => "Mathematica",
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
 [ { MLANG => 'C C++ C# Java CommonLisp Prolog Logo VisualBasic JavaScript Haskell Eiffel Beta OCaml SML Smalltalk' }, # Haskell is "concat (replicate n str)"
  '*' => "Ruby Pike Python E Ada",
  'x' => "Perl Perl6",
  'times' => "merd",
  'repeat' => "Pliant",
  'repeated' => "Io",
  'str_repeat' => "PHP",
  'string repeat' => "Tcl",
  'strrep' => "Lua",
  'repmat' => "Matlab",
  'insert/dup' => "Rebol",
  'COPIES' => "ClassicREXX",
  'cat(s$n)' => "Maple",
  'concat $ replicate' => "Haskell",
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
  'uppercase / lowercase' => "OCaml F# Logo",
  'strupper / strlower' => "Lua",
  'ToUpper / ToLower' => "Oz C# F#",
  'toupper / tolower' => 'Awk C C++',
  'string toupper / string tolower' => "Tcl",
  'asLowercase / asUppercase' => "Smalltalk Io",
  "upCase / lowCase" => "Beta",
  'uppercase form / lowercase form' => "Rebol",
  'char-upcase / char-downcase' => "CommonLisp Scheme",
  'char_type(C_, to_upper(C)), char_type(C_, to_lower(C))' => "Prolog",
 ],

 'uppercase / lowercase / capitalized string' =>
 [ { MLANG => 'C C++ Haskell' }, # Haskell is "map toUpper / map toLower"
  'upcase / downcase' => "Ruby EmacsLisp",
  'upper / lower' => "SQL92 Matlab",
  'upper / lower / capitalize' => "Python",
  'uppercase/lowercase' => "OCaml F# Rebol Logo",
  'upcase_atom/downcase_atom' => "Prolog",
  'toUpperCase / toLowerCase' => "Java E JavaScript",
  'ToUpperCase / ToLowerCase' => "Mathematica",
  'ToUpper / ToLower' => "C# F#",
  'to_upper / to_lower' => "Eiffel Ada",
  'toupper / tolower' => 'Awk YCP',
  'uc / lc' => "Perl Perl6",
  'UpperCase / LowerCase' => "Pascal",
  'StringTools[UpperCase] / StringTools[LowerCase] / StringTools[Capitalize]' => "Maple",
  'uppercaseString / lowercaseString / capitalizedString' => "Objective-C",
  'UCase / LCase' => "VisualBasic",
  'strtoupper / strtolower' => "PHP Yorick",
  'strupper / strlower' => "Lua",
  'string toupper / string tolower / string totitle' => "Tcl",
  'string-upcase / string-downcase' => "CommonLisp Scheme",
  'asLowercase / asUppercase / asUppercaseFirst' => "Smalltalk",
  'asLowercase / asUppercase / makeFirstCharacterUppercase' => "Io",
  'upcase_atom / downcase_atom' => "Prolog",
  "makeLC / makeUC" => "Beta",
  'parse upper var in_var out_var / parse lower var in_var out_var' => "ClassicREXX",
 ],

 'ascii to character' =>
 [ { MLANG => "Eiffel" },
  'chr' => "Ruby Perl Perl6 F# Python SML OCaml Haskell Pascal PHP",
  'chr$' => "VisualBasic",
  'char' => "Matlab",
  'format %c $c' => "Tcl",
  'toChar' => "E",
  'strchar' => "Lua",
  'from_integer' => "Eiffel",
  'fromCharCode' => "JavaScript",
  'FromCharacterCode' => "Mathematica",
  'character' => "Pliant",
  'Character value: c' => "Smalltalk",
  'asCharacter' => "Io",
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
  'ord' => "Perl Perl6 F# Python Haskell SML Pascal PHP",
  'asc' => "VisualBasic",
  'getNumericValue' => "Java",
  'charCodeAt' => "JavaScript",
  'asciiValue' => "Smalltalk",
  'code' => "OCaml Eiffel",
  'char-code' => "CommonLisp",
  'char->integer' => "Scheme",
  's[0]' => "Ruby",
  's 0 get' => "PostScript",
  's at(0)' => "Io",
  'scan $s %c' => "Tcl",
  'strbyte' => "Lua",
  'toInteger' => "E",
  "'Pos" => "Ada",
  'number' => "Pliant",
  '(int) c' => "C C++ C# Java",
  'to integer! / to-integer' => "Rebol",
  'ToCharacterCode' => "Mathematica",
  'C2X, C2D' => "ClassicREXX",
  '$ASCII(s)' => "MUMPS",
  '(done automatically when applying mathematical operations on char, such as +)' => "Matlab",
  'char' => "Logo",
  'char_code' => "Prolog",
  'StringTools[Ord]' => "Maple",
 ],

 'accessing n-th character' =>
 [ { MLANG => 'Perl Perl6 ClassicREXX VisualBasic Yorick' },
  's[n]' => "Maple C C++ C# E Pike Python PHP Ruby",
  's(n)' => "Ada Matlab",
  's:n' => "Pliant",
  's.[n]' => "OCaml F#",
  's !! n' => "Haskell",
  's @ n' => "Eiffel",
  's/:n' => "Rebol",
  'string index s n' => "Tcl",
  'sub' => "SML",
  'char, aref, schar, svref' => "CommonLisp",
  'GetChar' => "Modula-3",
  's at(n)' => "Io",
  'at (in C++, is range-checked whereas a[i] is not)' => "C++ Smalltalk",
  'aref' => "CommonLisp",
  'char(s, i)' => "B",
  'charAt' => "Java JavaScript",
  'characterAtIndex' => "Objective-C",
  "n -> s.inxGet" => "Beta",
  'string-ref' => "Scheme",
  'StringTake[s, {n}]' => "Mathematica",
  '$EXTRACT(s, n)' => "MUMPS",
  'item' => "Logo",
 ],

 'extract a substring' =>
 [ { MLANG => "Haskell Logo" }, # Haskell is "take len (drop n s)"
  's[n..m]' => "Pike Maple Ruby",
  's.[n..m]' => "F#",
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
  'sub' => "OCaml F# Lua",
  'SUB' => "Modula-3",
  'subseq' => "CommonLisp",
  'slice' => "JavaScript Io",
  'mid$' => "JavaScript",
  'string range' => "Tcl",
  'StringTake[s, {n, m}]' => "Mathematica",
  'strpart(s, n, m)' => "Yorick",
  'copy/part at s n len' => "Rebol",
  'copy/part at s n at s m' => "Rebol",
  's copyFrom: n to: m' => "Smalltalk",
  "(n,m)->s.sub" => "Beta",
  '[s substringWithRange:NSMakeRange(n, len)]' => "Objective-C",
  'SUBSTRING(s FROM n len)' => "SQL92",
  '$EXTRACT(s, n, m)' => "MUMPS",
  'sub_string / sub_atom' => "Prolog",
  '(take len . drop n) s' => "Haskell"
 ],

 'locate a substring' => 
 [ { MLANG => "OCaml" }, # index in C is BSD only
  'index' => "Ruby Perl Ada Perl6 Python",
  'indexOf' => "JavaScript Java",
  'IndexOf' => "C# F#",
  'indexOfString' => "Smalltalk",
  'startOf' => "E",
  'search' => "Pike CommonLisp PostScript",
  'StringTools[Search]' => "Maple",
  'StringPosition' => "Mathematica",
  'strstr strchr' => "C",
  'find' => "Rebol YCP Logo Python Lua",
  'findSeq' => "Io",
  'strfind' => "Matlab Yorick",
  '$FIND' => "MUMPS",
  'index_non_blank / find_token' => "Ada",
  'substring_index' => "Eiffel",
  'rangeOfString' => "Objective-C",
  'POS' => "ClassicREXX",
  'POSITION(needle IN s)' => "SQL92",
  'sub_string / sub_atom' => "Prolog",
  'string first' => "Tcl",
 ],

 'locate a substring (starting at the end)' => 
 [ { MLANG => "Prolog Io" }, # rindex in C is BSD only
  'rindex' => "Ruby Perl Perl6 OCaml Python",
  'rfind' => "C++ Python",
  'find/last' => "Rebol",
  'strrchr' => "C",
  'index(Going => Backward)' => "Ada",
  'lastStartOf' => "E",
  'lastIndexOf' => "JavaScript Java",
  'last_index_of (ESI dialect)' => "Eiffel",
  'LastIndexOf' => "C# F#",
  'lastIndexOfString' => "Smalltalk",
  'string last' => "Tcl",
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
  'bool' => "C# F# C++ C99 SML OCaml Python YAML",
  'Boolean' => "Smalltalk Lua Pascal VisualBasic Ada",
  'boolean' => "Java CommonLisp YCP Maple",
  'BOOLEAN' => "Eiffel",
  'logic!' => "Rebol",
  'logical' => "Matlab",
 ],

 'false value' =>
 [
   'false' => "C99 C++ BCPL C# F# OCaml Io Logo SML Pascal YCP Smalltalk PostScript Java E Ada Beta Pliant FL Oz Tcl Maple JavaScript Rebol Matlab BourneShell PHP Ruby Lua YAML",
   'False' => "VisualBasic Python Haskell merd Eiffel Mathematica",
   'FALSE' => "Modula-3 SQL92",
   'false()' => "XPath",
   '#f' => "Dylan Scheme",

   'n' => "YAML",
   'nil' => "EmacsLisp CommonLisp Io Lua Ruby Lua",
   'no' => "Tcl YAML",
   'No' => "Prolog",
   'none' => "Rebol",
   'None' => "Python",
   'null' => "JavaScript",
   'NULL' => "C99 C++ C PHP",
   'off' => "Tcl YAML",
   'undef' => "Perl Perl6",
   'undefined' => "JavaScript",

   'fail' => "Prolog",
   'FAIL' => "Maple",
   'array containing at least one false value' => "Matlab",
   'exit status different from 0' => "BourneShell",

   '0 (beware of 0.0 which is true in Pike!)' => "B VisualBasic C99 C++ Tcl Python JavaScript ClassicREXX MUMPS Awk Perl Perl6 Matlab XPath C PHP Pike Yorick",
   '0.0' => "Matlab PHP",
   'NaN' => "JavaScript XPath",

   '""' => "Python JavaScript Awk Perl Perl6 XPath PHP",
   '"0"' => "Awk Perl PHP Perl6",
   "''" => "Matlab",
   q('\0') => "C99 C++ C",

   '()' => "Python Perl Perl6",
   '[]' => "Python Matlab",
   '{}' => "Python Matlab",
   'array()' => "PHP",
 ],

 'true value' =>
 [
  'TRUE' => "Modula-3 SQL92",
  'True' => "Haskell merd Eiffel VisualBasic Python Mathematica",
  'true' => "BCPL C# F# OCaml Io Maple SML Pascal Logo Ruby Smalltalk PostScript Java E Ada PHP Beta Pliant FL Oz YCP Tcl BourneShell YAML Prolog Rebol",
  'true()' => "XPath",
  't' => "EmacsLisp CommonLisp",
  '#t' => "Scheme Dylan",
  'y' => "YAML",
  'yes' => "Tcl YAML",
  'Yes' => "Prolog",

  'on' => "Tcl YAML",

  'exit status 0' => "BourneShell",
  'anything not false' => "Awk Perl Perl6 B C Pike Matlab Ruby MUMPS XPath EmacsLisp CommonLisp Python Scheme Dylan Rebol Yorick",

  '1' => "ClassicREXX MUMPS",
  'non zero number' => "Tcl",
  'non-zero-numbers' => "VisualBasic",
 ],

 'logical not' =>
 [
  '!' => "Awk B C C++ C# E Java Pike Perl Perl6 Ruby YCP Tcl PHP Mathematica JavaScript Yorick",
  'not' => "OCaml F# Logo SML Rebol Io Maple Pascal PostScript Ruby Scheme Haskell Perl Perl6 XPath Python Smalltalk merd Eiffel Lua EmacsLisp CommonLisp Ada Beta Pliant Forth Prolog",
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
   '|| / &&' => "Awk C C++ C# F# Java Matlab Mathematica Pike Perl Perl6 YCP Ruby OCaml Haskell merd Tcl E PHP JavaScript Yorick",
   '| / &' => "B BCPL Dylan",
   'or / and' => "Perl Perl6 Logo Io Ruby Python Modula-2 PHP Smalltalk EmacsLisp CommonLisp Scheme Lua Pliant",
   'OR / AND' => "Modula-3",
   'or / & (in Modula-2, "&" and "and" are synonyms)' => "Modula-2",
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
   'a list' => "SML F# OCaml",
   '[a]' => 'Haskell',
   'a[]' => "C#",
   'list' => "Python Maple",
   'List' => "Pliant Io Mathematica",
   'Array or List' => "Perl6",
   'ARRAY or LINKED_LIST' => "Eiffel",
   'Array or OrderedCollection' => "Smalltalk",
   'cell' => "Matlab",
 ],

 'list concatenation' =>
 [ { MLANG => 'C++' }, # C++ has the typical OO disease, it does everything in-place
  '+' => "Ruby Eiffel Pike Python merd E",
  ',' => "Smalltalk Maple Matlab Perl",
  '@' => "SML OCaml F#",
  '++' => "Haskell",
  '|||' => "Icon",
  'array_merge' => "PHP",
  'merge' => "YCP",
  'concat' => "Tcl JavaScript",
  'concatenate' => "Dylan",
  'nconc' => "EmacsLisp CommonLisp",
  "append" => "Beta Rebol Prolog Scheme EmacsLisp CommonLisp",
  'Append' => "Oz",
  'appendSeq' => "Io",
  'arrayByAddingObjectsFromArray' => "Objective-C",
  'sentence' => "Logo",
  'Join' => "Mathematica",
 ],

 'list flattening' =>
 [ { MLANG => 'Perl Matlab Logo Python C++ Smalltalk JavaScript' },
  'one level depth' =>
  [
   'concat' => "F# Haskell Mercury SML",
   'flatten' => "F# OCaml merd YCP Io Prolog",
   'Flatten' => "Oz",
   'eval concat' => "Tcl",
   'ListTools[FlattenOnce]' => "Maple",
   '{*}$l' => "Tcl8.5",
   '"$l"' => "FishShell",
  ],

  'recursive' =>
  [
   'flatten' => "Pike Ruby",
   'ListTools[Flatten]' => "Maple",
   'Flatten' => "Mathematica",
  ],
 ],

 'list constructor' =>
 [ { MLANG => "Beta C++" },
  '[ a, b, c ]' => "Haskell Maple Ruby Python Matlab YCP JavaScript SML YAML Perl Perl6 Prolog merd PostScript E",
  '( a, b, c )' => "Perl Perl6",
  '{ a, b, c } (restricted to initialisation of a local variable in C and C++)' => "Lua C C++ Mathematica",
  "#(a, b, c)" => "Dylan",
  '#(a b c) (a b c must be constants)' => "Smalltalk",
  '{ a. b. c }' => "Squeak",
  '[ a ; b ; c ]' => "F# OCaml",
  '[ a b c ]' => "Rebol Oz Logo",
  '({ a, b, c })' => "Pike",
  "'(a b c)" => "EmacsLisp CommonLisp Scheme",
  '<< a, b, c >>' => "Eiffel",
  'list(a, b, c)' => "Io",
  'list' => "Tcl Dylan EmacsLisp CommonLisp Scheme",
  'new t[] { a, b, c }' => 'C#',
  'new[] { a, b, c }' => 'C#3',
  'new List<t> { a, b, c}' => "C#3",
  'Array(a, b, c) (beware, if you give only one integer argument, it is the size!)' => "JavaScript",
  '[NSArray arrayWithObjects:a, b, c, nil]' => "Objective-C",
  'set l a b c' => "FishShell",
  pre('  - a
  - b
  - c') => "YAML",
 ],

 'list/array indexing' =>
 [ { MLANG => "Beta" }, # eiffeil favors a.put(v, i) for write access
  'a[i]' => "B C C++ C# Java Pike BourneShell KornShell FishShell Maple Ruby Python merd Pascal E PHP Perl Perl6 Dylan Lua JavaScript Modula-3 MSH",
  'a*[i] or a!i or a*(i) depending on the version' => "BCPL",
  'a[[i]]' => "Mathematica",
  'a[i]:default' => "YCP",
  'a(i)' => "Ada Matlab",
  'a:i' => "Pliant",
  'a/:i' => "Rebol",
  'a.(i)' => "F# OCaml",
  'a.[i]' => "F#",
  'a !! i' => "Haskell Mercury",
  'a @ i' => "Eiffel",
  'a i get (for write access: a i o put)' => "PostScript",
  'a at(i)' => "Io",
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
 [ { MLANG => 'Python Objective-C Tcl FishShell' },
  'return the new list (no side-effect)' =>
  [
   ':' => "Haskell merd",
   '::' => "SML OCaml F#",
   '|' => "Oz",
   '[ e | l ]' => "Prolog Erlang",
   '[e l[]]' => "Maple",
   '[e l]' => "Matlab",
   'cons' => "EmacsLisp CommonLisp Scheme",
   'pair' => "Dylan",
   'fput' => "Logo",
   'Prepend' => "Mathematica",
  ],

  'side-effect' =>
  [ { MLANG => "Beta Maple" }, # Beta: (obj, list.head) -> list.insertBefore
   'unshift' => "Perl Perl6 Ruby JavaScript",
   'prepend' => "YCP",
   'push_front' => "C++",
   'addFirst' => "Smalltalk",
   'insert' => "Rebol",
   'put_first' => "Eiffel",
   'push' => "CommonLisp Io",
   'array_unshift' => "PHP",
   'PrependTo' => "Mathematica",
  ],
 ],

 'adding an element at index' =>
 [ { MLANG => 'Eiffel OCaml Logo Prolog Matlab Maple' },
  'return the new list (no side-effect)' =>
  [
   'linsert l i e' => "Tcl",
   'Insert' => "Mathematica",
  ],

  'side-effect' =>
  [ {},
   '[a insertObject:e atIndex:i]' => "Objective-C",
   'a.insert(i, e)' => "Ruby Python",
   'a insertAt(e, i)' => "Io",
   'a add: e beforeIndex: i / a add: e afterIndex: i' => "Smalltalk",
  ],
 ],

 'adding an element at the end' =>
 [ { MLANG => 'CommonLisp Tcl Haskell Beta Prolog Maple OCaml SML FishShell' },
  'return the new list (no side-effect)' =>
  [
   'push' => "merd",
   'arrayByAddingObject' => "Objective-C",
   'lput' => "Logo",
   'linsert l end e' => "Tcl",
   'Append' => "Mathematica",
  ],

  'side-effect' =>
  [
   'push' => "Perl Perl6 Ruby JavaScript",
   'push_back' => "C++",
   'append' => "Python Pliant Rebol Io",
   'AppendTo' => "Mathematica",
   'lappend' => "Tcl",
   '+=' => "Pliant",
   'add' => "Java Smalltalk YCP",
   'put_last' => "Eiffel",
   'array_push' => "PHP",
   'addObject' => "Objective-C",
  ],
 ],

 'first element' =>
 [ { MLANG => "Beta Tcl Perl Python Ruby JavaScript Maple Matlab FishShell" },
  '' =>
  [
   'head' => "Haskell F#",
   'Head' => "F#",
   'hd' => "OCaml",
   'car' => "Scheme EmacsLisp CommonLisp",
   'first' => "Eiffel Pliant Rebol Smalltalk Io Logo",
   'First (see also Head)' => "Mathematica",
  ],

  'iterator' =>
  [ 
   'head' => "Beta",
   'begin' => "C++",
  ],
 ],

 'all but the first element' =>
 [ { MLANG => 'Perl C++ Ruby JavaScript Maple Io Eiffel' },
  'tail' => "F# Haskell",
  'Tail' => "F#",
  'tl' => "OCaml",
  'cdr' => "Scheme EmacsLisp CommonLisp",
  'Rest' => "Mathematica",
  'butfirst' => "Logo",
  'allButFirst' => "Smalltalk",
  'l[1:]' => 'Python',
  'a(2:end)' => "Matlab",
  'L = [_|ButFirst]' => "Prolog",
  'lrange l 1 end' => "Tcl",
 ],

 'last element' =>
 [ { MLANG => 'C++ OCaml JavaScript Maple' },
  '' =>
  [
   'last' => "Eiffel Rebol Io Logo Haskell Prolog Pliant Smalltalk E Scheme",
   'Last' => "Oz Mathematica",
   'lastObject' => "Objective-C",
   'a[-1]' => "Python Perl Pike Ruby",
   'a(end)' => "Matlab",
   'node[last()]' => "XPath",
   '(car (last l))' => "CommonLisp EmacsLisp",
   'lindex l end' => "Tcl",
  ],

  'iterator' =>
  [
  ],
 ],

 'all but the last element' =>
 [ { KIND => 'rare', MLANG => 'Perl C++ Ruby JavaScript Maple Io Eiffel' },
  'Most' => "Mathematica",
 ],

 'get the first element and remove it' =>
 [ { MLANG => 'Haskell Matlab Tcl Python C++ OCaml Maple Eiffel' },
  'shift' => "Perl Perl6 Ruby JavaScript",
  'shift!' => "merd",
  'pop' => "CommonLisp Logo",
  'removeFirst' => "Smalltalk Io",
  'array_shift' => "PHP",
 ],

 'get the last element and remove it' =>
 [ { MLANG => 'Haskell C++ OCaml Tcl Maple Eiffel Matlab' },
  'pop' => "E Perl Perl6 Ruby Python Io JavaScript",
  'pop!' => "merd",
  'array_pop' => "PHP",
  'removeLast' => "Smalltalk Io",
  'dequeue' => 'Logo',
 ],

 'for each element do something' =>
 [ { MLANG => "XSLT Matlab" }, # could be KIND has_lambda, but many languages has special cases for this
  'each' => "Ruby merd Pliant",
  'for v in l ...' => "Ruby E Maple",
  'for v in l: ...' => "Python",
  'for v in l; do ...; done' => "BourneShell",
  'for v in l do ...' => "F#",
  'for v in l; ...; end' => "FishShell",
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
  'l foreach(v, ...)' => "Io",
  'for_each' => "C++",
  'for-each' => "Scheme",
  'forall' => "PostScript Prolog",
  'ForAll' => "Oz",
  'iter' => "OCaml F#",
  'do' => "Smalltalk",
  'do_all' => "Eiffel",
  'app' => "SML",
  'mapc' => "EmacsLisp",
  'mapM_' => "Haskell",
  'Scan' => "Mathematica",
  '(dolist (v l) ...)  (loop for v in l do ...)  mapc' => "CommonLisp",
  "list.iterate (# do current ... #) " => "Beta",
  'l.Iterate(...)' => "F#",
 ],

 'transform a list (or bag) in another one' =>
 [ { KIND => 'has_lambda' },
  'map' => "Perl F# Pike Io Maple Ruby SML OCaml Haskell Mercury Scheme Python merd Dylan",
  'Map' => "Oz F# Mathematica",
  'mapcar' => "EmacsLisp CommonLisp",
  'maplist' => "YCP Prolog",
  'sublist' => "Prolog",
  'map / map.se' => "Logo",
  'for-each' => "XSLT",
  'foreach or selected' => "MSH",
  'collect' => "Ruby Smalltalk",
  'transform' => "C++",
  'array_map' => "PHP",
  '/@' => 'Mathematica',
  '[ f x | x <- l ] (list comprehension)' => "Haskell",
  '[ f(x) for x in l ] (list comprehension)' => "Python",
  'magical: sin(x) computes sin on each element' => "Matlab",
 ],

 'transform two lists in parallel' =>
 [ { KIND => 'has_lambda', MLANG => "Perl Ruby" },
  'map2' => "OCaml F#",
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
 [ { KIND => 'has_lambda', MLANG => "Perl Scheme" },
  'find' => "C++ F# CommonLisp Ruby Rebol SML OCaml Haskell Scheme-SRFI1 merd YCP",
  'find_if' => "C++",
  'find-if' => "CommonLisp",
  'first (in List::Util)' => "Perl",
  'detect' => "Ruby Smalltalk",
  'search' => "Pike",
  'ListTools[Search]' => "Maple",
  'lsearch -exact' => "Tcl",
  'index' => "Python",
  'indexOf' => "Io",
  'indexOfObject, indexOfObjectIdenticalTo' => "Objective-C",
  'find(a == 3)' => "Matlab",
  'Position' => "Mathematica",
 ],

 'keep elements' =>
 [ { KIND => 'has_lambda', MLANG => 'Scheme Tcl', },
  'matching' => [
   'find_all' => "Ruby F# OCaml",
   'filter' => "Pike OCaml F# SML Haskell Mercury Scheme-SRFI1 Python merd YCP",
   'filter!' => "Scheme-SRFI1",
   'Filter' => "Oz",
   'grep' => "Perl Perl6",
   'where' => "MSH",
   'select' => "Ruby Maple Smalltalk Io",
   'Select / Case' => "Mathematica",
   'selectInPlace' => "Io",
   'remove-if-not delete-if-not' => "CommonLisp",
 #  'remove-each' => "Rebol", # unpure
   'choose' => "Dylan",
   '[ x | x <- l, p x ] (list comprehension)' => "Haskell",
   '[ x for x in l if p(x) ] (list comprehension)' => "Python",
   'a(a == 3)' => "Matlab",
  ],
  'non matching' => [
   'remove-if delete-if' => "CommonLisp",
   'reject' => "Ruby",
  ],
 ],

 'partition a list: elements matching, elements non matching' => 
 [ { KIND => 'functional', MLANG => 'Smalltalk Scheme Matlab' },
   'partition' => "OCaml F# Ruby SML Haskell Scheme-SRFI1 merd",
   'partition!' => "Scheme-SRFI1",
   'Partition' => "Oz",
 ],

 'split a list' => 
 [ { KIND => 'functional', MLANG => 'Smalltalk Scheme Matlab' },
  'in 2 based on a predicate' => [
   'break' => "Haskell",
   'span' => "Haskell",
  ],

  'into sublists delimited by elements matching a predicate' => [
   'split-sequence (not in standard)' => "CommonLisp",
   'ListTools[Split]' => "Maple",
  ],

  'into a list of lists of same value' => [
   'group' => "Haskell",
   'Split' => "Mathematica",
  ],

  'into sublists based on a predicate' => [
   'groupBy' => "Haskell",
   'Split' => "Mathematica",
  ],
 ],

 'is an element in the list' =>
 [ { MLANG => "C++ Perl Maple" },
  'member?' => "Ruby merd Dylan",
  'include?' => "Ruby",
  'mem' => "F# OCaml",
  'member' => "CommonLisp Prolog Scheme",
  'Member' => "Oz",
  'MemberQ' => "Mathematica",
  'memq memv' => "Scheme",
  'memberp / member?' => "Logo",
  'contains' => "E YCP Io",
  'containsObject' => "Objective-C",
  'in' => "Tcl8.5 Python JavaScript SQL92",
  'in_array' => "PHP",
  'includes' => "Smalltalk",
  'elem' => "Haskell Mercury",
  'has' => "Eiffel",
  'has_value' => "Pike",
  'ismember' => "Matlab",
 ],

 'is the predicate true for an element' =>
 [ { KIND => 'has_lambda', MLANG => "Perl Scheme" },
  'any (Python >= 2.5)' => "Haskell Mercury Matlab Scheme-SRFI1 Python",
  'any?' => "Ruby Dylan",
  'anySatisfy' => "Smalltalk",
  'exists' => "F# OCaml SML",
  'exists?' => "merd",
  'some' => "CommonLisp",
  'Some' => "Oz",
  'ormap' => "Maple",
  'detect' => "Io",
 ],

 'is the predicate true for every element' =>
 [ { KIND => 'has_lambda', MLANG => "Perl Io Scheme" },
  'all (Python >= 2.5)' => "Haskell SML Mercury Python",
  'All' => "Oz",
  'all?' => "Ruby merd",
  'allSatisfy' => "Smalltalk",
  'every' => "Scheme-SRFI1 CommonLisp",
  'every?' => "Dylan",
  'for_all' => "F# OCaml",
  'andmap' => "Maple",
 ],

 'smallest / biggest element' =>
 # accepting n-ary functions if a "*" tupling function exists		     
 [ { MLANG => 'OCaml Tcl Logo JavaScript' },
  'min / max' => "Eiffel Maple Perl6 Pike Io Matlab Ruby Scheme Prolog Python CommonLisp Smalltalk Java",
  'Min / Max' => "Mathematica",
  'minimum / maximum' => "Haskell Mercury merd",
  'minimum-of / maximum-of' => "Rebol",
  'min minstr / max maxstr (in List::Util)' => "Perl",
  'min_element / max_element' => "C++",
 ],

 'join a list of strings in a string using a glue string' =>
 [ { MLANG => "Eiffel Logo" },
  'join(s, l)' => "PHP Perl Perl6",
  'String.Join(s, l)' => "C#",
  's.join(l)' => "Python",
  'l.join(s)' => "Ruby JavaScript Perl6",
  'l asStringWith: s' => "Smalltalk",
  'join l s' => "Tcl",
  'implode(s, l)' => "PHP",
  'ListTools[Join]' => "Maple",
  'rjoin' => "E",
  'join' => "Io",
  'concat' => "F# OCaml",
  'strcat' => "Matlab",
  'concat_atom' => "Prolog",
  'l * s' => "Pike Ruby",
  "(mapconcat 'identity l s)" => "EmacsLisp",
  'componentsJoinedByString' => "Objective-C",
  'intercalate' => "Haskell",
  'StringJoin @@ Riffle[l, s]' => "Mathematica",
 ],

 'list size' =>
 [
  'size' => "Ruby merd E Io Matlab Dylan Java C++ YCP Pliant Smalltalk",
  'sizeof' => "Pike",
  'length' => "C# F# Ruby Matlab SML Haskell Mercury OCaml Scheme PostScript Java JavaScript EmacsLisp CommonLisp Prolog",
  'Length' => "Oz F# Mathematica",
  'length?' => "Rebol",
  'len' => "Python",
  'llength' => "Tcl",
  '$LENGTH' => "MUMPS",
  'elems' => "Perl6",
  'getn' => "Lua",
  'count' => "PHP Eiffel XPath Objective-C SQL92 FishShell",
  'numel' => "Matlab",
  'scalar @l' => "Perl",
  'nops' => "Maple",
  '#' => "Lua",
 ],

 'iterate with index' =>
 [ { MLANG => 'Haskell Tcl Maple Logo Matlab Prolog Perl C++ OCaml Smalltalk JavaScript' }, # Haskell is "zipWith (\i v -> expr) [1..] l"
  'each_with_index' => "Ruby merd",
  'enumerate(l)' => "Python",
  'foreach($l as $i => $v)' => "PHP",
  'a foreach(i, e, ...)' => "Io",
  'for i => v in l' => "E",
  'for (v in l, i from 0) ... end' => "Dylan",
  'forAllInd' => "Oz",
  'foreachi' => "Lua",
  'foreach(l; typ0 i; typ1 v) { ... }' => "Pike",
  'withIndexDo' => "Squeak",
  'iteri' => "F# OCaml",
  'IterateIndexed' => "F#",   
  'MapIndexed' => "Mathematica",
  '(loop for v in l as i upfrom 0 do ...)' => "CommonLisp",
 ],

 'remove duplicates' =>
 [ { MLANG => "Perl Python OCaml Smalltalk JavaScript Scheme" },
  'uniq' => "Ruby Perl6 Pike merd",
  'uniq!' => "Ruby",
  'uniq2' => "Pike",
  'unique (in C++, it is done in place)' => "Rebol Matlab Io C++",
  'nub' => "Haskell",
  'array_unique' => "PHP",
  'ListTools[MakeUnique]' => "Maple",
  'delete-duplicates' => "Scheme-SRFI1 CommonLisp",
  'delete-duplicates!' => "Scheme-SRFI1",
  'remove-duplicates' => "Dylan CommonLisp",
  'lsort -unique' => "Tcl",
  'toset' => "YCP",
  'distinct' => "SQL92",
  'set' => "Python",
  'Union' => "Mathematica",
 ],

 'sort' =>
 [ { MLANG => 'Logo' },
  'sort (in Scheme, not standard, but nearly standard)' => "Eiffel Io Pike F# Prolog Matlab Maple Rebol Ruby C++ C# OCaml XSLT Haskell Java JavaScript CommonLisp Python Perl Perl6 merd E PHP Lua YCP Scheme",
  'sort!' => "Ruby",
  'sorted' => "Python",
  'Sort' => "Oz Mathematica",
  'sort_by' => 'merd Ruby',
  'sortBy' => "Haskell Smalltalk Io",
  'order by' => "SQL92",
  'lsort' => "Tcl",
  'asort' => "Awk",
  'sort-object' => "MSH",
  'sortedArrayUsingSelector, sortUsingSelector' => "Objective-C",
  'predsort / keysort / mergesort' => "Prolog",
 ],

 'reverse' =>
 [ { MLANG => 'Tcl' },
  'reverse' => "Pike Rebol Io Logo Ruby Haskell Prolog Perl Perl6 Java JavaScript Mercury Scheme Python merd Dylan EmacsLisp CommonLisp C++",
  'Reverse' => "C# Oz Mathematica",
  'reversed' => "Smalltalk Python",
  'reverse_copy' => "C++",
  'rev' => "OCaml F# SML",
  'lreverse' => 'Tcl8.5',
  'array_reverse' => "PHP",
  'ListTools[Reverse]' => "Maple",
  'fliplr flipud...' => "Matlab",
  'l[::-1]' => "Python",
 ],

 'list of couples from 2 lists' =>
 [ { KIND => 'functional', MLANG => 'Smalltalk Io Scheme' },
  'combine' => "OCaml F#",
  'zip' => "Haskell Maple F# SML Scheme-SRFI1 Ruby Python Perl6 merd",  
  'pairlis (the result is not guaranteed to be the same as the order in the input)' => "CommonLisp",
  'transpose' => "Ruby",
  'Transpose' => "Mathematica",
  '[a b]' => "Matlab",
 ],

 '2 lists from a list of couples' =>
 [ { KIND => 'functional', MLANG => 'Smalltalk CommonLisp Scheme' },
  'split' => "OCaml F#",
  'unzip' => "Haskell F# SML merd",
  'unzip2' => "Scheme-SRFI1",
  'transpose' => "Ruby",
  'Transpose' => "Mathematica",
  'zip(*l)' => "Python",
  'a(1,:), a(2,:)' => "Matlab",
 ],

 'lookup an element in a association list' =>
 [ { KIND => 'functional', MLANG => 'Smalltalk Io' },
  'lookup' => "Haskell",
  'assoc' => "Ruby F# OCaml CommonLisp EmacsLisp Scheme",
  'assq' => "EmacsLisp Scheme",
  'assv' => "Scheme",
  'get_assoc' => "Prolog",
  'select' => "Rebol",
  'a.(e)' => "Matlab",
  'a[e]' => "Maple",
  'gprop' => "Logo",
  '/.' => "Mathematica",
 ],

 'list out of a bag' =>
 [ { KIND => 'rare' },
  'to_a' => "Ruby",
  'toArray' => "Java",
  'asArray' => "Smalltalk",
  'to_list' => "merd",
  'list' => "Python",
  'map-as(<list>, bag)' => "Dylan",
  '[a.(:)]' => "Matlab",
  'array get' => "Tcl",
 ],

 'f(... f(f(init, e1), e2) ..., en)' =>
 [ { KIND => 'has_lambda', MLANG => "Perl Scheme Matlab" },
  'foldl' => "Haskell Maple SML Mercury merd",
  'FoldL' => "Oz",
  'fold_left' => "F# OCaml",
  'fold' => "Scheme-SRFI1",
  'Fold' => "Mathematica",
  'reduce (in Perl, in List::Util)' => "Pike Io Python CommonLisp Dylan Perl Perl6",
  'inject' => "Ruby",
  'inject into' => 'Smalltalk',
 ],

 'f(e1, f(e2, ... f(en, init) ...))' =>
 [ { KIND => 'functional', MLANG => 'Smalltalk Scheme' },
  'foldr' => "Haskell Maple SML Mercury merd",
  'FoldR' => "Oz",
  'fold-right' => "Scheme-SRFI1",
  'fold_right' => "F# OCaml",
  'rreduce' => "Pike",
  "(reduce f '(e1 e2 ... en) :from-right t :initial-value init)" => "CommonLisp",
  'reverseReduce' => "Io",
 ],

],

'Various Data Types' => [

 'tuple type' =>
 [ { KIND => 'typed', MLANG => 'C C++ C# Maple Eiffel Pascal Java Smalltalk JavaScript Ada' },
   't1 * ... * tn' => "SML F# OCaml",
   '(t1, ..., tn)' => "Haskell",
   't1, ..., tn' => "merd",
   'tuple' => "Python",
   'tuple!' => "Rebol",
   'Tuple[T1, T2, T3]' => "E",
 ],

 'tuple constructor' =>
 [ { MLANG => 'C C++ C# Eiffel Tcl Java JavaScript ClassicREXX FishShell' },
   'a, b, c' => "OCaml F# Ruby Python Lua merd",
   '( a, b, c )' => "SML Haskell Prolog Ada Perl",
   '{ a. b. c }' => 'Smalltalk',
   '{ a, b, c }' => 'Matlab',
   '[ a, b, c ]' => "E",
   'a . b . c' => "Rebol",
   '(cons a b)' => "CommonLisp",
 ],

 'computable tuple (these are a kind of immutable lists playing a special role in parameter passing)' =>
 [ { ALL => 1, KIND => 'dynamic', MLANG => "ClassicREXX Logo Io Tcl FishShell" },
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
   'f @@ t' => "Mathematica",
   'L =.. [ F | Args ], call(L)' => "Prolog",
  ],
 ],

 'reference (pointer)' =>
 [ { ALL => 1, MLANG => 'Ruby Io Python Tcl CommonLisp Matlab Scheme Smalltalk Eiffel Java ClassicREXX' },
  'creation' =>
  [
   '&' => "B C C++ C#",
   '\\' => "Perl",
   'AddressOf' => "VisualBasic",
   'addr (Borland Pascal extension)' => "Pascal",
   '@ (Borland Pascal extension)' => "Pascal",
   'lv' => "BCPL",
   'ref' => "C# F# SML OCaml",
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
   '! (prefix)' => "OCaml F# SML",
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
   ':=' => "OCaml F# SML",
   '!' => "Forth",
  ],

 ],

 'optional value' => 
 [ { ALL => 1, MLANG => "ClassicREXX Prolog" },
  'null value' =>
  [
   '0 (optional value is only for pointers)' => 'C++',
   'NULL' => 'C SQL92 Maple',
   'nil' => "Ruby EmacsLisp Io CommonLisp Smalltalk Lua Objective-C",
   'null' => "C# JavaScript Java",
   'Null (only for "access" types in Ada)' => "Ada Mathematica",
   'undef' => "Perl",
   'None' => "Python F# OCaml",
   'NONE' => "SML",
   'Nothing' => "Haskell",
   'Void' => "Eiffel",
   '#f ()' => "EmacsLisp",
   '(empty) / ~ / null' => "YAML",
  ],

  'value' =>
  [
   'v' => "Ada C# Eiffel Java Perl JavaScript Smalltalk Python Perl Ruby EmacsLisp CommonLisp Lua Scheme",
   '*v (optional value is only for pointers)' => "C C++",
   'Just v' => "Haskell",
   'Some v' => "OCaml F#",
   'SOME v' => "SML",
  ],

  'type name' =>
  [ { KIND => 'functional typed', MLANG => "Smalltalk" },
    'option' => "SML F# OCaml",
    'Maybe' => "Haskell",
  ],

 ],

 'record selector' =>
 [ { MLANG => "Perl Prolog Logo ClassicREXX" },
  '.' => "C C++ C# F# Ruby OCaml Matlab Ada Beta Pascal Python E Eiffel Java Modula-2 Modula-3 JavaScript Lua Oz",
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
  'normal function call' => "Haskell CommonLisp Dylan Smalltalk Io",
 ],

 'dictionary' =>
 [ { ALL => 1, MLANG => 'C Ada Logo Smalltalk ClassicREXX Prolog Tcl' },

  'type name' =>
  [ { KIND => 'typed' },
    'map' => "YAML",
    'Map' => "Io F#",
    'dict' => "Python",
    'Dictionary' => "Pliant Smalltalk",
    'Hash' => "Perl6 Ruby",
    'HASH_TABLE' => "Eiffel",
    'HashTable' => "Java",
    'Hashtbl' => "F#",
    'Hashtbl.t' => "OCaml",
    'struct' => "Matlab",
    'table' => "Maple",
    'Data.Map, Data.HashTable' => "Haskell",
  ],

  'constructor' =>
  [ { MLANG => 'C++ C# OCaml Java Io' },
   '[ a => b, c => d ]' => "E",
   'array( a => b, c => d )' => "PHP",
   '{ a => b, c => d }' => "Ruby Perl Perl6",
   '{ a, b, c, d }' => "Ruby Perl",
   '{ a: b, c: d }' => "Python JavaScript YAML",
   '{ a: b; c: d }' => "CSS",
   '$[ a: b, c: d ]' => "YCP",
   '{ a->b. c->d }' => "Squeak",
   '{ a = b, c = d }' => "Lua",
   '@{ a = b; c = d }' => "MSH",
   '([ a:b, c:d ])' => "Pike",
   '([a]=b [c]=d)' => 'KornShell',
   '<< a b c d >>' => "PostScript",
   "struct(a, b, c, d)" => "Matlab",
   'Hash[ a, b, c, d ]' => "Ruby",
   'Map.of_list [a, b; c, d]' => "F#",
   'Hashtbl.of_list [a, b; c, d]' => "F#",
   'table([a=b, c=d])' => "Maple",
   'define table foo a => b; c => d end' => "Dylan",
   'dict create a b c d' => "Tcl8.5",
   'fromList' => "Haskell",
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
    'h.[k]' => "F#",
    'h.k' => "Lua JavaScript Matlab",
    'h:k' => "Pliant",
    'h["k"] or h->k' => "Pike",
    '(gethash k h)' => "CommonLisp",
   ],
   
   'read' =>
   [
    'h k get' => "PostScript",
    'find' => "F# OCaml",
    'fetch' => "Ruby",
    'get' => "Java",
    'dict get' => "Tcl8.5",
    'at' => "Smalltalk Io",
    'h@k or h.at(k)' => "Eiffel",
    'h[k]:default' => "YCP",
    '${h[k]}' => 'KornShell',
    'h.get(k, returned_value_when_k_unfound)' => "Python",
    'objectForKey' => "Objective-C",
    'lookup' => "Haskell",
   ],

   'write' =>
   [
    'h k o put' => 'PostScript',
    'put' => "Eiffel Java",
    'add, replace' => "F# OCaml",
    'store' => "Ruby",
    'dict set' => "Tcl8.5",
    'h[k]' => "YCP KornShell",
    'atPut' => "Io",
    'h at: k put: o' => "Smalltalk",
    '[h setObject:o forKey:k]' => "Objective-C",
    'insert' => "Haskell",
   ],
  ],

  'has the key ?' =>
  [ { MLANG => "Maple CommonLisp Objective-C" },
   'exists $h{k}' => "Perl",
   'exists' => "Pliant Perl6",
   'dict exists' => "Tcl8.5",
   'has' => "Eiffel",
   'haskey' => "YCP",
   'hasKey' => "Io",
   'has_key' => "Python",
   'has_key?, include?, key?, member?' => "Ruby",
   'Contains' => "F# C#",
   'containsKey' => "Java",
   'includesKey' => "Smalltalk",
   'k in h' => "Python",
   'k not in h' => "Python",
   'in' => "Awk",
   'mem' => "F# OCaml",
   'member' => "Haskell",
   'isfield' => "Matlab",
   'find (returns an iterator)' => "C++",
   'h[k]' => "Pike",
   '(gethash k h)' => "CommonLisp",
   'maps' => "E",
   'known' => "PostScript",
   'isset(h[k]), array_key_exists(k, h)' => "PHP",
  ],

  'remove by key' =>
  [
   'delete $h{k}' => "Perl",
   'del h[k]' => "Python",
   'unset(h[k])' => "PHP",
   'remove' => "Eiffel Java F# OCaml YCP",
   'Remove' => "C# F#",
   'dict remove' => "Tcl8.5",
   'removeAt' => "Io",
   'removeKey' => "E Smalltalk",
   'remhash' => "CommonLisp",
   'delete' => "Ruby Haskell JavaScript Perl6",
   'erase' => "C++",
   'm_delete' => "Pike",
   'removeObjectForKey' => "Objective-C",
   'undef' => "PostScript",
   'rmfield' => "Matlab",
  ],

  'list of keys' =>
  [ { MLANG => 'C++ OCaml F# CommonLisp' },
   'keys' => 'Perl Io Perl6 Maple Python MSH Ruby Smalltalk Haskell',
   'dict keys' => "Tcl8.5",
   'keySet' => "Java",
   'allKeys' => "Objective-C",
   'AllKeys' => "C#",
   'indices' => "Pike",
   'current_keys' => "Eiffel",
   'getKeys' => "E",
   'array_keys' => "PHP",
   'fieldnames' => "Matlab",
   'findall(Key, item(Key, _), Keys)' => 'Prolog',
   '${!h[@]}' => 'KornShell',
  ],

  'list of values' =>
  [ { MLANG => 'C# C++ F# OCaml CommonLisp' },
   'values' => "Perl Io Perl6 Java Pike Python Ruby Smalltalk",
   'dict values' => "Tcl8.5",
   'getValues' => "E",
   'content' => "Eiffel",
   'array_values' => "PHP",
   'struct2cell' => "Matlab",
   'entries' => "Maple",
   'elems' => "Haskell",
   '${h[@]}' => 'KornShell',
  ],
 ],

 'range' => 
 [ { MLANG => 'C C++ Maple OCaml Java ClassicREXX Eiffel' },
  'inclusive .. inclusive' =>
  [
   'a .. b' => "Ruby Perl Pascal merd E Ada MSH",
   'a:b' => "Matlab",
   '[ a .. b ]' => "F# Haskell",
   'to' => "Smalltalk Io",
   'seq a b / jot - a b (jot on BSD)' => "BourneShell FishShell",
   '{a..b}' => 'KornShell',
   'range' => "PHP",
   'range(from: a, to: b, by: step)' => "Dylan",
   'Range' => "Mathematica",
   'Range with' => "Io",
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
   'int' => "YAML SML OCaml",
   'Int' => "Perl6",
   'Int, uInt, Int8, Int16...' => "Pliant",
   'int, long (long is a big integer)' => "Python",
   'integer' => "Maple",
   'INTEGER, INT, SMALLINT' => "SQL92",
   'INTEGER, INTEGER_8, NATURAL_8...' => "Eiffel",
   'int8, uint8, int16, uint16, ...64' => "Matlab",
   'int, int8, uint8, int16, uint16, int32, uint32, int64, uint64, bigint, bignum' => "F#",
   'Int, Integer, Int8, Int16, Int32, Int64' => "Haskell",
   'Integer, FixNum, BigNum' => 'Ruby',
   'Integer, SmallInteger, LargeInteger' => "Smalltalk",
   'Integer' => "Mathematica",
  ],

  'decimal' =>
  [
   'float, double' => "C C#",
   'float' => "YAML SML OCaml Maple",
   'Float' => "Ruby",
   'float, float32' => "F#",
   'Float, Float32, Float64' => "Pliant",
   'NUMERIC, DECIMAL, DOUBLE PRECISION' => "SQL92",
   'Rat' => "Perl6",
   'DOUBLE, REAL' => "Eiffel",
   'single, double' => "Matlab",
   'Float, Double, Ratio' => "Haskell",
   'Float, Double, Fraction, FixedPoint' => "Smalltalk",
   'float, decimal.Decimal' => "Python",
   'Real, Rational' => "Mathematica",
   'Number' => "Io",
  ],
 ],


 'numbers syntax' =>
 [ { ALL => 1 },
   'integers' =>
   [
    '1000' => 'Awk Ada Io C++ F# B Maple Mathematica Prolog Logo Rebol C C# Haskell E Java JavaScript Pascal Pliant Pike Python BourneShell Tcl Scheme Smalltalk Perl Perl6 Ruby Eiffel OCaml merd Oz SQL92 Yorick',
    '1000, 1000.' => 'CommonLisp EmacsLisp',
    '1000, 1000., 1000.0' => 'Awk Matlab',
    "1000, '1000'D" => "ClassicREXX",
   ],

   'integers in base 2, octal and hexadecimal' =>
   [ { MLANG => "Matlab Prolog Logo" },
    '0b1, 07, 0xf' => 'Ruby Pike Perl Oz',
    '0b1, 0o7, 0xf' => 'OCaml F# Perl6',
    '07, 0xf' => "C C++ Python Tcl Awk JavaScript",
    '0xf' => "C# E Io Yorick",
    '07' => "B",
    '0o7, 0xf' => "Haskell",
    '1b' => "Eiffel",
    '2#1#, 8#7#, 16#f#' => "Ada",
    '2#{1}, #{F}' => "Rebol",
    '#b1, #o7, #xf' => "CommonLisp EmacsLisp Scheme",
    '2^^1, 8^^7, 16^^f' => "Mathematica",
    '2r1, 8r7, 16rf' => "Smalltalk",
    '#2r1, #8r7, #16rf' => "CommonLisp EmacsLisp",
    '1b, Fh' => "Pliant",
    "'1'B, 'F'X" => "ClassicREXX",
    "B'1', X'F'" => "SQL92",
   ],

   'integer thousand-seperator' => 
   [ { MLANG => "Awk C Tcl C++ C# Logo Smalltalk Maple Python Prolog Haskell Oz JavaScript CommonLisp ClassicREXX SQL92 Matlab" },
    '1_000, 10_00, 100_0' => 'E Perl Perl6 Ruby Eiffel OCaml',
    "1'000, 10'00, 100'0" => 'Rebol',
    '1_000' => 'merd Ada',
   ],

   'decimals' =>
   [ { MLANG => 'BourneShell' },
    '1000., 1E3' => 'C C++ E Logo F# Maple OCaml Java Python Scheme Tcl JavaScript Ada ClassicREXX SQL92',
    '1000., 1E3, 1,0' => 'Rebol',
    '1000., 1.E3' => 'Oz Eiffel',
    '1000.0, 1E3' => 'C# Pike Ruby Prolog CommonLisp EmacsLisp Smalltalk',
    '1000.0, 1.0E3' => 'Haskell',
    '1000, 1000.0, 1E3 (integers are decimals)' => 'Awk Perl Perl6 merd',
    '1000., 1*^3, 1000`' => "Mathematica",
   ],
 ],

 'addition / subtraction / multiplication / division' =>
 [
  '+ / - / * / /' => "Io C C++ Java Eiffel Maple C# F# Logo Perl Perl6 Matlab MUMPS Python Pliant Ruby BourneShell merd Tcl Haskell Scheme CommonLisp EmacsLisp Smalltalk ClassicREXX SQL92 Prolog Yorick",
  '+ / - / * or nothing / /' => "Mathematica",
  '+ +. / - -. / * *. / / /. (with mathematical priorities)' => "OCaml",
  'sum / difference / product / quotient' => "Logo",
  'add / sub / mul / idiv div' => "PostScript",
 ],

 'exponentiation (power)' =>
 [ { MLANG => 'Pascal' },
  '**' => 'PL/I Perl Perl6 F# Io Tcl8.5 Rebol Ruby Python OCaml E Ada merd Fortran Prolog ClassicREXX',
  '^' => 'Eiffel Awk Dylan Lua Matlab Mathematica Pliant Yorick',
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
  '-' => "B Io BCPL Awk Maple Logo C++ C C# F# E Java Matlab Mathematica Rebol Pike MUMPS Pliant Haskell Python BourneShell Tcl Scheme Smalltalk Perl Perl6 Ruby Eiffel merd EmacsLisp CommonLisp JavaScript Ada Prolog ClassicREXX Yorick",
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
    'rand' => 'C Perl Perl6 Maple Ruby Matlab Tcl',
    'random' => "Prolog Logo Python CommonLisp Yorick",
    '$RANDOM' => "MUMPS",
    'randomR' => "Haskell",
    'Random.int' => "OCaml",
    'Random, RandomReal, RandomInteger' => "Mathematica",
    'Random value' => "Io",
    'Random new nextInteger' => "Smalltalk",
  pre('r: RANDOM
create r.make
r.start
r.item') => "Eiffel",    
    pre('Random ran = new Random();
ran.Next(...);') => "C#",
    pre('let r = System.Random()
r.Next()') => "F#",
   ],

   'seed the pseudo random generator' =>
   [ { MLANG => "Prolog" },
    'srand' => 'C Perl Perl6 Ruby Tcl',
    'set_seed' => "Eiffel",
    'random.seed' => "Python",
    'Random setSeed' => "Io",
    'Random.init, Random.self_init' => "OCaml",
    "rand('state',...)" => "Matlab",
    'rerandom' => "Logo",
    'RandomTools[MersenneTwister][SetState]' => "Maple",
    'Random new setSeed' => "Smalltalk",
    'SeedRandom' => "Mathematica",
    'mkStdGen' => "Haskell",
    'make-random-state' => "CommonLisp",
   ],
 ],

 'operator priorities and associativities' =>
 [ { MLANG => 'Scheme CommonLisp EmacsLisp' },
  'addition vs multiplication' =>
  [
   'mathematical' => "C C++ F# Maple Mathematica Io Java C# Eiffel Perl Perl6 Python Ruby BourneShell merd Matlab Tcl Haskell ClassicREXX Yorick",
   'same priorities' => "Smalltalk MUMPS",
  ],

  'exponentiation vs negation (is -3^2 equal to 9 or -9)' =>
  [ { MLANG => 'C C++ MUMPS' },
   'mathematical' => "Perl Perl6 Maple Mathematica Io Eiffel Ruby Python Haskell Matlab ClassicREXX",
   'negation first' => "OCaml F#",
  ],
 ],

 'square root / e-exponential / absolute value' =>
 [
  'sqrt / exp / abs' => 'C C++ Eiffel Maple Io F# Prolog Python Ruby Smalltalk SML Perl Perl6 Pascal Tcl Java E PHP JavaScript Lua Ada Haskell OCaml EmacsLisp CommonLisp Scheme Yorick',
  'sqrt realsqrt / exp / abs' => "Matlab",
  'sqrt / exp /' => "Awk Logo",
  'Sqrt / Exp / Abs' => 'C# Oz Mathematica',
  'sqrt / / abs' => "PostScript",
  'Sqrt / / ABS' => "Modula-3",
  '/ exp / abs' => "Pliant",
  'sqrt / /' => "Pike",
  'square-root / exp / abs or absolute' => "Rebol",
  'Sqrt / Exp / ABS' => "ClassicREXX",
  'sqrt,isqrt / exp / abs' => "Tcl8.5",
 ],

 'trigonometry' =>
 [ { ALL => 1 },
   'basic' => 
   [ # perl's tan is in Math::Trig
    'sin / cos / tan' => "C C++ Pike Maple Io F# Prolog Ruby Matlab Python Smalltalk SML Pascal Perl Perl6 Tcl Java E PHP JavaScript EmacsLisp CommonLisp Scheme Lua Ada Haskell OCaml Pliant Yorick",
    'Sin / Cos / Tan' => "C# Oz ClassicREXX Mathematica",
    'sin / cos /' => "Awk PostScript",
    'sine / cosine / tangent' => "Eiffel Rebol",
    'radsin / radcos / radtan' => "Logo",
   ],

   'inverse' =>
   [ # perl's are in Math::Trig, JavaScript are in Math
    'asin / acos / atan (Ruby >= 1.7)' => "C C++ F# Io Haskell Pike Tcl Prolog Pliant CommonLisp Scheme Python OCaml Perl Perl6 Ruby JavaScript Ada Matlab Yorick",
    'Asin / Acos / Atan' => "C# Oz",
    'ASin / ACos / ATan' => "ClassicREXX",
    'arcsin / arccos / arctan' => "Maple",
    'arcSin / arcCos / arcTan' => "Smalltalk",
    'ArcSin / ArcCos / ArcTan' => "Mathematica",
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
    'log' => "C C++ Eiffel Perl Maple Prolog Io Perl6 Ruby Python Tcl PHP Lua F# OCaml Pliant Matlab Awk Pike Java E JavaScript EmacsLisp Scheme CommonLisp Yorick",
    'Log' => "C# ClassicREXX Oz Ada Mathematica",
    'log 10' => "Haskell",
    'log-e' => "Rebol",
   ],
   'base 10' =>
   [
    'log10' => "C C++ Eiffel Perl Prolog Perl6 Io Ruby Python Tcl PHP Lua F# OCaml Pliant Matlab Delphi-Kylix Logo Yorick",
    'Log10' => "C# ClassicREXX",
    'log' => "PostScript SML",
    'log: 10' => "Smalltalk",
    'log-10' => "Rebol",
    'log[10]' => "Maple",
    'Log[10, val]' => "Mathematica",
    'logBase 10' => "Haskell",
    'Log(X => val, Base => 10.0)' => "Ada",
    '(log x 10) ' => "CommonLisp",
   ],
   'base 2' =>
   [ { KIND => 'rare' },
    'log2' => "Matlab",
    'log-10 / log-2' => "Rebol",
    'Log(X => val, Base => 2.0)' => "Ada",
    'log(val, 2)' => "Python",
    'Log[2, val]' => "Mathematica",
    'frexp' => "C",
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
   'Mod' => "Mathematica",
   'MOD' => "Modula-3",
   'modulo' => "Ruby Dylan Logo",
   'rem' => "Prolog",
  ],

  'modulo of -3 / 2 is -1' =>
  [
   '%' => "Awk B C C++ C# F# Java Io E PHP JavaScript Pliant Yorick",
   '#' => "MUMPS",
   'mod' => "Pascal PostScript Prolog Lua F# OCaml XPath Oz",
   'remainder' => "Ruby Scheme Logo",
   'rem' => "BCPL Ada Haskell Smalltalk Matlab",
   '//' => "Rebol ClassicREXX",
   '\\\\' => "Eiffel",
  ],
 ],

 'truncate / round / floor / ceil' =>
 [
  'trunc / round / floor / ceil' => "C C++ Maple Matlab",
  'truncate / round / floor / ceiling' => "CommonLisp Haskell Scheme PostScript Prolog Perl6",
  'int / round / floor / ceil' => "Pike JavaScript Python",
  'int / round / /' => "Logo",
  'to_i, Integer() / round / floor / ceil' => "Ruby",
  'TRUNC / FORMAT / Floor / Ceil' => "ClassicREXX",
  '/ round / floor / ceil' => "SML Tcl Java E PHP Lua Io",
  '/ Round / Floor / Ceiling' => "C#",
  '/ Round / Floor / Ceil' => "Oz",
  '/ round / floor / ceiling' => "PostScript Dylan EmacsLisp XPath",
  '/ ROUND / FLOOR / CEILING' => "Modula-3",
  '/ rounded / floor / ceiling' => "Smalltalk Eiffel",
  'int / / floor / ceil' => "Perl F#",
  'int_of_float / / floor / ceil' => "OCaml F#",
  ' / / floor / ceil' => "Lua Yorick",
  'IntegerPart / Round / Floor / Ceiling' => "Mathematica",
  ' / Rounding / Floor / Ceiling' => "Ada",
  'to-integer / / /' => "Rebol",
 ],

 'bitwise operators' =>
 [
  'and / or / xor' =>
  [
   '& / | / ^' => "C C++ C# Tcl E Eiffel Pike Ruby Python Perl Java JavaScript",
   '& / | / ~' => "Yorick",
   '& / |' => "YCP",
   '+& / +| / +^' => "Perl6",
   '.&. / .|. / xor (in module Data.Bits)' => "Haskell",
   '&&& / ||| / ^^^' => "F#",
   'and / or / xor' => "Rebol PostScript",
   'land / lor / lxor' => "F# OCaml",
   'logand / logior / logxor (see also bit-and / bit-or / bit-xor)' => "CommonLisp",
   'bitand / bitor / bitxor' => "Matlab Logo",
   'BITAND / BITOR / BITXOR' => "ClassicREXX",
   'BitAnd / BitOr / BitXor' => "Mathematica",
   'bitAnd / bitOr / bitXor' => "Smalltalk",
   'bitwiseAnd / bitwiseOr / bitwiseXor' => "Io",
   '/\ / \/ / xor' => "Prolog",
  ],

  'negation' => 
  [
   '~' => "C C++ C# Pike Ruby Tcl Python Perl Java JavaScript YCP SML",
   '~~~' => "F#",
   'not' => 'Eiffel PostScript',
   'lnot' => "F# OCaml",
   'lognot (see also bit-not)' => "CommonLisp",
   'bitnot' => "Eiffel Logo",
   'BitNot' => "Mathematica",
   'complement' => "Rebol Haskell",
   'bitcmp' => "Matlab",
   'bitInvert' => "Smalltalk",
   'bitwiseComplement' => "Io",
   '\\' => "Prolog",
  ],

  'left shift / right shift / unsigned right shift' => 
  [ { MLANG => "Mathematica" },
   '<< / >> / >>>' => "Java JavaScript",
   '<< / >>' => "C C++ C# Pike Tcl Prolog Ruby Python Perl YCP",
   '<<< / >>>' => "F#",
   '|<< / |>>' => "Eiffel",
   'lsl / lsr or asr' => "F# OCaml",
   'bitshift' => "PostScript Matlab",
   'bitShift' => "Smalltalk",
   'ashift lshift' => "Logo",
   'shiftL / / shiftR' => "Haskell",
   'shiftLeft / shiftRight' => "Io",
   '(ash x positive-integer) / (ash x negative-integer) / ' => "CommonLisp",
  ],
 ],

],

'Threads' => 
 [ { KIND => 'rare' },

  'thread definition' =>
  [
   'class class_name(threading.Thread) {[override run method] }' => "Python",
   'task task_name is [entry entry_name[(parameter ...)]...] end task_name' => "Ada",
   'task type task_type_name is [entry entry_name[(parameter ...)]...] end task_type_name' => "Ada",
   'class class_name extends Thread {[override run method] }' => "Java",
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
   'set t [thread create {code}]' => "Tcl",
   'Thread createThread(...)' => "Io",
  ],
  
  'thread object creation' =>
  [
   'MyTask : task_type_name;' => "Ada",
   'class_name MyThread = new class_name()' => "Java",
   'p :=  [ ... ] newProcess.' => "Smalltalk",
   'p :=  [ ... ] fork. (equivalent to newProcess + resume)' => "Smalltalk",
  ],

  'starting / stopping threads' =>
  [
   'start() / stop() ("stop" is now deprecated)' => "Java",
   'resume / suspend / terminate' => "Smalltalk",
   'Tasks are started when created / call Stop entry or "abort task-object-name"' => "Ada",
   'thread send $t {script}' => "Tcl",
  ],
  
  'passing data directly between threads' =>
  [
   'call an entry with parameters' => "Ada",
   'call any public method' => "Java",
   'common variables are copied at thread creation, in abscence of a "share" statement' => "Pliant",
   'use messages, parameters or shared variables (a thread is created from a block, which is a closure on the variables as seen by the block)' => "Smalltalk",
  ],
  
  'terminating thread communication due to a time-out' =>
  [
   'select task_entry_call; or delay timeout_limit; end select;' => "Ada",
  ],
  
  'Thread Synchronization' =>
  [
   'Defining a Synchronized Shared Resource' =>
   [
    'thread::mutex' => "Tcl",
    pre('protected Object_Name is [entry entry_name(Parameter : [in out] is type [...]);
procedure procedure_name(Parameter : [in out] is type [...]);
function function_name return type;
private
shared data declaration
end Object_Name;') => "Ada",
    'synchronize (this){ ... }' => "Java",
    'SharedQueue, Semaphore critical: [...], Future, LazyValue' => "Smalltalk",
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
    'OtherThread.join()' => "Java Python",
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
    'p priority: n' => "Smalltalk",
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
    print CAT end('../');
}

print comments();
$print_both->(similar_pages());
$print_both->(credits());
print end('');
print SHORT end('../');

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
<li><a href="http://www.exept.de:8080/doc/online/english/programming/stForLispers.html">Smalltalk, Lisp</a>
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
<li>P&eacute;ter Varga (BourneShell, Common Lisp, ...)
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
<li>Garrett Wollman (Ruby, BourneShell, C...)
<li>Claus Gittinger (Smalltalk)
<li>Bobby 'nneonneo' Xiao (Python)
<li>Michael Schlenker (Tcl)
<li>Alex Vondrak (Python)
<li>Daniel Laberge (Pascal)
<li>Daniel Wagner (Haskell)
<li>Bob Bane (Common Lisp)
<li>Alan Hogan (Java)
<li>Steve Davison (C)
<li>Konstantin Yegupov (PHP, Python...)
<li>No&eacute; Rubinstein (Io)
<li>Florentin Millour (Yorick)
<li>Axel Liljencrantz (FishShell)
<li>Sanghyeon Seo (Lua)
<li>Szabolcs Horv&aacute;t (Mathematica)
<li>Laurent Le Brun (F#)
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
    my ($rel) = @_;

    sprintf(<< 'EOF', $rel) . "\n</body></html>";
<hr><address><a href="mailto:pixel@rigaux.org">Pixel</a></address>
This document is licensed under <a href="http://www.gnu.org/copyleft/fdl.html">GFDL</a> (GNU Free Documentation License).
<br>Generated from <a href="%ssyntax-across-languages.html.pl">syntax-across-languages.html.pl</a>
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
print end('../');


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
print end('../');


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
print end('../');


open STDOUT, ">syntax-across-languages-per-language/index.html" or die '';

print "<html><head><title>syntax across languages per language</title></head><body>";
print "<ul>";
printf qq(<li><a href="%s.html">%s</a>\n), url_quote($_), html_quote($_) foreach @langs_;
print "</ul>";

print comments();
print credits();
print end('../');

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
    print end('../');
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
