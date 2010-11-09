# - easiness to run from cmdline, from stdin
# - \n
# - easiness to have a valid exit code
# - tmpfile
# - no segmentation fault accepted, nor silent exception (rationale: a scripting language must not fail (eg: perl) or throw an explicit and useful exception

# - shebang_aware: needs either "#" being the start of a comment, or a special case (eg: runhugs)


# I think the results are representative of the fact that Python aspires
# to be more than just a scripting language. It strikes a balance between
# being concise and being self-documenting and therefore I wouldn't
# expect it to come first in tests that measure just one particular niche
# like this.
# -- 
# Ben Sizer

use strict;

my @snippets = ('smallest', 'hello_world', 'argv', 'env', 'test_file_exists', 'test_file_readable', 'formatting', 'system', 'sed_in_place', 'compile_what_must_be', 'grep');
my %snippets_comments =
  (
   smallest => 'the smallest running program doing nothing',
   hello_world => 'print a simple string on stdout',   
   argv => 'access command line parameters (no segmentation fault accepted, nor silent exception, so some languages must explicitly check the presence of the argument)',
   env => 'access environment variable',
   formatting => 'print integers in a simple formatted string',
   system => 'call an external program and check the return value',
   test_file_exists => 'return exit code error (non zero) if a file does not exist',
   test_file_readable => 'return exit code error (non zero) if a file is not readable',
   sed_in_place => 'remove #-comments from a file (modifying the file, i.e. in place)',
   compile_what_must_be => "find and compile .c files into .o when the .o is old or absent",
   grep => "grep with -F -i -h handling, usage, grep'ing many files",
  );
my %all = (

################################################################################
  sh => {
        implementation => "bash 2.05", implementation_www => 'www.gnu.org/software/bash/',
        run_file => 'sh %s', file_extension => '.sh',
        run_stdin => 'sh', run_cmdline => 'sh -c %s', 
        interactive_interpreter => 'sh',
        verbose_execution => 'sh -x',
        shebang_aware => 1,
        smallest => '', 
        hello_world => 'echo Hello World',
        argv => 'echo $1', #- :-( doesn't work the same with "sh -c"
        env => 'echo $HOME',
        test_file_exists => '[ -e /etc/mtab ]',
        test_file_readable => '[ -r /etc/mtab ]',
        formatting => 'a=1; b=2; echo "$a + $b = $[$a + $b]"',
        system => 'false || echo "false failed" 1>&2; echo done',
        sed_in_place => 'sed -i -e "s/#.*//" $1',
        compile_what_must_be => <<'END',
for c in `find -name "*.c"`; do
  o=`echo $c | sed 's/.c$/.o/'`
  if [ "$c" -nt "$o" ]; then
    echo "compiling $c to $o"
    gcc -c -o "$o" "$c"
  fi
done
END
        grep => <<'END',
opts=''
while [ -n "$1" ]; do
    case $1 in
        -i) opts="-i $opts" ;;
        -F) opts="-F $opts" ;;
        -h) h=1 ;;
        *) break ;;
    esac
    shift
done

if [ $# = 0 ] || [ -n "$h" ]; then
    echo "usage: grep [-F] [-i] regexp [files...]"
    exit 1
fi

r=$1
shift

grep $opts $r "$@"
END
  },

################################################################################
  Awk => {
        implementation => "gawk 3.1.1", implementation_www => 'www.gnu.org/software/gawk/',
        run_file => 'awk -f %s --', file_extension => '.awk',
        run_cmdline => 'awk %s', 
        shebang_aware => 1,
        smallest => '', 
        hello_world => 'BEGIN { print "Hello World" }',
        argv => 'BEGIN { print ARGV[1] }',
        env => 'BEGIN { print ENVIRON["HOME"] }',
        test_file_exists => 'BEGIN { if (system("[ -e /etc/mtab ]")) exit 1 }',
        test_file_readable => 'BEGIN { if (system("[ -r /etc/mtab ]")) exit 1 }',
        formatting => 'BEGIN { a=1; b=2; print a " + " b " = " a + b }',
        system => <<'END',
BEGIN { 
  if (system("false")) print "false failed" > "/dev/stderr" 
  system("echo done")
}
END
        sed_in_place => <<'END',
BEGIN { 
  if (!("mktemp /tmp/sed.XXXXXX" | getline tmp)) exit 1
}
{ sub("#.*", ""); print >> tmp }
END {
  system("cp -f " tmp " " ARGV[1])
  system("rm -f " tmp)
}
END
        compile_what_must_be => <<'END',
BEGIN { 
  while ("find -name '*.c'" | getline) {
    c = $0
    sub(/.c$/, ".o")
    if (system(sprintf("[ %s -nt %s ]", c, $0)) == 0) {
      print "compiling", c, "to", $0
      system(sprintf("gcc -c -o '%s' '%s'", $0, c))
    }
  }    
}
END
        grep => <<'END',
function usage() {
  print "usage: grep [-F] [-i] regexp [files...]" > "/dev/stderr"
  exit 1
}

BEGIN {
  for (i = 1; i < ARGC; i++) {
    s = ARGV[i]
    ARGV[i] = ""
         if (s == "-h") usage()
    else if (s == "-i") IGNORECASE = 1
    else if (s == "-F") F = 1
    else break
  }
  if (i == ARGC) usage()
  re = s
  prefix = i + 2 < ARGC
}

{
  if (F ? index($0, re) : match($0, re))
    print (prefix ? (FILENAME ":") : "") $0
}
END
  },

################################################################################
  Perl => { 
        www => 'www.perl.org',
        implementation => "Perl 5.6.0",
        run_file => 'perl %s', file_extension => '.pl',
        run_stdin => 'perl', run_cmdline => 'perl -e %s', 
        interactive_interpreter => 'perl -de 1',
        verbose_execution => 'perl -d:Trace',
        debugger => 'perl -d %s', interpreter_in_debugger => 1,
        shebang_aware => 1,
        smallest => '', 
        hello_world => q(print "Hello World\n"),
        argv => q(% perl -le 'print $ARGV[0]'),
        env => q(% perl -le 'print $ENV{HOME}'),
        test_file_exists => '-e "/etc/mtab" or exit 1',
        test_file_readable => '-r "/etc/mtab" or exit 1',
        formatting => q(% perl -le '$a=1; $b=2; print "$a + $b = ", $a + $b'),
        system => 'system "false" and warn "false failed\n"; system "echo done"',
        sed_in_place => '% perl -pi -e "s/#.*//"',
        compile_what_must_be => <<'END',
use File::Find;

find({ no_chdir => 1, 
       wanted => sub {
           if (($o = $_) =~ s/\.c$/.o/ 
               && -M $_ <= -M $o) {
               print "compiling $_ to $o\n";
               system qw(gcc -c -o), $o, $_;
           }
} }, '.');
END
        grep => <<'END',
use Getopt::Std;
getopts('Fih', \%h);

!@ARGV || $h{h} and die "usage: grep [-F] [-i] regexp [files...]\n";

$r = ($h{i} && '(?i)') . shift;
$r = "\Q$r" if $h{F};
$prefix = @ARGV > 1;

while (<>) {
    print $prefix && "$ARGV:", $_ if /$r/o;
}
END
  },

################################################################################
  PHP => { 
        www => 'www.php.net',
        implementation => "PHP 5",
        run_file => 'php %s', file_extension => '.php',
        run_stdin => 'php', run_cmdline => 'php -r %s', 
        interactive_interpreter => 'php -a',
#        verbose_execution => 'perl -d:Trace',
#        debugger => 'perl -d %s', interpreter_in_debugger => 1,
        shebang_aware => 1,
        smallest => '', 
        hello_world => q(Hello World
),
        argv => q(<?= "{$_SERVER[argv][1]}\n";),
        env => q(<?= "$_ENV[HOME]\n";),
	test_file_exists => q(<? file_exists("/etc/mtab") or exit(1);),
        test_file_readable => q(<? is_readable("/etc/mtab") or exit(1);),
        formatting => q(<? $a=1; $b=2; echo "$a + $b = " . ($a + $b) . "\n";),
        system => q(<? system("false") or fwrite(STDERR, "false failed\n"); 
system("echo done");),
        sed_in_place => <<'END',
<?
$f = $_SERVER[argv][1];
file_put_contents($f, 
    preg_replace("/#.*/", '', file_get_contents($f)));
END
        compile_what_must_be => <<'END',
<?
function doit($p) {
    foreach (glob($p . '/*', GLOB_ONLYDIR) as $d) doit($d);
    foreach (glob($p . '/*.c') as $c) {
        $o = substr($c, 0, -1) . 'o';
        if (filemtime($o) < filemtime($c)) {
            echo "compiling $c to $o\n";
            system("gcc -c -o $o $c");
        }
    }
}
doit('.');
END
        grep => <<'END',
<?
$args = array_slice($_SERVER[argv], 1);

while ($o = array_shift($args)) {
    if (ereg('^-[ihF]$', $o)) $$o{1} = $o{1}; 
    else break;
}

if (!$o || @$h)
    die("usage: grep [-F] [-i] regexp [files...]\n");

if (empty($args))
    $args []= 'php://stdin';

$r = $F ? preg_quote($o, '/') : $o;

foreach ($args as $f) {
    foreach (preg_grep("/$r/$i", file($f)) as $l)
        echo count($args) > 1 ? "$f:$l" : $l; 
END
  },

################################################################################
  Tcl => { 
        www => 'tcl.tk',
        implementation => "Tcl 8.3",
        run_file => 'tclsh %s', file_extension => '.tcl',
        run_stdin => 'tclsh',
        interactive_interpreter => 'tclsh',
        shebang_aware => 1,
        smallest => '', 
        hello_world => 'puts "Hello World"',
        argv => 'puts [lindex $argv 0]',
        env => 'puts $env(HOME)',
        test_file_exists => 'exit [expr ![file exists /etc/mtab]]',
        test_file_readable => 'exit [expr ![file readable /etc/mtab]]',
        formatting => 'set a 1; set b 2; puts "$a + $b = [expr $a + $b]"',
        system => <<'END',
if {[catch {exec false}]} {puts stderr "false failed"}
exec echo >@ stdout done
END
        sed_in_place => <<'END',
set f [lindex $argv 0]
set fd [open $f r]
regsub -all {#[^\n]*\n} [read $fd] {} contents
close $fd
set fd [open $f w]
puts -nonewline $fd $contents
close $fd
END
        compile_what_must_be => <<'END',
proc doit {dir} {
    foreach f [glob -nocomplain $dir/*] {
        if [file isdir $f] {
            doit $f
        } elseif { 
            [regsub \\.c$ $f .o o] && !( 
               [file exists $o] && 
               [file mtime $o] > [file mtime $f]
            ) } {
                puts "compiling $f to $o"
                exec gcc -c -o $o $f
        }
    }
}

doit .
END
        grep => <<'END',
set i 0
set F 0
set ind 0
set usage 1
foreach s $argv {
    if { "$s" == "-i" } { 
        set i 1 
    } elseif { "$s" == "-F" } { 
        set F 1 
    } elseif { "$s" == "-h" } { 
        incr usage
    } {
        incr usage -1
        break
    }
    incr ind
}

if $usage {
    puts stderr {usage: grep [-F] [-i] regexp [files...]}
    exit 1
}

set re [lindex $argv $ind]
incr ind
set files [lrange $argv $ind end]

if $i { set re (?i)$re }
if $F { set re (?q)$re }

set nb [llength $files]

proc grep { prefix fd re } {
    while {[gets $fd s] >= 0} {
        if [regexp $re $s] { puts $prefix$s }
    }
}

if { $nb == 0 } {
    grep "" stdin $re
} {
    set prefix ""
    foreach f $files {
        if { $nb > 1 } { set prefix $f: }
        set fd [open $f]
        grep $prefix $fd $re
        close $fd
    }
}
END
  },

################################################################################
  Ruby => { 
        www => 'www.ruby-lang.org',
        implementation => "Ruby 1.6.7",
        run_file => 'ruby %s', file_extension => '.rb',
        run_stdin => 'ruby', run_cmdline => 'ruby -e %s', 
        interactive_interpreter => 'irb',
        verbose_execution => 'ruby -r tracer',
        debugger => 'ruby -r debug %s', interpreter_in_debugger => 1,
        shebang_aware => 1,
        smallest => '', 
        hello_world => 'puts "Hello World"',
        argv => 'puts ARGV[0]',
        env => 'puts ENV["HOME"]',
        test_file_exists => 'test ?e, "/etc/mtab" or exit 1',
        test_file_readable => 'test ?r, "/etc/mtab" or exit 1',
        formatting => 'a=1; b=2; puts "#{a} + #{b} = #{a + b}"',
        system => 'system "false" or $stderr.puts "false failed"; system "echo done"',
        sed_in_place => q(% ruby -pi -e '$_.sub!(/#.*/, "")'),
        compile_what_must_be => <<'END',
Dir['**/*.c'].each{|c|
  o = c.sub('.c$', '.o')
  if test ?>, c, o then
    puts "compiling #{c} to #{o}"
    system("gcc", "-c", "-o", o, c)
  end
}
END
        grep => <<'END',
require 'getopts'
getopts('Fih')

ARGV.empty? || $OPT_h and 
  (puts "usage: grep [-F] [-i] regexp [files...]"; exit 1)

r = ARGV.shift
r = Regexp.quote(r) if $OPT_F
r = /#{r}/i if $OPT_i

prefix = ARGV.size > 1

while gets
  print prefix ? "#{ARGF}:" : '', $_ if $_ =~ /#{r}/
end
END
  },

################################################################################
  Pike => { 
        www => 'pike.ida.liu.se',
        implementation => "Pike 7.5.1",
        run_file => 'pike %s', file_extension => '.pike',
        run_cmdline => 'pike -e %s', 
        interactive_interpreter => 'pike',
        verbose_execution => 'pike -t1',
        shebang_aware => 1,
        smallest => '', 
        hello_world => 'return "Hello World"',
        argv => 'return argv[0]',
        env => 'return env->HOME',
        test_file_exists => 'return !file_stat("/etc/mtab")',
        test_file_readable => 'return !!catch(Stdio.File("/etc/mtab"))',
        formatting => 'int a=1, b=2; write("%d + %d = %d", a, b, a + b);',
        system => <<'END',
Process.system("false") && werror("false failed\n");
Process.system("echo done");
END
        sed_in_place => <<'END',
int main(int n, array args) {
   array r = ({});
   foreach(Stdio.read_file(args[1]) / "\n", string x) {
     sscanf(x, "%s#", x);
     r += ({x});
   }
   Stdio.write_file(args[1], r*"\n");
}
END
        compile_what_must_be => <<'END',
int main() {
  object i = Filesystem.Traversion(".");
  foreach (i; string d; string c) {
     if (!has_suffix(c, ".c")) continue;
     c = d+c;
     string o = c;
     o[-1] = 'o';
     object s = file_stat(o);
     if (s && i->stat()->mtime < s->mtime) continue;
     write("compiling %s to %s\n", c, o);
     Process.Process(({"gcc", "-c", "-o", o, c}))->wait();
  }
}
END
  },

################################################################################
  Lua => { 
        www => 'www.lua.org',
        implementation => "Lua 4.0",
        run_file => 'lua -f %s', file_extension => '.lua',
        run_stdin => 'lua', run_cmdline => 'lua -e %s', 
        interactive_interpreter => 'lua',
        shebang_aware => 1,
        smallest => '', 
        hello_world => 'print "Hello world"',
        argv => 'print(arg[1])',
        env => 'print(getenv"HOME")',
#        test_file_exists => [you can test existence using the Posix library, which is not included in the distribution but is available separately]
        test_file_readable => 'if not openfile("/etc/mtab", "r") then exit(1) end',
        formatting => 'a=1; b=2; print(a .. " + " .. b .. " = " .. a + b)',
        system => <<'END',
if execute "false" ~= 0 then _ALERT "false failed\n" end
execute "echo done"
END
  },

################################################################################
  JudoScript => { 
        www => 'www.judoscript.com',
        implementation => "JudoScript 0.9",
        run_file => 'java judo %s', file_extension => '.judo',
        shebang_aware => 1,
        smallest => '', 
        hello_world => '. "Hello World";',
        argv => '. $args[0];',
        env => '. #home;',
        test_file_exists => 'exit(!"/etc/mtab".fileExists());',
        test_file_readable => 'exit(!"/etc/mtab".fileReadable());',
        formatting => '. a=1," + ",b=2," = ",a+b;',
        system => <<'END',
exec <r> 'false'; if (r) . "false failed"; exec 'echo done';
END
	sed_in_place => <<'END',
getFileAsString(#args[0]).replaceAll('#.*', '').writeToFile(#args[0]);
END
        compile_what_must_be__NOT_VALID => <<'END',
listFiles '*.c' recursive {
    if (!(o = $_.trunc(1) @ 'o').fileExists()) {
        . "Compiling $_ to ${o}";
        exec('gcc -c -o "$o" "$c"');
    }
}
END
  },

################################################################################
  merd => { 
        www => 'merd.net',
        run_file => 'merd %s', file_extension => '.me', tmpfile => '%s.mec',
        run_stdin => 'merd', run_cmdline => 'merd -e %s', 
        interactive_interpreter => 'merd', # will be one day...
        shebang_aware => 1,
        smallest => '', 
        hello_world => '"Hello World".println',
        argv => 'Sys::args[0].println',
        env => 'Sys::env{"HOME"}.println',
        test_file_exists => 'Sys::exit(1) if not File::exists?("/etc/mtab")',
        test_file_readable => 'File::open("/etc/mtab") or Sys::exit(1)',
        formatting => 'a=1; b=2; "{a} + {b} = {a + b}".println',
        system => 'Sys::system("false") or warn("false failed"); Sys::system("echo done")',
  },

################################################################################
  Python => { 
        www => 'python.org',
        implementation => "CPython 2.4",
        run_file => 'python %s', file_extension => '.py',
        run_stdin => 'python', run_cmdline => 'python -c %s', 
        verbose_execution => 'python -m trace -t',
        interactive_interpreter => 'python',
        debugger => 'python -m pdb %s', interpreter_in_debugger => 1,
        shebang_aware => 1,
        smallest => '', 
        hello_world => 'print "Hello World"',
        argv => 'import sys; print sys.argv[1]',
        env => 'import os; print os.environ["HOME"]',
        test_file_exists => <<'END',
import os, sys
sys.exit(not os.path.exists("/etc/mtab"))
END
        test_file_readable => <<'END',
import sys
try: open("/etc/mtab")
except: sys.exit(1)
END
        formatting => q(a=1; b=2; print a, '+', b ,'=', a + b),
        system => <<'END',
import os, sys
if os.system("false"):
    sys.stderr.write("false failed\n")
os.system("echo done")
END
        sed_in_place => <<'END',
import fileinput, re

for s in fileinput.input(inplace = 1):
    print re.sub("#.*", "", s),
END
        compile_what_must_be => <<'END',
from os import *
from fnmatch import *

for dir, _, files in walk('.'):
    for f in filter(files, '*.c'):
        c = path.join(dir, f)
        o = c[0:-2] + '.o'
        if not path.exists(o) or stat(c)[9] > stat(o)[9]:
            print 'compiling', c, 'to', o
            system("gcc -c -o '%s' '%s'" % (o, c))
END
        grep => <<'END',
import getopt, sys, fileinput, re

opts, args = getopt.getopt(sys.argv[1:], 'hiF')
opts = [ x[0] for x in opts ]

if not args or '-h' in opts:
    print >> sys.stderr, "usage: grep [-F] [-i] regexp [files...]"
    sys.exit(1)

r, files = args[0], args[1:]

if '-F' in opts:
    r = re.sub("\W", lambda i: "\\" + i.group(0), r)

if '-i' in opts:
    re = re.compile(r, re.I)
else:
    re = re.compile(r)
  
for s in fileinput.input(files):
    prefix = ""
    if len(files) > 1:
        prefix = fileinput.filename() + ":"
    if re.search(s):
        print prefix + s,
END
  },

################################################################################
  Ch => { 
        www => 'www.softintegration.com',
        implementation => "Ch 4.0",
        run_file => 'ch %s', file_extension => '.ch',
        run_stdin => 'ch', run_cmdline => 'ch -c %s', 
        interactive_interpreter => 'ch',
        debugger => <<'EOF', interpreter_in_debugger => 1,
_debug =1
parse foo.ch
run
EOF
        shebang_aware => 1,
        smallest => '', 
        hello_world => 'puts("Hello World");',
        argv => 'echo $(_argv[1]);',
        env => 'echo $HOME',
        test_file_exists => 'access("/etc/mtab", 0);',
        test_file_readable => 'access("/etc/mtab", 4);',
        formatting => <<'END',
int a=1, b=2; 
echo $a + $b = $(a+b)
END
        system => <<'END',
if (system("false")) fprintf(stderr, "false failed\n");
system("echo done");
END
        sed_in_place => <<'END',
char *tmp=tmpnam(NULL);
sed 's/#.*//' < $(_argv[1]) > $tmp
cp -f $tmp $(_argv[1])
rm -f $tmp
END
        compile_what_must_be => <<'END',
#include <sys/stat.h>

string_t c, o;
struct stat cstat, ostat;
foreach (c; `find / -name "*.c"`) {
          o=`echo $c | sed 's/.c$/.o/'`;
          stat(o, &ostat);
          stat(c, &cstat);
          if (ostat.st_mtime  > cstat.st_mtime) { 
            echo "compiling $c to $o";
            gcc -c -o "$o" "$c";
          }
}
END
        grep_removed_cuz_using_external => <<'END',
#include <unistd.h>

extern char *optarg;
extern int optind, opterr, optopt;

int main(int n, char * const argv[])
{
   int ret;
   string_t opts, r;

   while (1)
   {
        ret = getopt(n, argv, "Fih");
        if ( ret == -1) break;
        switch (ret)
        {
            case 'i':
                 opts= stradd("-i ", opts);  break;
            case 'F':
                 opts= stradd("-F ", opts);  break;
            case 'h':
                 opts="1"; break;
            default:
                 break;
        }
  }

   if (n == 1 || strcmp(opts, "1") ==0 )
   {
            printf("usage: grep [-F] [-i] regexp [files...]\n");
            exit(1);
   }
   while (optind < n) {
          r = stradd(r, " ",  argv[optind++]);
   }
  grep $opts $r;
}
END
  },

################################################################################
  'Scheme' => { 
        www => 'schemers.org',
        implementation => "guile 1.4", implementation_www => 'www.gnu.org/software/guile/guile.html',
        run_file => 'guile -s %s', file_extension => '.scm', comment => '; %s',
        run_cmdline => 'guile -c %s', 
        interactive_interpreter => 'umb-scheme',
        shebang_aware => "#!/usr/bin/guile -s\n",
        smallest => '', 
        hello_world => '(display "Hello World\n")',
        argv => '(format #t "~A\n" (cadr command-line))',
        env => '(format #t "~A\n" (getenv "HOME"))',
        test_file_exists => '(exit (access? "/etc/mtab" F_OK))',
        test_file_readable => '(exit (access? "/etc/mtab" R_OK))',
        formatting => <<'END',
(let ((a 1) (b 2))
  (format #t "~A + ~A = ~A\n" a b (+ a b)))
END
        system => <<'END',
(if (not (= (system "false") 0)) 
  (display "false failed\n" (current-error-port)))
(system "echo done")
END
  },

################################################################################
  'JScheme' => {
        www => 'jscheme.sf.net',
        file_extension => '.scm', comment => '; %s',
        run_stdin => 'runscheme',
        shebang_aware => "#!/usr/bin/env runscheme\n!#",
        smallest => '',
        hello_world => '(display "Hello World\n")',
        argv => '(print ARGS)',
        env => <<EOF,
(load "using/run.scm")
(define getvar
  (let ((table (Properties.)))
    (for-each*
     (lambda (r) (let ((it (.split r "=")))
                   (.put table (vector-ref it 0) (vector-ref it 1))))
     (BufferedReader (inputReader 
          (if (.startsWith ($ "os.name") "Windows")
              (run (cmd cmd /c set))
              (run (cmd /bin/sh set))))))
    (lambda (name) (.get table name))))
(print (getvar "HOME"))
EOF
        test_file_exists => '(if (not (.exists (java.io.File. "/etc/mtab"))) (System.exit 1))',
        test_file_readable => '(if (not (.canWrite (java.io.File. "/etc/mtab"))) (System.exit 1))',
        formatting => <<'END',
(let ((a 1)
      (b 2))
  (print {[a] + [b] = [(+ a b)]}))
END
        sed_in_place => <<'EOF',
(load "using/run.scm")
(import "java.io.File")
(let ((file (vector-ref ARGS 1)))
  (let ((out (File.createTempFile "foo" "bar")))
    (print out)
    (call-with-output-file out
      (lambda (s)
        (for-each* 
            (lambda (r) (.println s (.replaceFirst r "#.*" "")))
            (BufferedReader (File. file)))
        (.close s)))
    (.renameTo out (File. file))))
EOF
	compile_what_must_be => <<'EOF',
(import "java.io.File")
(define (s->o srcDir classDir fromtype totype)
  ;; Source file to object file converter.
  (lambda (file)
    (let ((f (.toString (relativize srcDir file))))
      (File. classDir
        (string-append (.substring f 0
                          (- (.length f) (.length fromtype)))
                       totype)))))

(define (needsUpdate? s->o)
  (lambda (jf)
    ;; Does .java file jf need to be recompiled?
    (let ((cf (s->o jf)))
      (or (not (.exists cf)) 
          (<= (.lastModified cf) (.lastModified jf))))))

(let* ((dir (java.io.File. "."))
      (s->o (s->o dir dir ".c" ".o"))
      (update? (needsUpdate? s->o)))
  (for-each (lambda (c)
              (if (update? c)
                  (let ((o (s->o c)))
                    (display {compiling [c] to [o]\n})
                    (out (run (cmd gcc -c -o ,c ,o))))))
            (files* (java.io.File. ".") (isFile ".c"))))
EOF
        grep => <<'EOF',
(load "using/run.scm")
(define (any p xs) 
    (and (pair? xs) (or (p (car xs)) (any p (cdr xs)))))

(define (forLines f files)
    (for-each 
        (lambda (file) (for-each* f (BufferedReader (File. file))))
        files))

(define (show x) (display x) (newline))

(define (grep-F -i re files)
  (let ((lines (vector->list (.split re "\n")))
        (f (if -i .equalsIgnoreCase .equals)))
    (forLines 
        (lambda (r) (if (any (lambda (p) (f p r)) lines) (show r)))
        files)))

(define (grep re files)
  (let ((p (Pattern.compile re)))
    (forLines (lambda (r) (if (.find (.matcher p r)) (show r)))
              files)))

(let* ((args (cdr (vector->list ARGS)))
       (-F (member "-F" args))
       (-i (member "-i" args))
       (-h (member "-h" args))
       (args (filter (lambda (a) (not (.startsWith a "-"))) args))
       (re (if -i {[(?i)][(car args)]} (car args)))
       (files (cdr args)))
  (cond ((or -h (null? args))
         (display {usage: grep [-F] [-i] regexp file ...\n}))
        (-F (grep-F -i re files))
        (else (grep re files))))
EOF
  },



################################################################################
  REXX => { 
        www => 'www.rexxla.org',
        implementation => "Regina 3.3", implementation_www => 'regina-rexx.sourceforge.net',
        run_file => 'rexx %s', file_extension => '.rex or .rexx',
        run_stdin => 'rexx', 
	shebang_aware => 1,
#        interactive_interpreter => '',
        verbose_execution => 'rexx -ti %s',
        smallest => '', 
        hello_world => 'say "Hello World"',
        argv => 'say ARG(1)',
        env => 'say VALUE("HOME", , SYSTEM)',
        test_file_exists => q(exit STREAM(ARG(1), 'C', 'QUERY EXISTS') == ""),
        test_file_readable => q(exit \STREAM(ARG(1), 'C', 'READABLE')),
        formatting => 'a = 1 ; b = 2 ; say a "+" b "=" a + b',
        system => <<'END',
address SYSTEM "false"
if RC == 0 then ; call LINEOUT 'STDERR', "false failed"
address SYSTEM "echo done"
END
        sed_in_place => <<'END',
call rxFuncAdd 'sysLoadFuncs', 'rexxUtil', 'sysLoadFuncs'
call sysLoadFuncs

fin = ARG(1) ; fout = RANDOM() || ".tmp"
do while LINES(fin) > 0
  call LINEOUT fout, CHANGESTR("#", LINEIN(fin), "")
end
call STREAM fin, 'C', 'CLOSE' ; call STREAM fout, 'C', 'CLOSE'

call sysFileDelete fin ; call sysMoveObject fout, fin
call sysDropFuncs
END
        compile_what_must_be => <<'END',
call rxFuncAdd 'sysLoadFuncs', 'rexxUtil', 'sysLoadFuncs'
call sysLoadFuncs

call sysFileTree '*.c', 'cfilelist.', 'sf'

do i = 1 for cfilelist.0
  cfilelist.i = WORD(cfilelist.i, 5)
  ofile = LEFT(cfilelist.i, LASTPOS(".c", cfilelist.i) - 1) || ".o"
  otime = STREAM(ofile, 'C', 'QUERY TIMESTAMP')
  if otime < STREAM(cfilelist.i, 'C', 'QUERY TIMESTAMP') then do
    say "compiling" cfilelist.i "to" ofile
    address SYSTEM "gcc -c -o" cfilelist.i ofile
  end
end

call sysDropFuncs
END
        grep => <<'END',
call rxFuncAdd 'reLoadFuncs', 'rexxRE', 'reLoadFuncs'
call reLoadFuncs

cmdline = ARG(1) ; opt = "" ; regexp = "" ; filelist = ""

do while cmdline <> ""
  parse var cmdline token cmdline
  if LEFT(token, 1) == "-" then
    if TRANSLATE(token) == "-H" | VERIFY(token, "-iF") \= 0 then
      call usage ; else ; opt = opt || CHANGESTR("-", token, "")
  else ; if regexp == "" then
    regexp = token
  else
    filelist = filelist token
end

/* 'F' not recognised by 'reComp'; just remove it */
opt = CHANGESTR("F", opt, "")
if POS("i", opt) > 0 then ; opt = "" ; else ; opt = "c"

cre = reComp(regexp, opt)

do while filelist <> ""
  parse var filelist file filelist
  do while LINES(file) > 0
    if reExec(cre, LINEIN(file), 'match.') then
      call LINEOUT , file || ":" || match.!match
  end
end

call reFree cre ; call reDropFuncs ; exit 0

usage :
  say "usage: grep [-h] | [-F|-i] regexp [files...]"
  call reDropFuncs ; exit 1
END
  },

################################################################################
  Prolog => { 
        www => 'www.swi-prolog.org',
        implementation => "SWI-Prolog 5.6.x",
        run_file => 'pl -q -t halt -g main -s %s -- ', file_extension => '.pl or .pro',
	shebang_aware => 1,
        interactive_interpreter => 'pl',
#        verbose_execution => 'pl',
        debugger => 'use debug/0', interpreter_in_debugger => 1,
        smallest => 'main.', 
        hello_world => q(main :- write('Hello World'), nl.),
        argv => <<'END',
main :- current_prolog_flag(argv, CmdLine),
        append(_, [--, Arg1|_], CmdLine),
        write(Arg1), nl.
END
        env => q(main :- getenv('HOME', Home), write(Home), nl.),
        test_file_exists => q(main :- exists_file('/etc/mtab') -> halt(0) ; halt(1).),
        test_file_readable => q(main :- access_file('/etc/mtab', read) -> halt(0) ; halt(1).),
        formatting => q(main :- A is 1, B is 2, Result is A + B, 
        format('~d + ~d = ~d\n', [A, B, Result]).),
        system => <<'END',
main :- shell('false', Status),
        (Status \= 0 ->
              current_stream(2, _, StdErr),
              format(StdErr, 'false failed~n', [])
           ;
              true),
        shell('echo done', _).
END
        sed_in_place => <<'END',
main :-
  getUserArg(1, File),
  atom_concat(File, '.tmp', TmpFile),
  strip_comments_from_file(File, TmpFile),
  delete_file(File),
  rename_file(TmpFile, File).

strip_comments_from_file(FileIn, FileOut) :-
  open(FileIn, read, In, [type(binary)]), 
  open(FileOut, write, Out, [type(binary)]),
  process_lines(In, Out),
  close(In), close(Out).

process_lines(In, _) :- at_end_of_stream(In), !.

process_lines(In, Out) :-
  read_line_to_codes(In, Line),
  partition(Line, 35, Prefix, _),
  format(Out, '~s~n', [Prefix]),
  !, process_lines(In, Out).

partition(ListIn, Element, Prefix, Suffix) :-
  (member(Element, ListIn) ->
    append(Prefix, [Element|Suffix], ListIn)
  ;
    Prefix = ListIn, Suffix = []).

getUserArg(N, Arg) :-
  current_prolog_flag(argv, Cmdline),
  append(_, [--|Args], Cmdline),
  nth1(N, Args, Arg).
END
        compile_what_must_be => <<'END',
main :-
  expand_file_name('*.c', CFiles),
  forall(member(CFile, CFiles),
	(sub_atom(CFile, 0, _, 2, BaseName),
	 atom_concat(BaseName, '.o', ObjFile),
	 (should_compile(CFile, ObjFile) 
            -> compile(CFile, ObjFile) ; true))).

should_compile(SrcFile, ObjFile) :-
  (exists_file(ObjFile) ->
    time_file(SrcFile, SrcTime), 
    time_file(ObjFile, ObjTime), 
    ObjTime < SrcTime
  ;
    true).

compile(SrcFile, ObjFile) :-
  concat_atom(['gcc -c "', SrcFile, '" -o "', ObjFile, '"'], '', Cmd),
  format('compiling ~w to ~w\n', [SrcFile, ObjFile]),
  trap(Cmd, _).

trap(Cmd, Output) :-
  open(pipe(Cmd), read, Input),
  read_line_to_codes(Input, Output),
  close(Input).
END
  },

################################################################################
  CommonLisp => { 
        www => 'common-lisp.net',
#        implementation => "clisp", implementation_www => 'clisp.cons.org',
        implementation => "sbcl 0.9.2", implementation_www => 'www.sbcl.org',
        run_file => 'sbcl --noinform -load %s', file_extension => '.lisp',
        run_stdin => 'sbcl --noinform', 
	run_cmdline => 'sbcl --noinform -eval %s', 
#	shebang_aware => 1,
        interactive_interpreter => 'sbcl',
        verbose_execution => "sbcl --noinform -eval '(trace)'",
        debugger => '<builtin>', interpreter_in_debugger => 1,
        smallest => '', 
        hello_world => '(format t "Hello world~%")',
        argv => '(write-line (second *posix-argv*))',
        env => '(write-line (posix-getenv "HOME"))',
        test_file_exists => '(unless (probe-file "/etc/mtab") (quit :unix-status 1))',
        test_file_readable => <<'END',
(require :sb-posix)
(unless (sb-posix:access "/etc/mtab" r-ok)
  (quit :unix-status 1))
END
        formatting => '(let ((a 1) (b 2)) (format t "~a + ~a = ~a~%" a b (+ a b)))',
        system => <<'END',
(unless 
  (zerop (process-exit-code (run-program "false" nil :search t)))
  (format *error-output* "false failed~%"))
(run-program "echo" '("done") :search t :output *standard-output*)
END
        sed_in_place => <<'END',
(defun remove-hash-comments (path)
  (let ((lines (with-open-file (inf path :direction :input)
                 (loop for line = (read-line inf nil nil)
                       while line
                       collect line))))
    (with-open-file (ouf path
                         :direction :output
                         :if-exists :supersede)
      (dolist (line lines)
        (write-line (subseq line 0 (position #\# line)) ouf)))))
END
        compile_what_must_be => <<'END',
(defun find-files-by-type (dir type)
  (mapcan (lambda (path)
            (cond
              ((null (pathname-name path))
               (find-files-by-type path type))
              ((string= (pathname-type path) type)
               (list path))
              (t
               nil)))
          (directory (make-pathname :name :wild
                                    :type :wild
                                    :defaults (truename dir))))))

(defun compile-older-files (dir)
  (dolist (c-path (find-files-by-type dir "c"))
    (let* ((c-name (namestring c-path))
           (o-path (make-pathname :type "o" :defaults c-path))
           (o-name (namestring o-path)))
      (when (or (not (probe-file o-path))
                (< (file-write-date o-path) (file-write-date c-path)))
        (format t "Compiling ~a to ~a~%" c-name o-name)
        (run-program "gcc"
                     (list "-c" "-o" o-name c-name)
                     :output *standard-output*
                     :search t)))))
END
        grep => <<'END',
(require :asdf)
(require :cl-ppcre)

(defun grep (pattern files &key (no-case nil) (fixed-strings nil))
  (let ((scanner (cl-ppcre:create-scanner
                  (if fixed-strings
                      (cl-ppcre:quote-meta-chars pattern)
                      pattern)
                  :case-insensitive-mode no-case))
        (prefix-p (> (length files) 1)))
    (dolist (path files)
      (with-open-file (inf path :direction :input)
        (loop for line = (read-line inf nil nil)
              while line
              when (cl-ppcre:scan scanner line)
              do (format t "~@[~a:~]~a~%"
                         (when prefix-p
                           (pathname-name path))
                           line))))))
(defun parse-args (argv)
  (let ((flags nil)
        (args nil))
    (dolist (arg argv)
      (if (eql (char arg 0) #\-)
          (push arg flags)
          (push arg args)))
    (values (nreverse flags) (nreverse args))))
 
(multiple-value-bind (flags args)
    (parse-args (rest *posix-argv*))
  ;; Handle help flag
  (when (or (null args)
            (member "-h" flags :test #'string=))
    (format *error-output*
            "Usage: grep [-h] [-i] [-F] <pattern> <path> ...~%")
    (quit :unix-status 1))
  ;; Do grep
  (grep (first args) (rest args)
        :no-case (member "-i" flags :test #'string=)
        :fixed-strings (member "-F" flags :test #'string=)))
END
  },

################################################################################
  'Smalltalk' => {
        www => 'smalltalk.org',
        implementation => "GNU<br>Smalltalk 1.95", implementation_www => 'www.gnu.org/software/smalltalk/',
        run_file => 'gst -Q %s -a', file_extension => '.st', tmpfile => 'gst.im', comment => '" %s "',
        interactive_interpreter => 'gst',
        smallest => "",
        hello_world => "'Hello World' displayNl !",
        argv => "(Smalltalk arguments) at: 1 displayNl !",
        env => "(Smalltalk getenv: 'HOME') displayNl !",
        test_file_exists => "(File exists: '/etc/mtab') ifFalse: [ ObjectMemory quit: 1 ] !",
        test_file_readable => "(File isReadable: '/etc/mtab') ifFalse: [ ObjectMemory quit: 1 ] !",
        formatting => <<'END',
| a b |
a:=1.
b:=2.
(a printString, ' + ', b printString, ' = ', (a + b) printString)
   displayNl !
END
        system => <<'END',
(Smalltalk system: 'false') = 0
  ifFalse: [ stderr display: 'false failed' ; nl ].
Smalltalk system: 'echo done' !
END
  },

################################################################################
  Erlang => {
        www => 'www.erlang.org',
        implementation => "sae-r9b-1", implementation_www => 'www.sics.se/~joe/sae.html',
        run_file => 'escript %s', file_extension => '.erl',
        run_stdin => 'escript',
        interactive_interpreter => 'erl',
#        verbose_execution => 'perl -d:Trace',
#        debugger => 'perl -d %s', interpreter_in_debugger => 1,
        shebang_aware => 1,
        smallest => "\n" . 'main(_) -> ok',
        hello_world => "\n" . q(main(_) -> io:fwrite("hello world\n").),
        argv => "\n" . q(main([Arg|_]) -> io:format("~s~n", [Arg]).),
        env => "\n" . q(main(_) -> io:format("~s~n", [os:getenv("HOME")]).),
        test_file_exists => <<'EOF',

main(_) ->
    case filelib:is_file("/etc/mtab") of
        true -> ok;
	_ -> erlang:halt(1)
    end.
EOF
        test_file_readable => <<'EOF',

main(_) ->
    case file:read_file("/etc/mtab") of
        {ok, _} -> ok;
	_ -> erlang:halt(1)
    end.
EOF
        formatting => "\n" . q(main(_) -> A = 1, B = 2, io:fwrite("~p + ~p = ~p~n", [A, B, A+B]).),
        system => <<'EOF',

main(_) ->
    case os:cmd("false; echo $?") of
         [$0|_] -> ok;
         _ -> io:fwrite("false failed~n")
    end,
    io:fwrite(os:cmd("echo done")).
EOF
        sed_in_place => <<'EOF',

main([F|_]) ->
     {ok, B} = file:read_file(F),
     Lines = string:tokens(erlang:binary_to_list(B), "\n"),
     Not_comment = fun ($#) -> false; (_) -> true end,
     New = lists:map(
         fun(Line) ->
               L = lists:takewhile(Not_comment, Line),
               L ++ "\n"
         end,
         Lines
     ),
     file:write_file(F, erlang:list_to_binary(New)).
EOF
        compile_what_must_be => <<'END',

main(_) ->
      Fun = fun (Cfile, Acc) ->
		  [$c | File] = lists:reverse(Cfile),
		  Ofile = lists:reverse([$o | File]),
		  case is_compile_time(Cfile, Ofile) of
		      true ->
		          io:format("compiling ~s to ~s~n", [Cfile, Ofile]),
			  os:cmd(io_lib:format("gcc -c -o ~s ~s~n", [Ofile, Cfile])),
			  [Ofile | Acc];
		      false -> Acc
		  end
	  end,
     Regexp = regexp:sh_to_awk("*.c"),
     cc_recurse('.', Regexp, Fun).

is_compile_time(C, O) ->
     case filelib:is_file(O) of
	true -> filelib:last_modified(C) > filelib:last_modified(O);
	false -> true
     end.

cc_recurse(Directory, Regexp, CC_Fun) ->
     catch filelib:fold_files(Directory, Regexp, false, CC_Fun, []),
     {ok, Files} = file:list_dir(Directory),
     lists:foreach(
         fun (Subdir) -> cc_recurse(Subdir, Regexp, CC_Fun) end,
         lists:filter(fun (File) -> filelib:is_dir(File) end, Files)
     ).
END
  },

################################################################################
  Haskell => { 
        www => 'haskell.org',
        implementation => "ghc 6.6", implementation_www => 'www.haskell.org/ghc',
        run_file => 'runghc %s', file_extension => '.hs', comment => '-- %s',
        interactive_interpreter => 'ghci', run_cmdline => 'ghc -e %s', 
        shebang_aware => 1,
        smallest => 'main = putStr ""',
#       smallest => 'main = return () :: IO()', 
        hello_world => 'main = putStrLn "Hello World"',
        argv => <<'END',
import System
main = do l <- getArgs
          putStrLn (head l)
END
        env => <<'END',
import System
main = getEnv "HOME" >>= putStrLn
END
        test_file_exists => <<'END',
import Directory
import System
main = catch (getPermissions "/etc/mtab") (\_ -> exitFailure)
END
        test_file_readable => <<'END',
import System
main = catch (readFile "/etc/mtab") (\_ -> exitFailure)
END
        formatting => <<'END',
main = putStrLn $ show a ++ " + " ++ show b ++ " = " ++ show(a + b) 
    where a=1
          b=2
END
        system => <<'END',
import System
import IO
import Monad
main = 
    do ret <- system "false"
       when (ret /= ExitSuccess) (hPutStrLn stderr "false failed")
       system "echo done"
END
        sed_in_place => <<'END',
import System
import System.IO
import Monad
import Control.Exception

c = unlines . map (takeWhile (/= '#')) . lines
fop f n = do l <- fmap f (readFile n)
             evaluate (length l)
             writeFile n l

main = getArgs >>= (fop c) . head
END
        compile_what_must_be => <<'END',
import System
import System.IO
import System.Directory
import Data.List
import Control.Monad

gmt = getModificationTime
op f = do catch (do ct <- gmt f; ot <- gmt o;
                    when (ct > ot) comp)
                (\_ -> comp)
 where o = take (length f - 2) f ++ ".o"
       comp = do putStrLn ("Compiling "++f++" to "++o)
                 system ("gcc -c -o "++o++" "++f)

main = do files <- getCurrentDirectory >>= getDirectoryContents
          mapM_ op $ filter (".c"`isSuffixOf`) files
END

  },

################################################################################
  OCaml => { 
        www => 'www.ocaml.org',
        implementation => "OCaml 3.07 + Extlib 1.1",
        run_file => 'ocaml -I `ocamlc -where`/extlib str.cma unix.cma extLib.cma %s', file_extension => '.ml', comment => '(* %s *)', emacs_mode => 'caml',
#       run_stdin => 'ocaml', # not equivalent to running from a file due to ";;"s
        interactive_interpreter => 'ocaml',
        shebang_aware => "#!/usr/bin/env ocaml\n", # or "#!/usr/bin/ocamlrun ocaml\n"
        debugger => 'ocamldebug %s (nice go-back-time feature)',
        smallest => '', 
        hello_world => 'print_endline "Hello World"',
        argv => 'print_endline Sys.argv.(1)',
        env => 'print_endline(Sys.getenv "HOME")',
        test_file_exists => 'if not(Sys.file_exists "/etc/mtab") then exit 1',
        test_file_readable => 'try open_in("/etc/mtab") with _ -> exit 1',
        formatting => <<'END',
let a, b = 1, 2 in
Printf.printf "%d + %d = %d\n" a b (a + b)
END
        system => <<'END',
if Sys.command "false" != 0 then prerr_endline("false failed");
ignore(Sys.command "echo done")
END
        sed_in_place => <<'END',
let l = Std.input_list (open_in Sys.argv.(1)) in
let f = open_out Sys.argv.(0) in
List.iter (fun l -> output_string f 
 ((Str.replace_first (Str.regexp "#.*") "" l) ^"\n")) l;;
END
        compile_what_must_be => <<'END',
open Unix

let listdir dir =
  let rec listdir_ hdir =
    try
      let s = readdir hdir in
      if s = "." || s = ".." then
        listdir_ hdir
      else 
        s :: listdir_ hdir
    with End_of_file -> []
  in
  let hdir = opendir dir in
  let l = listdir_ hdir in
  closedir hdir ;
  l

let rec doit dir =
  List.iter (fun s ->
    let f = Filename.concat dir s in
    if (lstat f).st_kind = S_DIR then
      doit f
    else if Filename.check_suffix f ".c" then
      let o = Filename.chop_extension f ^ ".o" in
      if not (Sys.file_exists o && 
              (stat o).st_mtime > (stat f).st_mtime) then
        let cmd = Printf.sprintf "gcc -c -o '%s' '%s'" o f in
        print_endline ("compiling " ^ f ^ " to " ^ o) ;
        ignore (Sys.command cmd)
  ) (listdir dir)
;;
doit(".")
END
        grep => <<'END',
open List

let rec iter_lines f fd =
  try
    f (input_line fd) ; iter_lines f fd
  with End_of_file -> ()

let _ = 
  let i, fixed, usage = ref false, ref false, ref false in

  Arg.parse [ "-i", Arg.Set i, "" ; 
              "-F", Arg.Set fixed, "" ;
              "-h", Arg.Set usage, "" ;
            ] (fun _ -> ()) "" ;
  match filter (fun s -> s.[0] <> '-') (Array.to_list Sys.argv) with
  | _ :: r :: files when not !usage ->
      let r = if !fixed then Str.quote r else r in
      let re = (if !i then Str.regexp_case_fold else Str.regexp) r in
      let prefix = length files > 1 in

      iter (fun (name, fd) ->
        iter_lines (fun s ->
          try 
            let _ = Str.search_forward re s 0 in
            print_endline ((if prefix then name ^ ":" else "") ^ s)
          with Not_found -> ()
        ) fd
      ) (if files = [] then 
           ["", stdin] 
         else
           map (fun s -> s, open_in s) files)

  | _ ->
      prerr_endline "usage: grep [-F] [-i] regexp [files...]" ;
      exit 1
END
  },

################################################################################
  'C#' => { 
        www => 'csharp.org',
        implementation => "mono 0.15", implementation_www => 'www.go-mono.com',
        run_file => 'mcs %s && mint %s.exe', file_extension => '.cs', tmpfile => '%s.exe', comment => '// %s', emacs_mode => 'java',
        smallest => 'class z { static void Main() { } }',
        hello_world => <<'END',
class z {
   static void Main() {
      System.Console.WriteLine("Hello World");
   }
}
END
        argv => <<'END',
class z {
   static void Main(string[] args) {
      if (args.Length > 0) System.Console.WriteLine(args[0]);
   }
}
END
        env => <<'END',
using System;

class z {
    static void Main() {
	Console.WriteLine(Environment.GetEnvironmentVariable("HOME"));
    }
}
END
        test_file_readable => <<'END',
public class z {
    static int Main() {
	try {
	    (new System.IO.FileInfo("/etc/mtab")).OpenRead();
	    return 0;
	} catch(System.Exception ex) {
	    return 1;
	}
    }
}
END
        test_file_exists => <<'END',
public class z {
    static int Main() {
	return (new System.IO.FileInfo("/etc/mtab")).Exists ? 0 : 1;
    }
}
END
      system => <<'END',
using System;
using System.Diagnostics;

public class z {
    static void Main() {
	Process proc = new Process();
	proc.EnableRaisingEvents = false;

	proc.StartInfo.FileName = "false";
	proc.Start();
	proc.WaitForExit();
	if (proc.ExitCode != 0) Console.Error.WriteLine("false failed");

	proc.StartInfo.FileName = "echo";
	proc.StartInfo.Arguments = "done";
	proc.Start();
	proc.WaitForExit();
    }
}
END
        formatting => <<'END',
class z {
   static void Main() {
      int a=1, b=2;
      System.Console.WriteLine("{0} + {1} = {2}", a, b, a + b);
   }
}
END
   },

################################################################################
  JavaScript => { 
        www => 'developer.netscape.com/tech/javascript',
        implementation => "NJS 0.2.5", implementation_www => 'www.bbassett.net/njs',
        run_file => 'ngs-js %s', file_extension => '.js', comment => '// %s',
        run_cmdline => 'ngs-js -e %s', 
        shebang_aware => 1,
        smallest => '',
        hello_world => 'System.print("Hello World\n")',
        argv => 'System.print(ARGS[1], "\n")',
        env => 'System.print(System.getenv("HOME"), "\n")',
        test_file_exists => 'if (!new File("/etc/mtab").exists()) System.exit(1)',
        test_file_readable => 'if (!new File("/etc/mtab").open("r")) System.exit(1)',
        formatting => 'a=1; b=2; System.print(a, " + ", b, " = ", a + b, "\n")',
        system => <<'END',
if (System.system("false") != 0) System.error("false failed\n")
System.system("echo done")
END
   },

################################################################################
  Scala => { 
        www => 'www.scala-lang.org',
        implementation => "Scala 2.8.0 and Sun JVM 6",
        run_file => 'scala %s', file_extension => '.scala', comment => '/* %s */',
        run_cmdline => 'scala -e %s', 
        shebang_aware => "#!/path/to/scala\n!#\n",
        interactive_interpreter => 'scala',
        debugger => 'any jvm debugger',
        smallest => '',
        hello_world => 'println("Hello World")',
        argv => 'println(args(0))',
        env => 'println(System.getenv("HOME"))',
        test_file_exists => 'System.exit(if(new java.io.File("/etc/mtab").exists) 0 else 1)',
        test_file_readable => 'System.exit(if(new java.io.File("/etc/mtab").canRead) 0 else 1)',
        formatting => <<'END',
val (a, b) = (1, 2)
println(a + " + " + b + " = " + (a + b))
END
        system => <<'END',
def exec = Runtime.getRuntime.exec (_:String)

if (exec("false").waitFor != 0)
  error("false failed")

for (i <- new io.BufferedSource(exec("echo done").getInputStream))
  print(i)
END
	compile_what_must_be => <<'END',
import java.io._

for (src <- new File(args(0)).listFiles if (""+src) endsWith ".c") {
  val obj = new File((""+src).replaceAll(".c",".o"))
  if (!obj.exists || obj.lastModified < src.lastModified) {
    println("compiling "+src+" to "+obj)
    Runtime.getRuntime.exec("gcc -c " + src + " -o " + obj)
  }
}
END
	sed_in_place => <<'END',
import java.io._

val source = new File(args(0))
val target = new File(source+".tmp")

val fw = new FileWriter(target)

for (l <- new io.BufferedSource(new FileInputStream(source)).getLines 
          if !(l startsWith "#"))
  fw write (l+"\n")

fw.close

target renameTo source

END
   },

################################################################################
  E => { 
        www => 'www.erights.org',
#        implementation => "NJS 0.2.5", implementation_www => 'www.bbassett.net/njs',
	run_stdin => 'rune -',
        run_file => 'rune %s', file_extension => '.e',
        run_cmdline => 'rune -src %s', 
	interactive_interpreter => 'rune',
        shebang_aware => 1,
        smallest => '',
        hello_world => 'println("Hello World")',
        argv => 'println(interp.getArgs()[0])',
        env => <<'END',
# Only when run on Java1.5
println(<unsafe:java.lang.System>.getenv("HOME"))
END
        test_file_exists => 'require(<file:/etc/mtab>.exists())',
        test_file_readable => 'require(<file:/etc/mtab>.canRead())',
        formatting => <<'END',
def a := 1
def b := 2
println(`$a + $b = ${a + b}`)
END
        sed_in_place => <<'END',
def f := <file: interp.getArgs()[0]>
var text := ""
for line in f {
     if (line =~ `@left#@right`) {
         text += left + "\n"
     } else {
         text += line
     }
}
f.setText(text)
END
	system => <<'END',
def runFalse := makeCommand("/bin/false")
def runEcho := makeCommand("/bin/echo")
def ok := try {
     runFalse()
     true
} catch ex {
     stderr.println("false failed")
     false
}
if (ok) {
     println(runEcho("done")[0])
}
END
	compile_what_must_be => <<'END',
def runGcc := makeCommand("c:/cygwin/bin/gcc.exe")
def leaves := <import:org.erights.e.tools.files.leaves>

for c in leaves(<file:.>) {
     if (c.getPath() =~ `@base.c`) {
         def tmp := <file: `$base.o`>
         if (!tmp.exists() || tmp.lastModified() < c.lastModified()) {
             println(`compiling $base.c to $base.o`)
             try {
                 runGcc("-c", `$base.c`, "-o", `$base.o`)
             } catch ex {
                 stderr.println(`$base.c failed with $ex`)
             }
         }
     }
}
END
   },

################################################################################
  Java => { 
        www => 'java.sun.com',
        implementation => "gcj 3.2", implementation_www => 'gcc.gnu.org/java',
        run_file => 'gcj %s --main=%s && ./a.out', file_extension => '.java', tmpfile => 'a.out', comment => '// %s',
        debugger => 'gdb',
        smallest => 'public class smallest { public static void main(String[] args) { } }',
        hello_world => <<'END',
public class hello_world {
  public static void main(String[] args) {
    System.out.println("Hello World");
  }
}
END
        argv => <<'END',
public class argv {
  public static void main(String[] args) {
    System.out.println(args[0]);
  }
}
END
        env => <<'END',
import java.io.*;

public class env {
    public static void main(String[] args) {
	System.out.println(System.getenv("HOME"));
    }
    // NB: System.getenv was re-introduced in JDK 1.5, 
    //     and is not yet in gcj
}
END
	test_file_exists => <<'END',
import java.io.*;

public class exists {
  public static void main(String[] args) {
      System.exit((new File("/etc/mtab")).exists() ? 0 : 1);
  }
}
END
	test_file_readable => <<'END',
import java.io.*;

public class readable {
  public static void main(String[] args) {
      System.exit((new File("/etc/mtab")).canRead() ? 0 : 1);
  }
}
END
        formatting => <<'END',
public class formatting {
  public static void main(String[] args) {
    int a=1, b=2;
    System.out.println("" + a + " + " + b + " = " + (a + b));
  }
}
END
        system => <<'END',
import java.io.*;
public class system {
  public static void main(String[] args) throws Exception {
    if (Runtime.getRuntime().exec("false").waitFor() != 0)
      System.err.println("false failed");
  
    BufferedReader input = new BufferedReader(new InputStreamReader(
        Runtime.getRuntime().exec("echo done").getInputStream()
    ));
    String line;
    while ((line = input.readLine()) != null)
      System.out.println(line);
  }
}
END
        compile_what_must_be => <<'END',
import java.io.*;

public class compile {
  public static void main(String[] args) {
      filter ft = new filter();
      if (args.length > 0) {
          File f = new File(args[0]);
          File[] cs = f.listFiles(ft);
	  for (int i = 0; i < cs.length; i++) tryFile(cs[i]);
      }
  }

  static void tryFile(File c) {
      String o = c.toString().replaceAll(".c$", ".o");
      File tmp = new File(o);
      if (!tmp.exists() || 
          tmp.lastModified() < c.lastModified()) {
          System.out.println("compiling " + 
                             c.toString() + " to " + o);
          String cmd = "gcc -c " + c.toString() + " -o " + o;
	  try {
	      java.lang.Runtime.getRuntime().exec(cmd);
          } catch(Exception ex) { ex.printStackTrace(); }
      }
  }
}

class filter implements FileFilter
{
  public filter() { }
  public boolean accept(File f) {
      return f.toString().endsWith(".c");
  }
}
END
	 sed_in_place => <<'END',
import java.io.*;

public class uncomment {
    public static void main(String[] args) {
	try {
	    FileReader fr = new FileReader(args[0]);
	    BufferedReader br = new BufferedReader(fr);

	    File f = new File(args[0] + ".tmp");
	    FileWriter fw = new FileWriter(f);

	    String line = "";
	    while ((line = br.readLine()) != null) {
		int pos = line.indexOf('#');
		if (pos > -1) line = line.substring(0,pos);
		fw.write(line + '\n');
	    }
	    fr.close();
	    fw.close();
	    f.renameTo(new File(args[0]));
	} catch(Exception ex) {
	    ex.printStackTrace();
	}
    }
}
END
   },

################################################################################
  'VBScript' => { 
        www => 'msdn.microsoft.com/scripting',
        implementation => "VBScript", #implementation_www => 'www.gnu.org/software/smalltalk/',
        run_file => 'cscript //nologo %s', file_extension => '.vbs', comment => "' %s",
	debugger => 'cscript //X',
	interactive_interpreter => 'cscript //X 1', interpreter_in_debugger => 1,
        shebang_aware => 1,
        smallest => "", 
        hello_world => q(Wscript.echo "Hello World"),
        argv => q(Wscript.echo Wscript.Arguments(0)),
        env => <<'END',
Set s = WScript.CreateObject("WScript.Shell")
Wscript.echo s.Environment("PROCESS")("HOME")
END
        test_file_exists => <<'END',
Set f = CreateObject("Scripting.FileSystemObject")
if not f.FileExists("/etc/mtab") then wscript.quit(1)
END
        test_file_readable => <<'END',
On Error Resume Next
Set f = CreateObject("Scripting.FileSystemObject")
if f.GetFile("file2.txt") = Nothing then wscript.quit(1)
END
        formatting => <<'END',
a=1 : b=2
WScript.echo a & " + " & b & " = " & a + b
END
        system => <<'END',
Set s = CreateObject("WScript.Shell")
Set e = s.Exec("false")
If e.exitCode > 0 Then wscript.stderr.writeline "false failed"
s.Exec("echo done")
END
  },

################################################################################
  C => {
        implementation => "tcc 0.9.12", implementation_www => 'tinycc.org',
        run_file => 'tcc %s', file_extension => '.c', comment => '/* %s */',
        shebang_aware => 1,
        debugger => 'gdb (arguments must be passed separately)',
        smallest => 'main() { return 0; }',
        hello_world => <<'END',
#include <stdio.h>
main() {
    puts("Hello World");
    return 0;
}
END
        argv => <<'END',
#include <stdio.h>
main(int n, char **argv) {
    if (n > 1) puts(argv[1]);
    return 0;
}
END
        env => <<'END',
#include <stdlib.h>
main() {
    char *s = getenv("HOME");
    if (s) puts(s);
    return 0;
}
END
        test_file_exists => <<'END',
#include <unistd.h>
main() {
    return access("/etc/mtab", F_OK);
}
END
        test_file_readable => <<'END',
#include <unistd.h>
main() {
    return access("/etc/mtab", R_OK);
}
END
        formatting => <<'END',
#include <stdio.h>
main() {
    int a=1, b=2;
    printf("%d + %d = %d\n", a, b, a + b);
    return 0;
}
END
        system => <<'END',
#include <stdio.h>
main() {
    if (system("false")) fprintf(stderr, "false failed\n");
    system("echo done");
    return 0;
}
END
        sed_in_place => <<'END',
#include <stdio.h>
#include <unistd.h>
main (int argc, char **argv)
{
 FILE *f;
 int c=1, t=0, i=0;
 if (argc-c)
  {
   f=fopen(argv[c],"r+");
   while (c) switch (c=getc(f))
    {
    case '#' : if (!t) { t=1,i-=2; break; }
    case '\n' : if (t) { t=0; break; }
    default : if (t==1) { i--; break; }
         fseek(f,i-1,1);
         putc(c++,f);
         fseek(f,-i,1);
    }
   truncate(argv[1],ftell(f)+i);
  }
 return 0;
}
END
   },
################################################################################

  D => { 
        www => 'www.d-programming-language.org',
        implementation => "DMD 2.050",
        run_file => 'dmd -run', file_extension => '.d',
        run_cmdline => "rdmd --eval='%s'",
        shebang_aware => "#!/usr/bin/rdmd\n",
        debugger => 'see http://www.prowiki.org/wiki4d/wiki.cgi?DebugEnvironments',
        smallest => 'void main() {}', 
        hello_world => 'import std.stdio;
void main() { write("Hello World"); }',
        argv => 'import std.stdio;
void main(string[] a) { if(a.length>1) writeln(a[1]); }',
        env => 'import std.stdio,std.process;
void main() { writeln(getenv("HOME")); }',
        test_file_exists => 'import std.file;
int main() { return !exists("/etc/mtab"); }',
        test_file_readable => 'import std.stdio;
void main() { File("/etc/mtab"); }',
        formatting => <<'END',
import std.stdio;
void main() {
   int a=1,b=2;
   writeln(a," + ",b," = ",a+b);
}
END
        system => <<'END',
import std.stdio,std.process;
void main() {
   if (system("false"))
       stderr.writeln("false failed");
   system("echo done");
}
END
        sed_in_place => <<'END',
import std.file,std.regex;
void main(string[] a) {
   a[1].write(a[1].readText().replace(regex("#.*","g"),""));
}
END
        compile_what_must_be => <<'END',
import std.stdio,std.file,std.process;
void main() {
   foreach (c; listdir("","*.c")) {
       auto o = c[0..$-1]~'o';
       if (lastModified(o,0) < lastModified(c)) {
           writeln("compiling "~c~" to "~o);
           system("gcc -c -o '"~c~"' '"~o~"'");
       }
   }
}
END
        grep => <<'END',
import std.stdio,std.array,std.regex;
int main(string[] a) {
   auto o = ["-h":0,"-F":0,"-i":0];
   while (!(a=a[1..$]).empty) {
       if(auto b=a[0] in o) *b=1;
       else break;
   }
   if (!a.length || o["-h"]) {
       writeln("usage: grep [-F] [-i] regexp [files...]");
       return 1;
   }
   auto e = o["-F"] ? a[0].replace(regex(r"\W","g"),r"\$&") : a[0];
   foreach (f; a[1..$])
       foreach (l; File(f).byLine())
           if (!l.match(regex(e, o["-i"] ? "i" : "")).empty)
               writeln(a.length>2 ? f~':'~l : l);
   return 0;
}
END
  },
################################################################################

);

$all{$_}{lang} = $_ foreach keys %all;
my $all = [ values %all ];
various::set_default_values($all);
various::compute_snippet_lengths($all);
my $bounds = various::compute_snippet_lengths_bounds($all);
score::all($all, $bounds);

if ($ARGV[0] eq 'test') {
    shift @ARGV;
    if ($ARGV[0] eq '--snippet') {
        shift @ARGV; @snippets = shift @ARGV;
    }
    if ($ARGV[0] eq '--lang') {
        shift @ARGV; $all = [ $all{shift @ARGV} ];
    }
    test::all($all);
} else {
    html::all($all, $bounds);
    listing::all($all);
}


################################################################################
package test;

sub init {
    my $tmpdir = "$ENV{HOME}/tmp/.test-scriptometer";
    mkdir $tmpdir; -d $tmpdir or die "bad tmpdir\n";
    chdir $tmpdir or die '';
}

sub comment_file {
    my $f = "t.txt";
    local *F;
    open F, ">t.txt";
    print F "ab\nfoo#bar\n#foo\n# foo.bar\n";
    $f;
}

sub all {
    my ($all) = @_;
    init();
    one($_) foreach @$all;
}

sub one {
    my ($e) = @_;
    return if $e->{lang} eq 'merd'; # no working yet
    one_snippet($e, $_) foreach @snippets;
}
sub one_snippet {
    my ($e, $snippet) = @_;

    defined $e->{$snippet} or return;
    
    my @args;
    if ($snippet eq 'argv') {
        @args = qw(one two);
    } elsif ($snippet eq 'sed_in_place') {
        @args = comment_file();
    } elsif ($snippet eq 'grep') {
        @args = ('foo.bar', comment_file());
    }

    my $run;
    print STDERR "$e->{lang}: testing $snippet\n";

    if (my ($prog) = $e->{$snippet} =~ /^% (.*)/) {
        my $ret = system("$prog " . join(' ', @args));
        $ret == 0 or die "$e->{lang}: program $prog failed using run_file\n";
        return;
    }

    if ($e->{run_stdin} && !@args) {
        local *F;
        open F, "|$e->{run_stdin}";
        print F $e->{$snippet};
        close F or die "$e->{lang}: program $snippet failed using run_stdin\n";
        $run = 1;
    }

    if ($e->{run_cmdline} && !@args && $e->{lang} ne 'Php') {
        my @l = map { sprintf($_, $e->{$snippet}) } split ' ', $e->{run_cmdline};
        system(@l, @args) == 0 or die "$e->{lang}: program $snippet failed using run_cmdline\n";
        $run = 1;
    }
    if ($e->{run_file}) {
        my $file = $e->{filename} || do {
	    my ($extension) = $e->{file_extension} =~ /(\S+)/ or die "$e->{lang}: missing filename or file_extension\n";
	    "$snippet$extension";
        };
        local *F;
        open F, ">$file" or die '';
        print F $e->{$snippet};
        close F;
        my $s = sprintf $e->{run_file}, $file, $snippet;
        my $ret = system("$s " . join(' ', @args));
        unlink $file;
        unlink sprintf($e->{tmpfile}, $snippet) if $e->{tmpfile};
        $ret == 0 or die "$e->{lang}: program $snippet failed using run_file\n";
        $run = 1;
    }

    $run or die "$e->{lang}: missing run_stdin, run_cmdline or run_file\n";

}

################################################################################
package score;


sub general {
    (
     [ 20, 'compilation and execution in one command' => sub { $_[0]{run_file} !~ /&&/ } ],
     [ 15, '<a href="http://wombat.doc.ic.ac.uk/foldoc/foldoc.cgi?shebang">shebang</a> aware (#!)' => sub { $_[0]{shebang_aware} } ],
     [  5, 'program can be passed<br>on command line' => sub { $_[0]{run_cmdline} } ],
     [  5, 'interactive interpreter (<a href="http://en.wikipedia.org/wiki/Read-eval-print_loop">REPL</a>)' => sub { $_[0]{interactive_interpreter} } ],
     [  5, 'debugger' => sub { $_[0]{debugger} } ],
     [  5, 'full interpreter in debugger' => sub { $_[0]{interpreter_in_debugger} } ],
     [  2, 'execution tracer (a la "sh -x")' => sub { $_[0]{verbose_execution} } ],
    );
}

sub all {
    my ($all, $bounds) = @_;
    one($_, $bounds) foreach @$all;
    @$all = sort { $b->{score} <=> $a->{score} } @$all;
}

sub one {
    my ($e, $bounds) = @_;

    $e->{various_score} = 0;
    foreach (general()) {
        my ($points, $descr, $predicate) = @$_;
        $e->{various_score} += $points if $predicate->($e);
    }

    $e->{lengths_score} = 0;
    foreach my $snippet (@snippets) {
        my $n = $e->{lengths_scores}{$snippet};
        $e->{lengths_score} += 10 * ($bounds->{$snippet}{max} - $n) / ($bounds->{$snippet}{max} - $bounds->{$snippet}{min})
    }
    warn "$e->{lang} $e->{lengths_score}\n";
    $e->{lengths_score} = int ($e->{lengths_score} + 0.5);

    $e->{score} = $e->{various_score} + $e->{lengths_score};
}
sub max { my $n = shift; $_ > $n and $n = $_ foreach @_; $n }

################################################################################
package html;

my %snippet_lengths;

sub all {
    my ($all, $bounds) = @_;

    $\ = "\n";
    header();
    general($all);
    snippet_lengths_table($all, $bounds);
    information($all);
    print '<hr>';
    related_pages();
    credits();
    footer();
}

sub general {
    my ($all) = @_;

    print '<h1>Scriptometer Overall Scores</h1>';
    print '<table border="1"><tr><th></th><th>Score</th></tr>';
    foreach (@$all) {
	printf "<tr><td>%s</td><td align='right'>%s</td></tr>\n", 
	  format_title($_, www => 1), $_->{score};
    }
    print '</table>';

    print '<p><small>conflict of interest warning: the author of this page is also the author of
           merd, so some things may be unintentionally subconsciously biased.
           <p>merd is mostly vapourware and a dead project.</small>';

    print '<h1><a name="tools">Tools</a></h1>';
    print '<table border="1">';
    print '<tr><th></th><th>Implementation</th>', join('', map { 
        my ($points, $descr, $predicate) = @$_;
	"<th>$descr<br>($points points)</th>";
    } score::general()), '<th>Score</th></tr>';
    foreach my $e (@$all) {
	print "<tr>";

        print "<td>", format_title($e), "</td>";

        my $name = $e->{implementation};
        ($name, my $version) = ($1, $2) if $name =~ /(.*)\s(.*)/;
        if (my $www = $e->{implementation_www} || $e->{www}) {
            $name = qq(<a href="http://$www">$name</a>);
        }
        print "<td>$name<br>$version</td>";

	foreach (score::general()) {
	    my ($points, $descr, $predicate) = @$_;
	    print "<td align='center'>" . ($predicate->($e) ? 'X' : '&nbsp;') . '</td>';
	}
        print '<td>' . $e->{various_score} . '</td>';
	print '</tr>';
    }
    print '</table>';
}

sub information {
    my ($all) = @_;

    print '<h1>Collected Information</h1>';

    print "Here is the various information collected for each programming language";

    programs($all);

    print '<h2>Tools</h2>';
    foreach my $field ('run_stdin', 'run_cmdline', 'run_file', 'file_extension', 'interactive_interpreter', 'verbose_execution', 'debugger') {
        print "<h3>", various::to_english($field), "</h3>";
        print '<table border="1">';
        foreach (@$all) {
            exists $_->{$field} or next;
            print "<tr><td>$_->{lang}</td><td><pre>", quote($_->{$field}), "</pre></td></tr>";
        }
        print '</table>';
    }
}

sub programs {
    my ($all) = @_;

    print '<h2><a name="programs">Programs</a></h2>';
    foreach my $snippet (@snippets) {
        print qq(<h3><a name="$snippet">), various::to_english($snippet), "</a></h3>";
        print $snippets_comments{$snippet} || '';
        print '<table border="1" cellpadding="4">';
        foreach my $e (@$all) {
            exists $e->{$snippet} or next;
            my $s = $e->{$snippet};

            print STDERR "$e->{lang}:$snippet: bad line $_" foreach grep { length > 70 } split("\n", $s);

            print "<tr><td>$e->{lang}</td><td>", 
              $s ? "<pre>" . quote($s) . "</pre>" : "<small>" . quote('<empty>') . "</small>", 
              "</td></tr>";
        }
        print '</table>';
    }
}

sub snippet_lengths_table {
    my ($all, $bounds) = @_;

    print '<h1>Program Lengths by Language</h1>';
    print 'Typical SOP (Script-Oriented Programming) tasks';
    print '<p>';
    print '<br><small>(contiguous spaces count as one character)</small>';
    print '<br><small>(the length of the "smallest" program is partially removed in the length of other programs)</small>';
    print '<table border="1">';
    print '<tr><th></th>', join('', map {
	qq(<th><a href="#$_">) . various::to_english($_) . qq(</a></th>);
    } @snippets), '<th>Score</th></tr>';
    foreach my $e (@$all) {
	print '<tr><td>', format_title($e, listing => 1), '</td>';
    
	foreach my $snippet (@snippets) {
	    my ($min, $max) = ($bounds->{$snippet}{min}, $bounds->{$snippet}{max});

	    #print STDERR "$min $max\n";

            my $s = snippet_length($e, $snippet, $min, $max);
            print "<td>$s</td>";
        } 
        print '<td>' . $e->{lengths_score} . '</td>';
	print '</tr>';
    }
    print '</table>';
    print 'TD = "TODO"';
}

sub snippet_length {
    my ($e, $snippet, $min, $max) = @_;
    my ($s, $nb) = ('TD', 0x100);
    if (exists $e->{$snippet}) {
	$s  = length various::rationalize_snippet($e->{$snippet});
	$nb = ($e->{lengths_scores}{$snippet} - $min) / ($max - $min || 1) * 0x100;
    }
    #warn "$e->{lang}: $s $min $max -> $nb\n" if $snippet eq 'system';
    my ($a, $b) = map { $_ > 0xff ? 0xff : $_ < 0 ? 0 : $_ } (2 * $nb, 0xff - $nb);
    sprintf("<font color='#%02x%02x00'>$s</font>", $a, $b);
}

sub format_title {
    my ($e, %options) = @_;

    my $s = $e->{lang};
    # ($s) = $e->{implementation} =~ /(\S+)/ if $s eq 'C';
    $s = qq(<a href="http://$e->{www}">$s</a>) if $e->{www} && $options{www};
    $s = qq(<a href=") . url_quote($e->{lang}) . qq(.listing">$s</a>) if $options{listing};
    $s;
}

sub nbsps {
    my ($nb) = @_;
    $nb = 0 if $nb < 0;
    (
     '&nbsp; ' x ($nb / 2) . '&nbsp;' x ($nb % 2),
     ' &nbsp;' x ($nb / 2) . '&nbsp;' x ($nb % 2),
    );
}
sub url_quote {
    local $_ = $_[0];
    s/#/%23/g;
    $_;
}

sub header {
    print q<
<html>
  <head>
    <meta content="text/html; charset=utf-8"> 
    <style type="text/css">
      th { font-weight: normal; }
    </style>
    <title>Scriptometer: measuring the ease of SOP (Script-Oriented Programming) of programming languages</title>
  </head>

<body>

The Scriptometer tries to measure whether a programming language can be easily used for SOP (Script-Oriented Programming).
A script is here a command line program, mostly used in a terminal.
(more scripting stuff (GUI, web...) could be added...)
<p>
For this:
<ul>
<li>the <a href="#tools">programming environment</a> is checked: ability to compile and run in one command, REPL (Read-Eval-Print Loop)...

<li>some typical <a href="#programs">SOP tasks are written</a> in each programming language, and the length of the resulting program is measured.
<br>
There are few yet. I plan TODO:
<ul>
<li>ease of file access
<li>ease of converting between numbers and strings
</ul>

</ul>

<p>
Suggestions and comments <a href="mailto:pixel@rigaux.org"><b>welcome</b></a> (including spelling, grammatical and stylistic corrections :)
>;
}

sub related_pages {
print <<'EOF';
<p>
<h2>Related Pages</h2>

<h3>Small snippets</h3>
<ul>
<li><a href="http://www.99-bottles-of-beer.net/">99 Bottles of Beer</a>
<li><a href="http://ucsub.colorado.edu/~kominek/rot13/">rot13 in different languages</a> (string manipulation)
<li><a href="http://www.nyx.net/~gthompso/quine.htm">Quine</a> (self-reproducing code)
<li><a href="http://www.ntecs.de/old-hp/uu9r/lang/html/lang.en.html">various examples</a> (Michael Neumann)
</ul>

<h3>Various</h3>

<ul>
<li><a href="http://rigaux.org/language-study/syntax-across-languages.html">Syntax Across Languages</a>
<li><a href="http://shootout.alioth.debian.org/">The Great Computer Language Shootout</a> and <a href="http://dada.perl.it/shootout/">Win32 version</a> (benchmark comparison of a number of programming languages)
<li><a href="http://www.angelfire.com/tx4/cus/shapes/index.html">OO shape examples</a>
<li><a href="http://pleac.sf.net/">PLEAC</a> (Programming Language Examples Alike Cookbook)
<li><a href="http://page.mi.fu-berlin.de/~prechelt/Biblio/jccpprt_computer2000.pdf">An empirical comparison of C, C++, Java, Perl, Python, Rexx, and Tcl</a>
</ul>

EOF
}

sub credits {
print <<'EOF';
<p>
<h2>Credits</h2>
<ul>
<li>Daniel Weinreb (english corrections, suggestions)
<li>Bruce Williams (Ruby enhancements)
<li>Ken Anderson (suggested compile_what_must_be example)
<li>Jakub Travnik (suggested tcc)
<li>Joe Marshall (use logarithm for scoring)
<li>Neil Madden (Tcl enhancements)
<li>Martin Nilsson (Pike)
<li>Luiz Henrique de Figueiredo (Lua)
<li>Andreas Rottmann (Scheme enhancements)
<li>Scott Anderson (Java enhancements)
<li>Matt Shaw (Perl enhancements)
<li>Michael Scherer (Python enhancements)
<li>DH "crazyinsomniac" (Perl enhancements)
<li>Bob Hicks (Tcl enhancements)
<li>Damián Viano (sh enhancements)
<li>Kevin Scaldeferri (Perl enhancements)
<li>Chris Hostetter (Perl enhancements)
<li>Matthew Good (Python enhancements)
<li>Sander van Vliet (Java enhancements, C#)
<li>Shandy Brown (Python enhancements)
<li>Johan Vromans (Perl enhancements)
<li>Jason Matheson (PHP)
<li>Thomas Zander (Java enhancements)
<li>John Goerzen (OCaml enhancements)
<li>Jerrad Pierce (Perl enhancements)
<li>Olof Johansson (VBScript)
<li>Heilig (Cece) Szabolcs (PHP)
<li>Túri Gábor (PHP enhancements)
<li>Daniel Lowe (Common Lisp)
<li>Anthony Borla (REXX, Prolog)
<li>Michael Sloan (Haskell enhancements)
<li>Tomek Sowiński (D)
<li>Jari-Matti Mäkelä (Scala)
</ul>
EOF
}

sub footer {
    print q<
<hr>
<address><a href="mailto:pixel@rigaux.org">Pixel</a></address>
This document is licensed under <a href="http://www.gnu.org/copyleft/fdl.html">GFDL</a> (GNU Free Documentation License).
<p>Automatically generated from this <a href="index_pl.txt">file</a>.

</body>
</html>
>;
}

sub quote {
    local $_ = $_[0];
    if (!/<a/ && !/<pre>/) {
	s/&/&amp;/g;
        s/</&lt;/g;
        s/>/&gt;/g;
        s/&lt;br&gt;/<br>/g; # put back <br>
    }
    $_;
}

sub max { my $n = shift; $_ > $n and $n = $_ foreach @_; $n }


################################################################################
package listing;

sub all {
    my ($all) = @_;

    foreach my $e (@$all) {
        local *F;
        open F, "> $e->{lang}.listing";
        printf F $e->{comment} . "\n\n", '-*- ' . $e->{emacs_mode} . ' -*-';;

        foreach my $snippet (@snippets) {
            my $s = $e->{$snippet} or next;

            printf F $e->{comment} . "\n", various::to_english($snippet);
            printf F $e->{comment} . "\n", $snippets_comments{$snippet} || '';
            print F $s, "\n";
        }
        print F
    }
}

################################################################################
package various;

sub compute_snippet_lengths {
    my ($all) = @_;
    foreach my $lang (@$all) {
	my $smallest;
	foreach my $snippet (@snippets) {
            exists $lang->{$snippet} or next;
            (my $s = rationalize_snippet($lang->{$snippet})) =~ s/\s+/ /gs;
            my $n = length $s;
	    if ($snippet eq 'smallest') {
		$smallest = $n;
	    } else {
		$n -= $smallest / 2;
	    }
            $lang->{lengths_scores}{$snippet} = 4 * log($n + 1);
        }
    }
}

sub rationalize_snippet {
    my ($s) = @_;
    $s =~ s/^% (\w+)\s+//;
    $s =~ s/e? '(.*)'$/ $1/;
    $s;
}

sub compute_snippet_lengths_bounds {
    my ($all) = @_;
    my %bounds = map {
        my $snippet = $_;
        my @l = sort { $a <=> $b } map { 
            my $n = $_->{lengths_scores}{$snippet};
            defined $n ? $n : ();
        } @$all;
        
        foreach (grep { ! exists $_->{$snippet} } @$all) {
            $_->{lengths_scores}{$snippet} = $l[-1];
        }

        $snippet => { min => $l[0], max => $l[-1], median => $l[$#l / 2] }
    } @snippets;
    \%bounds;
}

sub set_default_values {
    my ($all) = @_;
    foreach (@$all) {
        $_->{comment} ||= '# %s';
        $_->{emacs_mode} ||= lc($_->{lang});
    }
}

sub to_english {
    local ($_) = @_;
    s/run_/run_from_/;
    s/_/ /g;
    $_;
}
