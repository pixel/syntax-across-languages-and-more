? l
? various/pure-data-structures-advantages
? various/python-faq.html
? various/python-vs-perl-way-of-thinking.txt
? various/python-vs-perl.html
? various/python-vs-perl.txt
? various/static-typing.txt
Index: diagram.html.m4
===================================================================
RCS file: /cvsroot/language-study/language-study/diagram.html.m4,v
retrieving revision 1.6
diff -u -r1.6 diagram.html.m4
--- diagram.html.m4	28 Nov 2002 13:58:25 -0000	1.6
+++ diagram.html.m4	16 May 2003 11:05:31 -0000
@@ -27,9 +27,10 @@
 
 Other diagrams:
 <ul>
-<li>here is a nice <a href="http://perso.wanadoo.fr/levenez/lang">one</a>. 
+<li>here is a nice <a href="http://www.levenez.com/lang/">one</a>. 
 <li>and for the early history, this <a href="http://www.epemag.com/zuse/Images/43ab.gif">one</a> is light but well
 structured. 
+<li><a href="http://www.digibarn.com/collections/posters/tongues/">Mother Tongues of Computer Languages</a>
 <li>a <a href="http://www.dreamsongs.com/NewFiles/Hopl2Slides.pdf">Lisp centered one</a>.
 </ul>
 <p>
Index: language-study.txt
===================================================================
RCS file: /cvsroot/language-study/language-study/language-study.txt,v
retrieving revision 1.36
diff -u -r1.36 language-study.txt
--- language-study.txt	29 Oct 2002 11:20:31 -0000	1.36
+++ language-study.txt	16 May 2003 11:05:31 -0000
@@ -76,7 +76,6 @@
   proposition for haskell (talks about other languages too) http://www.cl.cam.ac.uk/~kw217/research/papers.html#Wansbrough99:Macros
   Meta Haskell (very good!) http://www.research.microsoft.com/~simonpj/papers/meta-haskell/
 
-
 ********************************************************************************
 Value sharing, aliasing, references/pointers
 
@@ -219,6 +218,7 @@
   (bound checking, null pointers dereference, bad down-casting...)
   
   linux kernel source checking http://www.stanford.edu/~engler http://www.stanford.edu/~engler/mc-osdi.ps
+             http://www.stanford.edu/~engler/p401-xie.pdf
 
   Soft Typing http://merd.net/types.html#soft_typing
 
@@ -431,10 +431,13 @@
 
   ****************************************
   D:
-  http://www.digitalmars.com/d/  
+  http://www.digitalmars.com/d/
   -- no boolean type
-  -- no templates/genericity
-  - C-like comments still not nested
+  -- templates a la Ada => limited genericity and no container type (except for the builtin "array")
+  - fall through cases in a "switch"
+  + /+ ... +/ comments which can nest
+  anonymous functions and nested functions but no closures
+  interfaces a la Java, no multiple-inheritance
   'xxx' strings a la perl  
   some reflexivity
        
@@ -1098,3 +1101,22 @@
 Pattern Transfer: bridging the gap between theory and practice http://citeseer.nj.nec.com/461895.html
 
 Composable and Compilable Macros: You Want it When? http://www.cs.utah.edu/plt/publications/macromod.pdf
+
+strongtalk http://www.cs.ucsb.edu/projects/strongtalk/pages/index.html
+
+Survey of OOPL http://www.rescomp.berkeley.edu/~hossman/cs263/paper.html
+http://www.nongnu.org/needle/
+
+macros http://citeseer.nj.nec.com/brabrand00growing.html
+
+CDuce
+
+Copying, Cloning, and Marshalling in .NET http://www.ondotnet.com/pub/a/dotnet/2002/11/25/copying.html
+
+Checked and unchecked exceptions in Java http://www.octopull.demon.co.uk/java/ExceptionalJava.html
+
+XML Is Too Hard For Programmers http://tbray.org/ongoing/When/200x/2003/03/16/XML-Prog
+
+How to make a fast curry: push/enter vs eval/apply http://www.research.microsoft.com/~simonpj/papers/eval-apply/
+
+Java 1.5 new features http://java.sun.com/features/2003/05/bloch_qa.html
\ No newline at end of file
Index: syntax-across-languages.html.pl
===================================================================
RCS file: /cvsroot/language-study/language-study/syntax-across-languages.html.pl,v
retrieving revision 1.155
diff -u -r1.155 syntax-across-languages.html.pl
--- syntax-across-languages.html.pl	28 Jan 2003 12:52:43 -0000	1.155
+++ syntax-across-languages.html.pl	16 May 2003 11:05:32 -0000
@@ -2427,6 +2427,27 @@
   '/Rounding/Floor/Ceiling' => "Ada",
  ],
 
+ 'bitwise operators' =>
+ [
+  'and / or' =>
+  [
+   '& / |' => "C C++ Ruby Python Perl",
+   'land / lor' => "OCaml",
+  ],
+
+  'negation' => 
+  [
+   '~' => "C C++ Ruby Python Perl",
+   'lnot' => "OCaml",
+  ],
+
+  'left shift / right shift' => 
+  [
+   '<< / >>' => "C C++ Ruby Python Perl",
+   'lsl / lsr or asr' => "OCaml",
+  ],
+ ],
+
 ],
 
 'Threads' => 
Index: usenet-traffic-ranking/index.html
===================================================================
RCS file: /cvsroot/language-study/language-study/usenet-traffic-ranking/index.html,v
retrieving revision 1.10
diff -u -r1.10 index.html
--- usenet-traffic-ranking/index.html	15 May 2002 13:11:36 -0000	1.10
+++ usenet-traffic-ranking/index.html	16 May 2003 11:05:32 -0000
@@ -79,6 +79,7 @@
 <h2>Similar Pages</h2>
 <ul>
 <li><a href="http://www.norvig.com/Lisp-retro.html">on Lisp</a>
+<li><a href="http://www.cs.berkeley.edu/~flab/languages.html">SourceForge projects</a>
 </ul>
 
 <hr>
