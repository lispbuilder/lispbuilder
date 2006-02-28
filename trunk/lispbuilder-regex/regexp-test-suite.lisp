;; *****************************************************************************
;; FILE
;; Name             : regexp-test-suite.cl
;; Date             : 2002-03-01
;; Author           : Sébastien SAINT-SEVIN
;; Purpose          : testing module for regular expressions
;;
;; Modified by KMP to run under both rightmost and leftmost matches.
;; -----------------------------------------------------------------------------

(in-package :LISPBUILDER-REGEX-TEST)

(allow-nonregister-groups)
(allow-nongreedy-quantifiers)
(allow-rangematch)
(allow-backmatch)

;; rightmost matches are more canonical, but much slower.
;(registers-match-rightmost t)
;(pushnew :regex-right *features*)

;; leftmost matches are usually a lot faster with this engine.
(registers-match-rightmost nil)
(setq *features* (remove :regex-right *features*))

(defparameter *regexp-tests*
   '(
      ;; *********************************************************************
      ;; <test> ::= (<string>  <pattern>  <compile-p>  <matched-p>  <results>)
      ;; <results> ::= (<global match> <group_capture>*)
      ;; ---------------------------------------------------------------------


      ;; *******************************************************
      ;; the tests that follows are from:
      ;; -------------------------------------------------------
      ;; (c) Sudhir Shenoy, 1996
      ;;
      ;; The tests here are from:
      ;;
      ;; (a) Tom Lord's GNU rx package
      ;; (b) from the Zebu parser generator package
      ;;     (modified to use new syntax)
      ;; -------------------------------------------------------
      ;; All have been slightly modified to follow the syntax
      ;; I use in this module - Sébastien Saint-Sevin, 2002
      ;; -------------------------------------------------------

      ("a*a*"           "aaaaaa"        t t       ("aaaaaa"))
      ("a*a*a*"         "aaaaaa"        t t       ("aaaaaa"))
      ("a*a*a*a*"       "aaaaaa"        t t       ("aaaaaa"))
      ("a*a*a*a*a*"     "aaaaaa"        t t       ("aaaaaa"))
      ("a*a*a*a*a*a*"   "aaaaaa"        t t       ("aaaaaa"))
      ("a*a*a*a*a*a*a*" "aaaaaa"        t t       ("aaaaaa"))

      (""               ""              nil nil   ())
      ("b{0,6}"         ""              t t       (""))
      ("ab{0,0}c"       "abc"           t nil     ())
      ("ab{1,1}c"       "abbc"          t nil     ())
      ("ab{3,7}c"       "abbbbbbbbc"    t nil     ())
      ("ab{3,7}c"       "abbbbbbbbbc"   t nil     ())
      ("ab{3,7}c"       "abbbbbbbbbbc"  t nil     ())
      ("ab{3,7}c"       "abbbbbbbbbbbc" t nil     ())
      ("b{2,7}"         "bb"            t t       ("bb"))
      ("b{1,6}"         ""              t nil     ())
      ("b{1,6}"         "b"             t t       ("b"))
      ("b{2,7}"         "b"             t nil     ())
      ("ab{0,7}c"       "ac"            t t       ("ac"))
      ("ab{1,7}c"       "abc"           t t       ("abc"))
      ("ab{2,7}c"       "abbc"          t t       ("abbc"))
      ("ab{3,7}c"       "abbbc"         t t       ("abbbc"))
      ("ab{3,7}c"       "abbbbc"        t t       ("abbbbc"))
      ("ab{3,7}c"       "abbbbbc"       t t       ("abbbbbc"))
      ("ab{3,7}c"       "abbbbbbc"      t t       ("abbbbbbc"))
      ("ab{3,7}c"       "abbbbbbbc"     t t       ("abbbbbbbc"))
      ("ab{3,7}c"       "abbbbbbbbc"    t nil     ())
      ("ab{3,7}c"       "abbc"          t nil     ())
      ("ab{3,7}c"       "abc"           t nil     ())

      ("(a|b)*c|(a|ab)*c" "xc"          t t       ("c" "" ""))
      ("(a)*"           "b"             t t       ("" ""))
      ("(..)*(...)*"    "a"             t t       ("" "" ""))
      ;;the following fails coz sshenoy's engine is a posix NFA
      ;;("(..)*(...)*" "abc" t t ("abc" "" "abc"))
      ("(..)*(...)*"    "abc"           t t       ("ab" "ab" ""))

      ("^"              ""              t t       (""))
      ("$"              ""              t t       (""))
      ("^$"             ""              t t       (""))
      ("^a$"            "a"             t t       ("a"))
      ("abc"            "abc"           t t       ("abc"))
      ("abc"            "xbc"           t nil     ())
      ("abc"            "axc"           t nil     ())
      ("abc"            "abx"           t nil     ())
      ("abc"            "xabcy"         t t       ("abc"))
      ("abc"            "ababc"         t t       ("abc"))
      ("ab*c"           "abc"           t t       ("abc"))
      ("ab*bc"          "abc"           t t       ("abc"))
      ("ab*bc"          "abbc"          t t       ("abbc"))
      ("ab*bc"          "abbbbc"        t t       ("abbbbc"))
      ("ab+bc"          "abbc"          t t       ("abbc"))
      ("ab+bc"          "abc"           t nil     ())
      ("ab+bc"          "abq"           t nil     ())
      ("ab+bc"          "abbbbc"        t t       ("abbbbc"))
      ("ab?bc"          "abbc"          t t       ("abbc"))
      ("ab?bc"          "abc"           t t       ("abc"))
      ("ab?bc"          "abbbbc"        t nil     ())
      ("ab?c"           "abc"           t t       ("abc"))
      ("^abc$"          "abc"           t t       ("abc"))
      ("^abc$"          "abcc"          t nil     ())
      ("^abc"           "abcc"          t t       ("abc"))
      ("^abc$"          "aabc"          t nil     ())
      ("abc$"           "aabc"          t t       ("abc"))
      ("^"              "abc"           t t       (""))
      ("$"              "abc"           t t       (""))
      ("a.c"            "abc"           t t       ("abc"))
      ("a.c"            "axc"           t t       ("axc"))
      ("a.*c"           "axyzc"         t t       ("axyzc"))
      ("a.*c"           "axyzd"         t nil     ())

      ("a[bc]d"         "abc"           t nil     ())
      ("a[bc]d"         "abd"           t t       ("abd"))
      ("a[b-d]e"        "abd"           t nil     ())
      ("a[b-d]e"        "ace"           t t       ("ace"))
      ("a[b-d]"         "aac"           t t       ("ac"))
      ("a[-b]"          "a-"            t t       ("a-"))
      ("a[b-]"          "a-"            t t       ("a-"))

      ;;*** following is supposed to compile but what should it match ?
      ;;*** I don't know and that is why I reject the pattern.
      ;("a[b-a]"  "-"       t    NIL  NIL)
      ;("a[]b"    "-"       NIL  NIL  NIL)
      ;("a[" "-"       NIL  NIL  NIL)
      ;("a]" "a]"      t    "a]" NIL)
      ;("a[]]b"   "a]b"          t    "a]b"     NIL)

      ("a[^bc]d"        "aed"           t t       ("aed"))
      ("a[^bc]d"        "abd"           t nil     ())
      ("a[^-b]c"        "adc"           t t       ("adc"))
      ("a[^-b]c"        "a-c"           t nil     ())
      ("a[^\\]b]c"      "a]c"           t nil     ())
      ("a[^\\]b]c"      "adc"           t t       ("adc"))
      ("ab|cd"          "abc"           t t       ("ab"))
      ("ab|cd"          "abcd"          t t       ("ab"))

      ;;FAILED ("()ef"    "def"          t t   ("ef" ""))
      ;;FAILED ("()*"     "-"            t t   ("" ""))
      ;;FAILED ("*a"      "-"            t t   (""))
      ("^*"             "-"             t t       (""))
      ("$*"             "-"             t t       (""))
      ;;FAILED ("(*)b"    "-"       t t      ("" ""))
      ("$b"             "b"             t nil     ())

      ("a\\(b"          "a(b"           t t       ("a(b"))
      ("a\\(*b"         "ab"            t t       ("ab"))
      ("a\\(*b"         "a((b"          t t       ("a((b"))
      ("a\\\\b"         "a\\b"          t t       ("a\\b"))
      ("(abc"           "-"             nil nil   ())
      ("((a))"          "abc"           t t       ("a" "a" "a"))
      ("(a)b(c)"        "abc"           t t       ("abc" "a" "c"))
      ("a+b+c"          "aabbabc"       t t       ("abc"))

      ("a**"            "-"             t t       (""))
      ("a*?"            "-"             t t       (""))
      ("(a*)*"          "-"             t t       ("" ""))
      ("(a*)+"          "-"             t t       ("" ""))
      ("(a|)*"          "-"             t t       ("" ""))
      ("(a*|b)*"        "-"             t t       ("" ""))
      #+:regex-right("(a+|b)*"        "ab"            t t       ("ab" "b"))
      #-:regex-right("(a+|b)*"        "ab"            t t       ("ab" "a"))
      #+:regex-right("(a+|b)+"        "ab"            t t       ("ab" "b"))
      #-:regex-right("(a+|b)+"        "ab"            t t       ("ab" "a"))
      ("(a+|b)?"        "ab"            t t       ("a"  "a"))
      ("[^ab]*"         "cde"           t t       ("cde"))
      ("(^)*"           "-"             t t       ("" ""))
      ("(ab|)*"         "-"             t t       ("" ""))
      (")("             "-"             nil nil   ())
      (""               "abc"           nil nil   ())
      ("abc"            ""              t nil     ())
      ("a*"             ""              t t       (""))
      #+:regex-right("([abc])*d"      "abbbcd"        t t       ("abbbcd" "c"))
      #-:regex-right("([abc])*d"      "abbbcd"        t t       ("abbbcd" "a"))
      ("([abc])*bcd"    "abcd"          t t       ("abcd"   "a"))
      ("a|b|c|d|e"      "e"             t t       ("e"))
      ("(a|b|c|d|e)f"   "ef"            t t       ("ef" "e"))
      ("((a*|b))*"      "-"             t t       ("" "" ""))
      ("abcd*efg"       "abcdefg"       t t       ("abcdefg"))
      ("ab*"            "xabyabbbz"     t t       ("ab"))
      ("ab*"            "xayabbbz"      t t       ("a"))
      ("(ab|cd)e"       "abcde"         t t       ("cde" "cd"))
      ("[abhgefdc]ij"   "hij"           t t       ("hij"))
      ("^(ab|cd)e"      "abcde"         t nil     ())
      ("(abc|)ef"       "abcdef"        t t       ("ef" ""))
      ("(a|b)c*d"       "abcd"          t t       ("bcd" "b"))
      ("(ab|ab*)bc"     "abc"           t t       ("abc" "a"))
      ("a([bc]*)c*"     "abc"           t t       ("abc"  "bc"))
      ("a([bc]*)(c*d)"  "abcd"          t t       ("abcd" "bc" "d"))
      ("a([bc]+)(c*d)"  "abcd"          t t       ("abcd" "bc" "d"))
      ("a([bc]*)(c+d)"  "abcd"          t t       ("abcd" "b" "cd"))
      ("a[bcd]*dcdcde"  "adcdcde"       t t       ("adcdcde"))
      ("a[bcd]+dcdcde"  "adcdcde"       t nil     ())
      ("(ab|a)b*c"      "abc"           t t       ("abc"  "ab"))
      ("((a)(b)c)(d)"   "abcd"          t t       ("abcd" "abc" "a" "b" "d"))
      ("[a-zA-Z_][a-zA-Z0-9_]*" "alpha" t t       ("alpha"))
      ("^a(bc+|b[eh])g|.h$"     "abh"   t t       ("bh" ""))
      ("(bc+d$|ef*g.|h?i(j|k))" "effgz" t t       ("effgz" "effgz" ""))
      ("(bc+d$|ef*g.|h?i(j|k))" "ij"    t t       ("ij" "ij" "j"))
      ("(bc+d$|ef*g.|h?i(j|k))" "effg"  t nil     ())
      ("(bc+d$|ef*g.|h?i(j|k))" "bcdd"  t nil     ())
      ("(bc+d$|ef*g.|h?i(j|k))" "reffgz" t t      ("effgz" "effgz" ""))


      ("((((((((((a))))))))))"  "a"       t t     ("a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a"))
      ("(((((((((a)))))))))"    "a"       t t     ("a" "a" "a" "a" "a" "a" "a" "a" "a" "a"))
      ("multiple words of text" "uh-uh"   t nil   ())
      ("multiple words"         "multiple words, yeah" t t ("multiple words"))
      ("(.*)c(.*)"              "abcde"   t t     ("abcde" "ab" "de"))
      ("\\((.*), (.*)\\)"       "(a, b)"  t t     ("(a, b)" "a" "b"))
      ("[k]"                    "ab"      t nil   ())
      ("abcd"                   "abcd"    t t     ("abcd"))
      ("a(bc)d"                 "abcd"    t t     ("abcd" "bc"))
      ("a[-]?c"                 "ac"      t t     ("ac"))
      ("a[-]?c"               "ac"     t t     ("ac"))
      ("a[-]?c"               "ac"      t t     ("ac"))
      ("[ -~]*"                 "abc"     t t     ("abc"))
      ("[ -~ -~]*"              "abc"     t t     ("abc"))
      ("[ -~ -~ -~]*"           "abc"     t t     ("abc"))
      ("[ -~ -~ -~ -~]*"        "abc"     t t     ("abc"))
      ("[ -~ -~ -~ -~ -~]*"     "abc"     t t     ("abc"))
      ("[ -~ -~ -~ -~ -~ -~]*"  "abc"     t t     ("abc"))
      ("[ -~ -~ -~ -~ -~ -~ -~]*" "abc"   t t     ("abc"))
      ;;
      ;; Tests from from the Zebu package (originally for nregex.lisp)
      ;;
      ("(na)x+"                 "naxna"      t t     ("nax" "na"))
      ("(na)x+na"               "naxna123"   t t     ("naxna" "na"))
      ("(na)x+"                 "naxxos"     t t     ("naxx" "na"))
      ("(na)x+"                 "naxos"      t t     ("nax" "na"))
      ("(na)x+"                 "naos"       t nil   ())
      ("(na)x*"                 "naxxos"     t t     ("naxx" "na"))
      ("(na)x*"                 "naxos"      t t     ("nax" "na"))
      ("(na)x*"                 "naos"       t t     ("na" "na"))
      ("[0-9]+"                 "123ab"      t t     ("123"))
      ("[a-zA-Z]+"              "aAbb123"    t t     ("aAbb"))
      ("[0-9a-z]+"              "1234&&*"    t t     ("1234"))
      ("[0-9a-z]+"              "1234a&&*"   t t     ("1234a"))
      ("[0-9a-zA-Z]+"           "a1234a"     t t     ("a1234a"))
      ("[0-9a-zA-Z&]+"          "aAbb123&&*" t t     ("aAbb123&&"))
      ("[0-9]+\\.[0-9]*"        "0.123cm"    t t     ("0.123"))
      ;    ("{[^}
      ;]*}" "{M.D. Harrison and A. Monk (Ed.)} \n\t foo: 2"
      ;T    "{M.D. Harrison and A. Monk (Ed.)}"     NIL)
      ;    ("{[^}
      ;]*}" "{M.D. Harrison and
      ;A. Monk (Ed.)} \n\t foo: 2"   t    NIL  NIL)
      ;    ("{[^}
      ;]*}"
      ;     "{M.D. Harrison and {A. Monk} (Ed.)} \n\t foo: 2"
      ;     t    "{M.D. Harrison and {A. Monk}"     NIL)

      ("ca?r"          "car"          t t   ("car"))
      ("ca?r"          "cr"           t t   ("cr"))
      ("c[ad]+r"       "caaar"        t t   ("caaar"))
      ("c[ad]+r"       "caaar aa1"    t t   ("caaar"))
      ("c[ad]+r$"      "caaar"        t t   ("caaar"))
      (".*"            ""             t t   (""))
      (".*"            "aa"           t t   ("aa"))
      ("c[ad]?r"       "cr"           t t   ("cr"))
      ("c[ad]?r"       "car"          t t   ("car"))
      ("c[ad]?r"       "cdr"          t t   ("cdr"))
      ("c[0-9]?r"      "cr"           t t   ("cr"))
      ("c[0-9]?r"      "c9rxx"        t t   ("c9r"))
      ("c[0-9]?r"      "crxx"         t t   ("cr"))
      ("a|b"           "a"            t t   ("a"))
      ("ab.yz"         "ab yz"        t t   ("ab yz"))
      ;("ab.yz"   "ab
      ;yz"                 t t   ("ab
      ;yz"))
      ("(abc){1,2}"    "abcabc"       t t   ("abcabc" "abc"))
      ;("(abc){1,2}x*(def)y*def" "abcabcxxxxdefyyyyyyydef$%%%%%"
      ;     t    "abcabcxxxxdefyyyyyyydef"     #("abc" "def"))
      ("a|bc*"         "a"            t t   ("a"))
      ("[A-Z]+"        "ABCY"         t t   ("ABCY"))
      ("[0-9]+\\.[0-9]*(e[+-]?[0-9]+)"    "12.3e4  k"    t t   ("12.3e4" "e4"))
      ("[0-9]+\\.[0-9]*(e[+-]?[0-9]+)"    "12.3e-4  k"   t t   ("12.3e-4" "e-4"))
      ("[0-9]+\\.[0-9]*(e[+-]?[0-9]+)?"   "12.3  k"      t t   ("12.3" ""))
      ;;
      ;; The Gadaffi tests
      ;; Note that the first group matches NULL because it is always sucked
      ;; up by the preceding .* in case of a successful match.
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muammar Qaddafi"       t t ("Muammar Qaddafi" "" "dd"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Mo'ammar Gadhafi"      t t ("Mo'ammar Gadhafi" "" "dh"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muammar Kaddafi"       t t ("Muammar Kaddafi" "" "dd"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muammar Qadhafi"       t t ("Muammar Qadhafi" "" "dh"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Moammar El Kadhafi"    t t ("Moammar El Kadhafi" "" "dh"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muammar Gadafi"        t t ("Muammar Gadafi" "" "d"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Mu'ammar al-Qadafi"    t t ("Mu'ammar al-Qadafi" "" "d"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Moamer El Kazzafi"     t t ("Moamer El Kazzafi" "" "zz"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Moamar al-Gaddafi"     t t ("Moamar al-Gaddafi" "" "dd"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Mu'ammar Al Qathafi"   t t ("Mu'ammar Al Qathafi" "" "th"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muammar Al Qathafi"    t t ("Muammar Al Qathafi" "" "th"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Mo'ammar el-Gadhafi"   t t ("Mo'ammar el-Gadhafi" "" "dh"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Moamar El Kadhafi"     t t ("Moamar El Kadhafi" "" "dh"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muammar al-Qadhafi"    t t ("Muammar al-Qadhafi" "" "dh"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Mu'ammar al-Qadhdhafi" t t ("Mu'ammar al-Qadhdhafi" "" "dh"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Mu'ammar Qadafi"       t t ("Mu'ammar Qadafi" "" "d"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Moamar Gaddafi"        t t ("Moamar Gaddafi" "" "dd"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Mu'ammar Qadhdhafi"    t t ("Mu'ammar Qadhdhafi" "" "dh"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muammar Khaddafi"      t t ("Muammar Khaddafi" "" "dd"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muammar al-Khaddafi"   t t ("Muammar al-Khaddafi" "" "dd"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Mu'amar al-Kadafi"     t t ("Mu'amar al-Kadafi" "" "d"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muammar Ghaddafy"      t t ("Muammar Ghaddafy" "" "dd"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muammar Ghadafi"       t t ("Muammar Ghadafi" "" "d"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muammar Ghaddafi"      t t ("Muammar Ghaddafi" "" "dd"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muamar Kaddafi"        t t ("Muamar Kaddafi" "" "dd"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muammar Quathafi"      t t ("Muammar Quathafi" "" "th"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muammar Gheddafi"      t t ("Muammar Gheddafi" "" "dd"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Muamar Al-Kaddafi"     t t ("Muamar Al-Kaddafi" "" "dd"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Moammar Khadafy "      t t ("Moammar Khadafy" "" "d"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Moammar Qudhafi"       t t ("Moammar Qudhafi" "" "dh"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Mu'ammar al-Qaddafi"   t t ("Mu'ammar al-Qaddafi" "" "dd"))
      ("M[ou]'?am+[ae]r .*([AEae]l[- ])?[GKQ]h?[aeu]+([dtz][dhz]?)+af[iy]"
       "Mulazim Awwal Mu'ammar Muhammad Abu Minyar al-Qadhafi" t t
       ("Mu'ammar Muhammad Abu Minyar al-Qadhafi" "" "dh"))
      ;;
      ;; tests involving back-refs
      #+:regex-right("((a|b{10,11})|(b))*-\\2"     "aaab-a"   t t ("aaab-a" "b" "a" "b"))
      #-:regex-right("((a|b{10,11})|(b))*-\\2"     "aaab-a"   t t ("aaab-a" "a" "a" "b"))
      ("(a)*-\\1"                    "aaa-a"    t t ("aaa-a" "a"))
      ("(a)*-\\1b"                   "aaa-b"    t t ("-b" ""))
      ("([xyz])(-\\2)"               "x-y"      t nil ())
      ("(([xyz])(-\\2))"             "x-y"      t nil ())
      ("(([xyz])(-\\2)*)*"           "x-y"      t t ("x" "x" "x" ""))
      ("(([xyz])(-\\2)*)*"           "x-"       t t ("x" "x" "x" ""))
      #+:regex-right("(([xyz])(-\\2)*)*"           "xy-yz-y"  t t ("xy-yz" "z" "z" "-y"))
      ;; kmp haven't fixed this one yet...
      ;#-:regex-right("(([xyz])(-\\2)*)*"           "xy-xz-x"  t t ("xy-xz" "z" "z" "-y"))

      ;; kmp -- this *should* match
      ; ("((.*)\\1)+"                  "xxxxxx"   t nil ())
      ; for rightmost register match:
      #+:regex-right("((.*)\\1)+"                  "xxxxxx"   t t ("xxxxxx" "" ""))
      ; for leftmost register match:
      #-:regex-right("((.*)\\1)+"                  "xxxxxx"   t t ("xxxxxx" "xxxxxx" "xxx"))

      ("(a*)\\1\\1(a*)\\2\\2\\2"     "aaaaaa"   t t ("aaaaaa" "aa" ""))
      ("(a*)(a*)\\1\\2"              "aaaa"     t t ("aaaa" "aa" ""))
      ("(a*)\\1(a*)\\2\\2"           "aaaa"     t t ("aaaa" "aa" ""))
      ("(a*)\\1\\1(a*)"              "aaaaaa"   t t ("aaaaaa" "aa" ""))
      ("(a*)\\1\\1(a*)\\2"           "aaaaaa"   t t ("aaaaaa" "aa" ""))
      ("(a*)\\1\\1(a*)\\2\\2"        "aaaaaa"   t t ("aaaaaa" "aa" ""))
      ("(.*)\\1\\1(.*)\\2\\2\\2"     "aaaaaa"   t t ("aaaaaa" "aa" ""))
      ;;the following fails coz sshenoy's engine is a posix NFA
      ;("(.*)\\1\\1(.*)\\2\\2\\2"     "aaaaaaa"  t t ("aaaaaaa" "a" "a"))
      ("(.*)\\1\\1(.*)\\2\\2\\2"     "aaaaaaa"  t t ("aaaaaa" "aa" ""))
      ("(.*)\\1\\1(.*)\\2\\2\\2"     "aaaaaa"   t t ("aaaaaa" "aa" ""))
      ;;the following fails coz sshenoy's engine is a posix NFA
      ;("(.*)\\1\\1(.*)\\2\\2\\2"     "aaaaa"    t t ("aaaa" "" "a"))
      ("(.*)\\1\\1(.*)\\2\\2\\2"     "aaaaa"    t t ("aaa" "a" ""))
      ("(.*)\\1\\1"                  "aaa"      t t ("aaa" "a"))
      #+:regex-right("(.*)*\\1"                    "xx"       t t ("xx" ""))
      #-:regex-right("(.*)*\\1"                    "xx"       t t ("xx" "x"))
      ("(....).*\\1"                 "beriberi" t t ("beriberi" "beri"))
      ;    ;;
      ;    ;; Some tests for class matches (my own)
      ;    ;;
      ;    ("[[:alpha:]_][[:alnum:]_]*" "c_identifier" t "c_identifier" NIL)
      ;    ("[[:xdigit:]]*"     "12aBcD89" t   "12aBcD89" NIL)
      ;    ;; In the following pattern, because :] is missing, the pattern is
      ;    ;; interpreted as an ordinary range
      ;    ("[[:xdigit]+"  "0[x:dig" t    "[x:dig" NIL)))



      ;; *******************************************************
      ;; the tests that follows are from:
      ;; -------------------------------------------------------
      ;; Sébastien Saint-Sevin, 2002
      ;; -------------------------------------------------------

      ;; some basics
      ;; -----------
      (".*"             "aa"        t t ("aa"))
      (".+"             "aa"        t t ("aa"))


      ;; anchor
      ;; ------


      ;; alternate
      ;; ---------
      ("(hello|man|)"   ""          t t ("" ""))
      ("(a+|b)"         "aaa"       t t ("aaa" "aaa"))
      ("(a+|b)"         "b"         t t ("b" "b"))


      ;; character classes
      ;; -----------------
      ("[abc]{1,3}"       "bcaa"    t t ("bca"))


      ("a[\\-]?c"         "ac"      t t ("ac"))
      ("a[\\-]?c"         "a-c"     t t ("a-c"))
      ("a[-]?c"           "ac"      t t ("ac"))
      ("a[-]?c"           "a-c"     t t ("a-c"))
      ("a[-b]?c"          "abc"     t t ("abc"))
      ("a[b-]?c"          "acc"     t t ("ac"))

      ;    "a[\\[]c"
      ;    "a[\\^]c"
      ;    "a[\\]]c"


      ;    ("a[^\\-]?c"         "ac"      t t ("ac"))
      ;    ("a[^\\-]?c"         "a-c"     t nil ())
      ;    ("a[^-]?c"           "ac"      t t ("ac"))
      ;    ("a[^-]?c"           "a-c"     t t ("a-c"))
      ;    ("a[^-b]?c"          "abc"     t t ("abc"))
      ;    ("a[^b-]?c"          "acc"     t t ("ac"))

      ;    "a[^\\[]c"
      ;    "a[^\\^]c"
      ;    "a[^\\]]c"


      ;; posix character classes
      ;; -----------------------


      ;; greedy quantifiers
      ;; ------------------
      ("a*"           "aaaa"        t t        ("aaaa"))
      ("a+"           "aaaa"        t t        ("aaaa"))
      ("a{2,3}"       "aaaa"        t t        ("aaa"))


      ;; nongreedy quantifiers
      ;; ---------------------
      ("a*?"          "aaaa"        t t        (""))
      ("a+?"          "aaaa"        t t        ("a"))
      ("a{2,3}?"      "aaaa"        t t        ("aa"))


      ("a+?bb*?"      "baaaabaaabbbaaaaa"     t t    ("aaaab"))
      ("a+?bb+?"      "baaaabaaabbbaaaaa"     t t    ("aaabb"))

      ("[abc]{10,20}?" "xxxbcbcbabcaabcbabcbcbabcbcaabcabxxx"  t t ("bcbcbabcaa"))
      

      ;; grouping
      ;; --------


      ;; nonregister grouping
      ;; --------------------
      ;    "((?a+)|b)"



      ;; greedy quantifiers + backrefs
      ;; -----------------------------
      ("^(x)+$"                 "xx"      t t        ("xx"      "x"))
      ("^(x)+\\1$"              "xx"      t t        ("xx"      "x"))
      ("^(x){1,2}$"             "xx"      t t        ("xx"      "x"))
      ("^(x){1,2}\\1$"          "xx"      t t        ("xx"      "x"))
      ("^(x)+[^x]+\\1$"         "xxaax"   t t        ("xxaax"   "x"))
      ("^x*(x)[^x]+\\1$"        "xxaax"   t t        ("xxaax"   "x"))

      ("(x)+\\1"                "xxxx"    t t        ("xxxx"    "x"))
      ("(x){1,2}"               "xxxx"    t t        ("xx"      "x"))
      ;; kmp By the letter, (x) can only match one character.  To get this
      ;;     affect, the pattern should be "(x{1,2})\\1"
      ; ("(x){1,2}\\1"            "xxxx"    t t        ("xxxx"    "x"))
      ("(x){1,2}\\1"            "xxxx"    t t        ("xxx"    "x"))

      ("(x)+[^x]+\\1"           "xxaax"   t t        ("xxaax"   "x"))
      ("x*(x)[^x]+\\1"          "xxaax"   t t        ("xxaax"   "x"))


      ;; nongreedy quantifiers + backrefs
      ;; --------------------------------
      ("(x)+?\\1"               "xxxx"    t t        ("xx"      "x"))
      ("(x){1,2}?"              "xxxx"    t t        ("x"       "x"))
      ("(x){1,2}?\\1"           "xxxx"    t t        ("xx"      "x"))
      ("(x)+?[^x]+\\1"          "xxaax"   t t        ("xxaax"   "x"))
      ("x*?(x)[^x]+\\1"         "xxaax"   t t        ("xxaax"   "x"))


      ;; misc
      ;; ----
      ;; kmp it is legal for a* to match nothing
      ; ("(a*)*"                  "aaaa"    t t        ("aaaa"    "aaaa"))
      #+:regex-right("(a*)*"                  "aaaa"    t t        ("aaaa"    ""))
      #-:regex-right("(a*)*"                  "aaaa"    t t        ("aaaa"    "aaaa"))
      ;; kmp it is legal for a* to match nothing
      ; ("(a*)+"                  "aaaa"    t t        ("aaaa"    "aaaa"))
      #+:regex-right("(a*)+"                  "aaaa"    t t        ("aaaa"    ""))
      #-:regex-right("(a*)+"                  "aaaa"    t t        ("aaaa"    "aaaa"))
      ("(a+)*"                  "aaaa"    t t        ("aaaa"    "aaaa"))
      ("(a+)*"                  "aaaa"    t t        ("aaaa"    "aaaa"))


      ))


;; *****************************************************************************
;; FUNCTION
;; Name             : run-sebastien-tests
;; Date             : 2002-03-08
;; Author           : 3S
;; Arguments        : none
;; Side Effects     : print testing results
;; Purpose          : guess it
;; -----------------------------------------------------------------------------

(defun run-sebastien-tests ()
   (lispbuilder-regex::clear-pattern-cache) ;; kmp helps for when I'm debugging the compiler
   (print ";; *****************************************************************************")
   (print ";; BEGIN OF TEST")
   (print ";; -----------------------------------------------------------------------------")
   (dolist (test *regexp-tests*)
      (destructuring-bind (pattern str expected-compile-p expected-matched-p expected-results) test
         (format t "~%pattern: ~A ~%string: ~A" pattern str)
         (let ((matcher (compile-str pattern)))
            (cond ((and matcher (not expected-compile-p))
                  (format t "~%Shouldn't have compiled, but did ******************** TEST FAILED"))
               ((and (not matcher) expected-compile-p)
                  (format t "~%Should have compiled, but didn't ******************** TEST FAILED"))
               )
            (when matcher
               (multiple-value-bind (matched-p start len regs)
                  (scan-str matcher str)
                  (cond ((and expected-matched-p (not matched-p))
                        (format t "~%Should have matched, but didn't ******************** TEST FAILED"))
                     ((and (not expected-matched-p) matched-p)
                        (format t "~%Shouldn't have matched, but did ******************** TEST FAILED"))
                     )
                  (when matched-p
                     (if (string= (car expected-results) (subseq str start (+ start len)))
                        (format t "~%Global match OK" )
                        (format t "~%Global match ******************** TEST FAILED")
                        )
                     (let ((num-groups (array-dimension regs 0))
                           )
                        (if (/= (length expected-results) num-groups)
                           (format t "~%Number of groups ******************** TEST FAILED")
                           (dotimes (i num-groups)
                              (let* ((group-start (register-start regs i))
                                    (group-end (register-end regs i))
                                    (expected-value (nth i expected-results))
                                    (calculated-value
                                       (if (register-matched-p regs i)
                                          (subseq str group-start group-end)
                                          ""))
                                    )
                                 (if (string= expected-value calculated-value)
                                    (format t "~%Group ~A OK ==> ~A" i calculated-value)
                                    (format t "~%Group ~A ==> ~A instead of ~A ******************** TEST FAILED" i calculated-value expected-value)
                                    )
                                 ))
                           )))
                  )))
         )
      (terpri))
   (print ";; *****************************************************************************")
   (print ";; END OF TEST")
   (print ";; -----------------------------------------------------------------------------")
   )


;; *****************************************************************************
;; END OF FILE
;; -----------------------------------------------------------------------------
