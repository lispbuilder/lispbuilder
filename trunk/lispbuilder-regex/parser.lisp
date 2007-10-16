;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LISPBUILDER-REGEX; Base: 10 -*-

(in-package :LISPBUILDER-REGEX)


;;;
;;; Pass 1 - Lexer
;;;

(defun re-scanner (str &optional (start 0) (length (length str)))
  (make-re-scanner :str str :pos start :end (+ start length)))

(defun nextchar (scanner)
  (let ((ch (char (re-scanner-str scanner) (re-scanner-pos scanner))))
    (incf (re-scanner-pos scanner))
    ch))

(defun ungetchar (scanner)
  (decf (re-scanner-pos scanner)))

(defun fix-escape-char (chr)
  (case chr
    ((#\t #\T) #\Tab)
    ((#\n #\N) #\Newline)
    ((#\r #\R) #\Return)
    ((#\h #\H) #\Backspace)
    ((#\p #\P) #\Page)
    (t chr)))

(defun scan-num (scanner)
  (let ((ch (nextchar scanner)))
    (cond
     ((digit-char-p ch)
      (cond
       ((< (re-scanner-pos scanner) (re-scanner-end scanner))
        (let ((ch2 (nextchar scanner)))
          (cond
           ((digit-char-p ch2)
            (parse-integer (concatenate 'string (string ch) (string ch2))))
           (t
            (ungetchar scanner)
            (parse-integer (string ch))))))
       (t (parse-integer (string ch)))))
     (t (ungetchar scanner)
        ch))))

(defun next (scanner)
  (declare (type re-scanner scanner))
  (cond
   ((re-scanner-ungot-token scanner)
    (let ((token (re-scanner-ungot-token scanner))
          (value (re-scanner-ungot-value scanner)))
      (setf (re-scanner-ungot-token scanner) nil
            (re-scanner-ungot-value scanner) nil)
      (values token value)))
   ((= (re-scanner-pos scanner) (re-scanner-end scanner))
    (values nil nil))
   (t (let ((ch (nextchar scanner)))
        (if *escape-special-chars*
            ;; escaped magic chars
            (case (re-scanner-mode scanner)
              (in-regex
               (if (and (char= ch #\\)
                        (< (re-scanner-pos scanner)
                           (re-scanner-end scanner)))
                   (let ((ch (nextchar scanner)))
                     (case ch
                       (#\\
                        (if (< (re-scanner-pos scanner)
                               (re-scanner-end scanner))
                            (if *allow-backmatch*
                                (let ((item (scan-num scanner)))
                                  (print item)
                                  (if (numberp item)
                                      (values 'backmatch item)
                                    (let ((nc (fix-escape-char (nextchar scanner))))
                                      (case nc
                                        (#\< (values 'startword nil))
                                        (#\> (values 'endword nil))
                                        (#\w (values 'wordchar nil))
                                        (#\W (values 'nonwordchar nil))
                                        (#\d (values 'digitchar nil))
                                        (#\D (values 'nondigitchar nil))
                                        (#\s (values 'spacechar nil))
                                        (#\S (values 'nonspacechar nil))
                                        (t (values 'char nc))))))
                              (let ((nc (fix-escape-char (nextchar scanner))))
                                (case nc
                                  (#\< (values 'startword nil))
                                  (#\> (values 'endword nil))
                                  (#\w (values 'wordchar nil))
                                  (#\W (values 'nonwordchar nil))
                                  (#\d (values 'digitchar nil))
                                  (#\D (values 'nondigitchar nil))
                                  (#\s (values 'spacechar nil))
                                  (#\S (values 'nonspacechar nil))
                                  (t (values 'char nc)))))
                          (values 'char ch)))
                       (#\*
                        (if (and *allow-nongreedy-quantifiers*
                                 (< (re-scanner-pos scanner)
                                    (re-scanner-end scanner)))
                            (let ((ch2 (nextchar scanner)))
                              (cond ((char= ch2 #\?)
                                     (values 'kleene 'non-greedy))
                                    (t (ungetchar scanner)
                                       (values 'kleene 'greedy))))
                          (values 'kleene 'greedy)))
                       (#\+
                        (if (and *allow-nongreedy-quantifiers*
                                 (< (re-scanner-pos scanner)
                                    (re-scanner-end scanner)))
                            (let ((ch2 (nextchar scanner)))
                              (cond ((char= ch2 #\?)
                                     (values 'plus 'non-greedy))
                                    (t (ungetchar scanner)
                                       (values 'plus 'greedy))))
                          (values 'plus 'greedy)))
                       (#\?
                        (if (and *allow-nongreedy-quantifiers*
                                 (< (re-scanner-pos scanner)
                                    (re-scanner-end scanner)))
                            (let ((ch2 (nextchar scanner)))
                              (cond ((char= ch2 #\?)
                                     (values 'optional 'non-greedy))
                                    (t (ungetchar scanner)
                                       (values 'optional 'greedy))))
                          (values 'optional 'greedy)))
                       (#\(
                        (if (and *allow-nonregister-groups*
                                 (< (re-scanner-pos scanner)
                                    (re-scanner-end scanner)))
                            (let ((ch2 (nextchar scanner)))
                              (cond ((char= ch2 #\?)
                                     (if (< (re-scanner-pos scanner)
                                            (re-scanner-end scanner))
                                         (let ((ch3 (nextchar scanner)))
                                           (cond ((char= ch3 #\=)
                                                  (values 'lparen 'lookahead))
                                                 ((char= ch3 #\!)
                                                  (values 'lparen 'nlookahead))
                                                 (t (ungetchar scanner)
                                                    (values 'lparen nil))))
                                       (values 'lparen nil)))
                                    (t
                                     (ungetchar scanner)
                                     (values 'lparen
                                             (incf (re-scanner-regnum scanner))))))
                          (values 'lparen (incf (re-scanner-regnum scanner)))))
                       (#\) (values 'rparen nil))
                       (#\| (values 'or nil))
                       (#\^ (values 'startanchor nil))
                       (#\$ (values 'endanchor nil))
                       (#\[ (setf (re-scanner-mode scanner) 'start-class)
                            (values 'startclass nil))
                       (#\] (values 'char ch))
                       (#\. (values 'any nil))
                       (#\{
                        (if *allow-rangematch*
                            (let ((rangebounds (scan-range-bounds scanner)))
                              (if (and *allow-nongreedy-quantifiers*
                                       (< (re-scanner-pos scanner)
                                          (re-scanner-end scanner)))
                                  (let ((ch2 (nextchar scanner)))
                                    (cond ((char= ch2 #\?)
                                           (values 'ngrange rangebounds))
                                          (t (ungetchar scanner)
                                             (values 'range rangebounds))))
                                (values 'range rangebounds)))
                          (values 'char ch)))
                       (t (values 'char ch))) )
                 (values 'char ch)))
              (start-class
               (if (and (char= ch #\\)
                        (< (re-scanner-pos scanner)
                           (re-scanner-end scanner)))
                   (let ((ch (nextchar scanner)))
                     (case ch
                       (#\\ (setf (re-scanner-mode scanner) 'in-class)
                            (values 'char (fix-escape-char (nextchar scanner))))
                       (#\]
                            ; since [] is non-sensical, if the first char is ']',
                            ; assume the user meant '\]'
                            ; (setf (re-scanner-mode scanner) 'in-regex)
                            ; (values 'endclass nil))
                            (values 'char ch))
                       (#\^ (setf (re-scanner-mode scanner) 'in-class)
                            (values 'nclass nil))
                       (t (setf (re-scanner-mode scanner) 'in-class)
                          (values 'char ch)) ))
                 (values 'char ch)))
              (in-class
               (if (and (char= ch #\\)
                        (< (re-scanner-pos scanner)
                           (re-scanner-end scanner)))
                   (let ((ch (nextchar scanner)))
                     (case ch
                       (#\\ (values 'char (fix-escape-char (nextchar scanner))))
                       (#\- (values 'dash nil))
                       (#\] (setf (re-scanner-mode scanner) 'in-regex)
                            (values 'endclass nil))
                       (t (values 'char ch))))
                 (values 'char ch))))
          ;; non-escaped magic chars
          (case (re-scanner-mode scanner)
            (in-regex
             (case ch
               (#\\ (if (< (re-scanner-pos scanner) (re-scanner-end scanner))
                        (if *allow-backmatch*
                            (let ((item (scan-num scanner)))
                              (if (numberp item)
                                  (values 'backmatch item)
                                (let ((nc (fix-escape-char (nextchar scanner))))
                                  (case nc
                                      (#\< (values 'startword nil))
                                      (#\> (values 'endword nil))
                                      (#\w (values 'wordchar nil))
                                      (#\W (values 'nonwordchar nil))
                                      (#\d (values 'digitchar nil))
                                      (#\D (values 'nondigitchar nil))
                                      (#\s (values 'spacechar nil))
                                      (#\S (values 'nonspacechar nil))
                                      (t (values 'char nc))))))
                          (let ((nc (fix-escape-char (nextchar scanner))))
                            (case nc
                              (#\< (values 'startword nil))
                              (#\> (values 'endword nil))
                              (#\w (values 'wordchar nil))
                              (#\W (values 'nonwordchar nil))
                              (#\d (values 'digitchar nil))
                              (#\D (values 'nondigitchar nil))
                              (#\s (values 'spacechar nil))
                              (#\S (values 'nonspacechar nil))
                              (t (values 'char nc)))))
                      (values 'char ch)))
               (#\* (if (and *allow-nongreedy-quantifiers*
                             (< (re-scanner-pos scanner)
                                (re-scanner-end scanner)))
                        (let ((ch2 (nextchar scanner)))
                          (cond ((char= ch2 #\?)
                                 (values 'kleene 'non-greedy))
                                (t (ungetchar scanner)
                                   (values 'kleene 'greedy))))
                      (values 'kleene 'greedy)))
               (#\+ (if (and *allow-nongreedy-quantifiers*
                             (< (re-scanner-pos scanner)
                                (re-scanner-end scanner)))
                        (let ((ch2 (nextchar scanner)))
                          (cond ((char= ch2 #\?)
                                 (values 'plus 'non-greedy))
                                (t (ungetchar scanner)
                                   (values 'plus 'greedy))))
                      (values 'plus 'greedy)))
               (#\( (if (and *allow-nonregister-groups*
                             (< (re-scanner-pos scanner)
                                (re-scanner-end scanner)))
                        (let ((ch2 (nextchar scanner)))
                          (cond ((char= ch2 #\?)
                                 (if (< (re-scanner-pos scanner)
                                        (re-scanner-end scanner))
                                     (let ((ch3 (nextchar scanner)))
                                       (cond ((char= ch3 #\=)
                                              (values 'lparen 'lookahead))
                                             ((char= ch3 #\!)
                                              (values 'lparen 'nlookahead))
                                             (t (ungetchar scanner)
                                                (values 'lparen nil))))
                                   (values 'lparen nil)))
                                (t (ungetchar scanner)
                                   (values 'lparen
                                           (incf (re-scanner-regnum scanner))))))
                      (values 'lparen (incf (re-scanner-regnum scanner)))))
               (#\) (values 'rparen nil))
               (#\? (if (and *allow-nongreedy-quantifiers*
                             (< (re-scanner-pos scanner) (re-scanner-end scanner)))
                        (let ((ch2 (nextchar scanner)))
                          (cond ((char= ch2 #\?)
                                 (values 'optional 'non-greedy))
                                (t (ungetchar scanner)
                                   (values 'optional 'greedy))))
                      (values 'optional 'greedy)))
               (#\| (values 'or nil))
               (#\^ (values 'startanchor nil))
               (#\$ (values 'endanchor nil))
               (#\[ (setf (re-scanner-mode scanner) 'start-class)
                    (values 'startclass nil))
               (#\] (values 'char ch))
               (#\. (values 'any nil))
               (#\{ (if *allow-rangematch*
                        (let ((rangebounds (scan-range-bounds scanner)))
                          (if (and *allow-nongreedy-quantifiers*
                                   (< (re-scanner-pos scanner)
                                      (re-scanner-end scanner)))
                              (let ((ch2 (nextchar scanner)))
                                (cond ((char= ch2 #\?)
                                       (values 'ngrange rangebounds))
                                      (t (ungetchar scanner)
                                         (values 'range rangebounds))))
                            (values 'range rangebounds)))
                      (values 'char ch)))
               (t (values 'char ch))) )
            (start-class
             (case ch
               (#\\ (setf (re-scanner-mode scanner) 'in-class)
                    (if (< (re-scanner-pos scanner) (re-scanner-end scanner))
                        (values 'char (fix-escape-char (nextchar scanner)))
                      (throw 'regex-parse-error (list "Invalid character class"))))
               (#\]
                    ; since [] is non-sensical, if the first char is ']',
                    ; assume the user meant '\]'
                    ; (setf (re-scanner-mode scanner) 'in-regex)
                    ; (values 'endclass nil))
                    (values 'char ch))
               (#\^ (setf (re-scanner-mode scanner) 'in-class)
                    (values 'nclass nil))
               (t (setf (re-scanner-mode scanner) 'in-class)
                  (values 'char ch)) ))
            (in-class
             (case ch
               (#\\ (if (< (re-scanner-pos scanner) (re-scanner-end scanner))
                        (values 'char (fix-escape-char (nextchar scanner)))
                      (throw 'regex-parse-error (list "Invalid character class"))))
               (#\- (values 'dash nil))
               (#\] (setf (re-scanner-mode scanner) 'in-regex)
                    (values 'endclass nil))
               (t (values 'char ch))))))))))


(defun scan-range-bounds (scanner)
  (let ((lowbound (scan-num scanner)))
    (unless (< (re-scanner-pos scanner) (re-scanner-end scanner))
      (throw 'regex-parse-error
             (list "Range pattern {nn,nn}: Unexpected end of pattern ~S"
                   (re-scanner-str scanner))))
    (when (not (numberp lowbound))
      (setq lowbound 0))
    (let ((ch (nextchar scanner)))
      (unless (or (char= ch #\,) (char= ch #\}))
        (throw 'regex-parse-error
               (list "Range pattern {nn,nn}: ',' or '}' expected in pattern ~S at ~D, encountered ~S"
                     (re-scanner-str scanner) (re-scanner-pos scanner) ch)))
      (cond ((char= ch #\})
             (cons lowbound lowbound))
            (t (let ((highbound (scan-num scanner)))
                 (cond ((not (numberp highbound))
                        (unless (char= highbound #\})
                          (list "Range pattern {nn,nn}: '}' expected in pattern ~S at ~D"
                                (re-scanner-str scanner) (re-scanner-pos scanner)))
                        (nextchar scanner)   ; skip over the '}'
                        (cons lowbound nil))
                       (t (unless (< (re-scanner-pos scanner) (re-scanner-end scanner))
                            (throw 'regex-parse-error
                                   (list "Range pattern {nn,nn}: Unexpected end of pattern ~S"
                                         (re-scanner-str scanner))))
                          (let ((ch (nextchar scanner)))
                            (unless (char= ch #\})
                              (throw 'regex-parse-error
                                     (list "Range pattern {nn,nn}: '}' expected in pattern ~S at ~D"
                                           (re-scanner-str scanner) (re-scanner-pos scanner))))
                            (cons (min lowbound highbound) (max lowbound highbound)))))))))))

(defun unget (scanner token value)
  (declare (type re-scanner scanner))
  (setf (re-scanner-ungot-token scanner) token
        (re-scanner-ungot-value scanner) value))

(defun show-tokens (str)
  "Parse a string regex expression into a regex parse tree."
  (let ((scanner (re-scanner str)))
    (labels ((getnext ()
               (multiple-value-bind (token value)
                   (next scanner)
                 (when token
                   (cons (list token value)
                         (getnext))))))
      (getnext))))




;;;
;;; Pass 2 - Parser
;;;

; <regex> ::= <union>
; <union> ::= <concat>"|"<union> | <concat>
; <concat> ::= <quant><concat> | <quant>
; <quant> ::= <group>"*" | <group>"+" | <group>"?" | <group>"{"<bound>"}" | <group>
; <group> ::= "("<re>")" | <term>
; <term> ::= "." | "$" | "^" | <char> | <set>
; <char> ::= <non-meta> | "\"<escaped> | \b | \B | \< | \>
; <bound> ::= <num> | <num>","<num>
; <set> ::= "[" <set-items> "]" | "[^" <set-items> "]"

(defun parse-str (str)
  "Parse a string into a parse tree."
  (let ((scanner (re-scanner str)))
    (multiple-value-bind (token value) (next scanner)
      (let ((regex (parse-regex token value scanner)))
        (multiple-value-bind (token value) (next scanner)
          (cond ((null token)
                 (make-register-node 0 regex))
                (t (throw 'regex-parse-error
                          (list "Regex parse error at ~S ~S" token value)))))))))

; <regex> ::= <union>
(defun parse-regex (token value scanner)
  (parse-union token value scanner))

; <union> ::= <concat>"|"<union> | <concat>
(defun parse-union (token value scanner)
  (let ((concat (parse-concat token value scanner)))
    (multiple-value-bind (token value) (next scanner)
      (cond
       ((eq token 'or)
        (multiple-value-bind (token value) (next scanner)
          (cond
           ((or (null token) (eq token 'rparen))
            (unget scanner token value)
            (make-alt-node-args concat nil))
           (t (let ((other-concat (parse-union token value scanner)))
                (make-alt-node-args concat other-concat))))))
        (t
          (unget scanner token value)
          concat)))))

; <concat> ::= <quant><concat> | <quant>
(defun parse-concat (token value scanner)
   (let ((seq)
         (quant (parse-quant token value scanner)))
     (setq seq (list quant))
     (multiple-value-bind (token value) (next scanner)
       (loop until (member token '(or rparen nil))
             do (progn
                  (setq seq (nconc seq (list (parse-quant token value scanner))))
                  (multiple-value-setq (token value) (next scanner)))
             finally (progn
                       (unget scanner token value)
                       (return-from parse-concat (make-seq-node-list seq)))))))

; <quant> ::= <group>"*" | <group>"+" | <group>"?" | <group>"{"<bound>"}" | <group>
(defun parse-quant (token value scanner)
  (let ((group (parse-group token value scanner)))
    (multiple-value-bind (token value) (next scanner)
      (loop while (member token '(kleene plus optional range ngrange))
            do (progn
                 (setq group (quantify token value group))
                 (multiple-value-setq (token value) (next scanner)))
            finally (progn
                      (unget scanner token value)
                      (return-from parse-quant group))))))


(defun quantify (token value expr)
  (cond
   ((eq token 'kleene)
    (make-kleene-node expr (eq value 'greedy)))
   ((eq token 'plus)
    (make-pkleene-node expr (eq value 'greedy)))
   ((eq token 'optional)
    (make-optional-node expr (eq value 'greedy)))
   ((eq token 'range)
    (make-range-node expr (car value) (cdr value) t))
   ((eq token 'ngrange)
    (make-range-node expr (car value) (cdr value) nil))
   (t 
    (throw 'regex-parse-error
           (list "quantify: Unexpected quantifier ~S ~S" token value)))))


; <group> ::= "("<re>")" | <term>
(defun parse-group (token value scanner)
  (cond
   ((eq token 'lparen)
    (multiple-value-bind (token2 value2) (next scanner)
      (let ((regex (parse-regex token2 value2 scanner)))
        (multiple-value-bind (token3 value3) (next scanner)
          (unless (eq token3 'rparen)
            (throw 'regex-parse-error
                   (list "parse-group: Expected ')' at token ~S ~S"
                         token3 value3)))
          (cond ((numberp value)
                 (make-register-node value regex))
                ((eq value 'lookahead)
                 (make-lookahead-node regex))
                ((eq value 'nlookahead)
                 (make-nlookahead-node regex))
                (t regex))))))
   ((member token '(any startanchor endanchor char backmatch startclass startword endword digitchar nondigitchar wordchar nonwordchar spacechar nonspacechar))
    (parse-term token value scanner))
   ((eq token 'or)
    (unget scanner token value)
    nil)
   (t 
    (throw 'regex-parse-error
           (list "parse-group: Unexpected token ~S ~S" token value)))))

; <term> ::= "." | "$" | "^" | <char> | <set>
(defun parse-term (token value scanner)
  (cond
   ((eq token 'any) (make-any-node))
   ((eq token 'startanchor) (make-start-anchor-node))
   ((eq token 'endanchor) (make-end-anchor-node))
   ((eq token 'char) (make-char-node value))
   ((eq token 'backmatch)
    (make-backmatch-node value))
   ((eq token 'startclass)
    (multiple-value-bind (token value)
        (next scanner)
      (parse-char-class token value scanner)))
   ((eq token 'startword)
    (make-startword-node))
   ((eq token 'endword)
    (make-endword-node))
   ((eq token 'wordchar)
    '(specclass alnum))
   ((eq token 'nonwordchar)
    '(nspecclass alnum))
   ((eq token 'digitchar)
    '(specclass digit))
   ((eq token 'spacechar)
    '(specclass space))
   ((eq token 'nonspacechar)
    '(nspecclass space))
   (t
    (throw 'regex-parse-error
           (list "parse-term: Unexpected token ~S ~S" token value)))))

(defun parse-char-class (token value scanner)
  (case token
    (nclass
     (multiple-value-bind (token2 value2)
         (next scanner)
       (let* ((chars (parse-char-class-contents token2 value2 scanner))
              (specclass (special-class chars)))
         (cond (specclass
                (make-specclass-node specclass :negated t))
               (t (make-charclass-node chars :negated t))))))
    (t
     (let* ((chars (parse-char-class-contents token value scanner))
            (specclass (special-class chars)))
       (cond (specclass
              (make-specclass-node specclass :negated nil))
             (t (make-charclass-node chars :negated nil)))))))

(defun parse-char-class-contents (token value scanner &aux lst)
  (loop while (or (eq token 'char) (eq token 'dash))
        do (progn
             (cond ((eq token 'char)
                    (push value lst))
                   ((eq token 'dash)
                    (push token lst)))
             (multiple-value-setq (token value) (next scanner))))
  (cond
   ((eq token 'endclass)
    (reverse lst))
   (t (throw 'regex-parse-error
             (list "character class improperly terminated by ~S" token)))))


(defun special-class (chars)
  (let* ((len (length chars))
         (firstchar (first chars))
         (lastchar (elt chars (1- len))))
    (when (and (characterp firstchar)
               (char= firstchar #\:)
               (characterp lastchar)
               (not (char= lastchar #\:)))
      (throw 'regex-parse-error
	(list "Parse error: Special character class not terminated by ':'")))
    (when (and (characterp firstchar) (char= firstchar #\:)
               (characterp lastchar) (char= lastchar #\:))
      (let ((scname (string-downcase (coerce chars 'string))))
        (second (assoc scname +special-class-names+ :test #'string=))))))



