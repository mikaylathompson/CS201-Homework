; ********************************************************************
; CS 201a HW #7  DUE Wednesday, November 20, at 11:59 pm
; using the submit command.  
; ********************************************************************
; Name: Mikayla Thompson
; Email address: mikayla.thompson@yale.edu
; ********************************************************************
; Topics: strings, regular expressions, finite-state
; acceptors, and context free grammars

; Unless the problem specifies otherwise:
; * You may solve the problem using any method 
; and any Scheme constructs, except mutators (set! and its relatives.)
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

; ********************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 5)

; ********************************************************************
; Representation of finite strings
; ********************************************************************
; Rather than use Scheme strings to represent strings, 
; which would limit the set of symbols to be characters,
; we define our own representation of Strings as follows.

; We define a set of four "reserved" symbols to use in Regular Expressions:
;   u:, *:, +:, ?:
; Any other Scheme symbol is defined as "permitted."
; A String is represented by a list of permitted Scheme symbols.

; Thus, (a b a b b a) represents a String with six symbols,
; the first, third and sixth being a's and the rest b's.
; Similarly, (the dog barked at the other dog) is a String of seven symbols.
; The empty String is denoted by the empty list ().

; Note that (append str1 str2) concatenates the two Strings str1 and str2.

; ********************************************************************
; ** problem 1 **  (9 points)
; Write three procedures

; (ok-symbol? value)
; (ok-string? value)
; (symbol-count sym st)

; (ok-symbol? value)
; takes an arbitrary Scheme value and returns #t
; if the value is a permitted symbol (as defined above)
; and #f otherwise.

(define ok-symbol?
  (lambda (value)
    (case value
      ((u: *: +: ?:) #f)
      (else #t))))


; (ok-string? value)
; takes an arbitrary Scheme value and returns #t 
; if the value is a String (that is, a list of 
; permitted symbols.)

(define ok-string?
  (lambda (value)
    (cond
      ((not (list? value)) #f)
      ((null? value) #t)
      ((ok-symbol? (car value)) (ok-string? (cdr value)))
      (else #f))))

; (symbol-count sym str)
; takes a permitted symbol sym and a String str
; and returns the number of occurrences of sym in str.

(define symbol-count
  (lambda (sym str)
    (apply + (map (lambda (test)
                    (if (equal? sym test)
                        1
                        0))
                  str))))

; Examples

; (ok-symbol? 'hi!) ;=> #t
; (ok-symbol? 'u:) ;=> #f
; (ok-symbol? 'a); => #t
; (ok-string? '()) ;=> #t
; (ok-string? '(man oh man)) ;=> #t
; (ok-string? '(this one is +: not)); => #f
; (symbol-count 'a '(a a b a)) ;=> 3
; (symbol-count 'c '(a a b a)) ;=> 0
; (symbol-count 'oh '(man oh man)) ;=> 1
; ********************************************************************


; ********************************************************************
; Representation of Regular Expressions
; ********************************************************************
; Regular Expressions, including the operations of
;   concatenation, union, Kleene star, Kleene plus, and question mark
; are represented as follows.
; 1) A permitted symbol (defined above) is a Regular Expression denoting
;    the language containing exactly one string consisting of just that symbol.
; 2) The concatenation of *zero or more* expressions is represented by
;    a list containing the Regular Expressions to be concatenated.
;    The concatenation of zero symbols is the empty list, (), which represents
;    the language containing just the empty String.
; 3) The union of *two or more* expressions is represented
;    by a list starting with the symbol u: and containing 
;    the Regular Expressions to be joined by union.
; 4) The Kleene star of *one* expression is represented by a list
;    starting with *: and containing one Regular Expression.
; 5) The Kleene plus of *one* expression is represented by a list
;    starting with +: and containing one Regular Expression.
; 6) The question mark of *one* expression is represented by a list
;    starting with ?: and containing one Regular Expression.

; Here are constructors for Regular Expressions.
; Two of them use the alternate form of lambda that allows any number
; of arguments.  When the procedure is called, its actual arguments
; are gathered into a list which is assigned to the formal argument.

(define conc (lambda exps exps))
(define union (lambda exps (cons 'u: exps)))
(define star (lambda (exp) (list '*: exp)))
(define plus (lambda (exp) (list '+: exp)))
(define qmark (lambda (exp) (list '?: exp)))

; Some examples of Regular Expressions using these constructors.

(define exp0 '())
(define exp1 '(a a b a b))
(define exp2 (union '(a b) '(b) '(c b b)))
(define exp3 (star 'very))
(define exp4 (star (union 'very 'quite)))
(define exp5 (conc 'c (plus (union 'a 'd)) 'r))
(define exp6 (conc '(the sleeping) (union 'dog 'cat 'skunk)))
(define exp7 (conc 'a (plus (union 'very 'quite)) 'long (union 'story 'tale)))
(define exp8 (conc (star 'a) (star 'b) (star 'a)))
(define exp9 (conc '(y o g) (qmark 'h) '(u r t)))
(define exp-chase
  (conc 'the 
	(union 'dog 'cat 'mouse) 
	'chased 
	'the 
	(union 'dog 'cat 'mouse)
	(star (conc 'that
		    'chased
		    'the
		    (union 'dog 'cat 'mouse)))))

; Here are selectors for a Regular Expression that is a
; union, star, plus or qmark.  Please use them when appropriate.

(define re-op car)
(define re-args cdr)

(define and-list
  (lambda (lst)
    (if (null? lst)
        #t
        (if (or (equal? #t (car lst)) 
                (equal? 1 (car lst)))
            (and-list (cdr lst))
            #f))))

(define or-list
  (lambda (lst)
    (if (null? lst)
        #f
        (if (or (equal? #t (car lst)) 
                (equal? 1 (car lst)))
            #t
            (or-list (cdr lst))))))

; ********************************************************************
; ** problem 2 **  (10 points)
; Write two procedures

; (reg-exp? value)
; (reg-exp-type exp)

; (reg-exp? value)
; takes an arbitrary Scheme value and returns #t
; if it is a Regular Expression according to the definition
; above, and #f otherwise.

(define reg-exp?
  (lambda (value)
    (cond
      ((not (list? value)) (if (ok-symbol? value) #t #f))
      ((ok-string? value) #t)
      ((ok-symbol? (car value)) (reg-exp? (cdr value)))
      ((case (car value)
         ((u:) (if (>= (length (cdr value)) 2)
                   (and-list (map reg-exp? (cdr value)))
                   #f))
         ((*: +* ?:) (if (= (length (cdr value)) 1)
                         (reg-exp? (cdr value))
                         #f))
         (else #f)))
      (else #f))))

; 1) A permitted symbol 
; 2) a list containing the Regular Expressions to be concatenated.
; 3) u: *two or more* expressions
; 4) *: and containing one Regular Expression.
; 5) +: and containing one Regular Expression.
; 6) ?: and containing one Regular Expression.

; (reg-exp-type exp)
; that takes a Regular Expression exp and returns its type
; as one of the symbols:

; symbol, union, star, plus, qmark, conc

; The type is symbol if the Regular Expression is a permitted symbol.
; The type is union, star, plus, or qmark if the top-level operation
; in the Regular Expression is u:, *:, +: or ?:, respectively.
; Otherwise, the type is conc.

(define reg-exp-type
  (lambda (value)
    (cond
      ((symbol? value) 'symbol)
      ((ok-string? value) 'conc)
      (else (case (car value)
              ((u:) 'union)
              ((*:) 'star)
              ((+:) 'plus)
              ((?:) 'qmark)
              (else 'conc))))))
                          


; Examples:

; The procedure reg-exp? should return #t for the Regular Expressions
; defined above.

; (reg-exp? 'dog) ;=> #t
; (reg-exp? 'u:) ;=> #f
; (reg-exp? '(u: a ?:)) ;=> #f
; (reg-exp? '(?: b c)) ;=> #f
; (reg-exp? '(u: (a b))) ;=> #f
; (reg-exp? '(u: (*: a) (*: b))) ;=> #t
; (reg-exp-type 'hi) ;=> symbol
; (reg-exp-type exp0) ;=> conc
; (reg-exp-type exp1) ;=> conc
; (reg-exp-type exp2) ;=> union
; (map reg-exp-type exp9) ;=> (conc qmark conc)
; ********************************************************************

; flatten turns a nested list into a flat one
(define flatten
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((list? (car lst))
       (append (flatten (car lst)) (flatten (cdr lst))))
      (else 
       (cons (car lst) (flatten (cdr lst)))))))

; ********************************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (pick-from-exp exp)

; that takes a Regular Expression exp
; and returns a randomly chosen String 
; in the language of exp.  Just how you do this is up to you,
; but your procedure should be capable of generating
; *any* String in the language, and none that aren't.

; Hint: try recursion on the structure of Regular Expressions.

; The following accesses an implementation of a procedure 
; (random-integer n)
; to return a random integer between 0 and n-1 inclusive
; in the R5RS language of DrRacket.
; For example,
; (random-integer 20) => 17
; (random-integer 20) => 3

(#%require srfi/27)

(define make-union
  (lambda (options)
    (pick-from-exp (list-ref options
                             (random-integer (length options))))))
  
(define make-star
  (lambda (sym)
    (let ((coin (random-integer 2)))
      (if (= 0 coin)
          '()
          (cons (pick-from-exp sym) (make-star sym))))))

(define make-plus
  (lambda (sym)
    (let ((coin (random-integer 2)))
      (if (= 0 coin)
          (list (pick-from-exp sym))
          (cons (pick-from-exp sym) (make-plus sym))))))

(define make-qmark
  (lambda (sym)
    (let ((coin (random-integer 2)))
      (if (= 0 coin)
          '()
          (list (pick-from-exp sym))))))

(define pick-from-exp
  (lambda (exp)
    (case (reg-exp-type exp)
      ((symbol) exp)
      ((conc) (if (null? exp)
                  '()
                  (flatten (cons (pick-from-exp (car exp)) 
                                 (pick-from-exp (cdr exp))))))
      ((star) (make-star (cadr exp)))
      ((plus) (make-plus (cadr exp)))
      ((qmark) (make-qmark (cadr exp)))
      ((union) (make-union (cdr exp))))))

; Examples (your random results may vary):

; (pick-from-exp exp0) => ()
; (pick-from-exp exp1) => (a a b a b)
; (pick-from-exp exp2) => (c b b)
; (pick-from-exp exp2) => (a b)
; (pick-from-exp exp3) => (very very very)
; (pick-from-exp exp4) => (very very quite)
; (pick-from-exp exp5) ;=> (c a d r)
; (pick-from-exp exp5) => (c a r)
; (pick-from-exp exp6) => (the sleeping dog)
; (pick-from-exp exp7) => (a quite long story)
; (pick-from-exp exp8) => (a a)
; (pick-from-exp exp8) => (a b b b b b a)
; (pick-from-exp exp9) => (y o g h u r t)
; (pick-from-exp exp9) => (y o g u r t)
; (pick-from-exp exp-chase) => (the cat chased the dog that chased the mouse)
; ********************************************************************


; ********************************************************************
; ** problem 4 ** (10 points)
; Write a procedure

; (reverse-exp exp)

(define reverse-exp
  (lambda (exp)
    (case (reg-exp-type exp)
      ((symbol) exp)
      ((conc) (if (null? exp) exp (map reverse-exp (reverse exp))))
      (else (cons (car exp) (map reverse-exp (cdr exp)))))))

; that takes a Regular Expression exp and returns a
; Regular Expression for the language that contains
; the reverse of every string in the language of exp,
; and no other strings.

; Hint: try recursion on the structure of a Regular Expression.

; Examples (your random results may vary):
;
; (pick-from-exp (reverse-exp exp0)) ;=> ()
; (pick-from-exp (reverse-exp exp1)) ;=> (b a b a a)
; (pick-from-exp (reverse-exp exp2)) ;=> (b b c)
; (pick-from-exp (reverse-exp exp4)) ;=> (very quite)
; (pick-from-exp (reverse-exp exp5)) ;=> (r a a c)
; (pick-from-exp (reverse-exp exp6)) ;=> (dog sleeping the)
; (pick-from-exp (reverse-exp exp7)) ;=> (story long very quite quite quite very a)
; (pick-from-exp (reverse-exp exp9)) ;=> (t r u g o y)
; ********************************************************************


; ********************************************************************
; Representation of deterministic finite state acceptors
; ********************************************************************
; A Deterministic Finite Acceptor is represented by a list of
; five items:
; the alphabet, a list of permitted symbols (as in problem (1)).
; the state set, a list of Scheme numbers and/or symbols
; the start state, an element the state set
; the list of accepting states, 
; and a table giving the state transition function.
; Please use the following selectors to access components of a dfa.

(define alphabet (lambda (dfa) (list-ref dfa 0)))
(define states (lambda (dfa) (list-ref dfa 1)))
(define start-state (lambda (dfa) (list-ref dfa 2)))
(define accepting-states (lambda (dfa) (list-ref dfa 3)))
(define transition-table (lambda (dfa) (list-ref dfa 4)))

; Here is a constructor for dfas.

(define make-dfa
  (lambda  (alphabet states start-state accepting-states transition-table)
    (list alphabet states start-state accepting-states transition-table)))

; Each entry in the transition table
; consists of 
; a left-value (a list consisting of a state and an alphabet symbol) 
; and a right-value (a state).
; Here are selectors for components of a transition entry.

(define transition-from-state (lambda (entry) (list-ref (car entry) 0)))
(define transition-symbol (lambda (entry) (list-ref (car entry) 1)))
(define transition-to-state (lambda (entry) (cadr entry)))

; An auxiliary procedure to search for a key in a table
; of keys and values might be useful.

; Here is a Deterministic Finite Acceptor
; that accepts all strings of a's and b's with
; an odd number of a's.

(define dfa1
  (make-dfa 
   '(a b)                            ; alphabet is a and b
   '(even odd)                       ; states are even and odd
   'even                             ; the start state is even
   '(odd)                            ; odd is the only accepting state
   '(((even a) odd)  ((even b) even)  
     ((odd a) even)  ((odd b) odd))))

; Here is another acceptor, which accepts strings of a's and b's
; such that no b is followed by an a.

(define dfa2
  (make-dfa
   '(a b)
   '(no-b some-bs fail)
   'no-b
   '(no-b some-bs)
   '(((no-b a) no-b)  ((no-b b) some-bs) 
     ((some-bs a) fail)  ((some-bs b) some-bs)  
     ((fail a) fail) ((fail b) fail))))

; Here is a third dfa, for the language of exp7.  Note that
; the transitions for some (state symbol) pairs are not defined.
; That is, this dfa is not *complete*.

(define dfa3
  (make-dfa
   '(a very quite long story tale)
   '(start det adv adj noun)
   'start
   '(noun)
   '(((start a) det) ((det very) adv) ((det quite) adv) 
     ((adv very) adv) ((adv quite) adv)
     ((adv long) adj)
     ((adj story) noun) ((adj tale) noun))))

; Here is a smaller dfa that is not *complete*
; It accepts the language of strings that are empty  or all a's or all b's.

(define dfa4
  (make-dfa
   '(a b)
   '(start as bs)
   'start
   '(start as bs)
   '(((start a) as) ((start b) bs)
     ((as a) as) ((bs b) bs))))

; contains returns true if item is a top level element in lst
(define contains?
  (lambda (item lst)
    (cond
      ((null? lst) #f)
      ((equal? item (car lst)) #t)
      (else (contains? item (cdr lst))))))

; ********************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (next-state dfa state symbol)
; (dfa-accepts? dfa str)

; (next-state dfa state symbol)
; takes a Deterministic Finite Acceptor dfa, 
; a state of the dfa, and a symbol from the alphabet of the dfa, 
; and returns the next state of the dfa.
; If there is no transition defined for the state and the symbol,
; the procedure returns #f.

(define next-state
  (lambda (dfa state symbol)
    (next-state-helper (transition-table dfa) state symbol)))
           
(define next-state-helper
  (lambda (table state symbol)
    (if (null? table)
        #f
        (if (and (equal? (transition-from-state (car table)) state)
                 (equal? (transition-symbol (car table)) symbol))
            (transition-to-state (car table))
            (next-state-helper (cdr table) state symbol)))))

; (dfa-accepts? dfa str)
; takes a Deterministic Finite Acceptor dfa
; and a String str of symbols from the alphabet of the dfa
; and returns #t if the acceptor accepts the string, #f otherwise.

(define dfa-accepts?
  (lambda (dfa str)
    (let ((final-state (run-states (transition-table dfa) 
                                   str 
                                   (start-state dfa))))
      (if final-state
          (if (contains? final-state (accepting-states dfa))
              #t
              #f)
          #f))))
    

(define run-states
  (lambda (trans-tbl syms now-state)
    (cond 
      ((null? syms) now-state)
      ((equal? #f (next-state-helper trans-tbl now-state (car syms)))
       #f)
      (else
       (run-states trans-tbl 
                   (cdr syms) 
                   (next-state-helper trans-tbl now-state (car syms)))))))
           
; Plan:
;   Run through each element in the string:
;     Look for a next state
;       If false: return #f
;     Run with that state and next string
;   At the end, check that it's in an accepting state

           
; Recall that if the dfa is not complete and the String attempts
; to take transition that is not in the transition table,
; the String is defined to be *not accepted*.

; Examples:
; (next-state dfa1 'even 'a) ;=> odd
; (next-state dfa1 'odd 'b) ;=> odd
; (next-state dfa3 'adv 'very) ;=> adv
; (next-state dfa3 'adv 'long) ;=> adj
; (next-state dfa4 'as 'b) ;=> #f
; (dfa-accepts? dfa1 '(a b b a b a)) ;=> #t
; (dfa-accepts? dfa1 '(b b b)) ;=> #f
; (dfa-accepts? dfa2 '(a a a a)) ;=> #t
; (dfa-accepts? dfa2 '(b a b)) ;=> #f
; (dfa-accepts? dfa3 '(a long story)) ;=> #f
; (dfa-accepts? dfa3 '(a quite very quite long tale)) ;=> #t
; (dfa-accepts? dfa3 '(a very)) ;=> #f
; (dfa-accepts? dfa3 '(a story very long)) ;=> #f
; ********************************************************************


; ********************************************************************
; ** problem 6 ** (10 points)
; Define a Deterministic Finite Acceptor

; dfa-chase

(define dfa-chase
  '((the dog cat mouse chased that)
    (first-the first-noun chased multi-the multi-noun that)
    first-the
    (that)
    (((first-the the) first-noun)
     ((first-noun dog) chased)
     ((first-noun cat) chased)
     ((first-noun mouse) chased)
     ((chased chased) multi-the)
     ((multi-the the) multi-noun)
     ((multi-noun dog) that)
     ((multi-noun cat) that)
     ((multi-noun mouse) that)
     ((that that) chased))))
; in the given format to accept the language of all sentences
; described by the Regular Expression exp-chase, defined before problem (2).

;; Examples
; (dfa-accepts? dfa-chase '(the cat chased the mouse)) ;=> #t
; (dfa-accepts? dfa-chase '(cat the mouse the chased)) ;=> #f
; (dfa-accepts? dfa-chase '(the mouse chased the cat that chased the dog)) ;=> #t
; (dfa-accepts? dfa-chase '(the cockroach chased the dog)) ;=> #f
; ********************************************************************

     
     


; ********************************************************************
; Representation of context free grammars
; ********************************************************************
; A Context Free Grammar is a list of two items:
; the start symbol of the grammar (a Scheme symbol) 
; and the list of rules of the grammar.

; Each rule is a list of two elements:
; (1) a Scheme symbol, the left-hand side of the rule, and
; (2) a list of Scheme symbols, the right-hand side of the rule.

; Here are selectors for the parts of a cfg and of a rule.

(define cfg-start-symbol car)
(define cfg-rules cadr)

(define rule-lhs car)
(define rule-rhs cadr)

; and a constructor for cfgs

(define make-cfg
  (lambda (start-symbol rules) (list start-symbol rules)))

; Here are the rules of a grammar for palindromes
; over the alphabet {a, b}.

(define rule1 '(s ()))           ; represents s -> empty string
(define rule2 '(s (a)))         ; represents s -> a
(define rule3 '(s (b)))         ; represents s -> b
(define rule4 '(s (a s a)))     ; represents s -> a s a
(define rule5 '(s (b s b)))     ; represents s -> b s b

; Here is the corresponding Context Free Grammar

(define cfg1 (make-cfg 's (list rule1 rule2 rule3 rule4 rule5)))

; This is a grammar for a fragment of English
; (With suitable apologies to Lewis Carroll.)

(define cfg2-rules
  (list
   '(<sentence> (<pronoun1> <verb1> <sentence>))
   '(<sentence> (<pronoun1> <verb2>))
   '(<sentence> (<pronoun1> gave <pronoun2> <pronoun3>))
   '(<pronoun1> (you))
   '(<pronoun1> (he))
   '(<pronoun1> (she))
   '(<pronoun1> (they))
   '(<pronoun2> (me))
   '(<pronoun2> (us))
   '(<pronoun2> (you))
   '(<pronoun2> (him))
   '(<pronoun2> (her))
   '(<pronoun2> (them))
   '(<pronoun3> (one))
   '(<pronoun3> (two))
   '(<pronoun3> (three or more))
   '(<verb1> (told <pronoun2>))
   '(<verb1> (sent <pronoun2> word))
   '(<verb2> (mentioned <pronoun2> to <pronoun2>))
   '(<verb2> (said <pronoun1> could not <verb3>))
   '(<verb3> (swim))
   '(<verb3> (spell))
   ))

; and the grammar:

(define cfg2
  (make-cfg '<sentence>
	    cfg2-rules))

; Here is a third example grammar

(define cfg3
  (make-cfg 
   's
   (list '(s (a vq long st))
	 '(vq (very vq1))
	 '(vq (quite vq1))
	 '(vq1 ())
	 '(vq1 (very vq1))
	 '(vq1 (quite vq1))
	 '(st (story))
	 '(st (tale)))))

; ********************************************************************
; ** problem 7 ** (10 points)
; Write a Context Free Grammar named

; cfg-chase

(define cfg-chase
  (make-cfg
   's
   '((s (first multi the noun))
     (first (the noun chased))
     (multi (the noun that chased))
     (multi ())
     (noun (cat))
     (noun (dog))
     (noun (mouse)))))

; in the given format that generates the language of the
; Regular Expression exp-chase (defined before problem (2).)

;; Some basic tests
; (list? cfg-chase) ;=> #t
; (length cfg-chase) ;=> 2
; (symbol? (cfg-start-symbol cfg-chase)) ;=> #t
; (list? (cfg-rules cfg-chase)) ;=> #t
; ********************************************************************


            
  

; ********************************************************************
; ** problem 8 ** (10 points)
; Write a Context Free Grammar named

; my-cfg

; in the representation given above
; to generate a set of strings of *your choosing*.
; It should have at least as many rules as cfg2, and
; generate a (somewhat) interesting infinite language.
; Please give (as comments) a number of examples
; of sentences it generates.

; Some basic tests

; (list? my-cfg) => #t
; (length my-cfg) => 2
; (symbol? (cfg-start-symbol my-cfg)) => #t
; (list? (cfg-rules my-cfg)) => #t
; (>= (length (cfg-rules my-cfg)) 22) => #t
; (equal? cfg2 my-cfg) => #f
; ********************************************************************

; _my_ _friend_ is _her_ _uncle's_ _gf's_ _sister's_ _aunt's_ _mother's_ _doctor_

 (define my-cfg
  (make-cfg
   's
   '((s (pro np verb pro np))
     (np (posn noun))
     (posn ())
     (posn (posn noun 's))
     (verb (is))
     (verb (likes))
     (verb (loves))
     (verb (knows))
     (verb (hates))
     (verb (met))
     (noun (friend))
     (noun (sister))
     (noun (father))
     (noun (brother))
     (noun (mother))
     (noun (aunt))
     (noun (uncle))
     (noun (cousin))
     (noun (wife))
     (noun (husband))
     (noun (girlfriend))
     (noun (boyfriend))
     (noun (boss))
     (noun (co-worker))
     (noun (doctor))
     (pro (my))
     (pro (her))
     (pro (his))
     (pro (your))
     (pro (our)))))
          
     
 ; remove-all function from hw2.scm
(define remove-all
  (lambda (item list)
    (cond
      ((null? list) 
       '())
      ((equal? (car list) 
               item) 
       (remove-all item (cdr list)))
      (else 
       (cons (car list) 
             (remove-all item (cdr list)))))))
; remove-duplicates function from hw2.scm
(define remove-duplicates
  (lambda (list)
    (if (null? list)
        '()
        (cons (car list) 
              (remove-duplicates (remove-all (car list) 
                                             (cdr list)))))))

; ********************************************************************
; ** problem 9 ** (10 points)
; Write six procedures

; (cfg-symbols cfg)
; (cfg-nonterminals cfg)
; (cfg-terminals cfg)
; (leftmost-nonterminal str cfg)
; (terminal-string? str cfg)
; (replace-leftmost sym str1 str2)

; (cfg-symbols cfg)
; returns the list of all the symbols that
; occur in cfg (including the start symbol
; and lhs and rhs of each rule), in the
; order of their first occurrence, with
; duplicates removed.

(define cfg-symbols
  (lambda (cfg)
    (remove-duplicates (flatten (cons (cfg-start-symbol cfg)
                                      (cfg-rules cfg))))))

; (cfg-nonterminals cfg)
; returns the list of all nonterminal symbols that
; occur in cfg, in the order of their first
; occurrence, with duplicates removed.
; A symbol is a *nonterminal symbol* if it 
; is the start symbol or it occurs as the
; lhs of some rule in the grammar.

(define cfg-nonterminals
  (lambda (cfg)
    (if (null? (cfg-rules cfg))
        '()
        (remove-duplicates
         (cons (rule-lhs (car (cfg-rules cfg)))
               (cfg-nonterminals
                (make-cfg (cfg-start-symbol cfg)
                          (cdr (cfg-rules cfg)))))))))

; (cfg-terminals cfg)
; returns the list of all terminal symbols that
; occur in cfg, in the order of their first
; occurrence, with duplicates removed.
; A symbol is a *terminal symbol* if it occurs
; in the grammar and is not a nonterminal symbol.

(define remove-overlap
  (lambda (lst targets)
    (if (null? targets)
        lst
        (remove-overlap (remove-all (car targets) lst) 
                        (cdr targets)))))

(define cfg-terminals
  (lambda (cfg)
    (remove-overlap (cfg-symbols cfg)
                    (cfg-nonterminals cfg))))

; (leftmost-nonterminal str cfg)
; returns the symbol that is the leftmost occurrence
; of a nonterminal symbol in the String str with respect to
; the grammar cfg.  If no nonterminal occurs in str,
; #f is returned.
(define first-overlap
  (lambda (lst targets)
    (cond
      ((null? lst) #f)
      ((contains? (car lst) targets) (car lst))
      (else (first-overlap (cdr lst) targets)))))

(define leftmost-nonterminal
  (lambda (str cfg)
    (first-overlap str (cfg-nonterminals cfg))))

; (terminal-string? str cfg)
; returns #t if the String str contains only terminal
; symbols of cfg, and #f otherwise.
; You may assume that str contains only symbols from cfg.

(define terminal-string?
  (lambda (str cfg)
    (if (leftmost-nonterminal str cfg)
        #f
        #t)))

; (replace-leftmost sym str1 str2)
; takes a symbol sym and two strings str1 and str2
; and returns the string obtained by substituting str2
; for the leftmost occurrence of sym in str1.
; If sym does not occur in str1, str1 is returned unchanged.

(define replace-leftmost
  (lambda (sym str1 str2)
    (if (contains? sym str1)
        (if (equal? (car str1) sym)
            (append str2 (cdr str1))
            (cons (car str1)
                    (replace-leftmost sym (cdr str1) str2)))
        str1)))

; Examples:
; (cfg-symbols cfg1) ;=> (s a b)
; (cfg-symbols cfg3) ;=> (s a vq long st very vq1 quite story tale)
; (cfg-nonterminals cfg1) ;=> (s)
; (cfg-nonterminals cfg3) ;=> (s vq vq1 st)
; (cfg-terminals cfg1) ;=> (a b)
; (cfg-terminals cfg3) ;=> (a long very quite story tale)
; (leftmost-nonterminal '(a a b s a) cfg1) ;=> s
; (leftmost-nonterminal '(a very long st) cfg3) ;=> st
; (terminal-string? '(a a b s a) cfg1) ;=> #f
; (terminal-string? '(long quite a) cfg3) ;=> #t
; (replace-leftmost '<thing> '(the <thing> eats the <thing>) '(big spider)) ;=> (the big spider eats the <thing>)
; (replace-leftmost 's '(alas and alack) '(matters not)) ;=> (alas and alack)
; (replace-leftmost  'yours '(what is mine is yours) '(thine)) ;=> (what is mine is thine)
; ********************************************************************

(define cfg-sym-options
  (lambda (rules sym)
    (cond
      ((null? rules) '())
      ((equal? (rule-lhs (car rules))
               sym)
       (cons (rule-rhs (car rules)) 
             (cfg-sym-options (cdr rules) sym)))
      (else (cfg-sym-options (cdr rules) sym)))))
        
(define nonterms-in-sym
  (lambda (sym cfg)
    (let ((nonterms (cfg-nonterminals cfg))
          (start (cfg-start-symbol cfg)))
      (cond
        ((null? sym) '())
        ((contains? (car sym) nonterms)
         (cons (car sym) (nonterms-in-sym (remove-all (car sym) (cdr sym))
                                          cfg)))
        (else (nonterms-in-sym (cdr sym)
                               cfg))))))

; ********************************************************************
; ** problem  10 ** (10 points)
; Write one procedure 

; (generate cfg)

(define generate
  (lambda (cfg)
    (let* ((start (cfg-start-symbol cfg))
           (start (if (list? start) start (list start)))
           (rules (cfg-rules cfg))
           (terms (cfg-terminals cfg)))
      (if (terminal-string? start cfg)
          start
          (let* ((start-nonterms (nonterms-in-sym start cfg))
                 (replace (car start-nonterms))
                 (options (cfg-sym-options rules replace))
                 (replacement (list-ref options 
                                        (random-integer (length options)))))
            (generate (make-cfg (replace-leftmost replace
                                                  start
                                                  replacement)
                                rules)))))))


; to generate a random String
; in the language of the Context Free Grammar cfg.
; Every String in the language of the grammar should
; be a possible output of your program, and no String
; not in the language of the grammar should be a possible output.
; You may assume that every nonterminal in the grammar
; generates at least one string of terminals.
; (This is to avoid problems like (s ((s t) (t s))).)

; Please include explanations of how your procedures work.

; Examples (your random examples may differ):
; (generate cfg1) => ()
; (generate cfg1) => (a)
; (generate cfg1) => (b a a a b)
; (generate cfg2) => (they told us she gave her one)
; (generate cfg2) => (he gave them one)
; (generate cfg2) => (he told us she sent me word he gave you three or more)
; (generate cfg3) => (a very very very quite long story)
; (generate cfg3) => (a quite very very long tale)
; (generate cfg-chase) => (the cat chased the dog that chased the mouse)
; ********************************************************************


; ********************************************************************
; End of homework #7
; ********************************************************************
