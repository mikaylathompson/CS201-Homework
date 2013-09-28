; ****************************************************************
; CS 201a HW #3  DUE by Wednesday, October 2, 11:59 pm
; using the submit command on the Zoo.
; ****************************************************************
; Name: Mikayla Thompson
; Email address: mikayla.thompson@yale.edu
; ****************************************************************

; Unless the problem specifies otherwise:
; (a) You may solve the problem using any method and 
; any Scheme constructs *except* mutators (set! and its relatives.)
; (b) You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one specifying what its arguments are
; and what result it returns; also, explain how it does it,
; if it is not obvious.
; (c) Please make your code as clear and readable as possible.

; The topics of this assignment are:
; a simulator for Turing machines and
; writing Turing machine programs.

; ****************************************************************
; ** problem 0 ** (1 easy point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 3)

; ****************************************************************
; Turing machines were described in the lectures;
; see also the lecture notes on the course web page.
; Here is a top-level procedure to simulate a Turing machine
; starting from a given configuration until either
; it halts or it has executed n steps.
; The procedure returns the list of the successive configurations,
; starting with the initial one.
; The length of the list of configurations is one more than 
; the number of steps taken by the machine.

(define simulate 
  (lambda (mach config n)
    (cond
     ((<= n 0) (list config))
     ((halted? mach config) (list config))
     (else
      (cons config
	    (simulate 
	     mach (next-config mach config) (- n 1)))))))

; mach is a representation of a Turing machine
; config is a representation of a configuration of the machine
; n is the maximum number of steps to simulate

; The procedures halted? and next-config will be
; developed in the problems below; you will then
; have a complete Turing machine simulator.

; ****************************************************************
; A Turing machine is represented as a list of instructions, 
; where each instruction is a 5-tuple, represented as a list: 

; current state, current symbol, new state, new symbol, and move direction

; The current state and new state are Scheme symbols,
; the current symbol and new symbol are symbols or numbers,
; and the move direction must be either the symbol l or the symbol r.
; (NB: L and l are the same symbols in R5RS, as are R and r.)

; Example: (q1 0 q3 1 l)
; is an instruction
; with current state q1, current symbol 0,
; new state q3, new symbol 1, and move direction l.

; Here are selectors for the parts of an instruction:
; Please use them in your code when you refer to parts
; of Turing machine instructions  -- they will be a lot more
; mnemonic than the corresponding list-refs or caddrs

(define i-state (lambda (inst) (list-ref inst 0)))
(define i-symbol (lambda (inst) (list-ref inst 1)))
(define i-new-state (lambda (inst) (list-ref inst 2)))
(define i-new-symbol (lambda (inst) (list-ref inst 3)))
(define i-direction (lambda (inst) (list-ref inst 4)))

; A Turing machine is simply a list of instructions.

; Example: a Turing machine that when started in state q1
; on the leftmost of a string of 0's and 1's 
; changes all the 0's to 1's and ; all the 1's to 0's 
; and then returns the head to the leftmost symbol and halts.

(define tm1 (list
	     '(q1 0 q1 1 r)
	     '(q1 1 q1 0 r)
	     '(q1 b q2 b l)
	     '(q2 0 q2 0 l)
	     '(q2 1 q2 1 l)
	     '(q2 b q3 b r)))

; ****************************************************************
; ** problem 1 (15 points)
; Define (in the format just given)
; a Turing machine named

; tm-mirror

; that takes an input string;
; and produces an output string equal
; to the reverse of the input string
; concatenated with the input string.

; That is, when started in state q1
; with the head on the leftmost of a
; string of 0's and 1's, it halts
; with the head on the leftmost of a
; string of 0's and 1's, and the
; output string is obtained from
; the input string by reversing it
; and concatenating it with itself.

; Your machine *may* use additional tape symbols
; but the output should contain no
; symbols other than 0, 1 and blank.
; When the machine halts, symbols
; other than the output should be blank.

; For example, the behavior of
; the machine should be:
; input  =>  output
; 110    =>  011110
; 101011 =>  110101101011
; 1      =>  11
; 0001   =>  10000001
; empty string => empty string

; (It may help to review ideas from the
; machine to make a copy of its input,
; described in lectures and in the 
; online lecture notes.)

; The initial state of your machine should
; be q1 -- other states may be named with
; Scheme symbols of your choice.

; IMPORTANT: please include an overview
; description of how your Turing machine works.
; Note that you'll be able to run it once you get
; the procedures for the simulator working.

; ****************************************************************

;  | |0|1|0|1| | | | | |    q1--go to end of row(don't change anything)
;     ^
;  | |0|1|0|1| | | | | |    q2--go backwards to nearest number. To q3 if one, q4 if zero
;             ^
;  | |0|1|0|y| | | | | |    q3--change this number to y(one) then forward until a blank, add a 1, to q5
;           ^
;  | |0|1|0|y|1| | | | |    q5--go backwards until a y or z, then one more.  To q2.
;             ^
;  | |0|1|0|y|1| | | | |    q2--go backwards to nearest number. To q3 if one, q4 if zero
;           ^
;  | |0|1|z|y|1| | | | |    q4--change to a z, then forward until a blank, add a 0, to q5
;         ^
;  | |0|1|z|y|1|0| | | |    repeat with q5-->q2-->q3/q4-->q5 until q5 hits a blank.  to q6
;               ^
;  | |z|y|z|y|1|0|1|0| |    q6--replace every z with 0 and y with 1 until it hits a number
;     ^
;  | |0|1|0|1|1|0|1|0| |    q7--return to beginning of string
;             ^
;  | |0|1|0|1|1|0|1|0| |
;     ^


(define tm-mirror (list
                   '(q1 0 q1 0 r)
                   '(q1 1 q1 1 r) ;  Goes forward to end of string
                   '(q1 b q2 b l) 
                   
                   '(q2 y q2 y l) 
                   '(q2 z q2 z l) 
                   '(q2 1 q3 y r) ;  Goes left until 0 or 1
                   '(q2 0 q2 z r) 
                   '(q2 b q2 b l) 
                   
                   '(q3 1 q3 1 r)
                   '(q3 0 q3 0 r) ;  Saves and deposits a 1
                   '(q3 b q5 1 l)
                   
                   '(q4 1 q4 1 r)
                   '(q4 0 q4 0 r) ;  Saves and deposits a 0
                   '(q4 b q5 0 l)
                   
                   '(q5 1 q5 1 l)
                   '(q5 0 q5 0 l)
                   '(q5 y q2 y l) ;  Go left until y, z, or b
                   '(q5 z q2 z l)
                   '(q5 b q6 b r)
                   
                   '(q6 y q6 1 r)
                   '(q6 z q6 0 r) ;  Replace y's and z's
                   '(q6 0 q7 0 l)
                   '(q6 1 q7 1 l)
                   
                   '(q1 0 q1 0 l)
                   '(q1 1 q1 0 l) ;  Goes left until a blank.  Ends.
 
                   ))
                   

; ****************************************************************
; ** problem 2 (9 points)
; Write the following two procedures.

; Please use the instruction selectors defined above:
; i-state, i-symbol, i-new-state, i-new-symbol, i-direction

; (i-match? state symbol inst)
; returns #t if state and symbol are equal to 
; the state and symbol of instruction inst
; otherwise returns #f

; (i-lookup state symbol mach)
; returns #f if no instruction of Turing machine mach 
; has state and symbol equal to state and symbol
; otherwise returns the instruction in mach that matches.
; You may assume that at most one instruction will match.

; Examples:
; (i-match? 'q1 'b '(q1 b q3 b l)) => #t
; (i-match? 'q1  0 '(q1 1 q4 1 l)) => #f
; (i-match? 'q2 1 '(q2 1 q2 1 l)) => #t
; (i-lookup 'q1 1 tm1) => (q1 1 q1 0 r)
; (i-lookup 'q2 'b tm1) => (q2 b q3 b r)
; (i-lookup 'q3 1 tm1) => #f

; ****************************************************************

(define i-match?
  (lambda (state symbol inst)
    (and (equal? state (i-state inst))
         (equal? symbol (i-symbol inst)))))

(define i-lookup
  (lambda (state symbol machine)
    (cond
      ((null? machine) #f)
      ((i-match? state symbol (car machine)) (car machine))
      (else (i-lookup state symbol (cdr machine))))))

; ****************************************************************
; A Turing machine configuration is a list, 
; each element of which is a symbol, a number, 
; or a list containing one symbol.  
; There must be exactly *one* element that
; consists of a list with one symbol.

; For example, we define the following two configurations:

(define config1 '(0 (q3) 1 1))
(define config2 '(1 b (q6) 0 b b))

; config1 represents the Turing machine configuration

;   -------------------------
;   .. |  | 0 | 1 | 1 |  | ..
;   -------------------------
;               ^
;               q3

; in which the non-blank symbols on the tape are 011,
; and the machine is in state q3 with the read/write head
; scanning the leftmost 1.


; config2 represents the Turing machine configuration

;   ------------------------------
;   .. |   | 1 |  | 0 |   |   | ..
;   ------------------------------
;                   ^
;                   q6

; in which the symbols 1, blank, 0, are on the tape, surrounded
; by blanks, and the machine is in state q6 with the read/write
; head scanning the 0.

; The single element that is a list gives the current state
; of the machine, and is placed immediately to the left of
; the symbol scanned by the read/write head.
; We use the symbol b for the tape symbol blank.  

; A configuration is *normalized* if neither its first nor its
; last symbol is the symbol b.  Of the two configurations above,
; config1 is normalized, but config2 is not (its last element is b).

; Note that tape squares not explicitly represented are
; assumed to contain blanks.  A normalized configuration
; to represent the machine in state q1 with all tape squares
; blank is thus ((q1)).

; ****************************************************************
; ** problem 3 (15 points)
; Write the following three procedures.

; (normalize config)
; takes a configuration config and
; returns an equivalent *normalized* configuration.
; That is, the same Turing machine configuration is
; represented by the input configuration and the
; output configuration, and the output configuration
; does not have a b as its first or last element.

; (change-state new-state config)
; takes a normalized configuration config and
; returns a normalized configuration
; in which the state of the machine is
; changed to new-state.

; (write-symbol new-symbol config)
; takes a normalized configuration config and
; returns a normalized configuration
; in which the symbol scanned by the read/write head
; has been replaced by new-symbol.

; Examples:
; (normalize '(b 0 (q3) b 1 1 0 b b)) => (0 (q3) b 1 1 0)
; (normalize '(b b (q4) b b b b)) => ((q4))
; (normalize '(b 0 b 0 (q33) 1 0 b 0 b)) => (0 b 0 (q33) 1 0 b 0)
; (change-state 'q2 '(0 (q1) 1)) => (0 (q2) 1)
; (change-state 'q13 '(0 1 1 (q4))) => (0 1 1 (q13))
; (write-symbol '1 '(0 (q41) 0 1 1 0)) => (0 (q41) 1 1 1 0)
; (write-symbol 'c '(0 0 1 (q2) 1 1 1)) => (0 0 1 (q2) c 1 1)
; (write-symbol 'b '(1 (q3) 0)) => (1 (q3))
; ****************************************************************
;normalize: check if first (car config) is blank.  If so, send (cdr config) to normalize
;           check if the last (list-ref config (length config)) is blank.  If so, remove it.

(define normalize
  (lambda (config)
    (cond
      ((and (equal? (car config) 'b)
            (equal? (list-ref config (- (length config) 1)) 'b))
       (normalize (reverse (cdr (reverse (cdr config))))))
      ((equal? (car config) 'b) (normalize (cdr config)))
      ((equal? (list-ref config (- (length config) 1)) 'b) (normalize (reverse (cdr (reverse config)))))
      (else config))))
    

(define change-state
  (lambda (state config)
    (if (list? (car config))
        (cons (cons state '()) (cdr config))
        (cons (car config) (change-state state (cdr config))))))

; write-symbol:
;              find the head
;              change the next symbol to sym (or add sym to the end)
;              renormalize
; but I can't normalize during the main loop, because that might take out blanks in the middle of the config.
; so write-symbol-helper does the finding and changing, and write-symbol itself just normalizes the output from there

(define write-symbol
  (lambda (sym config)
    (normalize (write-symbol-helper sym config))))

(define write-symbol-helper
  (lambda (sym config)
    (if (list? (car config))   ; If I'm on the head
        (if (null? (cdr config))   ; If the head is at the end of the tape
            (cons (car config) (cons sym '()))  ; add sym behind the head
            (cons (car config) (cons sym (cdr (cdr config)))))  ; Changes symbol, adds to rest of list
        (cons (car config) (write-symbol-helper sym (cdr config))))))  ; Not on the head, so keep looking



; ****************************************************************
; ** problem 4 ** (10 points)
; Write two procedures

; (shift-head-left config)
; takes a normalized configuration config and
; returns a normalized configuration in which
; the position of the read/write head has been moved one
; tape square to the left.

; (shift-head-right config)
; takes a normalized configuration config and
; returns a normalized configuration in which
; the position of the read/write head has been moved one
; tape square to the right.

; Examples:
; (shift-head-left '((q5))) => ((q5))
; (shift-head-left '(0 0 (q6) 1 1 1)) => (0 (q6) 0 1 1 1)
; (shift-head-left '((q12) 0 1 1 0)) => ((q12) b 0 1 1 0)
; (shift-head-right '((q2))) => ((q2))
; (shift-head-right '((q9) 0 1 1 1)) => (0 (q9) 1 1 1)
; (shift-head-right '(1 0 1 1 (q8))) => (1 0 1 1 b (q8))
; ****************************************************************

; shift-head-left: find the spot before the head.  (list? (car (cdr config)))
;                  add it to the front of the list, removing it from it's current spot.

(define shift-head-left
  (lambda (config)
    (cond
      ((list? (car config)) (normalize (cons (car config) (cons 'b (cdr config)))))  ;Already on the head?  Add a blank
      ((list? (car (cdr config))) (cons (car (cdr config)) (cons (car config) (cdr (cdr config))))) ;Next is the head?  Move it up.
      (else (cons (car config) (shift-head-left (cdr config))))))) ;Not to the head yet?  Keep looking.

(define shift-head-right
  (lambda (config)
    (reverse (shift-head-left (reverse config)))))


; ****************************************************************
; ** problem 5 ** (15 points)
; Write the following three procedures.

; (c-state config)
; given a normalized configuration config, returns
; the current state of the machine in the configuration.

; (c-symbol config)
; given a normalized machine configuration, returns
; the symbol currently scanned by the read/write head.

; (halted? mach config)
; that returns #t if the Turing machine mach is halted in 
; machined configuration config (ie, no instruction of the
; machine matches the current state and symbol in
; configuration) and #f otherwise.

; Examples:
; (c-state '(0 0 (q6) 1 1)) => q6
; (c-state '((q4))) => q4
; (c-symbol '(0 0 (q6) 1 1)) => 1
; (c-symbol '((q4))) => b
; (halted? tm1 '(1 1 0 (q1))) => #f
; (halted? '((q1 b q2 b r)) '((q2))) => #t
; ****************************************************************


(define c-state
  (lambda (config)
    (if (list? (car config))
        (car (car config))
        (c-state (cdr config)))))

(define c-symbol
  (lambda (config)
    (if (list? (car config))
        (if (null? (cdr config))
            'b
            (car (cdr config)))
        (c-symbol (cdr config)))))
        

; ****************************************************************
; ** problem 6 ** (20 points)
; Write a procedure 

; (next-config mach config)

; that takes a Turing machine mach
; and a normalized configuration config
; and returns the (normalized) next configuration 
; for the Turing machine mach in the configuration config.
; If there is no applicable instruction, the configuration
; returned should be just the input configuration.

; Hint: get your procedures
; halted?, i-lookup, write-symbol, shift-head-left, shift-head-right
; working and combine them appropriately.

; Examples:
; (next-config tm1 '((q1) 0 0 1)) => (1 (q1) 0 1)
; (next-config tm1 '(1 (q1) 0 1)) => (1 1 (q1) 1)
; (next-config tm1 '(1 1 0 (q1))) => (1 1 (q2) 0)
; (next-config tm1 '((q2) b 1 1 0)) => ((q3) 1 1 0)
; (next-config tm1 '((q3) 1 1 0)) => ((q3) 1 1 0)
; ****************************************************************

; (Please replace this comment with your procedure(s).)		   

; ****************************************************************
; If your procedures are working, then you should
; be able to run the following example, which
; shows the successive normalized configurations 
; of Turing machine tm1 when run from the given configuration.

;; > (simulate tm1 '((q1) 1 1 0 1 0) 20)
;; (((q1) 1 1 0 1 0)
;;  (0 (q1) 1 0 1 0)
;;  (0 0 (q1) 0 1 0)
;;  (0 0 1 (q1) 1 0)
;;  (0 0 1 0 (q1) 0)
;;  (0 0 1 0 1 (q1))
;;  (0 0 1 0 (q2) 1)
;;  (0 0 1 (q2) 0 1)
;;  (0 0 (q2) 1 0 1)
;;  (0 (q2) 0 1 0 1)
;;  ((q2) 0 0 1 0 1)
;;  ((q2) b 0 0 1 0 1)
;;  ((q3) 0 0 1 0 1))

; ****************************************************************
; ** problem 7 ** (15 points)
; Define (in the given Scheme representation)
; a Turing machine named

; tm-times

; that takes as input two positive integers m and n
; in unary and produces as output the product of
; m and n in unary.

; A number n is represented in unary by a sequence of n 1's.
; The numbers m and n are separated by the symbol c.
; The output of the machine should be
; a sequence of 1's representing the desired result.
; The read/write head should be on the leftmost
; nonblank symbol of the output
; and all other squares should be blank.

; You *may* use additional tape symbols.

; IMPORTANT: Include a clear overview
; description of how your Turing machine works.
; Give and justify a good estimate of
; number of steps your machine will take on
; inputs of lengths m and n.

; NOTE: you can still do this problem if your simulator
; is not working, assuming you understand Turing machines
; and the Scheme representation of them given above.

; Examples
; input          => output
; 1c1            => 1
; 11c1           => 11
; 11c111         => 111111

; Here are input configurations if
; you want to simulate your tm-times on
; these inputs.

(define times1c1 '((q1) 1 c 1))
(define times2c1 '((q1) 1 1 c 1))
(define times2c3 '((q1) 1 1 c 1 1 1))

; ****************************************************************

; (Please replace this comment with your definition of tm-times.)

; ****************************************************************
; *************** end of hw3.scm *********************************

