;**********************************************************
; CS 201a HW #5, due Wednesday, October 30, at 11:59 pm
; using the submit command.
;**********************************************************
; Name:
; Email address:
;**********************************************************

; Computer science topics: gates and circuits

; Unless the problem specifies otherwise:
; * You may solve the problem using any method 
; and any Scheme constructs, except mutators (set! and its relatives.)
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;**********************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 3)

;**********************************************************
; Here are procedures to compute some Boolean functions
; with 0 = false and 1 = true.

(define b-not (lambda (x) (if (equal? x 1) 0 1)))
(define b-and (lambda (x y) (if (equal? x 0) 0 y)))
(define b-or (lambda (x y) (if (equal? x 1) 1 y)))
(define b-xor (lambda (x y) (if (equal? x 0) y (b-not y))))
(define b-nand (lambda (x y) (b-not (b-and x y))))
(define b-nor (lambda (x y) (b-not (b-or x y))))
(define b-ident (lambda (x) x))

; Here is a useful table of symbols naming these functions

(define fn-table
  (list
   (list 'not b-not)
   (list 'and b-and)
   (list 'or b-or)
   (list 'xor b-xor)
   (list 'nand b-nand)
   (list 'nor b-nor)
   (list 'ident b-ident)))

;**********************************************************

; Wires and gates.

; A wire is identified by a Scheme symbol.

; A gate is a list of three elements:
; 1) a symbol indicating the type of gate, one of:
;       not, and, or, xor, nand, nor, ident
; corresponding to the Boolean functions defined above:
;       b-not, b-and, b-or, b-xor, b-nand, b-nor, b-ident
; 2) the list of the input wire identifiers
; 3) the output wire identifier

; Selectors for gates -- please use these to refer to parts of a gate.

(define gate-fn (lambda (gate) (list-ref gate 0)))
(define gate-inputs (lambda (gate) (list-ref gate 1)))
(define gate-output (lambda (gate) (list-ref gate 2)))

; Examples of gates:

(define gate1 '(and (x y) z))
(define gate2 '(or (x v) v))
(define gate3 '(not (s) t))
(define gate4 '(nand (x x) z))
(define gate5 '(ident (x) y))

; Examples of the gate selectors:

; (gate-fn gate1) => and
; (gate-inputs gate2) => (x v)
; (gate-output gate3) => t
; (gate-inputs gate4) => (x x)
; (gate-fn gate5) => ident

;**********************************************************

; Circuits.

; A circuit is represented as a list of 3 items:
; (1) a list of input wire identifiers,
; (2) a list of output wire identifiers.
; (3) a list of gates,

; Here are selectors for a circuit -- please use them
; to access parts of a circuit.

(define ckt-inputs (lambda (ckt) (list-ref ckt 0)))
(define ckt-outputs (lambda (ckt) (list-ref ckt 1)))
(define ckt-gates (lambda (ckt) (list-ref ckt 2)))

;**********************************************************
; Examples of circuits

; Here is a circuit to compare the values of its two inputs
; and output 1 if they are equal, 0 if they are unequal.
; This computes one-bit compare for equality, and implements
; the sum-of-products representation.  This is a combinational
; circuit (no cycle of wires and gates.)

(define ckt-eq1
  '((x y)
    (z)
    ((not (x) cx)
     (not (y) cy)
     (and (x y) t1)
     (and (cx cy) t2)
     (or (t1 t2) z))))

; This is interpreted as follows:
; the inputs of the circuit are the wires x and y,
; the outputs of the circuit consist of just the wire z,
; there are five gates specified as follows:
; wire cx is the output of a NOT gate with input x,
; wire cy is the output of a NOT gate with input y,
; wire t1 is the output of an AND gate with inputs x and y,
; wire t2 is the output of an AND gate with inputs cx and cy,
; wire z is the output of an OR gate with inputs t1 and t2.

; Here is another implementation of comparing two bits for equality.
; This uses the implementation as the NOT of (x XOR y).
; This is also a combinational circuit.
; The inputs and output of this circuit are named as in ckt-eq1.

(define ckt-eq2
  '((x y)
    (z)
    ((xor (x y) w)
     (not (w) z))))

; Here is a two-bit selector:
; z_1 = x_1 * s' + y_1 * s
; z_0 = x_0 * s' + y_0 * s
; This is also a combinational circuit.

(define ckt-sel
  '((x1 x0 y1 y0 s)
    (z1 z0)
    ((not (s) sc)
     (and (x1 sc) u1)
     (and (y1 s) v1)
     (or (u1 v1) z1)
     (and (x0 sc) u0)
     (and (y0 s) v0)
     (or (u0 v0) z0))))

; This is a NAND latch, used to store one bit.
; It is a sequential (not combinational) circuit,
; because it has a cycle of wires and gates.

(define ckt-latch
  '((x y)
    (q u)
    ((nand (x u) q)
     (nand (y q) u))))

; This is also a sequential circuit, with
; an OR gate one of whose inputs is its output.

(define ckt-seq-or
  '((x)
    (z)
    ((or (x z) z))))

; This is also a sequential circuit.
; It could serve as a clock.
; Note that this circuit has *no* inputs, but
; does have an output.

(define ckt-clock
  '(()
    (z)
    ((not (z) z))))

;**********************************************************
; ** problem 1 ** (9 points)
; Write one procedure:

; (circuit? value)

; which take an arbitrary Scheme value as input and
; returns #t if value is a well-formed circuit, 
; and returns #f otherwise.

; To be a well-formed circuit, it must be
; a list of 3 elements, the first two being
; lists of wires, and the third being a list of
; gates, as defined above.  In addition:
; (1) no input of the circuit is the output of a gate,

; (2) every input of a gate is either 
; an input of the circuit or the output of a gate,

; (3) no wire is the output of two or more gates,

; (4) every output of the circuit is either an input
; of the circuit or the output of a gate.

; Examples (may not test all the conditions above):
; (circuit? ckt-sel) => #t
; (circuit? ckt-latch) => #t
; (circuit? ckt-clock) => #t
; (circuit? ckt-seq-or) => #t
; (circuit? 'hi) => #f
; (circuit? '(() () ())) => #t
; (circuit? '((1 2) (3) ((and (1 2) 3)))) => #f
; (circuit? '((x y) (z) ((or (x y) x) (and (x y) z)))) => #f
; (circuit? '((x y) (z) ((and (x y) z) (or (x y) z)))) => #f
; (circuit? '((x y) (u z) ((and (x y) z)))) => #f
;**********************************************************




; Plan:
; list all gate inputs
; list all gate outputs
;    (3) return false if an output is already in the list
; (1) circuit inputs can not be included in gate outputs
; (2) all gate inputs must be in gate outputs or circuit inputs
; (4) all circuit outputs must be either gate outputs or circuit inputs

; flatten turns a nested list into a flat one
(define flatten
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((list? (car lst))
       (append (flatten (car lst)) (flatten (cdr lst))))
      (else 
       (cons (car lst) (flatten (cdr lst)))))))

; includes? takes a list and a target.  It returns true
; if target is a top level element of list. (from hw1)
(define includes?
  (lambda (list target)
    (if (equal? '() list)
        #f
        (or (equal? (car list) 
                    target)
            (includes? (cdr list) target)))))

; returns true if any items in a list are repeated within the list.
(define duplicates-in?
  (lambda (lst)
    (cond
      ((null? lst) #f)
      ((includes? (cdr lst) (car lst))
       #t)
      (else
       (duplicates-in? (cdr lst))))))

; returns the inputs of all gates in a circuit
(define all-gate-inputs
  (lambda (gates)
    (cond
      ((null? gates)
       '())
      (else
       (flatten (map gate-inputs gates))))))


; returns the outputs of all gates in a circuit, or false if any are repeated
(define all-gate-outputs
  (lambda (gates)
    (cond
      ((null? gates)
       '())
      (else
       (flatten (map gate-output gates))))))



; returns true if ANY of the items in targets are also included in pool.
(define any-included?
  (lambda (targets pool)
    (cond
      ((null? targets)
       #f)
      ((includes? pool (car targets))
        #t)
      (else
       (any-included? (cdr targets) pool)))))


; returns true if ALL of the items in targets are also included in pool.g
(define all-included?
  (lambda (targets pool)
    #t))


(define circuit?
  (lambda (ckt)
    (cond
      ((not (list? ckt))
       #f)
      ((not (= 3 (length ckt)))
       #f)
      (else
       (let 
           ((circuit-inputs (ckt-inputs ckt))
            (circuit-outupts (ckt-outputs ckt))
            (ckt-gate-inputs (all-gate-inputs (ckt-gates ckt)))
            (ckt-gate-outputs (all-gate-outputs (ckt-gates ckt))))
         (cond
           ((duplicates-in? ckt-gate-outputs)            ; (3)
             (display "Failed condition 3")
              #f)
           ((any-included? circuit-inputs 
                          ckt-gate-outputs)              ; (1)
            (display "Failed condition 1")
            #f)
           ((not (all-included? ckt-gate-inputs 
                               (append ckt-gate-outputs 
                                       circuit-inputs))) ; (2)
            (display "Failed condition 2")
            #f)
           ((not (all-included? circuit-outupts
                               (append ckt-gate-outputs
                                       circuit-inputs)))  ; (4)
           (display "Failed condition 4")
           #f)
           (else #t)))))))
            
        
 ;(circuit? ckt-sel) ;=> #t
 ;(circuit? ckt-latch) ;=> #t
 ;(circuit? ckt-clock) ;=> #t
 ;(circuit? ckt-seq-or) ;=> #t
 ;(display "\n\n")
 ;(circuit? 'hi) ;=> #f
 ;(circuit? '(() () ())) ;=> #t
 (display "\n\n")
 (circuit? '((1 2) (3) ((and (1 2) 3)))) ;=> #f
 (circuit? '((x y) (z) ((or (x y) x) (and (x y) z)))) ;=> #f
 (circuit? '((x y) (z) ((and (x y) z) (or (x y) z)))) ;=> #f
 (display "\nShould fail #4 \n")
 (circuit? '((x y) (u z) ((and (x y) z)))) ;=> #f


;**********************************************************
; Here are some procedures that allow the testing of
; outputs without regard to order.  

; tests whether all the top-level elements of lst are distinct

(define t-unique?
  (lambda (lst)
    (cond
     ((null? lst) #t)
     ((member (car lst) (cdr lst)) #f)
     (else (t-unique? (cdr lst))))))

; tests whether every top-level element of lst1 is a top-level
; element of lst2

(define t-subset?
  (lambda (lst1 lst2)
    (cond
     ((null? lst1) #t)
     ((not (member (car lst1) lst2)) #f)
     (else (t-subset? (cdr lst1) lst2)))))

; tests whether lst1 has all top-level elements distinct,
; whether lst2 has all top-level elements distinct, and
; whether the top-level elements of lst1 are a permutation
; of the top-level elements of lst2.

(define t-permutation?
  (lambda (lst1 lst2)
    (and (t-unique? lst1)
	 (t-unique? lst2)
	 (t-subset? lst1 lst2)
	 (t-subset? lst2 lst1))))

; tests whether lst1 and lst2 are lists of the same length
; and whether their corresponding pairs of top-level elements 
; are permutations of each other

(define t-compare-as-permutations?
  (lambda (lst1 lst2)
    (cond
     ((not (= (length lst1) (length lst2))) 'unequal-length-lists)
     (else (not (member #f (map t-permutation? lst1 lst2)))))))

; Examples:
; (t-unique? '(a b c b d)) => #f
; (t-subset? '(a a b) '(c b a)) => #t
; (t-permutation? '(b a c) '(c b a)) => #t
; (t-compare-as-permutations? '((a b) (c d e)) '((b a) (d e c))) => #t

;**********************************************************
; ** problem 2 ** (10 points)
; Write two procedures.

; (ckt-wires ckt) to return the list of all the wire names that appear
;      in the circuit ckt, as circuit inputs, circuit outputs, gate
;      inputs or gate outputs, with duplicates removed.
; (find-gate wire ckt) to return the gate in the circuit ckt with the given
;      output wire, or #f if there is no such gate.

; You may assume that ckt is a well-formed circuit; in particular,
; a wire is the output of *at most one* gate.

; Examples:

; (t-permutation? (ckt-wires ckt-eq1) '(x y z cx cy t1 t2)) => #t
; (t-permutation? (ckt-wires ckt-sel) '(x1 x0 y1 y0 s z1 z0 sc u1 v1 u0 v0)) => #t
; (find-gate 't2 ckt-eq1) => (and (cx cy) t2)
; (find-gate 'w ckt-eq2) => (xor (x y) w)
; (find-gate 'y ckt-sel) => #f
;**********************************************************


;**********************************************************
; ** problem 3 ** (10 points)
; Define circuits for a half-adder and a full-adder
; in the representation described above.

; Your half-adder should be called 
; ckt-ha
; and should have input wires: x and y and 
; output wires: z and co, 
; where  z is the mod 2 sum of x and y, 
; and co is 1 if both x and y are 1.

; Your full-adder should be called 
; ckt-fa 
; and should have input wires: x, y, and ci and 
; output wires: z and co,
; where the value of z is the mod 2 sum of x, y, and ci,
; and the value of co is 1 if and only if at least two
; of x, y, and ci are 1.

; The order and names of the circuit input and output 
; wires should be as specified above, but 
; the number and names of internal wires (wires that
; are neither circuit inputs nor circuit outputs)
; are up to you.

; Examples:
; (circuit? ckt-ha) => #t
; (circuit? ckt-fa) => #t
; (ckt-inputs ckt-ha) => (x y)
; (ckt-outputs ckt-ha) => (z co)
; (ckt-inputs ckt-fa) => (x y ci)
; (ckt-outputs ckt-fa) => (z co)

; (output-values ckt-ha (final-config ckt-ha (init-config ckt-ha '(1 1)))) => (0 1)
; (output-values ckt-fa (final-config ckt-fa (init-config ckt-fa '(1 1 1)))) => (1 1)

; For the last two tests, your procedures output-values, final-config, init-config
; must be working.
;**********************************************************


;**********************************************************
; A configuration of a circuit is a table giving a value (0 or 1)
; for each wire in the circuit.  Examples:

(define eq1-config1 '((x 0) (y 1) (z 0) (cx 0) (cy 0) (t1 0) (t2 0)))
(define eq1-config2 '((x 0) (y 0) (z 1) (cx 1) (cy 1) (t1 0) (t2 0)))
(define sel-config1 '((x1 0) (x0 1) (y1 1) (y0 0) (s 1) (z1 0) 
                      (z0 0) (sc 0) (u1 0) (v1 0) (u0 0) (v0 0)))
(define sel-config2 '((x1 1) (x0 1) (y1 0) (y0 0) (s 0) (z1 0)
		      (z0 0) (sc 1) (u1 0) (v1 0) (u0 0) (v0 0)))
(define latch-config1 '((x 0) (y 0) (q 0) (u 0)))
(define latch-config2 '((x 0) (y 1) (q 1) (u 0)))

;**********************************************************
; ** problem 4 ** (10 points)
; Write a procedure (next-value wire ckt config)
; that returns the value on the given wire
; of the given circuit ckt, *after one gate delay*
; starting with the given configuration config of ckt.

; You may assume that
; (1) ckt is a well-formed circuit,
;     according to the specifications in problem 1,
; (2) the given wire is one of the wires of ckt, and
; (3) the given configuration config specifies a value for every
;     wire in ckt.

; If the given wire is an input wire of the circuit,
; its next value is just its value in the configuration config.

; If the given wire is the output wire of a gate,
; its next value is obtained by finding the gate of ckt for
; which it is the output wire, looking up the values
; of the input wires of the gate in the configuration config,
; and applying the gate function of the gate to
; the input values.

; Hint: the definition of fn-table may be useful here.

; Note that this doesn't compute the *eventual* value
; (if any) of the wire, just the *next* value of the wire,
; after one gate delay.

; You may want to write an auxiliary procedure
; to look up the value of a wire in a configuration.

; Examples:
; (next-value 'cx ckt-eq1 eq1-config1) => 1
; (next-value 't2 ckt-eq1 eq1-config1) => 0
; (next-value 'z ckt-eq1 eq1-config2) => 0
; (next-value 'x0 ckt-sel sel-config1) => 1
; (next-value 'v1 ckt-sel sel-config1) => 1
; (next-value 'v0 ckt-sel sel-config2) => 0
;**********************************************************


;**********************************************************
; ** problem 5 ** (10 points)
; Write a procedure (next-config ckt config)
; that takes a circuit and a current configuration config
; and returns the "next" configuration of the circuit,
; after *one gate delay* has elapsed.

; In the "next" configuration of the circuit:
; the value of each wire is the result
; of applying the next-value procedure to the wire,
; circuit and the configuration config.

; Thus, values on the input wires do not
; change, and each wire that is the output
; of a gate has the value determined by
; its gate function applied the the values
; of its input wires in the configuration config.

; This is a rather simplified model of
; the time-varying behavior of wires and gates.

; Examples:
; (t-permutation? (next-config ckt-eq1 eq1-config1) '((x 0) (y 1) (z 0) (cx 1) (cy 0) (t1 0) (t2 0))) => #t
; (t-permutation? (next-config ckt-eq1 eq1-config2) '((x 0) (y 0) (z 0) (cx 1) (cy 1) (t1 0) (t2 1))) => #t
; (t-permutation? (next-config ckt-sel sel-config1) '((x1 0) (x0 1) (y1 1) (y0 0) (s 1) (z1 0) (z0 0) (sc 0) (u1 0) (v1 1) (u0 0) (v0 0))) => #t
; (t-permutation? (next-config ckt-sel (next-config ckt-sel sel-config1)) '((x1 0) (x0 1) (y1 1) (y0 0) (s 1) (z1 1) (z0 0) (sc 0) (u1 0) (v1 1) (u0 0) (v0 0))) => #t
; (t-permutation? (next-config ckt-latch latch-config1) '((x 0) (y 0) (q 1) (u 1))) => #t
; (t-permutation? (next-config ckt-latch latch-config2) '((x 0) (y 1) (q 1) (u 0))) => #t
;**********************************************************


;**********************************************************
; ** problem 6 ** (10 points)
; Write five procedures

; (stable? ckt config)
; (output-values ckt config)
; (zero-config ckt)
; (set-wires new-values config)
; (init-config ckt input-values)

; (stable? ckt config)
; returns #t if the next configuration
; of circuit ckt from the configuration config
; is the same as config, that is, this
; configuration is stable for the circuit ckt.

; (output-values ckt config)
; returns a list giving the values of each
; of the output wires of circuit ckt in the
; configuration config; the order is that of
; the list of output wires of the circuit.

; (zero-config ckt)
; returns a configuration for the given circuit
; in which every wire has value 0.

; (set-wires new-values config)
; takes a table new-values of wires and values
; and a configuration config
; and returns a configuration with
; the same wires as config, in the same order.
; In the output configuration each wire has
; the value it is given in new-values (if
; it occurs in new-values); otherwise, it has
; the value it has in config.

; (init-config ckt input-values)
; takes a circuit ckt
; and a list input-values of Boolean values
; which has the same length as the number of inputs of ckt
; and returns a configuration in which
; the circuit input wires have the values specified (in order)
; by the list inputs, and all other wires have the value 0.

; Examples:

; (stable? ckt-eq1 '((x 0) (y 0) (z 1) (cx 1) (cy 1) (t1 0) (t2 1))) => #t
; (stable? ckt-eq1 '((x 0) (y 0) (z 0) (cx 1) (cy 0) (t1 1) (t2 0))) => #f

; (output-values ckt-eq1 eq1-config2) => (1)
; (output-values ckt-latch latch-config2) => (1 0)

; (t-permutation? (zero-config ckt-eq2) '((x 0) (y 0) (z 0) (w 0))) => #t
; (zero-config ckt-clock) => ((z 0))

; (set-wires '((x 1) (y 0) (v 0)) '((u 0) (v 1) (x 0) (y 0) (z 1))) => ((u 0) (v 0) (x 1) (y 0) (z 1))
; (set-wires '((b 0) (a 1)) '((c 1) (a 0) (b 1) (d 0))) => ((c 1) (a 1) (b 0) (d 0))

; (t-permutation? (init-config ckt-eq1 '(1 0)) '((x 1) (y 0) (z 0) (cx 0) (cy 0) (t1 0) (t2 0))) => #t
; (init-config ckt-clock '()) => ((z 0))

;**********************************************************

	     
;**********************************************************
; ** problem 7 ** (10 points)
; Write a procedure (simulate ckt config n)
; which simulates the given circuit from the given
; configuration by repeatedly calling next-config
; until either
; the configuration reached is stable, or
; next-config has been called n times, whichever occurs first.

; Examples:

; (simulate ckt-clock '((z 0)) 5) => (((z 0)) ((z 1)) ((z 0)) ((z 1)) ((z 0)) ((z 1)))

; (t-compare-as-permutations? 
;  (simulate ckt-eq1 eq1-config1 5) 
;  '(((x 0) (y 1) (z 0) (cx 0) (cy 0) (t1 0) (t2 0)) 
;    ((x 0) (y 1) (z 0) (cx 1) (cy 0) (t1 0) (t2 0)))) => #t

; (t-compare-as-permutations? 
;  (simulate ckt-sel sel-config1 5) 
;  '(((x1 0) (x0 1) (y1 1) (y0 0) (s 1) (z1 0) (z0 0) 
;     (sc 0) (u1 0) (v1 0) (u0 0) (v0 0)) 
;    ((x1 0) (x0 1) (y1 1) (y0 0) (s 1) (z1 0) (z0 0) 
;     (sc 0) (u1 0) (v1 1) (u0 0) (v0 0)) 
;    ((x1 0) (x0 1) (y1 1) (y0 0) (s 1) (z1 1) (z0 0) 
;     (sc 0) (u1 0) (v1 1) (u0 0) (v0 0)))) => #t

; (t-compare-as-permutations? 
;  (simulate ckt-latch latch-config2 3) 
;  '(((x 0) (y 1) (q 1) (u 0)))) => #t

; (t-compare-as-permutations? 
;  (simulate ckt-eq2 (init-config ckt-eq2 '(0 1)) 5) 
;  '(((x 0) (y 1) (z 0) (w 0)) 
;    ((x 0) (y 1) (z 1) (w 1)) 
;    ((x 0) (y 1) (z 0) (w 1)))) => #t
;**********************************************************


;**********************************************************
; ** problem 8 ** (10 points)
; Write a procedure

; (final-config ckt config)

; that takes a circuit ckt and 
; a configuration config for the circuit.

; If the circuit would eventually reach a stable configuration 
; from config, then (final-config ckt config) returns the
; first stable configuration of the circuit that would be reached.

; Otherwise, (final-config ckt config) returns
; the symbol none.

; Examples:
; (final-config ckt-clock '((z 0))) => none

; (t-permutation? 
;  (final-config ckt-eq1 '((x 1) (y 1) (z 0) (cx 0) (cy 0) (t1 0) (t2 0))) 
;  '((x 1) (y 1) (z 1) (cx 0) (cy 0) (t1 1) (t2 0))) => #t

; (t-permutation? 
;  (final-config 
;   ckt-sel 
;   '((x1 0) (x0 1) (y1 1) (y0 0) (s 0) (z1 1) (z0 0) 
;     (sc 0) (u1 1) (v1 1) (u0 0) (v0 1)))
;  '((x1 0) (x0 1) (y1 1) (y0 0) (s 0) (z1 0) (z0 1) 
;    (sc 1) (u1 0) (v1 0) (u0 1) (v0 0))) => #t

; (t-permutation? (final-config ckt-latch '((x 1) (y 0) (q 0) (u 0))) 
;                 '((x 1) (y 0) (q 0) (u 1))) => #t
;**********************************************************


;**********************************************************
; ** problem 9 ** (10 points)
; Define a 4-bit ripple-carry adder circuit as described in lecture
; using the circuit representation developed above.

; Please name it: ckt-adder.

; Its inputs are x3, x2, x1, x0, y3, y2, y1, y0  (in order)
; Its outputs are z4, z3, z2, z1, z0 (in order)
; What it computes is the sum of the two 4-bit binary numbers
; represented by the x's and the y's.
; For example, if the inputs are 
; x3 = 1, x2 = 0, x1 = 0, x0 = 1    (representing 9 in binary)
; y3 = 1, y2 = 1, y1 = 0, y0 = 1    (representing 13 in binary)
; then the output should be
; z4 = 1, z3 = 0, z2 = 1, z1 = 1, z0 = 0 (representing 22 in binary)

; Examples:
; (circuit? ckt-adder) => #t
; (ckt-inputs ckt-adder) => (x3 x2 x1 x0 y3 y2 y1 y0)
; (ckt-outputs ckt-adder) => (z4 z3 z2 z1 z0)
; (output-values 
;  ckt-adder 
;  (final-config ckt-adder (init-config ckt-adder '(1 0 0 1 1 1 0 1)))) => (1 0 1 1 0)
; (output-values 
;  ckt-adder 
;  (final-config ckt-adder (init-config ckt-adder '(0 1 1 1 0 1 1 0)))) => (0 1 1 0 1)

; For the last two tests, your procedures output-values, final-config, init-config
; must be working.

; You may construct the circuit entirely by hand,
; or you may choose to write procedures to construct your circuit.
;**********************************************************


;**********************************************************
; ** problem 10 ** (10 points)
; Define a 4-bit register circuit
; using the circuit representation developed above.

; Please name it: ckt-register

; It has inputs: set, d3, d2, d1, d0  (in order)
; and outputs: q3, q2, q1, q0 (in order)

; The following procedure is useful in testing it.

(define next
  (lambda (wire-values config)
    (final-config ckt-register (set-wires wire-values config))))

; Examples:
; (circuit? ckt-register) => #t
; (ckt-inputs ckt-register) => (set d3 d2 d1 d0)
; (ckt-outputs ckt-register) => (q3 q2 q1 q0)

; (output-values 
;  ckt-register 
;  (final-config ckt-register (init-config ckt-register '(1 1 0 0 1)))) => (1 0 0 1)

; (let* ((c1 (final-config ckt-register (init-config ckt-register '(1 0 1 0 1)))) 
;        (c2 (next '((set 0)) c1)) 
;        (c3 (next '((d3 1) (d2 1) (d1 0) (d0 0)) c2)) 
;        (c4 (next '((set 1)) c3))) 
;   (map (lambda (config) (output-values ckt-register config)) 
;        (list c1 c2 c3 c4))) => ((0 1 0 1) (0 1 0 1) (0 1 0 1) (1 1 0 0))

; For the last two tests, your procedures output-values, final-config, init-config,
; set-wires must be working.

; You may construct the circuit entirely by hand,
; or you may choose to write procedures to construct your circuit.
;**********************************************************


;**************  end of hw # 5  ************************************
