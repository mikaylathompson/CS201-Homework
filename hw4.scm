; ****************************************************************
; CS 201b HW #4  DUE 11:59 pm Wednesday, October 16, 2013
; using the submit command on the Zoo.
; ****************************************************************
; Name:
; Email address:
; ****************************************************************
; ** problem 0 ** (1 easy point)
; Please modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 2)

; ****************************************************************
; Unless the problem specifies otherwise:
; * You may solve the problem using any method 
; and any Scheme constructs, except mutators (set! and its relatives.)
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

; The topics of this assignment are
; Scheme: deep recursion on a recursively defined data structure,
; procedures with a variable number of arguments, apply, case.
; Computer Science: Boolean functions, expressions, environments,
; truth tables, satisfiability, equivalence.

; For this assignment, you'll need two new constructs in Scheme:

; (apply proc lst)   
; applies a procedure proc to the list lst of arguments
; For example, (apply + '(3 4 5)) => 12  or
; (apply append '((a b) (c d e) (f) (g h))) => (a b c d e f g h).

; and the alternate lambda syntax to allow a procedure to have
; an arbitrary number of arguments.
; For example,

;  (define count-args
;    (lambda args
;      (length args)))

; The key is that args is *not* in parentheses.  When count-args is
; called, all of its arguments are gathered into a list named args,
; which is then manipulated as usual.  Examples: (count-args 12 13) => 2,
; (count-args 'a #t 10) => 3, and (count-args) => 0.
; The variable args may be given a more informative name.
; See the textbook or the Scheme reference for more details.

; You might also find uses for this special form: case.

; ****************************************************************
; ** problem 1 ** (9 points)
; Write three procedures

; (b-not arg)
; (b-and arg1 arg2 ... argn)
; (b-or arg1 arg2 ... argn)

; (bnot arg) takes in one Boolean value (0 or 1)
; and returns the NOT of it.

; (b-and arg1 arg2 ... argn) and (b-or arg1 arg2 ... argn)
; take in any finite number of Boolean values (each 0 or 1)
; and return the AND and OR of the arguments, respectively.

; Examples:
; (b-not 0) => 1
; (b-not 1) => 0
; (b-or) => 0
; (b-or 0 0) => 0
; (b-or 0 0 1 0 1) => 1
; (b-and 1 1) => 1
; (b-and) => 1
; (b-and 1 0 1) => 0
; ****************************************************************

(define b-not
  (lambda (arg)
    (if (= arg 0)
        1
        0)))


; and-list takes a list and returns #t if all elements are 1 or #t.

; or-list takes a list and returns #t if any elements are 1 or #t

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

(define b-or
  (lambda args
    (cond
      ((null? args)
       0)
      ((list? (car args))
       (if (or-list (car args))
           1
           0))
      (else
       (if (or-list args)
           1
           0)))))
              
      
(define b-and
  (lambda args
    (cond
      ((null? args)
       1)
      ((list? (car args))
       (if (and-list (car args))
           1
           0))
      (else
       (if (and-list args)
           1
           0)))))
 
; (b-not 0); => 1
; (b-not 1); => 0
; (b-or); => 0
; (b-or 0 0); => 0
; (b-or 0 0 1 0 1); => 1
; (b-and 1 1); => 1
; (b-and); => 1
; (b-and 1 0 1); => 0


; ****************************************************************
; We recursively define a representation of Boolean Expressions:
; 1) 0 and 1 represent the constants 0 and 1
; 2) Scheme symbols represent variables
; 3) The symbols -, +, * represent Boolean operations:
; the list (- exp) represents the Boolean NOT of exp
; the list (+ exp1 .. expn) represents the Boolean OR of exp1, .., expn
; the list (* exp1 .. expn) represents the Boolean AND of exp1, .., expn
; where exp, exp1, .., expn are Boolean Expressions and n > 1.

; Some examples of Boolean expressions:

(define exp0 '(- x))
(define exp1 '(+ x y))
(define exp2 '(* x y z))
(define exp3 '(* w  1 (+ u 0)))
(define exp4 '(+ x (- x)))
(define exp5 '(* (+ x 0) (* (+ y 1) (- z))))

; Selectors for Boolean expressions.
; Use these to access parts of a Boolean expression.

(define op car)      ; the top-level operation symbol for NOT, AND, OR
(define args cdr)    ; the list of arguments for NOT, AND, OR

; (Note that although NOT always has one argument, args returns
; that argument in a list.)

; Constructors for Boolean Expressions.
; Given Boolean expression(s) as arguments, make the NOT, AND, or OR of them.
; Use these to construct Boolean Expressions.

(define make-not (lambda (exp) (list '- exp)))
(define make-and (lambda exps (cons '* exps)))
(define make-or (lambda exps (cons '+ exps)))

; Examples:
; (op '(* x y)) => *
; (op '(- (* x z))) => -
; (op '(+ x 0)) => +
; (args '(- (* x y))) => ((* x y))
; (args '(+ (- x) x)) => ((- x) x)
; (args '(+ (* x y) (- z))) => ((* x y) (- z))
; (make-not 'x) => (- x)
; (make-and 1 '(+ y z)) => (* 1 (+ y z))
; (make-or 0 (make-and x 1 z)) => (+ 0 (* x 1 z))

; ****************************************************************
; ** problem 2 ** (10 points)
; Write two procedures

; (boolean-exp? exp)
; (type-of exp)

; (boolean-exp? exp) takes an arbitrary Scheme value exp
; and tests to see whether it is a Boolean Expression according
; to the definition above, returning #t if so and #f if not.

; (type-of exp)
; that takes a Boolean Expression as defined above
; and returns its type as one of the symbols:
;   constant, variable, not, or, and
; Note that the type is determined by the top-level
; operation in case the expression is not a constant or variable.

; Recall that (number? exp) tests whether exp is a number,
; and (symbol? exp) tests whether exp is a symbol.

; Examples
; (boolean-exp? 0) => #t
; (boolean-exp? 2) => #f
; (boolean-exp? '(* x)) => #f
; (boolean-exp? '(- 0)) => #t
; (boolean-exp? '(* x (+ 0 (- 1)))) => #t
; (boolean-exp? '(+ x y z)) => #t
; (boolean-exp? '(-> x y)) => #f
; (type-of 0) => constant
; (type-of 'hi) => variable
; (type-of '(- 0)) => not
; (type-of '(+ (* x 0) (* x 1))) => or
; (type-of '(* (- 0) (- 1))) => and
; ****************************************************************



(define boolean-exp?
  (lambda (exp)
    (if (not (list? exp))
        (if (or (equal? exp 0)
                (equal? exp 1)
                (symbol? exp))
            #t
            #f)
        (cond
          ((or (equal? '* (car exp)) (equal? '+ (car exp)))
           (if (<= 2 (length (cdr exp)))
               (and-list (map boolean-exp? (cdr exp)))
               #f))
          ((equal? '- (car exp))
           (if (= 1 (length (cdr exp)))
               (boolean-exp? (cadr exp))
               #f))
          (else #f)))))

; (boolean-exp? 0); => #t
; (boolean-exp? 2) ;=> #f
; (boolean-exp? '(* x)); => #f
; (boolean-exp? '(- 0)); => #t
; (boolean-exp? '(* x (+ 0 (- 1)))); => #t
; (boolean-exp? '(+ x y z)); => #t
; (boolean-exp? '(-> x y)); => #f


(define type-of
  (lambda (exp)
    (if (list? exp)
        (cond
          ((equal? (car exp) '-) 'not)
          ((equal? (car exp) '+) 'or)
          ((equal? (car exp) '*) 'and)
          (else 'error))
        (cond
          ((equal? exp 0) 'constant)
          ((equal? exp 1) 'constant)
          ((symbol? exp) 'variable)
          (else 'error)))))
    
    
; (type-of 0) ;=> constant
; (type-of 'hi) ;=> variable
; (type-of '(- 0)) ;=> not
; (type-of '(+ (* x 0) (* x 1))) ;=> or
; (type-of '(* (- 0) (- 1))) ;=> and

; ****************************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (all-vars exp)

; that takes a Boolean Expression exp 
; and makes a list containing all the variables
; that occur in exp.  The list should not contain duplicates,
; and should have the variables in the order of their
; first appearance in exp (scanning left to right.)

; Hint: args, type-of and deep recursion on the structure of Boolean Expressions.
; (remove-duplicates, anyone?)

; Examples:
; (all-vars 0) => ()
; (all-vars '(- (* x y (+ x z)))) => (x y z)
; (all-vars '(* 1 (+ 0 1) (- u))) => (u)
; (all-vars '(* x y x y x)) => (x y)
; (all-vars '(* c (* b (* c a) b) c)) => (c b a)
; ****************************************************************

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
; flatten turns a nested list into a flat one
(define flatten
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((list? (car lst))
       (append (flatten (car lst)) (flatten (cdr lst))))
      (else 
       (cons (car lst) (flatten (cdr lst)))))))

;(flatten '(3 (4 5) 6))

(define all-vars
  (lambda (exp)
    (cond
      ((null? exp) '())
      ((list? exp)
       (if (or (equal? (type-of exp) 'and)
               (equal? (type-of exp) 'or)
               (equal? (type-of exp) 'not))
           (remove-duplicates (flatten (map all-vars (cdr exp))))
           (remove-duplicates (flatten (cons (all-vars (car exp)) (all-vars (cdr exp)))))))
      ((equal? (type-of exp) 'constant) '())
      ((equal? (type-of exp) 'variable) exp)
      (else '()))))
    

; (all-vars 0); => ()
; (all-vars '(- (* x y (+ x z)))); => (x y z)
; (all-vars '(* 1 (+ 0 1) (- u))); => (u)
; (all-vars '(* x y x y x)); => (x y)
; (all-vars '(* c (* b (* c a) b) c)); => (c b a)
; ****************************************************************
; ** problem 4 ** (10 points)
; Write a procedure

; (substitute-in exp var value)

; that takes a Boolean Expression exp,
; a variable var and a Boolean value (0 or 1) 
; and returns a Boolean Expression with 
; the given value substituted for *every* occurrence
; of the variable var in exp.

; Hint: type-of and deep recursion on the structure of Boolean Expressions.
; Don't forget the Boolean Expression selectors and constructors.

; Examples:
; (substitute-in 0 'x 1) => 0
; (substitute-in 'x 'x 1) => 1
; (substitute-in 'x 'y 0) => x
; (substitute-in '(* x y) 'y 0) => (* x 0)
; (substitute-in '(+ (* (- x) y) (* x (- y))) 'x 0) => (+ (* (- 0) y) (* 0 (- y)))
; (substitute-in '(- (+ x y x)) 'x 1) => (- (+ 1 y 1))
; ****************************************************************

; includes? takes a list and a target.  It returns true
; if target is a top level element of list. (from hw1)
(define includes?
  (lambda (list target)
    (if (equal? '() list)
        #f
        (or (equal? (car list) 
                    target)
            (includes? (cdr list) target)))))

(define substitute-in
  (lambda (exp var value)
    (if (and (list? exp) (not (includes? (all-vars exp) var)))
        exp
        (cond
          ((not (list? exp))
           (if (equal? exp var)
               value
               exp))
          ((null? exp)
           '())
          (else
           (cons (substitute-in (car exp) var value)
                 (substitute-in (cdr exp) var value)))))))
        
 
 (substitute-in 0 'x 1) ;=> 0
 (substitute-in 'x 'x 1) ;=> 1
 (substitute-in 'x 'y 0) ;=> x
 (substitute-in '(* x y) 'y 0) ;=> (* x 0)
 (substitute-in '(+ (* (- x) y) (* x (- y))) 'x 0) ;=> (+ (* (- 0) y) (* 0 (- y)))
 (substitute-in '(- (+ x y x)) 'x 1) ;=> (- (+ 1 y 1))
; ****************************************************************
; We represent an environment as table  consisting of
; a list of two-element lists, each containing a variable 
; and either 1 or 0.
; An environment specifies a truth value (0 or 1)
; for each variable in the table.

; For example:

(define environ1 '((x 0) (y 1) (z 0)))
(define environ2 '((u 0) (x 1) (w 1) (y 0) (z 1)))

; ****************************************************************
; ** problem 5 ** (10 points)
; Write a procedure 

; (eval-in-env exp env)

; that takes a Boolean Expression exp
; and an environment env
; represented as described above
; and returns 0 or 1 giving 
; the value of the expression in the environment.

; If the Boolean Expression contains variables that do not
; occur in the environment, (eval-in-env exp env) should
; return the symbol: unspecified-variable.
; (You may want to check for this condition first.)

; Hint: deep recursion on the structure of Boolean expressions
; along with type-of, args, b-not, b-or, b-and.
; You may want to write a separate procedure to look up the value
; of a symbol in an environment.

; Examples:
; (eval-in-env 1 environ1) => 1
; (eval-in-env '(+ 0 0) '()) => 0
; (eval-in-env 'x environ1) => 0
; (eval-in-env 'x environ2) => 1
; (eval-in-env '(- z) environ1) => 1
; (eval-in-env '(- z) environ2) => 0
; (eval-in-env '(+ y (- x)) environ2) => 0
; (eval-in-env '(* (* (+ u x) (+ w 0)) (- (* y z))) environ2) => 1
; (eval-in-env exp5 environ1) => 0
; (eval-in-env '(* y (+ x u)) '((x 0) (y 1))) => unspecified-variable
; ****************************************************************


; ****************************************************************
; We define a Truth Table as represented by a list containing
; (1) a (possibly empty) list of n distinct variables, and
; (2) a list of 2^n rows, each of which is a list containing
;     (a) a list of n 0's and 1's, and
;     (b) a single 0 or 1, giving the value of the function
;         in the environment corresponding to the row.

; Note that the Truth Table rows should be in increasing order, considered
; as binary numbers.

; Selectors for Truth Tables (please use them to access Truth Table parts);

(define tt-vars car)
(define tt-rows cadr)

; Examples of truth tables for (- x), (* x y), (+ (- a) b), and (XOR u v).

(define tt-not '((x)
		 (((0) 1) 
		  ((1) 0))))

(define tt-and '((x y)
		 (((0 0) 0) 
		  ((0 1) 0) 
		  ((1 0) 0) 
		  ((1 1) 1))))

(define tt-imp  '((a b)
		  (((0 0) 1) 
		   ((0 1) 1) 
		   ((1 0) 0) 
		   ((1 1) 1))))

(define tt-xor '((u v)
		 (((0 0) 0)
		  ((0 1) 1)
		  ((1 0) 1)
		  ((1 1) 0))))

; Here is a truth table for a function of three arguments a, b, c.

(define tt-f1 '((a b c)
		(((0 0 0) 0)
		 ((0 0 1) 0)
		 ((0 1 0) 1)
		 ((0 1 1) 1)
		 ((1 0 0) 0)
		 ((1 0 1) 1)
		 ((1 1 0) 0)
		 ((1 1 1) 1))))

; ****************************************************************
; ** problem 6 ** (10 points)
; Write a procedure 

; (all-combs n)

; that takes a non-negative integer n
; and creates the list of all 
; lists of n 0's or 1's in the
; *specific order* required for
; a truth table.

; In other words, the lists, interpreted
; as binary numbers, should be in increasing
; order.

; Hint: if a recursive call gives the correct answer
; for (all-combs 2), what needs to happen to it
; to give the correct answer for (all-combs 3)?
; (This may remind you of all-moves from a previous assignment.)

; Use let or let* to avoid recomputing the recursive call!

; Examples:
; (all-combs 0) => (())
; (all-combs 1) => ((0) (1))
; (all-combs 2) => ((0 0) (0 1) (1 0) (1 1))
; (all-combs 3) => ((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))
; ****************************************************************


; ****************************************************************
; ** problem 7 ** (10 points)
; Write a procedure

; (truth-table exp)

; that takes a Boolean Expression exp
; and returns the Truth Table for exp
; where the variables for the table are
; extracted from exp using all-vars, and
; the function value for a row is obtained
; by evaluating exp in the corresponding
; environment. 

; Examples:
; (truth-table  0) => (() ((() 0)))
; (truth-table 'x) => ((x) (((0) 0) ((1) 1)))
; (truth-table '(- z)) => ((z) (((0) 1) ((1) 0)))
; (truth-table '(* x y)) => ((x y) (((0 0) 0) ((0 1) 0) ((1 0) 0) ((1 1) 1)))
; (truth-table '(+ (* z (- y)) (* y (- x)))) => 
; ((z y x)
;  (((0 0 0) 0)
;   ((0 0 1) 0)
;   ((0 1 0) 1)
;   ((0 1 1) 0)
;   ((1 0 0) 1)
;   ((1 0 1) 1)
;   ((1 1 0) 1)
;   ((1 1 1) 0)))
; ****************************************************************


; ****************************************************************
; ** problem 8 ** (10 points)
; Write two procedures

; (satisfiable? exp)
; (equivalent? exp1 exp2)

; (satisfiable? exp)
; takes one Boolean Expression exp and
; returns #t if exp is satisfiable and #f otherwise.

; (equivalent exp1 exp2)
; takes two Boolean Expressions exp1 and exp2
; and returns #t if they are equivalent and #f if they
; are not equivalent.

; A Boolean expression is satisfiable if there exists an environment
; in which its value is 1.  Two Boolean expressions are equivalent
; if for every environment they have the same value in the environment.
; These procedures will be tested on expressions with few enough
; variables that generating truth tables will be a feasible approach.

; Examples:
; (satisfiable? 0) => #f
; (satisfiable? 1) => #t
; (satisfiable? '(* x (* y z))) => #t
; (satisfiable? '(* x (* (- y) y))) => #f
; (satisfiable? '(* (+ v (- v)) 0)) => #f
; (equivalent? 0 '(* a (- a))) => #t
; (equivalent? 0 'a) => #f
; (equivalent? 'a 'b) => #f
; (equivalent? '(+ x (+ y z)) '(+ (+ y x) (+ z 0))) => #t
; (equivalent? '(+ x (* y z)) '(* (+ x y) (+ x z))) => #t
; ****************************************************************


; ****************************************************************
; ** problem 9 ** (10 points)
; Write a procedure

; (find-exp tt)

; This procedure takes a Truth Table
; and returns a Boolean Expression 
; for the given truth table.

; You may choose to use the sum-of-products algorithm
; from lecture, or some other method.
; Please include comments explaining your method
; in either case.

; Examples:
; (boolean-exp? (find-exp tt-and)) => #t
; (equal? tt-and (truth-table (find-exp tt-and))) => #t
; (equal? tt-imp (truth-table (find-exp tt-imp))) => #t
; (equal? tt-xor (truth-table (find-exp tt-xor))) => #t
; (equal? tt-f1 (truth-table (find-exp tt-f1))) => #t
; ****************************************************************


; ****************************************************************
; ** problem 10 ** (10 points)
; Write a procedure

; (simplify exp)

; that takes a Boolean Expression exp and attempts
; to "simplify" it as much as possible using the following rules:

; 0 + a = a + 0 = a
; 1 + a = a + 1 = 1
; 0 * a = a * 0 = 0
; 1 * a = a * 1 = a
; 0' = 1
; 1' = 0

; The return value should be a Boolean expression that is
; equivalent to the original expression and does not contain
; 0 or 1, unless it is equal to 0 or 1.

; Hint: imagine (recursively) that the arguments to an NOT, AND or
; OR expression are already simplified -- what must you do?

; Examples:
; (simplify 0) => 0
; (simplify '(- 0)) => 1
; (simplify '(* 1 0 1)) => 0
; (simplify '(+ x 0 y 0)) => (+ x y)
; (simplify '(+ 1 z)) => 1
; (simplify '(- (* x (- 0)))) => (- x)
; (simplify '(- (+ (- x) x))) => (- (+ (- x) x))
; (simplify '(+ (* x 0) (* y (- 1)))) => 0
; ****************************************************************


; ****************************************************************
; See if you can figure out what the following procedure is doing.
; Think about how to improve it.
; (No points, just bragging rights.)

(define what?
  (lambda (exp)
    (let* ((s-exp (simplify exp))
	   (vars (all-vars s-exp)))
      (cond
       ((null? vars) (if (equal? exp 1) #t #f))
       (else
	(let ((var (car vars)))
	  (if (what? (simplify (substitute-in exp var 0)))
	      #t
	      (what? (simplify (substitute-in exp var 1))))))))))

; **************** end of hw # 4 *********************************
