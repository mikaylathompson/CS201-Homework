; ********************************************************
; CS 201a HW #1  DUE Wednesday, 9/18/2013 at 11:59 pm
; via the submit system on the Zoo

; ********************************************************
; Name: Mikayla Thompson
; Email address: mikayla.thompson@yale.edu
; ********************************************************

; This file may be loaded into Scheme.  
; Lines beginning with semicolons are Scheme comments.

; If you are asked to write a procedure, please make sure it has 
; the specified name, and the specified number and order of arguments.  
; (The names of the formal arguments may be different from those in
; the specification.)
; For each problem, the intended inputs to your procedures
; are specified (for example, "positive integers") 
; and your procedures need not do anything reasonable 
; for other possible inputs.

; You may write auxiliary procedures in addition to
; the requested one(s) -- for each of your auxiliary 
; procedures, please include a comment explaining what 
; it does.  You may also use procedures you have written 
; elsewhere in this assignment or previous assignments.

; Please use the predicate equal? to test equality
; of values that may not be numbers.  To test equality
; of numbers, you can use =.

; Reading: Through section 2.8 of the textbook.

; ********************************************************
; ** problem 0 ** (1 easy point) 
; Replace the number 0 in the definition below to indicate
; the number of hours you spent doing this assignment
; Decimal numbers (eg, 6.237) are fine.  Exclude time
; spent reading.

(define hours 1)

; ********************************************************
; ** problem 1 ** (9 points)
; Write a procedure 

; (first-digit n base)

; that takes a nonnegative integer n
; and a positive integer base (at least 2)
; and returns the value of the first (leftmost) digit
; of the number n when written in the given base.

; Note that the leftmost digit must be nonzero
; if n is not 0.  Also, for bases larger than
; 10, its "digits" may be numbers larger than 9.

; Examples
; (first-digit 0 10) => 0
; (first-digit 19 3) => 2
; (first-digit 458 10) => 4
; (first-digit 51 4) => 3
; (first-digit 179 16) => 11
; ********************************************************

(define first-digit
	(lambda (n, base)
          (if (< n base)
              n
              (first-digit (quotient n base)))))

; ********************************************************
; ** problem 2 ** (10 points)
; Write a procedure 

; (one-roman-value exp)

; that takes one Scheme value exp as input.
; If the input is one of the symbols:
; i, v, x, l, c, d, m,
; the procedure returns the value of the symbol as a Roman numeral,
; that is, 1, 5, 10, 50, 100, 500, or 1000,
; respectively.  On any other input,
; the procedure should return the symbol ?.

; Examples
; (one-roman-value 'x) => 10
; (one-roman-value 'd) => 500
; (one-roman-value 'i) => 1
; (one-roman-value '()) => ?
; (one-roman-value 'xxii) => ?
; (one-roman-value '?) => ?

; ********************************************************

(define one-roman-value
	(lambda (x)
		(cond
			((= x 'i) 1)
			((= x 'v) 5)
			((= x 'x) 10)
			((= x 'l) 50)
			((= x 'c) 100)
			((= x 'd) 500)
			((= x 'm) 1000)
			(else '?))))


; ********************************************************
; ** problem 3 ** (10 points)

; (roman-notation n)

; that takes as input a positive
; integer n between 1 and 3000 inclusive,
; and returns a list of symbols from
; i, v, x, l, c, d, m
; that is a "correct" roman numeral representation
; of the number n.

; A representation is "correct" if the symbols
; occur left-to-right in order of decreasing
; value, *except* that no symbol may occur more
; than three times in the whole list.  To avoid
; this, symbols may be placed out of order in
; the following circumstances:
; (i v) is 4, (i x) is 9
; (x l) is 40, (x c) is 90
; (c d) is 400, (c m) is 900

; Examples
; (roman-notation 2) => (i i)
; (roman-notation 14) => (x i v)
; (roman-notation 96) => (x c v i)
; (roman-notation 201) => (c c i)
; (roman-notation 1904) => (m c m i v)
; (roman-notation 3000) => (m m m)

; ********************************************************

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 4 ** (10 points)
; Write two procedures

; (biggest-unit frac)
; (greedy-units frac)

; that each take a Scheme rational number frac
; strictly between 0 and 1 as input.

; (biggest-unit frac)
; returns the largest rational number 1/n
; such that n is a positive integer and
; 1/n is less than or equal to frac.

; (greedy-units frac)
; returns a list of rational numbers of the form 1/n,
; where n is a positive integer, obtained from frac as
; follows.  Call biggest-unit on frac, and make the
; result the first element of the return list.
; Subtract this result from frac, and repeat this
; operation on the difference, making the results
; the subsequent elements of the list, until the
; result is 0.

; It may help to investigate Scheme procedures
; related to rational numbers and integers.

; Examples
; (biggest-unit 2/5) => 1/3
; (biggest-unit 3/4) => 1/2
; (biggest-unit 4/17) => 1/5
; (biggest-unit 1/101) => 1/101
; (greedy-units 1/3) => (1/3)
; (greedy-units 2/5) => (1/3 1/15)
; (greedy-units 3/4) => (1/2 1/4)
; (greedy-units 3/7) => (1/3 1/11 1/231)
; ********************************************************

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 5 ** (10 points)
; Write a procedure 

; (subsequence? lst1 lst2)

; that takes two lists lst1 and lst2
; and returns #t if lst1 can be obtained
; by dropping zero or more top-level
; elements of lst2.  The elements dropped
; may be non-contiguous.
; Otherwise, #f is returned.

; Examples
; (subsequence? '(a b) '(a b c)) => #t
; (subsequence? '(a c) '(a b c)) => #t
; (subsequence? '() '(a b c)) => #t
; (subsequence? '(a b c) '((a b) c)) => #f
; (subsequence? '(b a) '(a b c)) => #f
; (subsequence? '(x x x x) '(x x)) => #f
; (subsequence? '(x x (x) x) '(a x b x c (x) d x)) => #t
; (subsequence? '(x x (x) x) '(a x b x c x d x)) => #f
; ********************************************************
 
; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 6 ** (10 points)
; Write a procedure 

; (top-replace item1 item2 lst)

; that takes two items and a list
; and returns a list equal to lst with
; every top-level member of lst that is
; equal? to item1 replaced by item2.

; Examples:
; (top-replace 1 0 '(1 2 3)) => (0 2 3)
; (top-replace 1 'x '((2 1) (4 5) (7 6))) => ((2 1) (4 5) (7 6))
; (top-replace 'a 'x '(f a f a b e b a)) => (f x f x b e b x)
; (top-replace '(2 1) '(1 2) '((3 4) (2 1) (1 2))) => ((3 4) (1 2) (1 2))
; ********************************************************

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 7 ** (10 points)
; Write two procedures

; (all? pred? lst)
; (exists? pred? lst)

; where pred? is a predicate of one argument
; and lst is a list of values which are
; acceptable arguments for pred?  

; If pred? is #t for every (top-level) element of
; lst, then (all? pred? lst) returns #t, otherwise
; it returns #f.
; If pred? is #t for at least one (top-level) element
; of lst, then (exists? pred? lst) returns #t, otherwise
; it returns #f.

; Examples:
; (all? odd? '(3 5 7)) => #t
; (all? odd? '()) => #t
; (all? odd? '(5 7 8 9)) => #f
; (all? (lambda (x) (or (symbol? x) (number? x))) '(a 1 2 b 33)) => #t
; (exists? even? '()) => #f
; (exists? even? '(3 4 6 8)) => #t
; (exists? list? '(1 2 (3 4) 5 6)) => #t
; ********************************************************

; (Replace this comment with your procedures.)

; ********************************************************
; ** problem 8 ** (10 points)
; Write a procedure

; (set-equal? lst1 lst2)

; that takes two lists, lst1 and lst2, of Scheme values, 
; and returns #t if every top-level element of lst1 
; is equal? to a top-level element of lst2,
; AND every top-level element of lst2 is equal? to 
; a top-level element of lst2.

; Examples
; (set-equal? '(a b) '(b a)) => #t
; (set-equal? '() '()) => #t
; (set-equal? '(a a b b c c) '(c a b)) => #t
; (set-equal? '(()) '()) => #f
; (set-equal? '(() (a) (a a)) '(() (a a))) => #f
; ********************************************************

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 9 (10 points)
; For this problem and the next, a "number expression"
; is either a number or
; a non-empty list of number expressions.

; Write two procedures:

; (number-expression? exp)
; (depth exp)

; For (number-expression? exp) the argument
; exp may be an arbitrary Scheme value, and
; (number-expression? exp) returns #t if
; exp is a "number expression" and #f otherwise.

; For (depth exp)
; the argument exp is a "number expression"
; and the value returned should be the maximum
; number of pairs of parentheses enclosing any
; number in the expression.

; Examples:
; (number-expression? 17) => #t
; (number-expression? '(17 18)) => #t
; (number-expression? '()) => #f
; (number-expression? '(a 1 b 2)) => #f
; (number-expression? '(((3 1) 4) (15 6) 2)) => #t
; (depth 77) => 0
; (depth '(17 18)) => 1
; (depth '((13 14 15))) => 2
; (depth '((2 (8 16 (10)) 3) 7)) => 4
; (depth '((2) ((3)) (((4))))) => 4
; ********************************************************

; (Replace this comment with your procedures.)

; ********************************************************
; ** problem 10 (10 points)

; Write a procedure

; (piece-of? exp1 exp2)

; that takes two number expressions exp1 and exp2
; and returns #t if some (possibly empty) sequence of 
; car's and cdr's would, when applied to exp2, 
; yield a value equal? to exp1,
; otherwise, returns #f.

; Examples
; (piece-of? 17 17) => #t
; (piece-of? 17 18) => #f
; (piece-of? 17 '(17 18)) => #t
; (piece-of? '(18) '(17 18)) => #t
; (piece-of? 18 '(17 18)) => #t
; (piece-of? '(18 (19)) '(17 18 (19))) => #t
; (piece-of? 18 '((18 19) 20)) => #t
; (piece-of? '(17 18) '(17 18 (19))) => #f
; ********************************************************

; (Replace this comment with your procedure(s).)

; ********************************************************
; **** end of hw #1
