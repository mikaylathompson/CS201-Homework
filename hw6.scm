;************************************************************
; CS 201a HW #6  DUE Wednesday, November 6, 2012 at 11:59 pm, 
; via submit.
;************************************************************
; Name:  Mikayla Thompson
; Email address:  mikayla.thompson@yale.edu
;************************************************************

; Computer science topics: 
; translating and simulating TC-201 instructions,
; computer representation of integers.

; Unless the problem specifies otherwise:
; * You may solve the problem using any method 
; and any Scheme constructs, except mutators (set! and its relatives.)
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment (including reading):

(define hours 3)

;************************************************************
; Random access memory (RAM)

; We represent the contents of the RAM of the TC-201
; as a table.  Each entry of the table consists of

;  (memory-address bit-pattern)

; where memory-address is an integer in the range 0 through 4095,
; and bit-pattern is a list of 16 bits, each either 0 or 1,
; representing the contents of the memory register with
; that address.

; For those addresses not listed in the table, the
; contents are assumed to be a list of 16 zeroes.

; Examples:

(define ram-ex1
  '((0 (0 0 0 1  0 0 0 0  0 0 0 0  0 1 0 0))
    (1 (0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 1))
    (2 (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
    (3 (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
    (4 (0 0 0 0  0 0 0 0  0 0 1 0  1 0 1 0))
    (5 (1 0 0 0  0 0 0 0  0 0 0 1  1 1 1 1))))

(define ram-ex2
  '((5 (1 0 0 0  0 0 0 0  0 0 0 1  1 1 1 1))
    (0 (0 0 0 1  0 0 0 0  0 0 0 0  0 1 0 0))
    (4 (0 0 0 0  0 0 0 0  0 0 1 0  1 0 1 0))
    (1 (0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 1))
    (6 (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))))

; Note that the addresses need not be in increasing order
; and there may be gaps in the addresses.  Addresses should
; not be duplicated.  The two representations above represent 
; the same RAM contents.

;************************************************************
; ** problem 1 ** (9 points)
; Write three procedures

; (ram-read address ram)
; (normalize ram)
; (ram-write address contents ram)

; (ram-read address ram)
; takes a memory address and a ram
; and returns the list of 16 bits giving the contents
; of the memory register in ram with the given address.

; (normalize ram)
; takes a ram and returns a normalized ram
; representing the same memory contents.
; A normalized ram has consecutive memory
; addresses in increasing order starting with
; 0 and ending with the last address whose contents
; is not equal to all zeroes.

; (ram-write address contents ram)
; takes a memory address (address),
; a list of 16 bits (contents) and a ram,
; and returns a normalized ram representing the result
; of copying the contents into the memory register
; of ram specified by the memory address.

; Examples:
; (ram-read 0 ram-ex1) => (0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0)
; (ram-read 5 ram-ex1) => (1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1)
; (ram-read 6 ram-ex1) => (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
; (equal? (normalize ram-ex1) ram-ex1) => #t
; (equal? (normalize ram-ex2) ram-ex1) => #t
; (normalize '((0 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))) => ()
; (ram-write 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0) ram-ex1) =>
;   ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;    (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;    (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (3 (1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;    (4 (0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0))
;    (5 (1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1)))
; (ram-write 1 '(0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1) '()) =>
;   ((0 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (1 (0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1)))
;************************************************************

(define ram-read
  (lambda (address ram)
    (cond
      ((null? ram) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
      ((equal? address (caar ram)) (cadar ram))
      (else (ram-read address (cdr ram))))))

 ;(ram-read 0 ram-ex1) ;=> (0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0)
 ;(ram-read 5 ram-ex1) ;=> (1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1)
 ;(ram-read 6 ram-ex1) ;=> (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
 
 ; remove-entry removes the specified address from ram.
 (define remove-entry
   (lambda (addr ram)
     (cond
       ((null? ram) 
        ram)
       ((equal? (caar ram) addr) 
        (cdr ram))
       (else (cons (car ram)
                   (remove-entry addr (cdr ram)))))))

; sort-ram is going to search for the smallest adress, remove
; it from the list, and then cons it to the sorted remainder
; of the list.
 (define sort-ram
   (lambda (ram)
      (if (null? ram)
         '()
         (let ((smallest (apply min (map car ram))))
           (cons (list smallest (ram-read smallest ram))
                 (sort-ram (remove-entry smallest ram)))))))
 
 ; adds entries of zero to registers that are missing in
 ; a sorted ram.
 (define fill-ram
   (lambda (ram num)
     (if (null? ram)
         '()
         (let ((largest (apply max (map car ram))))
           (cond
             ((< num largest)
              (cons (list num (ram-read num ram))
                    (if (= num (caar ram))
                        (fill-ram (cdr ram) (+ 1 num))
                        (fill-ram ram (+ 1 num)))))
             (else
               ram))))))
 
 ; given a ram in reverse sorted order, trim-zeros removes
 ; any leading entries of zero, and returns the register,
 ; still reversed.
 (define trim-zeros
   (lambda (rev-ram)
     (cond
       ((null? rev-ram)
         '())
       ((equal? '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (cadar rev-ram))
        (trim-zeros (cdr rev-ram)))
       (else
        rev-ram))))
   
; normalize is going to call sort ram, and then relabel the addresses
; in consecutive order.
 (define normalize
   (lambda (ram)
     (reverse 
      (trim-zeros 
       (reverse 
        (fill-ram 
         (sort-ram ram) 
         0))))))

; (equal? (normalize ram-ex1) ram-ex1) ;=> #t
; (equal? (normalize ram-ex2) ram-ex1) ;=> #t
 
; (ram-write address contents ram)
(define ram-write
  (lambda (address contents ram)
    (normalize
     (cons (list address contents)
           (remove-entry address
                         ram)))))


; (equal? (ram-write 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0) ram-ex1); =>
;   '((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;    (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;    (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (3 (1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;    (4 (0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0))
;    (5 (1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1))))
; (equal? (ram-write 1 '(0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1) '()) ;=>
;   '((0 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (1 (0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1))))

;************************************************************
; ** problem 2 ** (10 points)
; Write four procedures:

; (extract i j lst)
; (exactly n lst)
; (bits->int lst) 
; (int->bits n)

; (extract i j lst) 
; takes nonnegative integers i and j and a list lst
; and returns the list of elements indexed i through j.
; You may assume i and j are at least 0 and less than the
; length of the list, and i is less than or equal to j.
; As in list-ref, list elements are indexed from 0.

; (exactly n lst)
; takes a nonnegative integer n and a list lst
; If n is less than or equal to the length of lst,
; then the result is a list with the last n elements
; of lst.  If n is greater than the length of lst,
; then the result is a list equal to lst with enough
; 0's as elements at the beginning to make the length
; of the result exactly n.

; (bits->int lst) takes a list of bits lst
; and returns the value of the nonnegative number 
; represented in binary by those digits.

; (int->bits n) takes a nonnegative integer n
; and returns the list of bits representing n in binary.
; Note that for 0 the answer is (0) but for
; all other numbers the answer starts with 1.

; Examples:
; (extract 1 3 '(a b c d e)) => (b c d)
; (extract 4 4 '(a b c d e)) => (e)
; (exactly 3 '(a b c d e)) => (c d e)
; (exactly 8 '(a b c d e)) => (0 0 0 a b c d e)
; (bits->int '(0)) => 0
; (bits->int '(1 0)) => 2
; (bits->int '(0 0 0 1 1 0)) => 6
; (int->bits 0) => (0)
; (int->bits 6) => (1 1 0)
; (int->bits 14) => (1 1 1 0)
;************************************************************

(define powers-of-two
  '(32768 16384 8192 4096 2048 1024 512 256 128 64 32 16 8 4 2 1))

(define extract
  (lambda (i j lst)
    (cond
      ((> i 0) (extract (- i 1) (- j 1) (cdr lst)))
      ((= j 0) (list (car lst)))
      ((= i 0) (cons (car lst)
                     (extract 0 (- j 1) (cdr lst))))
      (else 'error))))

; (extract 1 3 '(a b c d e))
; (extract 4 4 '(a b c d e))

(define exactly
  (lambda (i lst)
    (cond
      ((= i (length lst)) lst)
      ((< i (length lst)) (exactly i (cdr lst)))
      (else (exactly i (cons 0 lst))))))

;(exactly 3 '(a b c d e))
;(exactly 8 '(a b c d e))

(define bits->int
  (lambda (bits)
    (if (and (= 16 (length bits))
             (= 1 (car bits)))
        (* -1 (apply + (map * (cdr bits) (exactly (length (cdr bits) )
                                  powers-of-two))))
        (apply + (map * bits (exactly (length bits) 
                                  powers-of-two))))))
;(bits->int '(1 0))
;(bits->int '(0 0 0 1 1 0))
;(bits->int '(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0)) ;=> -14

(define int->bits
  (lambda (int)
    (let ((new-int (if (<= 0 int)
                       int
                       (+ (- 0 int) 32768))))
      (cond
        ((= 0 new-int) '(0))
        ((= 1 new-int) '(1))
        ((even? new-int) (append (int->bits (/ new-int 2)) '(0)))
        (else (append (int->bits (/ (- new-int 1) 2)) '(1)))))))
    
;(int->bits 0) ;=> (0)
;(int->bits 6) ;=> (1 1 0)
;(int->bits 14) ;=> (1 1 1 0)
;(int->bits -1) ;=> (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)

;************************************************************
; Next we develop a simulator for the TC-201

; For the TC-201 Central Processing Unit (CPU),
; the contents of the registers are represented
; by a table with entries giving the contents of
; the CPU registers ** in this order **:

; the accumulator (acc)
; the program counter (pc)
; the run flag (run-flag)
; the arithmetic error bit (aeb)

; Each entry is a list containing 
; a symbol (one of acc, pc, run-flag, aeb)
; a list of bits of the correct length,
; namely, 16 bits for the acc, 12 bit for
; the pc, and 1 bit each for the run-flag
; and aeb.

; For example:

(define cpu-ex1 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
		  (pc (0 0 0 0 0 0 0 0 0 1 1 1))
		  (run-flag (1))
		  (aeb (0))))

(define cpu-ex2 '((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
		  (pc (0 0 0 0 0 0 0 0 1 0 0 0))
		  (run-flag (0))
		  (aeb (1))))

; A *configuration* of the TC-201 is a list containing
; two entries:
; (1) the contents of the CPU registers, in the above format,
; (2) the normalized contents of the RAM, in the format of problem 1.

; For example:

(define config1 (list cpu-ex1 ram-ex1))
(define config2 (list cpu-ex2 ram-ex1))

; Here are selectors for the CPU and RAM portions of a configuration.
; Please do not redefine these procedure names -- it may cause
; errors in grading your procedures.

(define config-cpu car)
(define config-ram cadr)

;************************************************************
; ** problem 3 ** (10 points)
; Write three procedures

; (incr-pc n config)
; (load address config)
; (store address config)

; (incr-pc n config)
; takes a nonnegative integer n
; and a TC-201 configuration config
; and returns the TC-201 configuration
; that is obtained by adding n to the value
; of pc.  Note that the sum should be taken
; modulo 4096.  (Scheme has a modulo procedure.
      
; (xload address config)
; takes a memory address and a TC-201 configuration
; and returns the TC-201 configuration
; that is obtained by loading the contents
; of the given memory address into the accumulator.
; The values of all other registers (including the pc)
; are unchanged.

; (store address config)
; takes a memory address and a TC-201 configuration
; and returns the TC-201 configuration 
; that is obtained by copying the contents of the accumulator 
; into the given memory address.
; The values of other all registers (including the pc)
; are unchanged.

; Examples:
; (config-cpu (incr-pc 1 config1)) =>
;   ((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;    (pc (0 0 0 0 0 0 0 0 1 0 0 0))
;    (run-flag (1))
;    (aeb (0)))

; (equal? (config-ram (incr-pc 1 config1)) (config-ram config1)) => #t

; (config-cpu (incr-pc 4090 config2)) =>
;   ((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
;    (pc (0 0 0 0 0 0 0 0 0 0 1 0))
;    (run-flag (0))
;    (aeb (1)))

; (config-cpu (xload 4 config1)) =>
;   ((acc (0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0))
;    (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;    (run-flag (1))
;    (aeb (0)))

; (config-cpu (xload 12 config1)) =>
;   ((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;    (run-flag (1))
;    (aeb (0)))

; (config-ram (store 5 config1)) =>
;   ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;    (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;    (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (4 (0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0))
;    (5 (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)))

; (config-ram (store 8 config2)) =>
;   ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;    (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;    (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (4 (0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0))
;    (5 (1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1))
;    (6 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (7 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (8 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1)))
;************************************************************

(define incr-pc
  (lambda (n config)
    (let* ((cpu (config-cpu config))
           (old-count (bits->int (cadadr cpu)))
           (new-count (modulo (+ old-count n) 4096))
           (counter (exactly 12 (int->bits new-count))))
      (list (list (car cpu)
                  (list 'pc counter)
                  (caddr cpu)
                  (cadddr cpu))
            (config-ram config)))))


(define xload
  (lambda (address config)
    (let* ((new-acc (ram-read address (config-ram config))))
      (list (cons (list 'acc new-acc)
                  (cdr (config-cpu config)))
            (config-ram config)))))


(define store
  (lambda (address config)
    (list (config-cpu config)
          (ram-write address 
                     (cadar (config-cpu config))
                     (config-ram config)))))


;************************************************************
; ** problem 4 ** (10 points)
; Write two procedures

; (add address config)
; (sub address config)

; (add address config)
; takes a memory address and a TC-201 configuration
; and returns a TC-201 configuration in which
; the contents of the memory register addressed has
; been added to the contents of the accumulator.

; add-bits takes two binary numbers, and returns their sum
; in binary.
(define add-bits
  (lambda (m n)
    (int->bits (+ (bits->int m)
                  (bits->int n)))))

(define pos-bits->int
  (lambda (bits)
    (apply + (map * bits (exactly (length bits) 
                                  powers-of-two)))))

(define aeb-add-bits
  (lambda (m n)
    (let* ((m-int (bits->int m))
           (n-int (bits->int n))
           (temp-sum (+ m-int n-int))
           (aeb (if (< (expt 2 15) (abs temp-sum))
                    '(1)
                    '(0)))
           (bit-sum (if (= 1 (car aeb))
                        (int->bits (modulo temp-sum (expt 2 15)))
                        (int->bits temp-sum))))
      (list aeb bit-sum))))

(define add
  (lambda (address config)
    (let* ((cpu (config-cpu config))
           (ram (config-ram config))
           (old-acc (cadar cpu))
           (adr (ram-read address ram))
           (aeb-sum (aeb-add-bits old-acc adr))
           (aeb (car aeb-sum))
           (new-acc (cadr aeb-sum)))
      ;(display (list "old-acc " old-acc (bits->int old-acc)
      ;               "\nadr " adr (bits->int adr)
      ;               "\naeb " aeb
      ;               "\nnew-acc " new-acc (bits->int new-acc)))
      (list (list 
             (list 'acc (exactly 16 new-acc))
             (cadr cpu)
             (caddr cpu)
             (list 'aeb aeb))
            ram))))
                
(define sub
  (lambda (address config)
    (let* ((cpu (config-cpu config))
           (ram (config-ram config))
           (old-acc (cadar cpu))
           (adr (ram-read address ram))
           
           (aeb-sum (aeb-add-bits old-acc (int->bits (* -1 (bits->int adr)))))
           (aeb (car aeb-sum))
           (new-acc (cadr aeb-sum)))
      (list (list 
             (list 'acc (exactly 16 new-acc))
             (cadr cpu)
             (caddr cpu)
             (list 'aeb aeb))
            ram))))


; (sub address config) is similar, except that the
; contents of the memory register addressed has
; been subtracted from the contents of the accumulator.

; Note that if the result is zero, the answer should
; be +0, not -0.

; If the result can be correctly represented in 16-bit sign/magnitude
; then the arithmetic error bit (aeb) should also be set to 0.

; If the result cannot be correctly represented in 16-bit sign/magnitude
; then the arithmetic error bit (aeb) should also be set to 1.
; In this case, the result in the accumulator
; should be the remainder on dividing the correct result by 2^(15).  
; (Recall that Scheme has a remainder procedure.)

; The contents of registers other than the accumulator and the
; arithmetic error bit should be unchanged.

; Examples:
;(equal? (config-cpu (add 4 config1)) ;=>
;   '((acc (0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 1))
;    (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;    (run-flag (1))
;    (aeb (0))))

; (equal? (config-ram (add 4 config1)) (config-ram config1)); => #t

;(equal? (config-cpu (add 5 config1)) ;=>
;   '((acc (1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0))
;    (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;    (run-flag (1))
;    (aeb (0))))

;(equal? (config-cpu (sub 4 config1)) ;=>
;   '((acc (1 0 0 0 0 0 0 0 0 0 0 1 1 0 1 1))
;    (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;    (run-flag (1))
;    (aeb (0))))

;(equal? (config-cpu (sub 5 config1)) ;=>
;   '((acc (0 0 0 0 0 0 0 0 0 0 1 0 1 1 1 0))
;    (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;    (run-flag (1))
;    (aeb (0))))

;(equal? (config-cpu (add 0 (list cpu-ex1 '((0 (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))))) ;=>
;   '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0))
;    (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;    (run-flag (1))
;    (aeb (1))))
;
;(equal? (config-cpu (sub 0 (list cpu-ex1 '((0 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0)))))) ;=>
;   '((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 1))
;    (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;    (run-flag (1))
;    (aeb (1))))
;************************************************************

;************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (input config)
; (output config)

; Each takes a TC-201 configuration
; and performs the appropriate action 
; (reading a number from the user or writing
; a number out to the user)
; and *returns* the resulting TC-201
; configuration.

; For input, the new configuration has
; the value read in the accumulator, and
; all other registers unchanged.
; To read in a value, you may use the following
; let construct:
; (let ((value (begin (display "input = ") (read)))) ...)

(define input
  (lambda (config)
    (let* ((value (begin (display "input = ") (read)))
          (new-acc (aeb-add-bits '(0) (int->bits value)))
          (cpu (config-cpu config)))
      (display (list
                "value" value
                "\naeb" (car new-acc)
                "\nacc" (cadr new-acc) (bits->int (cadr new-acc))))
      (list (list (list 'acc 
                        (exactly 16 (cadr new-acc)))
                  ;(if (= 0 (caar new-acc))
                      ;(cdr cpu)
                      (list (cadr cpu)
                            (caddr cpu)
                            (list 'aeb (car new-acc))));)
            (config-ram config)))))
       

; To ensure the number typed by the user is
; in the correct range, you may take its
; remainder on division by 2^(15).

; For output, the new configuration is
; returned *unchanged*.  
; If the integer value from the accumulator
; is in value-from-accumulator, then the
; output to the user can be produced by:

; (display "output = ")
; (display value-from-accumulator)
; (newline)

(define output
  (lambda (config)
    (display "output = ")
    (display (bits->int (cadar (config-cpu config1))))
    (newline)
    config))

; Examples (these show how the interaction looks)
; The lines input = .. and output = .. show
; the interaction between TC-201 and user.
; The TC-201 configuration shows the value
; returned.

; > (input config1)
; input = 13
; (((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1))
;   (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;   (run-flag (1))
;   (aeb (0)))
;  ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;   (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (4 (0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0))
;   (5 (1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1))))

; > (output config1)
; output = 15
; (((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;   (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;   (run-flag (1))
;   (aeb (0)))
;  ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;   (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (4 (0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0))
;   (5 (1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1))))

;************************************************************

;************************************************************
; ** problem 6 ** (10 points)
; Write four procedures

; (jump address config)
; (skipzero config)
; (skippos config)
; (skiperr config)

; (jump address config)
; takes a memory address and a TC-201 configuration
; and returns a TC-201 configuration in which
; the program counter contains the given address.
; All other registers are unaffected.

(define jump
  (lambda (address config)
    (list (list (car (config-cpu config))
                (list 'pc (exactly 12 (int->bits address)))
                (caddr (config-cpu config))
                (cadddr (config-cpu config)))
          (config-ram config))))

; (skipzero config)
; takes a TC-201 configuration
; and returns a TC-201 configuration in which
; the program counter is increased by 2
; if the accumlator contains 0 (NB: either +0 or -0)
; and is increased by 1 otherwise.
; All other registers are unaffected.

(define skipzero
  (lambda (config)
    (incr-pc (if (= 0 (bits->int (cadar (config-cpu config))))
                    2
                    1)
             config)))

; (skippos config)
; takes a TC-201 configuration
; and returns a TC-201 configuration in which
; the program counter is increased by 2
; if the accumulator contains a strictly positive
; number (not zero), and is increased by 1 otherwise.
; All other registers are unaffected.

(define skippos
  (lambda (config)
    (incr-pc (if (< 0 (bits->int (cadar (config-cpu config))))
                    2
                    1)
             config)))

; (skiperr config)
; takes a TC-201 configuration
; and returns a TC-201 configuration in which
; the program counter is increased by 2 if
; the arithmetic error bit (aeb) is 1, and
; is increased by 1 otherwise.  In the
; returned configuration, the aeb is set to 0.
; All other registers are unaffected.

(define skiperr
  (lambda (config)
    (incr-pc (if (= 1 (caadr (cadddr (config-cpu config))))
                 2
                 1)
             config)))


; Note: the program counter (pc) is incremented
; modulo 4096 -- see incr-pc in problem 3.

; Examples:
; (jump 1 config1) =>
;   (((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;     (pc (0 0 0 0 0 0 0 0 0 0 0 1))
;     (run-flag (1))
;     (aeb (0)))
;    ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;     (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;     (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (4 (0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0))
;     (5 (1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1))))

; (config-cpu (jump 20 config1)) =>
;   ((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;    (pc (0 0 0 0 0 0 0 1 0 1 0 0))
;    (run-flag (1))
;    (aeb (0)))

; (config-cpu (skipzero config1)) ;=>
;   ((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;    (pc (0 0 0 0 0 0 0 0 1 0 0 0))
;    (run-flag (1))
;    (aeb (0)))

; (config-cpu (skipzero (list '((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) (pc (0 0 0 0 0 0 0 0 0 0 0 1)) (run-flag (0)) (aeb (1))) ram-ex1))) ;=>
;   ((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (pc (0 0 0 0 0 0 0 0 0 0 1 1))
;    (run-flag (0))
;    (aeb (1)))

;  (config-cpu (skippos config2)) ;=>
;    ((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
;     (pc (0 0 0 0 0 0 0 0 1 0 1 0))
;     (run-flag (0))
;     (aeb (1)))

; (config-cpu (skippos (list '((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) (pc (0 0 0 0 0 0 0 0 0 0 0 1)) (run-flag (0)) (aeb (1))) ram-ex1))) ;=>
;   ((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (pc (0 0 0 0 0 0 0 0 0 0 1 0))
;    (run-flag (0))
;    (aeb (1)))

; (config-cpu (skiperr config1)) ;=>
;   ((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;    (pc (0 0 0 0 0 0 0 0 1 0 0 0))
;    (run-flag (1))
;    (aeb (0)))

;  (config-cpu (skiperr config2)) ;=>
;    ((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
;     (pc (0 0 0 0 0 0 0 0 1 0 1 0))
;     (run-flag (0))
;     (aeb (0)))
;************************************************************

;************************************************************
; ** problem 7 ** (10 points)
; Write two procedures

; (loadi address config)
; (storei address config)

; (loadi address config)
; takes a memory address and a TC-201 configuration
; and returns a TC-201 configuration that reflects
; the result of doing a "load indirect" from the
; given memory address to the accumulator.
; That is, the low-order 12 bits of the contents
; of the memory register addressed by address
; are extracted and used as the memory address
; from which the contents are loaded into the accumulator.
; All other registers are unaffected.

(define loadi
  (lambda (address config)
    (xload (bits->int (exactly 12 (ram-read address (config-ram config))))
           config)))
      

; (storei address config)
; takes a memory address and a TC-201 configuration
; and returns a TC-201 configuration that reflects
; the result of doing a "store indirect" to the
; given memory address from the accumulator.
; That is, the low-order 12 bits of the contents
; of the memory register addressed by address
; are extracted and used as the memory address
; to which the contents of the accumulator are copied.
; All other registers are unaffected.

(define storei
  (lambda (address config)
    (store (bits->int (exactly 12 (ram-read address (config-ram config))))
           config)))

; This example is useful for loadi and storei testing.

(define ram-ex3
  '((0 (0 0 0 0  0 0 0 0  0 0 0 0  0 1 0 0))
    (1 (1 1 1 1  0 0 0 0  0 0 0 0  0 1 0 1))
    (2 (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
    (3 (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
    (4 (0 0 0 0  0 0 0 0  0 0 0 0  0 0 1 0))
    (5 (0 0 0 0  0 0 0 0  0 0 0 0  0 0 1 1))))

(define config3 (list cpu-ex1 ram-ex3))

; Examples:
; (loadi 0 config3); =>
;   (((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
;     (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;     (run-flag (1))
;     (aeb (0)))
;    ((0 (0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0))
;     (1 (1 1 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;     (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (4 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
;     (5 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))))

; (storei 0 config1) ;=>
;   (((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;     (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;     (run-flag (1))
;     (aeb (0)))
;    ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;     (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;     (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (4 (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;     (5 (1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1))))

; (loadi 1 config1) ;=>
;   (((acc (1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1))
;     (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;     (run-flag (1))
;     (aeb (0)))
;    ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;     (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;     (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (4 (0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0))
;     (5 (1 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1))))

; (storei 1 config1) ;=>
;   (((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;     (pc (0 0 0 0 0 0 0 0 0 1 1 1))
;     (run-flag (1))
;     (aeb (0)))
;    ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;     (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 1))
;     (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (4 (0 0 0 0 0 0 0 0 0 0 1 0 1 0 1 0))
;     (5 (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))))
;************************************************************

;************************************************************
; ** problem 8 ** (10 points)
; Write one procedure

; (next-config config)

; that takes a TC-201 configuration
; and returns the next TC-201 configuration,
; after one iteration of the fetch/execute
; cycle.

; That is, if the run-flag is 0, then the
; configuration config is returned unchanged,
; because the machine is halted.

; The instructions that should be implemented are:

; halt, load, store, add, sub, input, output, jump
; skipzero, skippos, skiperr, loadi, storei.

; These are opcodes 0000 through 1100.
; You should intepret an undefined opcode 
; (1101 through 1111) as a halt instruction.

; For a halt instruction, in the returned
; configuration the run-flag is 0 and all
; other registers are unchanged.

; Otherwise, the program counter (pc) contains
; a memory address, and the TC-201 instruction
; at that location is fetched and executed,
; and the resulting configuration is returned.

(define get-instr
  (lambda (config)
    (bits->int 
     (extract 0 3 (ram-read 
                   (bits->int (cadadr (config-cpu config)))
                   (config-ram config))))))

(define get-contents
  (lambda (config)
    (bits->int (exactly 12 (ram-read 
                            (bits->int (cadadr (config-cpu config)))
                            (config-ram config))))))

(define halt?
  (lambda (config)
    (let ((instr (get-instr config)))
    (cond
      ((= 0 (caadr (caddr (config-cpu config)))) #t)
      ((< 12 instr) #t)
      ((= 0 instr) #t)
      (else #f)))))

(define set-rf
  (lambda (config rf)
    (cons (list (caar config)
                (cadar config)
                (list 'run-flag (list rf))
                (caddr (cdar config)))
          (cdr config))))

(define next-config
  (lambda (config)
    (if (halt? config)
        (set-rf config 0)
        (incr-pc 1
                 (case (get-instr config)
                   ((1) (xload (get-contents config) config))
                   ((2) (store (get-contents config) config))
                   ((3) (add (get-contents config) config))
                   ((4) (sub (get-contents config) config))
                   ((5) (input config))
                   ((6) (output config))
                   ((7) (jump (get-contents config) config))
                   ((8) (skipzero config))
                   ((9) (skippos config))
                   ((10) (skiperr config))
                   ((11) (loadi (get-contents config) config))
                   ((12) (storei (get-contents config) config)))))))
            

; This example is useful for testing next-config.

(define cpu-ex4
  '((acc (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
    (pc  (0 0 0 0  0 0 0 0  0 0 0 0))
    (run-flag (1))
    (aeb (0))))

(define ram-ex4
  '((0 (0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1))
    (1 (0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 0))
    (2 (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
    (3 (0 0 0 0  0 0 0 0  0 1 0 1  0 1 0 1))
    (4 (1 1 1 1  1 1 1 1  0 0 0 0  0 0 0 0))))

(define config4 (list cpu-ex4 ram-ex4))

; Examples:
; (next-config config4) =>
;   (((acc (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))
;     (pc (0 0 0 0 0 0 0 0 0 0 0 1))
;     (run-flag (1))
;     (aeb (0)))
;    ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;     (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;     (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (3 (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))
;     (4 (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0))))

; (next-config (next-config config4)) =>
;   (((acc (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))
;     (pc (0 0 0 0 0 0 0 0 0 0 1 0))
;     (run-flag (1))
;     (aeb (0)))
;    ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;     (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;     (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (3 (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))
;     (4 (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))))

; (next-config (next-config (next-config config4))) =>
;   (((acc (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))
;     (pc (0 0 0 0 0 0 0 0 0 0 1 0))
;     (run-flag (0))
;     (aeb (0)))
;    ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;     (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;     (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (3 (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))
;     (4 (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))))
;************************************************************

;************************************************************
; ** problem 9 ** (10 points)
; Write one procedure

; (salo prog)

; that takes a TC-201 "simplified assembly-language"
; program prog and returns a TC-201 configuration 
; in which the program is translated into a sequence 
; of 16-bit patterns and loaded into RAM beginning
; with address 0.  The accumulator and program counter 
; should be initialized to all 0's, the arithmetic error bit
; should be 0, and the run-flag should be 1.

; In this simplified assembly language,
; there are no labels (and no symbol table),
; but there are symbolic opcodes, namely:

; halt, load, store, add, sub, input, output
; jump, skipzero, skippos, skiperr, loadi, storei

; and a data statement.  Each instruction
; is a list containing a symbolic opcode,
; and, if the instruction takes an address,
; an integer memory address in the range of 0 through 4095.
; Each data statement is a list containing the
; symbol data and an integer in the range -32767 through
; 32767.  A program is a list of instructions and
; data statements.

(define prog-to-register
  (lambda (prog)
    (cond
      ((equal? 'data (car prog))
       (exactly 16 (int->bits (cadr prog))))
      ((equal? 'halt (car prog))
       (exactly 16 '(0)))
      ((equal? 'load (car prog))
       (append '(0 0 0 1) (exactly 12 (int->bits (cadr prog)))))
      ((equal? 'store (car prog))
       (append '(0 0 1 0) (exactly 12 (int->bits (cadr prog)))))
      ((equal? 'add (car prog))
       (append '(0 0 1 1) (exactly 12 (int->bits (cadr prog)))))
      ((equal? 'sub (car prog))
       (append '(0 1 0 0) (exactly 12 (int->bits (cadr prog)))))
      ((equal? 'input (car prog))
       (exactly 16 '(1 0 1)))
      ((equal? 'output (car prog))
       (exactly 16 '(1 1 0)))
      ((equal? 'jump (car prog))
       (append '(0 1 1 1) (exactly 12 (int->bits (cadr prog)))))
      ((equal? 'skipzero (car prog))
       (exactly 16 '(1 0 0 0)))
      ((equal? 'skippos (car prog))
       (exactly 16 '(1 0 0 1)))
      ((equal? 'skiperr (car prog))
       (exactly 16 '(1 0 1 0)))
      (else (exactly 16 '(0))))))
    
(define add-prog-to-ram
  (lambda (prog ram n)
    (if (null? prog)
        ram
        (cons (list n
                    (prog-to-register (car prog)))
              (add-prog-to-ram (cdr prog) ram (+ 1 n))))))

(define salo
  (lambda (prog)
    (list (list (list 'acc (exactly 16 '(0)))
                (list 'pc (exactly 12 '(0)))
                (list 'run-flag '(1))
                (list 'aeb '(0)))
          (normalize (add-prog-to-ram prog '() 0)))))


; Examples:

; (salo '((data 1) (data -2) (data 32767))) ;=>
;   (((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (pc (0 0 0 0 0 0 0 0 0 0 0 0))
;     (run-flag (1))
;     (aeb (0)))
;    ((0 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
;     (1 (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
;     (2 (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))
;(newline)
; (salo '((load 3) (store 4) (halt))) ;=>
;   ((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (pc (0 0 0 0 0 0 0 0 0 0 0 0))
;    (run-flag (1))
;    (aeb (0)))
;(newline)
;(salo '((load 3) (store 4) (halt) (data 15) (data 16) (data 17))) ;=>
;   ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;    (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;    (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;    (3 (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;    (4 (0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0))
;    (5 (0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1)))
;************************************************************

;************************************************************
; ** problem 10 ** (10 points)
; Write one procedure and
; one program for the TC-201:

; (simulate n config)
; sum-prog

; (simulate n config)
; simulates the TC-201 computer
; from the configuration config until
; either it halts (the run-flag is 0)
; or n iterations of the fetch/execute
; cycle have been performed, whichever is first.
; The result returned should be a list of
; the successive configurations of the TC-201
; starting with the config.

(define simulate
  (lambda (n config)
    (cond
      ((= 0 n)
       '())
      ((= 0 (caadr (caddr (config-cpu config))))
       config)
      (else
       (cons config (simulate (- n 1) (next-config config)))))))


; You are strongly advised to use your simulate
; procedure to help you test your simulator
; and instructions more extensively than the test cases 
; in the assignment.

; sum-prog
; reads in a zero-terminated sequence of numbers from
; the user, prints their sum, and halts.  You need
; not worry about arithmetic overflow.

; Examples: 

(equal? (simulate 5 config4) ;=>
 '((((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (pc (0 0 0 0 0 0 0 0 0 0 0 0))
    (run-flag (1))
    (aeb (0)))
   ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
    (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
    (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (3 (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))
    (4 (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0))))
  (((acc (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))
    (pc (0 0 0 0 0 0 0 0 0 0 0 1))
    (run-flag (1))
    (aeb (0)))
   ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
    (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
    (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (3 (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))
    (4 (1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0))))
  (((acc (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))
    (pc (0 0 0 0 0 0 0 0 0 0 1 0))
    (run-flag (1))
    (aeb (0)))
   ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
    (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
    (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (3 (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))
    (4 (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))))
  (((acc (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))
    (pc (0 0 0 0 0 0 0 0 0 0 1 0))
    (run-flag (0))
    (aeb (0)))
   ((0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
    (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
    (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (3 (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))
    (4 (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1))))))

; Test whether sum-prog is defined as a list of lists.
; (and (list? sum-prog) (not (member #f (map list? sum-prog)))) => #t

; Example of run-time behavior interacting with user.

;; > (define x (simulate 100 (salo sum-prog)))
;; input = 13
;; input = -14
;; input = 55
;; input = 0
;; output = 54

;************************************************************

;************************************************************
; ** problem 11 ** (10 points)
; Write one program for the TC-201:

; reverse-prog

; reads in a zero-terminated sequence of numbers from
; the user, prints them out in reverse, and halts.
; You need not worry about running out of memory.

; Hint: write it first using labels and symbolic addresses,
; and then construct a symbol table by hand to translate
; the labels into numeric addresses.

; Examples: 

; Test whether reverse-prog is defined as a list of lists.
; (and (list? reverse-prog) (not (member #f (map list? reverse-prog)))) => #t

; Example of run-time behavior interacting with user.
;; > (define x (simulate 200 (salo reverse-prog)))
;; input = 14
;; input = -15
;; input = 16
;; input = 0
;; output = 16
;; output = -15
;; output = 14

;************************************************************

;********************** end of hw6.scm **********************
