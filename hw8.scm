; ************************************************************************
; CS 201a HW #8 DUE 11:59 pm on Friday, Dec. 6
; using the Zoo submit command.
; ********************************************************************
; Name:
; Email address:
; ********************************************************************

; Note that Yale College regulations stipulate that
; NO work may be accepted after the last day of Reading Week, Dec. 11, 
; without a Temporary Incomplete authorized by your college dean.

; ************************************************************************

; Topics: objects and running times.
; This assignment is worth 50 points.  
; There are no public test cases for it -- please
; make sure you test your procedures thoroughly.

; You may (and for some problems, must!) use mutators.

; Unless the problem specifies otherwise:
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.

; ************************************************************************
; We include the following libraries.

; This command loads a library containing the random-integer procedure.
(#%require srfi/27)

; The following command loads a library of time procedures.
(#%require srfi/19)

; ************************************************************************
; ** problem 1 ** (20 points)
; Write a procedure

; (make-ttt-game)

; that takes no arguments and returns
; a procedure implementing a tic-tac-toe game object.
; The procedure has local state representing the positions
; of x's and o's on the board (and any other relevant
; information) and responds to the following commands.

; command           result
; -------           ------
; 'reset            changes the game back to its initial state,
;                   and returns the empty board: ((- - -) (- - -) (- - -)).

; 'board            returns the current game board as a list of 3
;                   lists, each of which is a list of 3 symbols
;                   chosen from: x, o, -
;                   The first list gives row 1, the second list row 2
;                   and the third list row 3 of the board.

; 'play sym i j     attempts to place symbol sym on the board, where
;                   sym is one of the symbols x or o
;                   i is the row (1, 2, or 3, from the top)
;                   j is the column (1, 2, or 3, from the left).

;                   If the move is permitted, the move is made:
;                   if x has won, the symbol x-won is returned,
;                   if o has won, the symbol o-won is returned,
;                   if the board is filled and neither has won,
;                   the symbol cats-game is returned,
;                   otherwise, the board is returned.

;                   If the move is not permitted, there is no
;                   change to the state of the board and a
;                   symbol is returned indicating the reason.
;                   Check the following conditions in order:
;                       (1) if x or o has already won or all
;                        the squares are filled, then return game-over
;                       (2) if the move is out of turn, return out-of-turn
;                       (3) if the square is already filled, return 
;                        square-filled.
 
; A game object is a procedure that may be called with *arbitrary*
; arguments.  It should cope gracefully, returning the symbol
; ill-formed-command if its arguments do not conform to the above
; syntax.  This is a departure from previous practice in this course --
; you should do careful error-checking in this procedure.

; Examples:

;; > (define g1 (make-ttt-game))
;; > (g1)
;; ill-formed-command
;; > (g1 'hi-there!)
;; ill-formed-command
;; > (g1 'board)
;; ((- - -) (- - -) (- - -))
;; > (g1 'play 'x 2 2)
;; ((- - -) (- x -) (- - -))
;; > (g1 'play 'x 1 1)
;; out-of-turn
;; > (g1 'play 'o 2 2)
;; square-filled
;; > (g1 'play 'o 3 3)
;; ((- - -) (- x -) (- - o))
;; > (g1 'play 'x 0 0)
;; ill-formed-command
;; > (g1 'play 1 2)
;; ill-formed-command
;; > (g1 'play 'x 1 2)
;; ((- x -) (- x -) (- - o))
;; > (g1 'play 'o 1 3)
;; ((- x o) (- x -) (- - o))
;; > (g1 'play 'x 3 2)
;; x-won
;; > (g1 'play 'o 2 3)
;; game-over
;; > (define g2 (make-ttt-game)) 
;; > (g2 'play 'o 2 3)
;; ((- - -) (- - o) (- - -))
;; > (g1 'board)
;; ((- x o) (- x -) (- x o))
;; > (g1 'reset)
;; ((- - -) (- - -) (- - -))
;; > (g2 'board)
;; ((- - -) (- - o) (- - -))

; ************************************************************************


; ************************************************************************
; Timing information.
; Here is a procedure to create a timer object
; that reports elapsed time since its creation
; in seconds.

(define make-timer
  (lambda ()
    (let ((start-time (current-time)))
      (lambda ()
	(let ((elapsed (time-difference (current-time) start-time)))
	  (+ (exact->inexact (time-second elapsed))
	     (/ (exact->inexact (time-nanosecond elapsed))
		(* 1.0e3 (time-resolution)))))))))

; Here is an illustration of using make-timer.
    
(define spin-cycle
  (lambda (n)
    (if (<= n 0) #t  (spin-cycle (- n 1)))))

(define time1
  (lambda (n)
    (let ((t1 (make-timer)))
      (spin-cycle n)
      (t1))))

; These are results on a Zoo workstation (Fall 2013.)
; The arguments are in Scheme's scientific
; notation (so 1e4 = 10000.0) and the results
; are in seconds.

;; > (time1 1e4)
;; 0.0
;; > (time1 1e5)
;; 0.018
;; > (time1 1e6)
;; 0.029
;; > (time1 1e7)
;; 0.285
;; > (time1 1e8)
;; 2.826
;; > (time1 1e9)
;; 28.308

; In this case, running (time1 1e7) took a bit
; over a quarter of a second, while running (time1 1e9)
; took about 28 seconds.  The times are fairly
; convincingly *linear* in n in these examples, at least
; for larger values of n.

; ************************************************************************
; ** problem 2 ** (10 points)
; Write two procedures:

; (random-list n m)
; (time-repeats n proc args)

; (random-list n m)
; takes positive integers n and m as inputs and
; returns a list of n random integers between 0 and m-1
; inclusive.

; (time-repeats n proc args)
; repeatedly applies proc to the list of arguments args
; n times, and returns the total time required by the
; n applications.  Recall Scheme's procedure apply.

; Examples:
; (Your random results may vary, and your
; timing results will depend on the machine you use
; and the random variation in the run.  These were
; run on a Zoo node, Fall 2013.)

;; > (random-list 10 2)
;; (1 0 1 1 0 0 1 0 0 1)
;; > (random-list 5 50)
;; (34 7 37 10 20)
;; > (time-repeats 1e6 + '(13 14))
;; 0.067
;; > (time-repeats 1e7 + '(13 14))
;; 0.52
;; > (time-repeats 1e8 + '(13 14))
;; 5.188
; ************************************************************************


; ************************************************************************
; ** problem 3 ** (10 points)

; In lecture it was claimed that the running time of the
; Scheme library procedure (append lst1 lst2) grows in proportion
; to the length of the list lst1, and does not depend on the length
; of list lst2.

; Here you are asked to provide timing evidence for
; this claim and also the claim that the time for the
; Scheme library procedure (length lst) grows in proportion 
; to the length of lst.  Note that random-list can be
; used to generate long lists of randomly-chosen numbers,
; and time-repeats can be used to time a number of repetitions
; of an operation, to get a more accurate estimate of its time.

; Include definitions of any Scheme procedures you use,
; but make sure your discussion is properly commented
; out so that it will load properly.  Also make sure that
; any definitions of long lists are commented out, to avoid
; a long loading time for your Scheme file.

; Also, please don't use the "comment boxes" feature in Racket!
; ************************************************************************


; ************************************************************************
; ** problem 4 ** (10 points)
; Write a procedure

; (make-queue)

; that takes no arguments and returns an object 
; (i.e., a procedure with state) that implements a queue
; that is initially empty and is capable of processing
; the following commands.

; empty?       test whether the current queue is empty, returning #t or #f
; getq         return the element at the beginning of the current queue
; delq!        remove the element at the beginning of the current queue
;              and return it
; putq! value  add the given value at the end of the current queue
;              and return it
; length       return the number of elements in the current queue

; If the command is not one of these, return the symbol error.
; You may assume that a queue object will be called with at
; least one argument, and if the first argument is one of the symbols
; above, the other arguments will have the correct syntax for
; that command.  Commands getq and delq! will not be invoked on
; an empty queue.

; The implementation of the queue object should be based on the queue data
; structure proposed by the text in Section 2.9 in order to achieve *constant*
; bounded time for each of the five commands, regardless of the number 
; of elements in the queue. 

; The text indicates how to do this for the first 4 commands,
; but you will have to figure out how to do this for length.
; Note that problem (3) claims that the Scheme library procedure
; length takes time proportional to the length of the argument list,
; which is NOT constant.

; (We will test both the correctness and the running time 
; of your queue implementation.)

; Note the data structure(s) should be *local to the queue object*,
; unlike the code given in the text.

; Examples of using make-queue

;; > (define q1 (make-queue))
;; > (q1 'empty?)
;; #t
;; > (q1 'putq! 'a)
;; a
;; > (q1 'putq! 'b)
;; b
;; > (q1 'length)
;; 2
;; > (q1 'getq)
;; a
;; > (define q2 (make-queue)) 
;; > (q2 'putq! 'g)
;; g
;; > (q2 'putq! 'h)
;; h
;; > (q2 'putq! 'i)
;; i
;; > (q2 'getq)
;; g
;; > (q2 'delq!)
;; g
;; > (q2 'getq)
;; h
;; > (q1 'getq)
;; a
;; > (q1 'hi)
;; error
;; >
; ************************************************************************


; *********************  end of hws! *************************************
