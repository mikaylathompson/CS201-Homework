; CS 201b HW #2  DUE Wednesday, September 25, 2013 at 11:59 pm
; electronically, using the submit system on the Zoo.

;**************************************************************
; Name: Mikayla Thompson
; Email address: mikayla.thompson@yale.edu
;**************************************************************

; If you are asked to write a procedure, you MAY write auxiliary
; procedures -- for each of your auxiliary procedures, 
; please include a comment explaining what it does.

; You MAY use procedures you have written on previous
; assignments.  Every procedure you define should have
; been originally written by you.

; Please do not use mutators (set! and its relatives.)

; Topics: Scheme, the game of shut-the-box

;**************************************************************
; ** problem 0 ** (1 easy point)
; Please change 0 in the definition below to
; reflect how long you spent on this assignment.

(define hours 1)

;**************************************************************
; ** problem 1 ** (10 points)
; Write two procedures:

; (remove-leftmost item lst)
; (remove-all item lst)

; (remove-leftmost item lst)
; returns a list equal to lst 
; with the leftmost occurrence of a top-level
; element equal? to item removed.

; (remove-all item lst)
; returns a list equal to lst
; with all occurrences of top-level elements
; equal? to item removed.

; In either case, if there is no top-level
; element of lst equal? to item, the original
; list is returned.

; Examples

; (remove-leftmost 'b '(a b c)) => (a c)
; (remove-leftmost 2 '(1 2 3 2 1)) => (1 3 2 1)
; (remove-leftmost '(1 2) '((1 3) (1 2) (1 3) (1 2))) => ((1 3) (1 3) (1 2))
; (remove-leftmost 7 '(3 2 1)) => (3 2 1)
; (remove-all 'b '(a b c)) => (a c)
; (remove-all 2 '(1 2 3 2 1)) => (1 3 1)
; (remove-all '(1 2) '((1 3) (1 2) (1 3) (1 2))) => ((1 3) (1 3))
; (remove-all 7 '(3 2 1)) => (3 2 1)
;**************************************************************

(define remove-leftmost
  (lambda (item list)
    (cond
      ((null? list) 
       '())
      ((equal? (car list) 
               item) 
       (cdr list))
      (else 
       (cons (car list) 
             (remove-leftmost item (cdr list)))))))

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

;**************************************************************
; ** problem 2 ** (10 points)
; Write a procedure:

; (filter pred? lst)

; that takes a predicate pred? and a list lst
; and returns a list (in order) of all those elements
; of lst for which pred? is #t.
; You may assume that pred? is a predicate of one argument
; that may be applied to each element of lst.

; Examples
; (filter odd? '()) => ()
; (filter even? '(3 2 1 2 4)) => (2 2 4)
; (filter (lambda (x) (< x 10)) '(17 18 3 2 55 10)) => (3 2)

;**************************************************************

(define filter
  (lambda (pred? list)
    (if (equal? list 
                '())
        '()
        (if (pred? (car list))
            (cons (car list) 
                  (filter pred? (cdr list)))
            (filter pred? (cdr list))))))

;**************************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (remove-duplicates lst)

; that takes as input a list lst
; and returns a list equal to the list
; (in order) of each top-level element
; of lst that is the leftmost occurrence
; of that value, that is, each element that
; is not equal? to any top-level element to
; its left in lst.

; Examples

; (remove-duplicates '()) => ()
; (remove-duplicates '(a c c b a)) => (a c b)
; (remove-duplicates '((1 2) 3 4 (3 4))) => ((1 2) 3 4 (3 4))
; (remove-duplicates '((1 2) (3 4) (3 4) (2 1))) => ((1 2) (3 4) (2 1))
; (remove-duplicates '(to be or not to be)) => (to be or not)

;**************************************************************


(define remove-duplicates
  (lambda (list)
    (if (null? list)
        '()
        (cons (car list) 
              (remove-duplicates (remove-all (car list) 
                                             (cdr list)))))))


;**************************************************************
; For the remaining problems,
; we consider the game of "shut the box,"
; which will be (or was) demonstrated in lecture.
; (Or try a web search with "shut the box game".)

; The traditional verson of the game has 9 flappers 
; numbered 1 to 9, each of which may be up or down.
; Initially they are all up.
; The player rolls 2 dice and notes the sum, S.
; The player then flips down any combination
; of up flappers whose sum is S, and rolls
; the dice again.
; This process repeats until there is no combination
; of up flappers whose sum is S, at which point
; the player's score is the sum of the remaining
; up flappers.

; Play of the game consists of player 1 playing
; to find his or her score, and then player 2
; playing to find his or her score -- the
; player with the *lower* score wins.

; For example, player 1 starts with all flappers up:
; initially up flappers: 1 2 3 4 5 6 7 8 9
; roll 2 and 6, and decide to flip down 8
; up flappers are now: 1 2 3 4 5 6 7 9
; roll 6 and 6, and decide to flip down 3 and 9
; up flappers are now: 1 2 4 5 6 7
; roll 1 and 3, and decide to flip down 4
; up flappers are now: 1 2 5 6 7
; roll 6 and 6, and decide to flip down 5 and 7
; up flappers are now: 1 2 6
; roll 5 and 6, and there is no way to make 11
; so the final score for player1 is 1+2+6 = 9

; Then player2 starts with all flappers up, and so on.

; If a player succeeds in putting down ALL
; the flappers, the player is said to have "shut the box."

;**************************************************************
; The following accesses an implementation
; of a procedure (random-integer n)
; to return a random integer
; between 0 and n-1 inclusive
; in the R5RS language of DrRacket.
; For example,
; (random-integer 20) => 17
; (random-integer 20) => 3

; (Note that the numbers returned
; are "pseudo-random" -- every
; time you re-enter Scheme, a
; sequence of calls to random-integer
; will return the same sequence of values.)

(#%require srfi/27)

;**************************************************************
; The following defines the symbol standard-flappers in the
; top-level environment to be the list of the numbers on
; the flappers in the standard game.

; Some of your procedures will be parameterized with
; a list of flappers and should be able to
; deal with variants of the game in which
; the number of flappers and/or 
; the numbers on the flappers are different.

(define standard-flappers '(1 2 3 4 5 6 7 8 9))

; A set of flappers will be represented by a list
; of positive integers in non-decreasing order.
; There may be duplicates, so (2 3 3 5 7) is
; a possible set of flappers.

; A die (singular of dice) is represented by a non-empty
; list of positive integers in non-decreasing order,
; representing the numbers on the faces of the die.
; The following is the standard die; there may
; also be non-standard ones that your procedures
; must deal with.

(define standard-die '(1 2 3 4 5 6))

; The standard set of dice is a list with two
; copies of the standard die.  Your procedures
; should be able to deal with a non-empty list
; of possibly different dice.

(define standard-dice (list standard-die standard-die))

; A state of the game is represented by a set of
; flappers (listed in non-decreasing order.)
; The initial state in the standard game is 
; (1 2 3 4 5 6 7 8 9).
; In the example above, the state at the end
; of the game is (1 2 6).

;**************************************************************
; ** problem 4 ** (10 points)
; Write three procedures:

; (sum-of lst)
; (pick-random lst)
; (roll dice)

; (sum-of lst) takes a list lst of
; numbers, and returns their sum.
; (Note that the sum of an empty
; list of numbers is 0.)

; (pick-random lst)
; takes a *non-empty* list lst
; and returns a uniformly randomly
; chosen element of lst.
; (Please use the procedure random-integer,
; accessed as shown above.)

; (roll dice)
; returns the sum of the values
; obtained by chosing a random face of each
; die in the list of dice.

; Examples:
; (Note that for the examples that involve
; randomness, your results may differ.)
; (sum-of '()) => 0
; (sum-of '(1 2 6)) => 9
; (sum-of '(4 5 6 1)) => 16
; (pick-random '(a b c)) => c
; (pick-random '(a b c)) => a
; (pick-random '(1 2 3 4 5 6)) => 2
; (roll '((1 2 3 4 5 6) (1 2 3 4 5 6))) => 7
; (roll '((3 3 3) (1 1 1))) => 4

;**************************************************************

(define sum-of
  (lambda (list)
    (if (null? list)
        0
        (+ (car list) 
           (sum-of (cdr list))))))

(define pick-random
  (lambda (list)
    (list-ref list (random-integer (length list)))))
    
(define roll
  (lambda (dice)
    (cond
      ((null? dice) 
       0)
      (else 
       (sum-of (list (pick-random (car dice)) 
                     (roll (cdr dice))))))))

;**************************************************************
; ** problem 5 ** (10 points)
; A move consists of a list of positive integers
; in non-decreasing order
; indicating that flappers with those numbers 
; should be flipped down.

; Write two procedures

; (ok-move? move state)
; (make-move move state)

; where state is a possible
; state of the flappers and
; move is a move as indicated above.

; (ok-move? move state)
; given a move and a state
; returns #t if the move is possible in the state
; and #f otherwise.
; A move is possible if we can flip down a flapper
; in state corresponding to each of the numbers
; in move.

; (make-move move state)
; given a move and a state
; returns the state that results from making the given move
; (ie, flipping down the indicated flappers.)
; You may assume that the move is possible in the
; given state.

; Examples

; (ok-move? '(1 2 6) '(1 2 3 5 6 9)) => #t
; (ok-move? '(3 7) '(1 2 3 5 6 9)) => #f
; (ok-move? '(4 6) '(4 5 6)) => #t
; (ok-move? '(2 2) '(1 2 3)) => #f
; (make-move '(1 2 6) '(1 2 3 5 6 9)) => (3 5 9)
; (make-move '() '(3 4 5)) => (3 4 5)
; (make-move '(3 4 5) '(3 4 5)) => ()
; (make-move '(4) '(1 2 2 3 4 4 5)) => (1 2 2 3 4 5)

;**************************************************************

; includes? takes a list and a target.  It returns true
; if target is a top level element of list. (from hw1)
; Examples:
; (includes? '() 3) => #f
; (includes? '(3 5) 3) => #t
; (includes? '(3 (4 5) 6) 4) => #f
; (includes? '(3 (4 5) 6) '(4 5)) =>t
(define includes?
  (lambda (list target)
    (if (equal? '() list)
        #f
        (or (equal? (car list) target)
            (includes? (cdr list) target)))))

(define ok-move?
  (lambda (move state)
    (cond
      ((null? move) 
       #t)
      ((includes? state (car move)) 
       (ok-move? (cdr move) 
                 (remove-leftmost (car move) state)))
      (else #f))))
    
(define make-move
  (lambda (move state)
    (if (null? move)
        state
        (make-move (cdr move) 
                   (remove-leftmost (car move) state)))))


;**************************************************************
; ** problem 6 ** (10 points)
; Write a procedure:

; (all-moves state)

; that returns a list of all the possible moves
; in the given state of the game.
; The output should not contain duplicates, but may list the 
; possible moves in a *different* order from the examples.

; Examples:

; (all-moves '()) => (())
; (all-moves '(1 2 3)) => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
; (all-moves '(2 2 2)) => (() (2) (2 2) (2 2 2))

; Hint: suppose you have the answer
; (() (3) (2) (2 3)) for (all-moves '(2 3))
; What needs to be done to this to get the correct answer
; for (all-moves '(1 2 3))?

;**************************************************************

; join-to should add element to all top-level items in list
; Examples:
; (join-to 3 '((a b) (c d) (e f))) => ((3 a b) (3 c d) (3 e f))
(define join-to
  (lambda (element lst)
    (if (equal? lst '()) 
        '()
        (cons (cons element (car lst)) 
              (join-to element (cdr lst))))))

(define all-moves
  (lambda (state)
    (if (null? state)
        '( () )
        (remove-duplicates (append
                            (all-moves (cdr state))
                            (join-to (car state) 
                                     (all-moves (cdr state))))))))



;**************************************************************
; ** problem 7 ** (10 points)
; Write two procedures:

; (possible-moves sum state)
; (next-states sum state)

; where sum is the sum of a dice roll
; and state is a possible game state.

; (possible-moves sum state)
; returns the list of all possible moves
; whose elements sum to the value sum in the given state.
; The moves may be listed in *any order*, but
; there should not be duplicates.

; (next-states sum state)
; returns a list of all the states reachable
; from the given state by making a possible
; move with value equal to sum.
; The states may be listed in *any order*, but
; there should be no duplicates.

; Examples

; (possible-moves 12 '(3 4 5 7 9)) => ((5 7) (3 9) (3 4 5))
; (possible-moves 6 '(1 2 3 4 5 6)) => ((6) (2 4) (1 5) (1 2 3))
; (possible-moves 11 '(1 2 6)) => ()
; (next-states 12 '(3 4 5 7 9)) => ((3 4 9) (4 5 7) (7 9))
; (next-states 11 '(1 2 6)) => ()

;**************************************************************
    
(define possible-moves
  (lambda (sum state)
    (filter (lambda (move) (and (equal? sum (sum-of move))
                                (ok-move? move state)))
            (all-moves state))))    

; get-states takes in a list of moves, applies each of 
; them to a given state, and a returns a list of the 
; resulting states.
(define get-states
  (lambda (state moves)
    (if (null? moves)
        '()
        (cons (make-move (car moves) state) 
              (get-states state (cdr moves))))))

(define next-states
  (lambda (sum state)
    (get-states state (possible-moves sum state))))
    

;**************************************************************
; ** problem 8 ** (10 points)
; Write two procedures:

; (choose-random-move sum state)
; (choose-short-move sum state)

; where sum is the sum of a dice roll
; and state is a possible game state.

; (choose-random-move sum state)
; returns a randomly selected one
; of the possible moves in the
; given state when the dice roll sum
; is as indicated.
; If there is no legal move, it
; returns the symbol none.

; (choose-short-move sum state)
; returns a possible move with the
; shortest length (as a list) in
; the given state when the dice roll sum
; is as indicated.
; If there are more than one shortest
; moves, it should choose one randomly.
; If there is no legal move, it
; returns the symbol none.

; Examples:

; (choose-random-move 6 '(1 2 3 4 5 6 7 8 9)) => (1 2 3)
; (choose-random-move 6 '(1 2 3 4 5 6 7 8 9)) => (2 4)
; (choose-random-move 6 '(1 2 3 4 5 6 7 8 9)) => (6)
; (choose-random-move 11 '(1 2 4 6)) => (1 4 6)
; (choose-random-move 2 '(3 4 5)) => none

; (choose-short-move 6 standard-flappers) => (6)
; (choose-short-move 7 '(2 3 4 5)) => (3 4)
; (choose-short-move 7 '(2 3 4 5)) => (2 5)
; (choose-short-move 10 '(2 3 4 5)) => (2 3 5)
; (choose-short-move 17 '(2 3 4 5)) => none

;**************************************************************

(define choose-random-move
  (lambda (sum state)
    (if (equal? 0 (length (possible-moves sum state)))
        'none
        (pick-random (possible-moves sum state)))))

; return the top-level element of a list with the shortest length.
; If there are elements with equal length, return a random one.
(define shortest-element
  (lambda (lst)
    (if (equal? 1 (length lst))
        (car lst)
        (cond
          ((< (length (car lst)) 
              (length (shortest-element (cdr lst))))
           (car lst))
          ((> (length (car lst)) 
              (length (shortest-element (cdr lst))))
           (shortest-element (cdr lst)))
          (else (if (equal? 0 (random-integer 2))
                    (car lst)
                    (shortest-element (cdr lst))))))))


(define choose-short-move
  (lambda (sum state)
    (if (equal? 0 
                (length (possible-moves sum state)))
        'none
        (shortest-element (possible-moves sum state)))))

;**************************************************************
; ** problem 9 ** (10 points)
; Write a procedure:

; (play move-chooser flappers dice)

; that takes a move-chooser
; a list flappers of initial flappers
; a list of dice
; and simulates playing shut the box
; using the move-chooser to choose
; moves until no further moves
; are possible, at which point
; it returns the sum of the remaining
; up flappers in the state.

; Like choose-random-move or choose-short-move,
; a move-chooser takes as input
; a sum of dice and a game state
; and returns either
; 1) a move that is possible in the
; given state and sums to the given sum,
; or 
; 2) the symbol none if there is
; no move possible in the current state
; that sums to the given sum.
; The procedure choose-random-move is a
; move-chooser.

; You may assume the move-chooser behaves
; correctly, that is,
; always returns either a move or the symbol none;
; if it returns a move, then the move is possible in the
; given state and sums to the given sum;
; if it returns the symbol none, then there is no move possible
; in the given state that sums to the given sum.

; Examples

; (Your choose-random-move and choose-short-move need 
;  to be working to run these tests.)

; (play choose-random-move standard-flappers standard-dice) => 25
; (play choose-random-move standard-flappers standard-dice) => 9
; (play choose-random-move standard-flappers standard-dice) => 13
; (play choose-short-move standard-flappers standard-dice) => 6
; (play choose-short-move standard-flappers standard-dice) => 0
; (play choose-short-move standard-flappers standard-dice) => 21

;**************************************************************

; In each turn: 
; Roll dice to get sum
; use sum and state to list possible moves
; choose appropriate move
; return the new state
;   if none: calculate the sum and return that.


(define play
  (lambda (player flaps dice)
    (let ((next (player (roll dice) flaps)))
    (if (equal? next 
                'none)
        (sum-of flaps)
        (play player
              (make-move next flaps)
              dice)))))
   


;**************************************************************
; ** problem 10 ** (10 points)
; In this problem you are asked to
; compare the two strategies in problem 9 for playing 
; shut the box, and whether you can improve upon both of them.

; Once you have your play procedure (problem #9)
; working, the following code may be used to
; play two players against each other
; for a number of games, returning the number
; of games won by player1, the number of games
; won by player2, and the number of tie games.

; Note that in this setting, player2 does NOT
; learn player1's score.

(define match
  (lambda (player1 player2 games flappers dice)
    (match-helper 
     player1 player2 games 0 0 0 flappers dice)))

(define match-helper
  (lambda (player1 player2 games won1 won2 ties flappers dice)
    (if (<= games 0)
	(list won1 won2 ties)
	(let ((score1 (play player1 flappers dice))
	      (score2 (play player2 flappers dice)))
	  (match-helper player1 player2 (- games 1)
			(+ won1 (if (< score1 score2) 1 0))
			(+ won2 (if (< score2 score1) 1 0))
			(+ ties (if (= score1 score2) 1 0))
			flappers dice)))))

; For example:
; (match choose-short-move choose-random-move 100 standard-flappers standard-dice) => (67 32 1)

; (Part a)
; Use match to compare the results of playing
; the choose-short-move strategy against 
; the choose-random-move strategy, as described above.  
; Estimate the fraction of matches won by choose-short-move, 
; won by choose-random-move, and tied.
; Include (as comments) your estimates and the data to support them.

; (Part b)
; Try to find a strategy for playing shut the box
; that is better than both choose-short-move and choose-random-move.
; Implement your player strategy as a procedure

; (choose-my-move sum state)

; and include (as comments) estimates and data and any procedures
; you write to support the claim that choose-my-move 
; is better than choose-short-move and choose-random-move.
; Include comments to explain CLEARLY how your procedure works.

;**************************************************************

; (a) My computer was not very happy with me, but I ran match with
;     1000 games between short-move and random-move.  The results
;     were 754 wins for short-move, 224 for random-move, and 22 ties.
;     Clearly, short-move is the better player, but it doesn't seem
;     to be all that good if it's only 3 times better than a totally
;     random player.

; return the top-level element of a list with the longest length.
; If there are elements with equal length, return a random one.
(define longest-element
  (lambda (lst)
    (if (equal? 1 (length lst))
        (car lst)
        (cond
          ((> (length (car lst)) 
              (length (longest-element (cdr lst))))
           (car lst))
          ((< (length (car lst)) 
              (length (longest-element (cdr lst))))
           (longest-element (cdr lst)))
          (else (if (equal? 0 
                            (random-integer 2))
                    (car lst)
                    (longest-element (cdr lst))))))))

; A player that always chooses the move that flips the most flaps
; It plays very poorly (only winning 2 of 10 games against short-move)
(define choose-long-move
  (lambda (sum state)
    (if (equal? 0 
                (length (possible-moves sum state)))
        'none
        (longest-element (possible-moves sum state)))))

; includes? takes a list and a target.  It returns true
; if target is a top level element of list. (from hw1)
(define includes?
  (lambda (list target)
    (if (equal? '() list)
        #f
        (or (equal? (car list) 
                    target)
            (includes? (cdr list) target)))))


; Results are against short-move unless otherwise noted
; a. 9/8=short, other=long: (49 46 5)
; b. 9=short, other=long: (45 46 9)
; c. 9/8=short, 1/2=long, other=random: (39 57 4)
; d. 9/8/7=short, 1/2/3=long, other=random: (51 44 5)
; e. 9/8/7=short, 1/2=long, other=rand: (54 41 5)
; f. 9/8=short, 1/2/3=long, other=rand: (40 57 3)

; Because the results were pretty close, I ran f against c 500 times
; and got a result of (243 236 21).  That's still very close,
; but I'm going to keep f as my player.  In 500 games against
; random-move, the result was (383 105 12).

; Another measure of success would be how many games a particular
; strategy can actually win (end with a score of 0).
; a. 3/100
; b. 6/10
; c. 4/100
; d. 4/10
; e. 5/10
; f. 4/100
; short. 7/10
; rand. 1/100
; long. 1/100
; So short is a bit better, and random and long are a bit worse, but
; there's not a large difference.

(define choose-my-move
  (lambda (sum state)
    (let ((moves (possible-moves sum state)))
    (if (equal? 0 (length moves))
        'none
        (cond
          ((or (includes? state 9)
               (includes? state 8))
           (choose-short-move sum state))
          ((or (includes? state 1)
               (includes? state 2)
               (includes? state 3))
           (choose-long-move sum state))
          (else (choose-random-move sum state)))))))

; count how many games a strategy can end with a score of 0
(define count-wins
  (lambda (player games won flappers dice)
    (if (<= games 0)
	won
	(let ((score (play player flappers dice)))
	  (count-wins player (- games 1)
			(+ won (if (= score 0) 1 0))
			flappers dice)))))

;******************  end of hw #2  *****************************
