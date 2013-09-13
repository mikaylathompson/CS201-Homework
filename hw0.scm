; ********************************************************
; CS 201a HW #0  DUE Wednesday 9/11/13, 11:59 pm
; ** using the Zoo submit system **

; ********************************************************
; Name:  Mikayla Thompson
; Email address:  mikayla.thompson@yale.edu
; ********************************************************

; This file may be loaded into Scheme.  
; Lines beginning with semicolons are Scheme comments.

; Homework #0 will be worth 20 points -- other
; homeworks will be worth 100 points.
; One purpose of homework #0 is to make sure 
; you can use the submit system on the Zoo.  
; You will receive no credit for this assignment
; unless you successfully use the submit system to submit it.

; You will be submitting two files for homework #0.
; Please name them:
; hw0.scm (for the Scheme definitions and procedures)
; response.txt (for the reading response)

; ********************************************************
; ** problem 0 ** (1 easy point) 
; replace the number 0 in this definition to
; indicate how many hours you spent doing this assignment.
; Fractions are fine, eg, 3.14159.

(define hours 0)

; ********************************************************
; ** problem 1 ** (5 points)

; Write a procedure (difference-squares x y)
; that takes as arguments integers x and y
; and returns the value of the square of x
; minus the square of y.

; Examples

; (difference-squares 4 3) => 7
; (difference-squares 3 5) => -16
; (difference-squares -10 0) => 100
; ********************************************************

(define difference-squares (lambda (n m) (- (* n n) (* m m))))

; ********************************************************
; ** problem 2 ** (4 points)

; Write a procedure (favorite)
; that takes no arguments and
; returns a *symbol* indicating
; something you like to eat.

; Example (yours will likely be different)

; (favorite) => pizza-with-artichoke-hearts
; ********************************************************

(define favorite
    (lambda ()
      'ice-cream))

; ********************************************************
; ** problem 3 ** (10 points)

; For this problem, you will find one
; article (of 2 pages or more) in
; the magazine "Communications of the ACM",
; in the June, July, August or September 2013
; issues, read the article, and answer 
; the following three questions:

;   a. What did you know about the topic
;      prior to reading the article?
;   b. What did you learn from reading the
;      article?
;   c. What more would you like to know
;      about the topic?

; Your answer should be *at most* 400 words,
; as counted by the wc utility on the Zoo,
; saved in .txt format, and submitted as
; the file response.txt for assignment 0.
; Please include your name and email address
; in the file.

; ********************************************************
; ********  end of homework #0
; ********************************************************
