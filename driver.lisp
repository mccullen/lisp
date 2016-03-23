;;;; Name: Jeff McCullen
;;;; Date: March 23, 2016
;;;; Class: CS 322: Programming Languages
;;;; Professor: Dr. Briggs
;;;; Description: Driver to test functions dealing with array
;;;; manipulation in LISP contained in the file array.lisp.

(load "array.lisp")

;;; Test my-make-array

(print "=> Testing (my-make-array (dimensions))")
(print "(my-make-array '(2 2)): ")
(print (my-make-array '(2 2)))
(print "(my-make-array '(1 5)): ")
(print (my-make-array '(1 5)))
(print "(my-make-array '(2 2 3)): ")
(print (my-make-array '(2 2 3)))


;;; Test my-array-dimension

(print "=> Testing (my-array-dimension (my-array nth-dimension))")

;;; Test my-aref

(print "=> Testing (my-aref (my-array indexes))")
(print "(my-aref '((1 2) (3 4) (5 6) (7 8)) '(3 2))")
(print (my-aref '((1 2) (3 4) (5 6) (7 8)) '(3 2)))
(print "(my-aref '((1 2) (3 4) (5 6) (7 8)) '(3 1))")
(print (my-aref '((1 2) (3 4) (5 6) (7 8)) '(3 1)))
(print "(my-aref '((1 2) (3 4) (5 6) (7 8)) '(1 1))")
(print (my-aref '((1 2) (3 4) (5 6) (7 8)) '(1 1)))
(print "(my-aref '((1 2) (3 4) (5 6) (7 8)) '(4 2))")
(print (my-aref '((1 2) (3 4) (5 6) (7 8)) '(4 2)))

;;; Test my-set-aref

(print "=> Testing (my-set-aref (my-array item indexes))")
(print "(my-set-aref '((1 2 3) (4 5 6) (7 8 9)) 88 '(1 1))")
(print (my-set-aref '((1 2 3) (4 5 6) (7 8 9)) 88 '(1 1)))
(print "(my-set-aref '((1 2 3) (4 5 6) (7 8 9)) 88 '(2 1))")
(print (my-set-aref '((1 2 3) (4 5 6) (7 8 9)) 88 '(2 1)))
(print "(my-set-aref '((1 2 3) (4 5 6) (7 8 9)) 88 '(3 3))")
(print (my-set-aref '((1 2 3) (4 5 6) (7 8 9)) 88 '(3 3)))
(print "(my-set-aref '((1 2 3) (4 5 6) (7 8 9)) 88 '(2 2))")
(print (my-set-aref '((1 2 3) (4 5 6) (7 8 9)) 88 '(2 2)))
