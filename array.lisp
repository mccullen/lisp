;;;; Name: Jeff McCullen
;;;; Date: March 23, 2016
;;;; Class: CS 322: Programming Languages
;;;; Professor: Dr. Briggs
;;;; Description: LISP functions dealing with arrays and array
;;;; manipulation. 

(defun my-second (list)
  "Returns second element in a list"
  ;; The second element is the first of the rest of the list.
  (first(rest list)))

(defun my-nth1 (list index)
  "Returns the index-th element of list starting at 1"
  ;; If input is invalid, return nil
  (cond ((null list) nil)
        ((< index 1) nil)
  ;; If the index is 1, return the first element
        ((= index 1) (first list))
	;((null list) 'error)
  ;; Else, return the nth element of the rest of the list
	(t (my-nth1 (rest list) (- index 1)))))

(defun my-nth (list index)
  "Returns the index-th element of list starting at 0"
  (cond 
  ;; If list is invalid, return 0
    ((null list) nil)
  ;; If the index is less than 0, return nil.
    ((< index 0) nil)
  ;; If the index is 0, return the first element
    ((= index 0) (first list))
  ;; Else, return the nth element of the rest of the list
    (t (my-nth (rest list) (- index 1)))))

(defun my-last (l1)
  "Return the last item in the list"
  (cond
  ;; If the length of the list is 1, return the first element.
    ((equal (my-length l1) 1)
      (first l1))
  ;; Else, return the last of the rest of the list.
    (t (my-last (rest l1)))))

(defun my-make-array (l1)
  "Make an array of the dimensions specified in l1"
  (cond
  ;; Return 0 if l1 is null
    ((null l1) 0)
  ;; Return a one-d array of the first item if rest is null
    ((null (rest l1)) (make-oned-array (first l1)))
  ;; Return a one-d array wrapped in a list if the first of l1 is 1
    ((equal (first l1) 1)
      (list (my-make-array (rest l1))))
  ;; Recurse to construct an array of the specified dimensions.
    (t
      (cons (my-make-array (rest l1)) 
        (my-make-array (cons (- (first l1) 1) (rest l1)))))))

(defun my-array-dimension (my-array nth-dimension)
  "Return the nth dimension of the array"
  (cond 
    ;((null my-array) -1)
    ;; Return nil if nth-dimension is invalid
    ((< nth-dimension 0) nil)
    ;; If the index is 0, return the lenth of the array
    ((equal nth-dimension 0)
      (length my-array))
    (t
    ;; Recurse to find the dimension.
      (my-array-dimension (first my-array) (- nth-dimension 1)))))

(defun my-set-aref (my-array item indexes)
  "Return a copy of my-array in which my-array[i,j] is set to value, i
   being the first element in indexes and j being the second."
  (cond
  ;; If the first index is 0, extract the row and input the item into it.
    ((equal (first indexes) 1)
      (cons (replace-nth-in-1d (my-second indexes) 
        (first my-array) item) (rest my-array)))
  ;; Else, recurse down to find the right row.
    (t
      (cons (first my-array) (my-set-aref (rest my-array)
        item (cons (- (first indexes) 1) (rest indexes)))))))

(defun replace-nth-in-1d (index arr item)
  "Return a copy of arr in which arr[index] is set to item."
  (cond
  ;; If the index is 1, replace the first item.
    ((equal index 1) 
      (cons item (rest arr)))
  ;; Else, recurse to find the right place to insert the item into
  ;; and construct the new array.
    (t
      (cons (first arr) 
        (replace-nth-in-1d (- index 1) (rest arr) item)))))

(defun my-aref (my-array indexes)
  "Returns my-array[row,column] assuming my-array is 
  a 2-dimensional array and indexes is a list of the
  form '(row column)"
  ;; Extract the row using my-nth1 and then extract the right
  ;; column using my-nth into that column.
  (my-nth1 (my-nth1 my-array (first indexes)) (my-second indexes)))

(defun make-oned-array (a1)
  "Make a 1d array of with a1 elements"
  (cond
  ;; If a1 is 0, return nil
    ((equal 0 a1) nil)
  ;; If a1 is 1, return a list of 0
    ((equal a1  1) (list 0))
  ;; Else, construct 0 onto a list a1 times.
    (t (cons 0 (make-oned-array (- a1 1))))))

