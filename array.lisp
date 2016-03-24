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
  ;; If the index is 1, return the first element
  (cond ((= index 1) (first list))
	;((null list) 'error)
  ;; Else, return the nth element of the rest of the list
	(t (my-nth1 (rest list) (- index 1)))))

(defun my-nth (list index)
  "Returns the index-th element of list starting at 0"
  ;; If the index is 1, return the first element
  (cond ((= index 0) (first list))
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

(defun my-assoc (key l1)
  "If l1 is a list of key-value pairs,
   return the pair that has the key"
   (cond 
   ;; If the key is in the first key-value pair
     ((null l1) nil)
   ;; If the key is equal to the first of the first of l1, 
   ;; return the first
     ((equal key (first (first l1)))
       (first l1))
     (t (my-assoc key (rest l1)))))

(defun my-make-array (l1)
  "Make an array of the dimensions specified in l1"
  (cond
    ((null l1) 0)
    ((null (rest l1)) (make-oned-array (first l1)))
    (t
      (setq l2 (reverse l1))
      (setq retval (make-oned-array (first l2)))
      (setq l2 (rest l2))
      (loop for x in l2 do
          (setq retval (make-n-lists x retval))
      )
      retval
    )
  )
)

(defun my-array-dimension (a n)
  "Return the nth dimension of the array a"
  (cond 
    ;((null a) -1)
    ;((<= 0 n) (length a))
    ; If the index is 0, return the lenth of the array
    ((equal n 0)
      (length a)
    )
    (t
      ;(first (my-array-dimension a (- n 1)))
      (my-array-dimension (first a) (- n 1))
      ;(length (my-nth a ))
      ;(my-array-dimension a (- n 1))
    )
  )
)

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

(defun my-make-array-auxl (l1 retval)
 (cond
   ((null l1) nil)
   ((null (rest l1)) )
   (t
     ;(my-make-array-auxl (butlast l1) retval)
     (make-n-lists (my-last l1) retval)
   )
 )
)

(defun construct-array (l1)
  (setq l2 (make-n-lists
  	(my-last (butlast l1)) (make-oned-array (my-last l1))))

  (cond
    ((null l1) nil)
    ((equal (length l1) 2) l2)
    (t
      (cons l2 (construct-array (rest l1)))
    )
  )
)

(defun make-n-lists (a1 l1)
  "Return a list containing a1 lists of l1"
  (cond
    ((equal 0 a1) nil)
    ((equal 1 a1) l1)
    (t
      (cons l1 (make-n-lists-aux (- a1 1) l1 (list l1)))
    )
  )
)

(defun make-n-lists-aux (a1 l1 l2)
  "lllll"
  (cond
    ((equal 0 a1) nil)
    ((equal 1 a1) l2)
    (t
      (cons l1 (make-n-lists-aux (- a1 1) l1 l2))
    )
  )
)


(defun make-n-oned-arrays (a1 a2)
  "Make a1 one-d arrays with a2 elements"
  (cond
  ;; If the 
    ((equal 1 a1) (make-oned-array a2))
    (t
      (cons (make-oned-array a2) (make-n-oned-arrays (- a1 1) a2)))))

(defun my-make-array-aux (l1)
  "Make array aux"
  (cond
    ((null l1) nil)
    ()
  )
)

(defun make-oned-array (a1)
  "Make a 1d array of with a1 elements"
  (cond
    ((equal 0 a1) nil)
    ((equal a1  1) (list 0))
    (t (cons 0 (make-oned-array (- a1 1))))
  )
)

