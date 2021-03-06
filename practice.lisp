; How to use on command line:
; clisp
; (load "practice.lisp")
; (my-length '(1 2 3 4))


(defun my-second (list)
  "Returns second element in a list"
  (first(rest list)))

(defun add-3-nums (a b c)
  "Returs sum of a, b, c"
  (+ a b c))

; 
(defun my-length (list1)
  "Return length"
  (cond ((null list1) 0)
	(t (+ 1 (my-length(rest list1))))))

(defun my-nth1 (list index)
  "Returns the index-th element of list"
  (cond ((= index 1) (first list))
	;((null list) 'error)
	(t (my-nth1 (rest list) (- index 1)))))

(defun my-nth (list index)
  "Returns the index-th element of list"
  (cond ((= index 0) (first list))
	;((null list) 'error)
	(t (my-nth (rest list) (- index 1)))))

(defun my-append (list1 list2)
  "Appends list1 and list2"
  ; If list 1 is null, return list2
  (cond
        ((null list1) list2)
  ; else, if  list2 is null, return list1
        ((null list2) list1)
  ; else, return appended
	(t 
	   (cons 
	      (first list1) 
	      (my-append (rest list1) list2)
	   )
	)
  )
)


(defun my-reverse (list1)
  "Reverse list1"
  ; The reverse of the empty list is the empty list
  (cond
    ((null list1) list1)
    (t 
	(my-append
	  (my-reverse (rest list1))
	  (list (first list1))
	)
    )  
  )
)

(defun my-last (l1)
  "Return the last item in the list"
  (cond
    ((equal (my-length l1) 1)
      (first l1)
    )

    (t
      (my-last (rest l1))
    )
  )
)

(defun my-member (a1 l1)
  "Return true if a1 is a member of l1
   and nil if it is not."
  (cond
    ((null l1) nil)
    ((equal a1 (first l1)) t)
    (t 
      (my-member a1 (rest l1))
    )
  )
)

(defun my-assoc (key l1)
  "If l1 is a list of key-value pairs,
   return the pair that has the key"
   ; If the key is in the first key-value pair
   (cond 
     ((null l1)
       nil
     )
     ((equal key (first (first l1)))
       (first l1)
     )
     (t
       (my-assoc key (rest l1))
     )
   
   )
)

(defun flatten (l1)
  (cond
    ((null l1)
      nil
    )
    ; If the first thing of l1 is an atom
    ((atom (first l1))
      ; Return a list of just that item
      (list (first l1))
    )
    (t
      ()
    )
  )
)

(defun my-make-array (l1)
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
      ;(my-make-array-auxl l1)
      ;(construct-array l1)
      ;(setq oned (make-oned-array (my-last l1)))
      ;(setq nlist (make-n-lists (my-last (butlast l1)) oned))
      ;(setq myrest (my-make-array (butlast l1)))
      ;myrest

      ;(cons (my-make-array (rest l1)))
      ;(list (cons 0 (my-make-array (cons (- 1 (first l1))(rest l1)))))
    )
  )
)

(defun my-array-dimension (A n)
  "Return the nth dimension of the array A"
  (my-nth1 A n)
)

(defun my-set-aref (arr item i j)
  "Return a copy of A in which A[i,j] is set to value"
  (cond
    ((equal i 1)
      (cons (replace-nth-in-1d j (first arr) item) (rest arr))
    )
    (t
      (cons (first arr) (my-set-aref (rest arr) item (- i 1) j))
    )
  )

)

(defun replace-nth-in-1d (index arr item)
  (cond
    ((equal index 1) 
      (cons item (rest arr)))
    (t
      (cons (first arr) (replace-nth-in-1d (- index 1) (rest arr) item))
    )
  )
)

(defun my-aref (A i j)
  "Returns A[i,j] assuming A is a 2-dimensional array"
  (setq dim (my-nth1 A i))
  (my-nth1 dim j)
)

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

(defun pop-last (l1)
  ()
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

;(defun my-make-array (l1)
;  "Make an array"
;  (cond
;    ((null l1) nil)
;    ((equal 1 (length l1)) (make-oned-array (first l1)))
;    ((equal 0 (first l1)) (list (my-make-array (rest l1))))
;    (t 
;      (my-make-array (cons (- (first l1) 1) (rest l1)))
;    )
;  )
;)

(defun make-n-oned-arrays (a1 a2)
  "Make a1 one-d arrays with a2 elements"
  (cond
    ((equal 1 a1) (make-oned-array a2))
    (t
      (cons (make-oned-array a2) (make-n-oned-arrays (- a1 1) a2))
    )
  )
)

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

