(defpackage #:spotlight-test
  (:use #:cl
        #:prove)
  (:import-from #:alexandria
                #:compose
                #:copy-hash-table
                #:alist-hash-table
                #:hash-table-alist)
  (:import-from #:spotlight
                #:lens
                #:id-setter
                #:const-setter
                #:id
                #:focus
                #:over
                #:put
                #:units
                #:passes
                #:nth!
                #:nth-
                #:fst!
                #:fst
                #:snd!
                #:snd
                #:tail
                #:key!
                #:key
                #:key?!
                #:key?))

(in-package :spotlight-test)

(setf *default-reporter* :tap)

(plan nil)

(subtest "Focus on identity"
  (let ((xs (list 1 2 3 4)))
    (is (focus id xs) xs)))

(subtest "Return the list unaltered"
  (let ((xs (list 1 2 3 4))
        (fn (lambda (x) (+ x 1)))
        (l  (compose (lens #'identity #'const-setter) (nth! 0))))
    (is (over l fn xs) xs)
    (is xs (list 1 2 3 4))))

(subtest "Focus only on elements that pass the predicate"
  (let ((odd (passes #'oddp)))
    (ok (not (focus odd 2)))
    (is 3 (focus odd 3))))

(subtest "Mutably replace third element of list with 12"
  (let ((xs (list 10 11 13)))
    (is (put (nth! 2) 12 xs)
        (list 10 11 12))))

(subtest "Immutably replace third element of list with 12"
  (let ((xs (list 10 11 13)))
    (is (put (nth- 2) 12 xs)
        (list 10 11 12))
    (is xs (list 10 11 13))))

(subtest "Mutably apply increment function over second value in list"
  (let ((xs (list 1 2 3 4))
        (fn (lambda (x) (+ x 1)))
        (l  (nth! 1)))
    (is (over l fn xs)
        (list 1 3 3 4))))

(subtest "Mutably apply increment function over second value in vector"
  (let ((xs #(1 2 3 4))
        (fn (lambda (x) (+ x 1))))
    (is (over (nth! 1) fn xs)
        #(1 3 3 4)
        :test #'equalp)))

(subtest "Immutably apply increment function over second value in list"
  (let ((xs (list 1 2 3 4))
        (fn (lambda (x) (+ x 1))))
    (is (over (nth- 1) fn xs)
        (list 1 3 3 4))
    (is xs (list 1 2 3 4))))

(subtest "Immutably apply increment function over second value in vector"
  (let ((xs #(1 2 3 4))
        (fn (lambda (x) (+ x 1))))
    (is (over (nth- 1) fn xs)
        #(1 3 3 4)
        :test #'equalp)
    (is xs
        #(1 2 3 4)
        :test #'equalp)))

(subtest "Immutably apply increment function over tail of list"
  (let* ((xs  (list 1 2 3 4))
         (inc (lambda (x) (+ x 1)))
         (fn  (lambda (xs) (mapcar inc xs))))
    (is (over tail fn xs)
        (list 3 4 5))
    (is xs (list 1 2 3 4))))

(subtest "Focus only on tail of list"
  (let* ((xs #(1 2 3 4)))
   (is (focus tail xs)
       #(2 3 4)
       :test #'equalp)))

(subtest "Focus on tail of second & third list, mutably decrement second value of third list"
  (let ((xs (list 1 (list 1 2 3 (list 11 22 33))))
        (fn (lambda (x) (- x 1)))
        (l  (compose snd! tail (nth! 2) tail snd!)))
   (is (over l fn xs)
       (list 1 (list 2 3 (list 22 32))))))

(subtest "Focus on tail of second & third list, immutably decrement second value of third list"
  (let ((xs (list 1 (list 1 2 3 (list 11 22 33))))
        (fn (lambda (x) (- x 1)))
        (l  (compose snd tail (nth- 2) tail snd)))
    (is (over l fn xs)
        (list 1 (list 2 3 (list 22 32))))
    (is xs (list 1 (list 1 2 3 (list 11 22 33))))))

(subtest "Mutably apply increment function over value under :A key in association list"
  (let* ((xs (list (cons :A 0) (cons :B 2) (cons :C 3)))
         (fn (lambda (x) (+ x 1)))
         (l  (key! :A :alist)))
    (over l fn xs)
    (is 1 (focus l xs))))

(subtest "Mutably apply increment function over value under :A key in property list"
  (let* ((xs (list :A 0 :B 2 :C 3))
         (fn (lambda (x) (+ x 1)))
         (l  (key! :A :plist)))
    (over l fn xs)
    (is 1 (focus l xs))))

(subtest "Immutably apply increment function over value under :A key in association list"
  (let* ((xs '((:A . 0) (:B . 2) (:C . 3)))
         (fn (lambda (x) (+ x 1)))
         (l  (key :A :alist)))
    (is 1 (focus l (over l fn xs)))))

(subtest "Immutably apply increment function over value under :A key in property list"
  (let* ((xs '(:A 0 :B 2 :C 3))
         (fn (lambda (x) (+ x 1)))
         (l  (key :A :plist)))
    (is 1 (focus l (over l fn xs)))))

(subtest "Mutably apply decrement function over third value in list, under :A key in hash-table"
  (let* ((ht (make-hash-table))
         (xs (list 24 25 27))
         (fn (lambda (x) (- x 1)))
         (l  (compose (key! :A) (nth! 2))))
    (setf (gethash :A ht) xs)
    (over l fn ht)
    (is (focus (key :A) ht)
        (list 24 25 26))))

(subtest "Mutably replace head of list with 4, traversing two hash-tables"
  (let ((htx (make-hash-table))
        (hty (make-hash-table))
        (xs  (list 1 2 3))
        (fn  (lambda (x) (cons 4 x)))
        (l   (compose (key! :Y) (key! :Z) tail)))
    (setf (gethash :Y htx) hty
          (gethash :Z hty) xs)
    (over l fn htx)
    (is (focus (compose (key :Y) (key :Z)) htx)
        (list 4 2 3))))

(subtest "Mutably drop head of list and decrement the third element twice, traversing two hash-tables"
  (let ((htx (make-hash-table))
        (hty (make-hash-table))
        (xs  (list 1 2 3))
        (fn  (lambda (x) (- x 2)))
        (l   (compose (key! :Y) (key! :Z) tail snd!)))
    (setf (gethash :Y htx) hty
          (gethash :Z hty) xs)
    (over l fn htx)
    (is (focus (compose (key :Y) (key :Z)) htx)
        (list 2 1))))

(subtest "Mutably increment number, traversing three hash-tables and two lists"
  (let ((htx (make-hash-table))
        (hty (make-hash-table))
        (htz (make-hash-table))
        (fn  (lambda (x) (+ x 1)))
        (l   (compose (key! :X) (key! :Y) fst! (key! :Z) fst!)))
    (setf (gethash :X htx) hty
          (gethash :Y hty) (list htz)
          (gethash :Z htz) (list 1))
    (is (focus l (over l fn htx))
        2)))

(subtest "Immutably increment number, traversing three hash-tables and two lists"
  (let ((htx (make-hash-table))
        (hty (make-hash-table))
        (htz (make-hash-table))
        (fn  (lambda (x) (+ x 1)))
        (l   (compose (key :X) (key :Y) fst (key :Z) fst)))
    (setf (gethash :X htx) hty
          (gethash :Y hty) (list htz)
          (gethash :Z htz) (list 1))
    (is (focus l (over l fn htx))
        2)
    (is (focus (compose (key :X) (key :Y) fst (key :Z)) htx)
        (list 1))))

(subtest "Mutably increment number, explicitly traversing an alist, plist, and hash-table"
  (let* ((ht (alist-hash-table (list (cons :G 7) (cons :H 7) (cons :I 9))))
         (pl (list :D 4 :E ht :F 6))
         (al (list (cons :A 1) (cons :B 2) (cons :C pl)))
         (fn (lambda (x) (+ x 1)))
         (l  (compose (key! :C :alist) (key! :E :plist) (key! :H :hash-table))))
    (over l fn al)
    (is 8 (focus l al))))

(subtest "Immutably increment number, explicitly traversing an alist, plist, and hash-table"
  (let* ((ht (alist-hash-table (list (cons :G 7) (cons :H 7) (cons :I 9))))
         (pl (list :D 4 :E ht :F 6))
         (al (list (cons :A 1) (cons :B 2) (cons :C pl)))
         (fn (lambda (x) (+ x 1)))
         (l  (compose (key :C :alist) (key :E :plist) (key :H :hash-table))))
    (is 8 (focus l (over l fn al)))
    (is 7 (focus l al))))

(subtest "Mutably increment number, implicitly traversing an alist, plist, and hash-table"
  (let* ((ht (alist-hash-table (list (cons :G 7) (cons :H 7) (cons :I 9))))
         (pl (list :D 4 :E ht :F 6))
         (al (list (cons :A 1) (cons :B 2) (cons :C pl)))
         (fn (lambda (x) (+ x 1)))
         (l  (compose (key?! :C) (key?! :E) (key?! :H))))
    (over l fn al)
    (is 8 (focus l al))))

(subtest "Immutably increment number, implicitly traversing an alist, plist, and hash-table"
  (let* ((ht (alist-hash-table (list (cons :G 7) (cons :H 7) (cons :I 9))))
         (pl (list :D 4 :E ht :F 6))
         (al (list (cons :A 1) (cons :B 2) (cons :C pl)))
         (fn (lambda (x) (+ x 1)))
         (l  (compose (key? :C) (key? :E) (key? :H))))
    (is 8 (focus l (over l fn al)))
    (is 7 (focus l al))))

(subtest "Convert an alist to a hash-table, add a new value, return an alist"
  (let ((xs '((:A . 1) (:B . 2)))
        (fn (lambda (ht) (prog1 ht (setf (gethash :C ht) 3))))
        (l  (units #'alist-hash-table #'hash-table-alist)))
    (is (over l fn xs)
        '((:C . 3) (:B . 2) (:A . 1)))))

(finalize)
