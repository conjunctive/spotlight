(defpackage #:spotlight
  (:nicknames #:spotlight #:sl)
  (:use #:cl)
  (:import-from #:alexandria
                #:compose
                #:copy-hash-table)
  (:export #:lens
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
           #:key))

(in-package #:spotlight)

(defun remove-from-alist (k xs)
  "Remove entry in association list matching the provided key."
  (remove-if (lambda (x) (equal (car x) k))
             xs))

(defun lens (getter &optional setter)
  "Provide a lens derived from a getter and setter function.
The getter function focuses on a value in the provided state.
The setter function applies a function to the provided state."
  (if setter
      (lambda (next)
        (lambda (state &optional fn)
          (if fn
              (funcall setter state (lambda (x) (funcall next x fn)))
              (funcall next (funcall getter state)))))
      (lambda (next)
        (lambda (state &optional fn)
          (if fn
              (error "Read only lens!")
              (funcall next (funcall getter state)))))))

(defun id-setter (state fn)
  "Apply a function to the state."
  (funcall fn state))

(defun const-setter (state _)
  "Provide the state unaltered."
  (declare (ignore _))
  state)

(defvar id
  (lens #'identity #'id-setter)
  "An identity lens.")

(defun focus (lens state)
  "Provide the value focused on by the lens."
  (let ((getter (funcall lens #'identity)))
    (funcall getter state)))

(defun over (lens fn state)
  "Apply a function over the value focused on by the lens."
  (let ((setter (funcall lens #'id-setter)))
    (funcall setter state fn)))

(defun put (lens v state)
  "Replace the value focused on by the lens."
  (over lens (constantly v) state))

(defun units (to from)
  "Provide a lens focusing on a converted value,
where the to function is A -> B,
and the from function is B -> A."
  (lens to
        (lambda (state fn)
          (funcall from
                   (funcall fn
                            (funcall to state))))))

(defun passes (applies-p)
  "Provide a lens focusing on a value only if it passes the given predicate."
  (lens (lambda (state)
          (when (funcall applies-p state)
            state))
        (lambda (state fn)
          (if (funcall applies-p state)
              (funcall fn state)
              state))))

(defun nth! (idx)
  "Provide a lens focusing on the given index of a sequence."
  (lens (lambda (state)
          (elt state idx))
        (lambda (state fn)
          (prog1 state
            (setf (elt state idx)
                  (funcall fn (elt state idx)))))))

(defun nth- (idx)
  "Provide a lens focusing on the given index of a sequence.
Immutable variant of nth! function (uses copy-seq)."
  (lens (lambda (state)
          (elt state idx))
        (lambda (state fn)
          (let ((xs (copy-seq state)))
            (prog1 xs
              (setf (elt xs idx)
                    (funcall fn (elt xs idx))))))))

(defvar fst! (nth! 0)
  "A lens focusing on the first element of a sequence (mutable).")

(defvar snd! (nth! 1)
  "A lens focusing on the second element of a sequence (mutable).")

(defvar fst (nth- 0)
  "A lens focusing on the first element of a sequence (immutable).")

(defvar snd (nth- 1)
  "A lens focusing on the second element of a sequence (immutable).")

(defun sdr (xs)
  "Access the tail of a sequence."
  (subseq xs 1))

(defvar tail
  (lens #'sdr
        (lambda (state fn)
          (funcall fn (sdr state))))
  "A lens focusing on the tail of a sequence.")

(defun hash-table-getter (k)
  "Provide a getter function for accessing hash-tables."
  (lambda (state)
    (gethash k state)))

(defun hash-table-setter! (k)
  "Provide a setter function for mutating a value
in a hash-table under a given key."
  (lambda (state fn)
    (prog1 state
      (setf (gethash k state)
            (funcall fn (gethash k state))))))

(defun hash-table-setter (k)
  "Provide a setter function for updating a value
in a hash-table under a given key (uses copy-hash-table)."
  (lambda (state fn)
    (let ((ht (copy-hash-table state)))
      (prog1 ht
        (setf (gethash k ht)
              (funcall fn (gethash k ht)))))))

(defun alist-getter (k)
  "Provide a getter function for accessing association lists."
  (lambda (state)
    (cdr (assoc k state))))

(defun alist-setter! (k)
  "Provide a setter function for mutating a value
in an association list under a given key."
  (lambda (state fn)
    (prog1 state
      (rplacd (assoc k state)
              (funcall fn (cdr (assoc k state)))))))

(defun alist-setter (k)
  "Provide a setter function for updating a value
in an association list under a given key."
  (lambda (state fn)
    (acons k
           (funcall fn (cdr (assoc k state)))
           (remove-from-alist k state))))

(defun plist-getter (k)
  "Provide a getter function for accessing property lists."
  (lambda (state)
    (getf state k)))

(defun plist-setter (k)
  "Provide a setter function for updating a value
in a property list under a given key."
  (lambda (state fn)
    (loop :for (key . rest)
            :on state :by #'cddr
          :if (equal key k)
            :collect key
            :and :collect (funcall fn (first rest))
          :else
            :collect key
            :and :collect (first rest))))

(defun plist-setter! (k)
  "Provide a setter function for mutating a value
in a property list under a given key."
  (lambda (state fn)
    (prog1 state
      (setf (getf state k)
            (funcall fn (getf state k))))))

(defun key-hash-table! (k)
  "Provide a lens focusing on the given key of a hash-table.
Mutable variant of key-hash-table function (uses setf)."
  (lens (hash-table-getter k)
        (hash-table-setter! k)))

(defun key-hash-table (k)
  "Provide a lens focusing on the given key of a hash-table.
Immutable variant of key-hash-table! function (uses copy-hash-table)."
  (lens (hash-table-getter k)
        (hash-table-setter k)))

(defun key-alist! (k)
  "Provide a lens focusing on the given key of an association list.
Mutable variant of key-hash-table function (uses rplacd)."
  (lens (alist-getter k)
        (alist-setter! k)))

(defun key-alist (k)
  "Provide a lens focusing on the given key of an association list.
Immutable variant of key-alist! function (uses acons & remove-if)."
  (lens (alist-getter k)
        (alist-setter k)))

(defun key-plist! (k)
  "Provide a lens focusing on the given key of a property list.
Mutable variant of key-plist function (uses setf)."
  (lens (plist-getter k)
        (plist-setter! k)))

(defun key-plist (k)
  "Provide a lens focusing on the given key of a property list.
Immutable variant of key-plist! function (uses loop)."
  (lens (plist-getter k)
        (plist-setter k)))

(defun key! (k &optional (type :hash-table))
  "Provide a lens focusing on the given key of either
a hash-table, association list, or property list.
Mutable variant of key function."
  (case type
    (:hash-table (key-hash-table! k))
    (:alist (key-alist! k))
    (:plist (key-plist! k))))

(defun key (k &optional (type :hash-table))
  "Provide a lens focusing on the given key of either
a hash-table, association list, or property list.
Immutable variant of key! function."
  (case type
    (:hash-table (key-hash-table k))
    (:alist (key-alist k))
    (:plist (key-plist k))))
