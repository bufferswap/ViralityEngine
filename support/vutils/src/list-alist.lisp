;;;; Association lists.
;;;; Various functions dealing with association lists.

(in-package #:vutils)

(deftype alist () '(satisfies alist-p))

(defun alist-p (item)
  "Check whether or not `ITEM` is an association list."
  (and (listp item)
       (every #'consp item)))

(defun alist-get (alist key &rest args)
  "Get the value associated with `KEY` in `ALIST`."
  (let ((cell (apply #'assoc key alist args)))
    (values (cdr cell) cell)))

(defun alist-rget (alist value &rest args)
  "Get the key associated with `VALUE` in `ALIST`."
  (let ((cell (apply #'rassoc value alist args)))
    (values (car cell) cell)))

(defun alist-remove (alist &rest keys)
  "Remove all `KEYS` and their associated values from `ALIST`. Non-destructive."
  (remove-if
   (lambda (x)
     (find (car x) keys :test #'eq))
   alist))

(define-modify-macro alist-removef (&rest keys) alist-remove
  "Place-modifying macro for ALIST-REMOVE.")

(defun alist-keys (alist)
  "Get a list of all keys in `ALIST`."
  (mapcar #'car alist))

(defun alist-values (alist)
  "Get a list of all values in `ALIST`."
  (mapcar #'cdr alist))

(defun alist->plist (alist)
  "Convert `ALIST` to a property list. A property list in this context has
keyword symbols for its keys."
  (mapcan
   (lambda (x)
     (list (alexandria:make-keyword (car x)) (cdr x)))
   (copy-list alist)))

(defun alist->hash (alist &rest args)
  "Convert `ALIST` to a hash table."
  (let ((table (apply #'make-hash-table args)))
    (dolist (cell alist)
      (setf (gethash (car cell) table) (cdr cell)))
    table))

(defmacro do-alist ((key value alist) &body body)
  "Iterate over the association list, `ALIST`, binding each key and value to
`KEY` and `VALUE` respectively, performing `BODY`."
  `(loop :for (,key . ,value) :in ,alist
         :do ,@body))

(defmacro do-alist-keys ((key alist) &body body)
  "Iterate over the association list, `ALIST`, binding each key to `KEY`,
performing `BODY`."
  (alexandria:with-gensyms (value)
    `(do-alist (,key ,value ,alist)
       ,@body)))

(defmacro do-alist-values ((value alist) &body body)
  "Iterate over the association list, `ALIST`, binding each value to `VALUE`,
performing `BODY`."
  (alexandria:with-gensyms (key)
    `(do-alist (,key ,value ,alist)
       ,@body)))

(defun map-alist (fn alist)
  "Map over the association list, `ALIST` applying `FN`, a function that takes 2
arguments, for the key and value of each iteration."
  (do-alist (key value alist)
    (funcall fn key value)))

(defun map-alist-keys (fn alist)
  "Map over the association list, `ALIST` applying `FN`, a function that takes 1
arguments for the key of each iteration."
  (do-alist (key value alist)
    (funcall fn key)))

(defun map-alist-values (fn alist)
  "Map over the association list, `ALIST` applying `FN`, a function that takes 1
arguments for the value of each iteration."
  (do-alist (key value alist)
    (funcall fn value)))
