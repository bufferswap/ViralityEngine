;;;; Property lists
;;;; Various functions dealing with property lists.

(in-package #:vutils)

(deftype plist () '(satisfies plist-p))

(defun plist-p (item)
  "Check whether or not `ITEM` is a property list."
  (and (listp item)
       (evenp (length item))))

(defun plist-get (plist key)
  "Get the value associated with `KEY` in `PLIST`."
  (getf plist key))

(defun plist-remove (plist &rest keys)
  "Remove all `KEYS` and their associated values from `PLIST`. Non-destructive."
  (if (plist-p plist)
      (loop :for (key value) :on plist :by #'cddr
            :unless (member key keys :test #'eq)
              :append (list key value))
      (error "~a is not a property list." plist)))

(define-modify-macro plist-removef (&rest keys) plist-remove
  "Place-modifying macro for PLIST-REMOVE.")

(defun plist->alist (plist)
  "Convert `PLIST` to an association list."
  (if (plist-p plist)
      (loop :for (key value) :on plist :by #'cddr
            :collect (cons key value))
      (error "~a is not a property list." plist)))

(defun plist->hash (plist &rest args)
  "Convert `PLIST` to a hash table."
  (if (plist-p plist)
      (let ((table (apply #'make-hash-table args)))
        (loop :for (key value) :on plist :by #'cddr
              :do (setf (gethash key table) value))
        table)
      (error "~a is not a property list." plist)))

(defmacro do-plist ((key value plist) &body body)
  "Iterate over the property list, `PLIST`, binding each key and value to `KEY`
and `VALUE` respectively, performing `BODY`."
  `(loop :for (,key ,value) :on ,plist :by #'cddr
         :do ,@body))

(defmacro do-plist-keys ((key plist) &body body)
  "Iterate over the property list, `PLIST`, binding each key to `KEY`,
performing `BODY`."
  (alexandria:with-gensyms (value)
    `(do-plist (,key ,value ,plist)
       ,@body)))

(defmacro do-plist-values ((value plist) &body body)
  "Iterate over the property list, `PLIST`, binding each value to `VALUE`,
performing `BODY`."
  (alexandria:with-gensyms (key)
    `(do-plist (,key ,value ,plist)
       ,@body)))

(defun map-plist (fn plist)
  "Map over the property list, `PLIST` applying `FN`, a function that takes 2
arguments, for the key and value of each iteration."
  (do-plist (key value plist)
    (funcall fn key value)))

(defun map-plist-keys (fn plist)
  "Map over the property list, `PLIST` applying `FN`, a function that takes 1
arguments for the key of each iteration."
  (do-plist (key value plist)
    (funcall fn key)))

(defun map-plist-values (fn plist)
  "Map over the property list, `PLIST` applying `FN`, a function that takes 1
arguments for the value of each iteration."
  (do-plist (key value plist)
    (funcall fn value)))
