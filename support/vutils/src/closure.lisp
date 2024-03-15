(in-package #:vutils)

;;; The following is an implementation of "Pandoric Macros", documented by Doug
;;; Hoyte in "Let Over Lambda". Some minor improvements and style changes are
;;; applied to the reference version.

(defun plet/get (args)
  `(case symbol
     ,@(mapcar
        (lambda (x)
          `(,(car x) ,(car x)))
        args)
     (t (error "Unknown pandoric getter: ~a." symbol))))

(defun plet/set (args)
  `(case symbol
     ,@(mapcar
        (lambda (x)
          `(,(car x) (setf ,(car x) value)))
        args)
     (t (error "Unknown pandoric setter: ~a." symbol))))

(defun pget (box symbol)
  (funcall box :getter symbol))

(defsetf pget (box symbol) (value)
  `(progn
     (funcall ,box :setter ,symbol ,value)
     ,value))

(defmacro dlambda (&body ds)
  (alexandria:with-gensyms (args)
    `(lambda (&rest ,args)
       (case (car ,args)
         ,@(mapcar
            (lambda (x)
              (destructuring-bind (name . rest) x
                `(,name
                  (apply
                   (lambda ,@rest)
                   ,(if (eq name t)
                        args
                        `(cdr ,args))))))
            ds)))))

(defmacro with-pvars (symbols box &body body)
  (alexandria:once-only (box)
    `(symbol-macrolet (,@(mapcar
                           (lambda (x)
                             `(,x (pget ,box ',x)))
                           symbols))
       ,@body)))

(defmacro plambda (args exports &body body)
  (let ((exports (mapcar #'list exports)))
    `(let (this self)
       (setf this (lambda ,args ,@body)
             self (dlambda
                    (:getter (symbol) ,(plet/get exports))
                    (:setter (symbol value) ,(plet/set exports))
                    (t (&rest args) (apply this args)))))))

(defmacro define-pfun (name args &body body)
  (multiple-value-bind (body decls doc)
      (alexandria:parse-body body :documentation t)
    `(defun ,name (self)
       ,@(when doc `(,doc))
       ,@decls
       ,(if args
            `(with-pvars ,args self ,@body)
            `(progn ,@body)))))
