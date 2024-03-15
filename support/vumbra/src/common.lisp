(in-package #:vumbra.common)

(defmacro mvlet* ((&rest bindings) &body body)
  (destructuring-bind (&optional car . cdr) bindings
    (typecase car
      (null
       `(progn ,@body))
      (list
       (case (length car)
         (0 (error "Missing variable in binding list."))
         ((1 2) `(let (,car) (mvlet* ,cdr ,@body)))
         (t `(multiple-value-bind ,(butlast car) ,(car (last car))
               (mvlet* ,cdr ,@body)))))
      (symbol
       `(mvlet* ,cdr ,@body)))))
