(in-package #:vumbra.common)

(u:eval-always
  (cl:defun %swizzle/combinations (n items)
    (if (= n 1)
        (mapcar #'list items)
        (mapcan
         (lambda (x)
           (mapcar
            (lambda (y)
              (cons x y))
            (%swizzle/combinations (1- n) items)))
         items)))

  (cl:defun %swizzle/component-groups (size)
    (loop :for masks :in '((x y z w) (r g b a) (s t p q))
          :append
          (loop :with set = (subseq masks 0 size)
                :for i :from 1 :to size
                :for items = (%swizzle/combinations i set)
                :append (mapcar (lambda (x) (format nil "~{~a~}" x)) items))))

  (cl:defun %swizzle/char-position (components index)
    (let ((char (char components index)))
      (or (position char "XYZW")
          (position char "RGBA")
          (position char "STPQ"))))

  (cl:defmacro define-vari-swizzle-macros ()
    (flet ((map-swizzle (mask)
             (u:make-keyword
              (map 'string
                   (lambda (x)
                     (elt "XYZW"
                          (%swizzle/char-position mask (position x mask))))
                   mask))))
      `(progn
         ,@(loop :for mask :in (%swizzle/component-groups 4)
                 :for op = (u:symbolicate "." mask)
                 :collect `(export ',op)
                 :collect `(v-defmacro ,op (vector)
                             `(swizzle ,vector ,,(map-swizzle mask))))))))

(define-vari-swizzle-macros)
