(in-package #:vutils)

(defun interleave (&rest lists)
  "Interleave the elements of `LISTS`."
  (apply #'mapcan #'list lists))

(defun combinations/repetition (n items)
  "Return a list of all combinations of `ITEMS` with with a length of `N`, and
repetitions allowed."
  (if (= n 1)
      (mapcar #'list items)
      (mapcan
       (lambda (x)
         (mapcar
          (lambda (y)
            (cons x y))
          (combinations/repetition (1- n) items)))
       items)))

(defun zip (&rest lists)
  "Zip the given `LISTS`."
  (apply #'mapcar #'list lists))

(defun tree-leaves (tree test result)
  "Replace leaves of `TREE` that satisfy `TEST` with `RESULT`.
From Let Over Lambda by Doug Hoyte."
  (when tree
    (if (listp tree)
        (cons (tree-leaves (car tree) test result)
              (tree-leaves (cdr tree) test result))
        (if (funcall test tree)
            (funcall result tree)
            tree))))
