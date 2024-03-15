(in-package #:vutils)

(defun flatten-tree (sequence)
  "Traverses a sequence in order, collecting non-nil values into a list. This is
different than Alexandria's version in that this also works for vectors or
hybrid sequences."
  (let (list)
    (labels ((traverse (sub-tree)
               (when sub-tree
                 (typecase sub-tree
                   (cons
                    (traverse (car sub-tree))
                    (traverse (cdr sub-tree)))
                   (vector
                    (map nil #'traverse sub-tree))
                   (t (push sub-tree list))))))
      (traverse sequence))
    (nreverse list)))

(defun flatten-numbers (sequence &key (type 'single-float))
  "Like FLATTEN, except only keeps real numbers and arranges for the result to
be a specialized array of element-type `TYPE`."
  (flet ((%coerce (sequence)
           (mapcar (lambda (x) (coerce x type))
                   (remove-if (complement #'realp) (flatten-tree sequence)))))
    (let ((sequence (%coerce sequence)))
      (make-array (length sequence)
                  :element-type type
                  :initial-contents sequence))))

(defun enumerate (sequence &key (start 0) (step 1) (key #'identity))
  "Return an alist with each car being a number determined by sequentially
incrementing from `START` by `STEP`, and each cdr being the element of
`SEQUENCE` applied to the function `KEY`."
  (loop :for item :in sequence
        :for i :from start :by step
        :for value = (funcall key item)
        :collect (cons i value)))

(defmacro do-seq ((var sequence) &body body)
  "Iterates over `SEQUENCE`, binding `VAR` to each element. Like DOLIST, but for
all sequence types."
  `(map nil (lambda (,var) ,@body) ,sequence))

(defun find-all (item sequence &rest args &key (test #'eql) test-not &allow-other-keys)
  "Find all elements in `SEQUENCE` that match `ITEM`. Taken from PAIP."
  (if test-not
      (apply #'remove item sequence :test-not (complement test-not) args)
      (apply #'remove item sequence :test (complement test) args)))

(defun copy-sequence-tree (object)
  "Perform a pseudo-deep copy of `OBJECT`. If `OBJECT` is a value type such as a
number or symbol, returns that object as-is. If `OBJECT` is a sequence such as a
list, string, or array, recursively copies the elements of that sequence,
returning a new sequence without shared structure. Note, that this does not
perform a full deep copy, as that is difficult to generalize and we do not need
that functionality. This will only recurse until a non-sequence is hit. This
means a sequence that has an instance of a structure-object, standard-object,
hash-table, or other non-sequence will be a terminal condition for that
recursion, and be copied by reference."
  (if (typep object 'sequence)
      (map-into (copy-seq object) #'copy-sequence-tree object)
      object))
