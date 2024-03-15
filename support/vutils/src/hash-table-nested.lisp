(in-package #:vutils)

(defun ensure-nested-hash-table (table tests keys)
  "Walk down the nested hash table `TABLE` ensuring that we have the correct
number of hash tables made (one less than the list of keys `KEYS`) with the
correct tests, received from the list of tests `TESTS`. NOTE: The first element
of `TESTS` is the test function for `TABLE` itself."
  ;; TODO: This looks painful for performance. Oh well, we'll see if the
  ;; profiler actually cares or not. It is likely that these won't be nested
  ;; deeply. Also, the algorithm is slightly painful, but the deal is that we
  ;; can't make a hash table containing the last key, since the last key is
  ;; where we'll either look something up or store it.
  (loop :with key-count = (length keys)
        :with current = table
        :for test :in (cdr tests)
        :for key :in keys
        :for i :below key-count
        :for last = (= i (1- key-count))
        :do (unless (nth-value 1 (href current key))
              ;; If the key doesn't exist, we make a new hash table and store it
              ;; at the key unless it is the last entry, in which case we do
              ;; nothing.
              (unless last
                (setf (href current key)
                      (dict test))))
            ;; The key is potentially newly minted.
            (setf current (href current key)))
  table)

(defmacro make-nested-dict (test-func &rest keys)
  `(dict ,test-func ,@(mapcan (lambda (x) `(,x (dict ,test-func))) (copy-seq keys))))
