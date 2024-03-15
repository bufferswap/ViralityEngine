(in-package #:vutils)

(defmacro do-hash ((key value table &optional result) &body body)
  "Iterates over hash table `TABLE`, binding each key to `KEY`, and its value to
`VALUE`."
  (alexandria:with-gensyms (block-name)
    (multiple-value-bind (body decls) (alexandria:parse-body body)
      `(block ,block-name
         (maphash
          (lambda (,key ,value)
            ,@decls
            ,@body)
          ,table)
         ,(when result
            `(let (,key ,value)
               ,result))))))

(defmacro do-hash-keys ((key table) &body body)
  "Iterate over hash table `TABLE`, binding each key to `KEY`."
  (alexandria:with-gensyms (value)
    `(do-hash (,key ,value ,table)
       (declare (ignore ,value))
       ,@body)))

(defmacro do-hash-values ((value table) &body body)
  "Iterate over hash table `TABLE`, binding each value to `VALUE`."
  (alexandria:with-gensyms (key)
    `(do-hash (,key ,value ,table)
       (declare (ignore ,key))
       ,@body)))

(defun hash->alist (table)
  "Convert the keys and values of the hash table `TABLE` to an association
list."
  (let (result)
    (do-hash (key value table)
      (push (cons key value) result))
    result))

(defun hash->plist (table)
  "Convert the keys and values of the hash table `TABLE` to a property list."
  (let (result)
    (do-hash (key value table)
      (setf result (list* key value result)))
    result))

(defun hash-keys (table)
  "Collect a list of all keys in the hash table `TABLE`."
  (let (keys)
    (alexandria:maphash-keys
     (lambda (x)
       (push x keys))
     table)
    (nreverse keys)))

(defun hash-values (table)
  "Collect a list of all values in the hash table `TABLE."
  (let (values)
    (alexandria:maphash-values
     (lambda (x)
       (push x values))
     table)
    (nreverse values)))

(defun hash-merge (table &rest tables)
  (let ((size (reduce #'+ tables
                      :key #'hash-table-count
                      :initial-value (hash-table-count table))))
    (reduce
     (lambda (table1 table2)
       (unless (eql (hash-table-test table1)
                    (hash-table-test table2))
         (error "Hash tables must have the same test function."))
       (do-hash (k v table2)
         (setf (gethash k table1) v))
       table1)
     tables
     :initial-value (alexandria:copy-hash-table table :size size))))

(defun dict (&rest keys/values)
  (let* ((length (length keys/values))
         (test (if (oddp length)
                   (pop keys/values)
                   #'eq)))
    (alexandria:plist-hash-table keys/values
                                 :test test
                                 :size (truncate length 2))))

(defun expand-href (table keys)
  (reduce
   (lambda (table key)
     `(gethash ,key ,table))
   keys
   :initial-value table))

(defun href (table &rest keys)
  (loop :for (key . rest) :on keys
        :unless rest
          :return (gethash key table)
        :do (setf table (gethash key table))))

(defun (setf href) (value table &rest keys)
  (loop :for (key . rest) :on keys
        :unless rest
          :return (setf (gethash key table) value)
        :do (setf table (gethash key table))))

(define-compiler-macro href (table &rest keys)
  (expand-href table keys))

(define-compiler-macro (setf href) (value table &rest keys)
  `(setf ,(expand-href table keys) ,value))
