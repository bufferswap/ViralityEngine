(in-package #:vutils)

(defmacro with-file-input ((stream path) &body body)
  "Open the file at location `PATH` as input and perform `BODY`."
  `(with-open-file (,stream
                    ,path
                    :direction :input
                    :if-does-not-exist :error)
     ,@body))

(defmacro with-file-output ((stream path &optional append-p) &body body)
  "Open the file at location `PATH`, as output and perform `BODY`.
If the file already exists, it is overwritten.
If the file does not exist, it is created."
  `(with-open-file (,stream
                    ,path
                    :direction :output
                    :if-exists ,(if append-p :append :supersede)
                    :if-does-not-exist :create)
     ,@body))

(defmacro with-binary-input ((stream file) &body body)
  "Open the file at location `PATH` as binary input and perform `BODY`."
  `(with-open-file (,stream
                    ,file
                    :direction :input
                    :if-does-not-exist :error
                    :element-type 'octet)
     ,@body))

(defmacro with-binary-output ((stream file &optional append-p) &body body)
  "Open the file at location `PATH`, as binary output and perform `BODY`.
If the file already exists, it is overwritten.
If the file does not exist, it is created."
  `(with-open-file (,stream
                    ,file
                    :direction :output
                    :if-exists ,(if append-p :append :supersede)
                    :if-does-not-exist :create
                    :element-type 'octet)
     ,@body))

(defun map-files (path function &key (test (constantly t)) (recursive-p t))
  "Map over all files located in the directory of `PATH`, applying `FUNCTION` to
each file's path. `TEST` is a function that takes a file path and decides if
`FUNCTION` should be applied to it. `RECURSIVE-P`, when non-NIL will descend
into sub-directories of `PATH` recursively."
  (labels ((process-files (dir)
             (map nil
                  (lambda (x)
                    (when (funcall test x)
                      (funcall function x)))
                  (uiop/filesystem:directory-files dir))))
    (uiop/filesystem:collect-sub*directories
     (uiop/pathname:ensure-directory-pathname path)
     t recursive-p #'process-files)))

(defun safe-read-file-form (path &key package)
  "Read the first form of the file located at `PATH`, with *PACKAGE* bound to
`PACKAGE`."
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (with-open-file (in path)
        (if package
            (let ((*package* (find-package package)))
              (read in))
            (with-temp-package
              (read in)))))))

(defun safe-read-file-forms (path &key package)
  "Read all forms of the file located at `PATH`, with *PACKAGE* bound to
`PACKAGE`."
  (with-standard-io-syntax
    (flet ((%read (in)
             (if package
                 (let ((*package* (find-package package)))
                   (read in nil in))
                 (with-temp-package
                   (read in nil in)))))
      (let ((*read-eval* nil))
        (with-open-file (in path)
          (loop :for form = (%read in)
                :until (eq form in)
                :collect form))))))

(defun file->string (path)
  "Read the file located at `PATH` into a string."
  (with-file-input (in path)
    (let ((string (make-string (file-length in))))
      (read-sequence string in)
      string)))

(defun string->file (string path)
  "Write `STRING` to the file located at `PATH`."
  (with-file-output (out path)
    (format out string)))
