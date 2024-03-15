(in-package #:vutils)

(defun posix-argv0 ()
  "Returns the POSIX C equivalent of argv[0],which is typically the program name
when executing a dumped executable. NOTE: This more than likely does not make
sense to be called in an interactive Lisp session, but we'll let callers decide
when to use it."
  (first (uiop:raw-command-line-arguments)))
