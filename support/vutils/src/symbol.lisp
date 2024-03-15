(in-package #:vutils)

(defun collect-symbols (package &key key test)
  "Collect a list of all symbols of `PACKAGE`."
  (let ((key (or key #'identity))
        (test (or test (constantly t)))
        (symbols nil))
    (do-symbols (symbol package)
      (when (funcall test symbol)
        (push (funcall key symbol) symbols)))
    (nreverse symbols)))

(defun collect-external-symbols (package &key key test)
  "Collect a list of all external symbols of `PACKAGE`."
  (let ((key (or key #'identity))
        (test (or test (constantly t)))
        (symbols nil))
    (do-external-symbols (symbol package)
      (when (funcall test symbol)
        (push (funcall key symbol) symbols)))
    (nreverse symbols)))

(defun make-keyword (object)
  "Interns `OBJECT`, a string designator or number, into the keyword package."
  (values
   (alexandria:make-keyword
    (etypecase object
      ((or character string symbol)
       object)
      (number
       (format nil "~a" object))))))

(defun symbol-name= (x y)
  "Return T if X and Y are symbols with the same name."
  (cond
    ((eql x y) t)
    ((and (symbolp x) (symbolp y))
     (string= (symbol-name x) (symbol-name y)))))
