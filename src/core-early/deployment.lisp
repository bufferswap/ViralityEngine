(in-package #:virality)

;; TODO: Change *deployed-p* such that it is passed an as argument to START in
;; the :toplevel lambda function below. And then default it to NIL otherwise.
;; Then *deployed-p* can be set fully internally and not even have to be a
;; global/special var, per se.

(defvar *deployed-p* nil)

(defun deploy-binary (file-name project scene &key compress-p)
  #+sbcl
  (progn
    (setf *deployed-p* t)
    (sb-ext:save-lisp-and-die
     file-name
     :toplevel (lambda () (start :project project :scene scene))
     :executable t
     :compression (when compress-p 9)))
  #-sbcl
  (error "Deployment is only supported on SBCL."))
