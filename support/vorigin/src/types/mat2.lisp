(in-package #:cl-user)

(defpackage #:vorigin.mat2
  (:local-nicknames
   (#:com #:vorigin.common)
   (#:u #:vutils)
   (#:v2 #:vorigin.vec2))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:random
   #:trace)
  (:export
   #:mat
   #:with-components
   #:pretty-print
   #:+zero+
   #:+id+
   #:zero
   #:zero!
   #:zero-p
   #:random
   #:id
   #:id!
   #:id-p
   #:=
   #:copy!
   #:copy
   #:clamp!
   #:clamp
   #:clamp-range!
   #:clamp-range
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:get-column!
   #:get-column
   #:set-column!
   #:set-column
   #:rotation-axis-to-vec2!
   #:rotation-axis-to-vec2
   #:rotation-axis-from-vec2!
   #:rotation-axis-from-vec2
   #:rotation-from-angle!
   #:rotation-from-angle
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
   #:scale!
   #:scale
   #:*v2!
   #:*v2
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal
   #:set-diagonal!
   #:set-diagonal))

(in-package #:vorigin.mat2)

(deftype mat () '(u:f32a 4))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  (u:once-only (matrix)
    `(symbol-macrolet
         ((,prefix ,matrix)
          (,(com:make-accessor-symbol prefix "00") (aref ,matrix 0))
          (,(com:make-accessor-symbol prefix "10") (aref ,matrix 1))
          (,(com:make-accessor-symbol prefix "01") (aref ,matrix 2))
          (,(com:make-accessor-symbol prefix "11") (aref ,matrix 3)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defun pretty-print (matrix &optional (stream *standard-output*))
  (with-components ((m matrix))
    (format stream "[~,6f, ~,6f~% ~,6f, ~,6f]" m00 m01 m10 m11)))

;;; Constructor

(u:fn-> mat (u:f32 u:f32 u:f32 u:f32) mat)
(declaim (inline mat))
(u:eval-always
  (defun mat (m00 m10 m01 m11)
    (declare (optimize speed))
    (let ((mat (u:make-f32-array 4)))
      (setf (aref mat 0) m00
            (aref mat 1) m10
            (aref mat 2) m01
            (aref mat 3) m11)
      mat)))

;;; Constants

(u:define-constant +zero+ (mat 0f0 0f0 0f0 0f0) :test #'equalp)

(u:define-constant +id+ (mat 1f0 0f0 0f0 1f0) :test #'equalp)
