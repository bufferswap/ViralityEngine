(in-package #:cl-user)

(defpackage #:vorigin.dquat
  (:local-nicknames
   (#:com #:vorigin.common)
   (#:dm3 #:vorigin.dmat3)
   (#:dm4 #:vorigin.dmat4)
   (#:dv3 #:vorigin.dvec3)
   (#:dv4 #:vorigin.dvec4)
   (#:q #:vorigin.quat)
   (#:u #:vutils))
  (:use #:cl)
  (:shadow
   #:=
   #:+
   #:-
   #:*
   #:conjugate
   #:length
   #:random)
  (:export
   #:quat
   #:with-components
   #:w
   #:x
   #:y
   #:z
   #:+id+
   #:id
   #:id!
   #:id-p
   #:=
   #:random!
   #:random
   #:copy!
   #:copy
   #:+!
   #:+
   #:-!
   #:-
   #:*!
   #:*
   #:scale!
   #:scale
   #:conjugate!
   #:conjugate
   #:cross!
   #:cross
   #:length-squared
   #:length
   #:normalize!
   #:normalize
   #:negate!
   #:negate
   #:dot
   #:inverse!
   #:inverse
   #:rotate-euler!
   #:rotate-euler
   #:rotate!
   #:rotate
   #:to-euler!
   #:to-euler
   #:to-mat3!
   #:to-mat3
   #:to-mat4!
   #:to-mat4
   #:from-mat3!
   #:from-mat3
   #:from-mat4!
   #:from-mat4
   #:slerp!
   #:slerp
   #:from-axis-angle!
   #:from-axis-angle
   #:orient!
   #:orient
   #:from-velocity!
   #:from-velocity))

(in-package #:vorigin.dquat)

(deftype quat () '(u:f64a 4))

(defmacro with-components (((prefix quat) &rest rest) &body body)
  (u:once-only (quat)
    `(symbol-macrolet
         ((,prefix ,quat)
          (,(com:make-accessor-symbol prefix "W") (aref ,quat 0))
          (,(com:make-accessor-symbol prefix "X") (aref ,quat 1))
          (,(com:make-accessor-symbol prefix "Y") (aref ,quat 2))
          (,(com:make-accessor-symbol prefix "Z") (aref ,quat 3)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

;;; Constructor

(u:fn-> quat (u:f64 u:f64 u:f64 u:f64) quat)
(declaim (inline quat))
(u:eval-always
  (defun quat (w x y z)
    (declare (optimize speed))
    (let ((quat (u:make-f64-array 4)))
      (setf (aref quat 0) w
            (aref quat 1) x
            (aref quat 2) y
            (aref quat 3) z)
      quat)))

;;; Accessors

(u:fn-> w (quat) u:f64)
(declaim (inline w))
(defun w (quat)
  (aref quat 0))

(u:fn-> (setf w) (u:f64 quat) u:f64)
(declaim (inline (setf w)))
(defun (setf w) (value quat)
  (setf (aref quat 0) value))

(u:fn-> x (quat) u:f64)
(declaim (inline x))
(defun x (quat)
  (aref quat 1))

(u:fn-> (setf x) (u:f64 quat) u:f64)
(declaim (inline (setf x)))
(defun (setf x) (value quat)
  (setf (aref quat 1) value))

(u:fn-> y (quat) u:f64)
(declaim (inline y))
(defun y (quat)
  (aref quat 2))

(u:fn-> (setf y) (u:f64 quat) u:f64)
(declaim (inline (setf y)))
(defun (setf y) (value quat)
  (setf (aref quat 2) value))

(u:fn-> z (quat) u:f64)
(declaim (inline z))
(defun z (quat)
  (aref quat 3))

(u:fn-> (setf z) (u:f64 quat) u:f64)
(declaim (inline (setf z)))
(defun (setf z) (value quat)
  (setf (aref quat 3) value))

;;; Constants

(u:define-constant +id+ (quat 1d0 0d0 0d0 0d0) :test #'equalp)
