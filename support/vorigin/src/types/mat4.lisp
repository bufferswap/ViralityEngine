(in-package #:cl-user)

(defpackage #:vorigin.mat4
  (:local-nicknames
   (#:com #:vorigin.common)
   (#:m2 #:vorigin.mat2)
   (#:m3 #:vorigin.mat3)
   (#:u #:vutils)
   (#:v3 #:vorigin.vec3)
   (#:v4 #:vorigin.vec4))
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
   #:id
   #:id!
   #:id-p
   #:=
   #:random!
   #:random
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
   #:get-translation!
   #:get-translation
   #:set-translation!
   #:set-translation
   #:translate!
   #:translate
   #:copy-rotation!
   #:copy-rotation
   #:rotation-to-mat3!
   #:rotation-to-mat3
   #:normalize-rotation!
   #:normalize-rotation
   #:rotation-axis-to-vec3!
   #:rotation-axis-to-vec3
   #:rotation-axis-from-vec3!
   #:rotation-axis-from-vec3
   #:rotation-x-from-angle!
   #:rotation-x-from-angle
   #:rotation-y-from-angle!
   #:rotation-y-from-angle!
   #:rotation-z-from-angle
   #:rotation-z-from-angle
   #:rotate!
   #:rotate
   #:get-scale!
   #:get-scale
   #:set-scale!
   #:set-scale
   #:scale!
   #:scale
   #:*v3!
   #:*v3
   #:*v4!
   #:*v4
   #:transpose!
   #:transpose
   #:orthogonal-p
   #:orthonormalize!
   #:orthonormalize
   #:trace
   #:diagonal-p
   #:main-diagonal!
   #:main-diagonal
   #:anti-diagonal!
   #:anti-diagonal
   #:set-diagonal!
   #:set-diagonal
   #:determinant
   #:invert-orthogonal!
   #:invert-orthogonal
   #:invert!
   #:invert
   #:look-at!
   #:look-at
   #:ortho!
   #:ortho
   #:perspective!
   #:perspective))

(in-package #:vorigin.mat4)

(deftype mat () '(u:f32a 16))

(defmacro with-components (((prefix matrix) &rest rest) &body body)
  (u:once-only (matrix)
    `(symbol-macrolet
         ((,prefix ,matrix)
          (,(com:make-accessor-symbol prefix "00") (aref ,matrix 0))
          (,(com:make-accessor-symbol prefix "10") (aref ,matrix 1))
          (,(com:make-accessor-symbol prefix "20") (aref ,matrix 2))
          (,(com:make-accessor-symbol prefix "30") (aref ,matrix 3))
          (,(com:make-accessor-symbol prefix "01") (aref ,matrix 4))
          (,(com:make-accessor-symbol prefix "11") (aref ,matrix 5))
          (,(com:make-accessor-symbol prefix "21") (aref ,matrix 6))
          (,(com:make-accessor-symbol prefix "31") (aref ,matrix 7))
          (,(com:make-accessor-symbol prefix "02") (aref ,matrix 8))
          (,(com:make-accessor-symbol prefix "12") (aref ,matrix 9))
          (,(com:make-accessor-symbol prefix "22") (aref ,matrix 10))
          (,(com:make-accessor-symbol prefix "32") (aref ,matrix 11))
          (,(com:make-accessor-symbol prefix "03") (aref ,matrix 12))
          (,(com:make-accessor-symbol prefix "13") (aref ,matrix 13))
          (,(com:make-accessor-symbol prefix "23") (aref ,matrix 14))
          (,(com:make-accessor-symbol prefix "33") (aref ,matrix 15)))
       ,(if rest
            `(with-components ,rest ,@body)
            `(progn ,@body)))))

(defun pretty-print (matrix &optional (stream *standard-output*))
  (with-components ((m matrix))
    (format stream "[~,6f, ~,6f, ~,6f, ~,6f~% ~
                     ~,6f, ~,6f, ~,6f, ~,6f~% ~
                     ~,6f, ~,6f, ~,6f, ~,6f~% ~
                     ~,6f, ~,6f, ~,6f, ~,6f]"
            m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33)))

;;; Constructor

(u:fn-> mat (u:f32 u:f32 u:f32 u:f32 u:f32 u:f32 u:f32 u:f32 u:f32 u:f32 u:f32 u:f32 u:f32 u:f32
                   u:f32 u:f32)
        mat)
(declaim (inline mat))
(u:eval-always
  (defun mat (m00 m10 m20 m30 m01 m11 m21 m31 m02 m12 m22 m32 m03 m13 m23 m33)
    (declare (optimize speed))
    (let ((mat (u:make-f32-array 16)))
      (setf (aref mat 0) m00 (aref mat 1) m10 (aref mat 2) m20 (aref mat 3) m30
            (aref mat 4) m01 (aref mat 5) m11 (aref mat 6) m21 (aref mat 7) m31
            (aref mat 8) m02 (aref mat 9) m12 (aref mat 10) m22 (aref mat 11) m32
            (aref mat 12) m03 (aref mat 13) m13 (aref mat 14) m23 (aref mat 15) m33)
      mat)))

;;; Constants

(u:define-constant +zero+
    (mat 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0)
  :test #'equalp)

(u:define-constant +id+
    (mat 1f0 0f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 0f0 1f0 0f0 0f0 0f0 0f0 1f0)
  :test #'equalp)
