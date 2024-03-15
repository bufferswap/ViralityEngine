(in-package #:vutils)

(declaim (inline make-b8-array))
(defun make-b8-array (size &optional (initial-element 0))
  (make-array size :element-type 'b8 :initial-element initial-element))

(declaim (inline make-b16-array))
(defun make-b16-array (size &optional (initial-element 0))
  (make-array size :element-type 'b16 :initial-element initial-element))

(declaim (inline make-b24-array))
(defun make-b24-array (size &optional (initial-element 0))
  (make-array size :element-type 'b24 :initial-element initial-element))

(declaim (inline make-b32-array))
(defun make-b32-array (size &optional (initial-element 0))
  (make-array size :element-type 'b32 :initial-element initial-element))

(declaim (inline make-ub8-array))
(defun make-ub8-array (size &optional (initial-element 0))
  (make-array size :element-type 'ub8 :initial-element initial-element))

(declaim (inline make-ub16-array))
(defun make-ub16-array (size &optional (initial-element 0))
  (make-array size :element-type 'ub16 :initial-element initial-element))

(declaim (inline make-ub24-array))
(defun make-ub24-array (size &optional (initial-element 0))
  (make-array size :element-type 'ub24 :initial-element initial-element))

(declaim (inline make-ub32-array))
(defun make-ub32-array (size &optional (initial-element 0))
  (make-array size :element-type 'ub32 :initial-element initial-element))

(declaim (inline make-fixnum-array))
(defun make-fixnum-array (size &optional (initial-element 0))
  (make-array size :element-type 'fixnum :initial-element initial-element))

(declaim (inline make-f32-array))
(defun make-f32-array (size &optional (initial-element 0f0))
  (make-array size :element-type 'f32 :initial-element initial-element))

(declaim (inline make-f64-array))
(defun make-f64-array (size &optional (initial-element 0d0))
  (make-array size :element-type 'f64 :initial-element initial-element))

(declaim (inline make-bit-vector))
(defun make-bit-vector (size &optional (initial-element 0))
  (make-array size :element-type 'bit :initial-element initial-element))

(defmacro do-array ((index value array) &body body)
  `(loop :for ,value :across ,array
         :for ,index :from 0
         :do ,@body))
