(in-package #:virality)

;;;; Implementation of datatype GEOMETRY-ATTRIBUTE

(defun make-geometry-attributes (spec)
  (let ((attrs (u:dict #'eq))
        (order))
    (dolist (attribute spec)
      (destructuring-bind (name &key normalize (type :float) (out-type type)
                                  (count 1))
          attribute
        (push name order)
        (setf (u:href attrs name)
              (make-instance 'geometry-attribute
                             :name name
                             :normalize normalize
                             :type (u:make-keyword type)
                             :out-type (u:make-keyword out-type)
                             :element-count count))))
    (values attrs (nreverse order))))

(defun get-geometry-attribute-size (attribute)
  (* (element-count attribute)
     (ecase (attribute-type attribute)
       ((:byte :unsigned-byte) 1)
       ((:short :unsigned-short :half-float) 2)
       ((:int :unsigned-int :float :fixed) 4)
       (:double 8))))

(defun configure-geometry-attribute (attribute index stride offset divisor)
  (let ((normalize (if (normalize attribute) 1 0)))
    (ecase (out-type attribute)
      ((:byte :unsigned-byte :short :unsigned-short :int :unsigned-int)
       (%gl:vertex-attrib-ipointer index
                                   (element-count attribute)
                                   (attribute-type attribute)
                                   stride
                                   offset))
      ((:half-float :float :fixed)
       (%gl:vertex-attrib-pointer index
                                  (element-count attribute)
                                  (attribute-type attribute)
                                  normalize
                                  stride
                                  offset))
      (:double
       (%gl:vertex-attrib-lpointer index
                                   (element-count attribute)
                                   (attribute-type attribute)
                                   stride
                                   offset)))
    (%gl:vertex-attrib-divisor index divisor)))
