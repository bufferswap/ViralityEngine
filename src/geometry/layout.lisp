(in-package #:virality)

;;;; Implementation of datatype: GEOMETRY-LAYOUT

(defun find-geometry-layout (layout-name)
  (or (u:href =meta/geometry-layouts= layout-name)
      (error "Geometry layout ~s not found." layout-name)))

(defun make-geometry-layout (name groups order)
  (let ((layout (make-instance 'geometry-layout :name name)))
    (setf (u:href =meta/geometry-layouts= name) layout)
    (update-geometry-layout name groups order)))

(defun update-geometry-layout (name groups order)
  (let ((layout (u:href =meta/geometry-layouts= name)))
    (setf (groups layout) groups
          (group-order layout) order)))

(defmacro define-geometry-layout (name options &body body)
  (declare (ignore options))
  (u:with-gensyms (groups order)
    `(u:mvlet ((,groups ,order (make-geometry-groups ',body)))
       (if (u:href =meta/geometry-layouts= ',name)
           (update-geometry-layout ',name ,groups ,order)
           (make-geometry-layout ',name ,groups ,order)))))
