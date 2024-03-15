(in-package #:virality)

;;;; Implementation of datatypes: GEOMETRY-GROUP, GEOMETRY-GROUP/SEPARATE,
;;;; GEOMETRY-GROUP/INTERLEAVED

(defun make-geometry-groups (spec)
  (let ((groups (u:dict #'eq))
        (order))
    (dolist (group spec)
      (destructuring-bind (name (&key (format 'interleaved) (divisor 0))
                           . attrs)
          group
        (u:mvlet ((group-type (u:format-symbol :virality "GEOMETRY-GROUP/~a"
                                               format))
                  (attributes attribute-order (make-geometry-attributes attrs)))
          (push name order)
          (setf (u:href groups name)
                (make-instance group-type
                               :name name
                               :format format
                               :divisor divisor
                               :attributes attributes
                               :attribute-order attribute-order)))))
    (values groups
            (nreverse order))))

(defgeneric get-geometry-group-buffer-count (group)
  (:method (group)
    1)
  (:method ((group geometry-group/separate))
    (hash-table-count (attributes group))))

(defun get-geometry-group-attribute-size (group)
  (reduce #'+ (u:hash-values (attributes group))
          :key #'get-geometry-attribute-size))

(defmethod configure-geometry-group ((group geometry-group) index buffers)
  (loop :for attribute-name :in (attribute-order group)
        :for attribute = (u:href (attributes group) attribute-name)
        :for i :from index
        :for buffer :across buffers
        :do (gl:bind-buffer :array-buffer buffer)
            (configure-geometry-attribute attribute i 0 0 (divisor group))))

(defmethod configure-geometry-group ((group geometry-group/interleaved) index
                                     buffers)
  (gl:bind-buffer :array-buffer (aref buffers 0))
  (loop :with stride = (get-geometry-group-attribute-size group)
        :with offset = 0
        :for attribute-name :in (attribute-order group)
        :for attr = (u:href (attributes group) attribute-name)
        :for i :from index
        :do (configure-geometry-attribute attr i stride offset (divisor group))
            (incf offset (get-geometry-attribute-size attr))))
