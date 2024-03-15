(in-package #:virality)

(defclass geometry-spec ()
  ((%id :reader id
        :initarg :id)
   (%layout :reader layout
            :initarg :layout)
   (%buffers :accessor buffers)
   (%buffer-names :reader buffer-names
                  :initform (u:dict #'eq))
   (%primitive :reader primitive
               :initarg :primitive)
   (%vertex-count :reader vertex-count
                  :initarg :vertex-count)
   (%primitive-count :accessor primitive-count
                     :initform 0)))
