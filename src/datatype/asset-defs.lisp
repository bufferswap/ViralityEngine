(in-package #:virality)

(defclass asset-spec ()
  ((%pool :reader pool
          :initarg :pool)
   (%name :reader name
          :initarg :name)
   (%path :reader path
          :initarg :path)))
