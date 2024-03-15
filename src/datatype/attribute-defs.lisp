(in-package #:virality)

(defclass geometry-attribute ()
  ((%name :reader name
          :initarg :name)
   (%normalize :reader normalize
               :initarg :normalize)
   (%type :reader attribute-type
          :initarg :type)
   (%out-type :reader out-type
              :initarg :out-type)
   (%element-count :reader element-count
                   :initarg :element-count)))
