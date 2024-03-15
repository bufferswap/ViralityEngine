(in-package #:virality)

(defclass geometry-group ()
  ((%name :reader name
          :initarg :name)
   (%format :reader buffer-format
            :initarg :format)
   (%divisor :reader divisor
             :initarg :divisor)
   (%attributes :reader attributes
                :initarg :attributes)
   (%attribute-order :reader attribute-order
                     :initarg :attribute-order)))

(defclass geometry-group/separate (geometry-group) ())

(defclass geometry-group/interleaved (geometry-group) ())
