(in-package #:virality)

(defclass flow-state ()
  ((%name :accessor name
          :initarg :name)
   (%policy :accessor policy
            :initarg :policy)
   (%exiting-p :accessor exiting-p
               :initarg :exiting-p)
   (%selector :accessor selector
              :initarg :selector)
   (%action :accessor action
            :initarg :action)
   (%transition :accessor transition
                :initarg :transition)
   (%reset :accessor reset
           :initarg :reset)))
