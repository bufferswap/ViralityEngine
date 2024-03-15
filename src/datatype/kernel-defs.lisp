(in-package #:virality)

;;; The root type for actors and components.

(defclass kernel ()
  ((%core :reader core)
   (%context :reader context
             :initarg :context)
   (%state :accessor state
           :initarg :state
           :initform :initialize)
   (%id :reader id
        :initarg :id
        :initform nil)
   (%uuid :reader uuid
          :initform (make-uuid))
   (%display-id :accessor display-id
                :initarg :display-id
                :initform "Un-named kernel")
   (%ttl :accessor ttl
         :initarg :ttl
         :initform 0)))
