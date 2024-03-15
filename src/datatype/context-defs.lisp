(in-package #:virality)

;;;; The gamedev facing interaface to the CORE abstraction for a running
;;;; game instance.

(defclass context ()
  ((%core :reader core
          :initarg :core)
   (%active-camera :accessor active-camera
                   :initform nil)
   (%storage :reader storage
             :initform (u:dict #'eq))
   (%state :accessor state
           :initform nil)))
