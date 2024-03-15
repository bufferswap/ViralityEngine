(in-package #:virality)

(defclass input-data ()
  ((%gamepad-instances :reader gamepad-instances
                       :initform (u:dict #'eq))
   (%gamepad-ids :accessor gamepad-ids
                 :initform (u:dict #'eq))
   (%detached-gamepads :accessor detached-gamepads
                       :initform nil)
   (%entering :accessor entering
              :initform (u:dict #'eq))
   (%exiting :accessor exiting
             :initform (u:dict #'eq))
   (%states :reader states
            :initform (u:dict #'equal))))
