(in-package #:virality)

;;;; Holder of the SDL2 window and opengl context information.

(defclass display ()
  ((%window :reader window
            :initarg :window)
   (%context :accessor context
             :initarg :context
             :initform nil)
   (%resolution :reader resolution
                :initarg :resolution)
   (%refresh-rate :reader refresh-rate
                  :initarg :refresh-rate)))
