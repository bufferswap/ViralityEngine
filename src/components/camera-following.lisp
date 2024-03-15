(in-package #:virality.component)

(v:define-component following-camera ()
  ((%slave :reader slave)
   (%target-actor :accessor target-actor
                  :initarg :target-actor)
   (%target-transform :reader target-transform)
   (%offset :reader offset
            :initarg :offset
            :initform (v3:zero))))

(defmethod camera-target-actor ((camera following-camera) actor)
  (with-slots (%target-transform) camera
    (setf (target-actor camera) actor)
    (when actor
      (setf %target-transform (v:component-by-type actor 'transform)))))

(defmethod v:on-component-initialize ((self following-camera))
  (with-slots (%slave) self
    (setf %slave (v:component-by-type (v:actor self) 'camera))
    (camera-target-actor self (target-actor self))))

(defmethod v:on-component-update ((self following-camera))
  (with-slots (%transform) (slave self)
    (let* ((target-position (m4:get-translation
                             (v:get-model-matrix (target-transform self))))
           (new-camera-position (v3:+! target-position
                                       target-position
                                       (offset self)))
           (model (v:get-model-matrix self)))
      (m4:set-translation! model model new-camera-position)
      (compute-camera-view (slave self)))))
