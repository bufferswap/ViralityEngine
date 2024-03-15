(in-package #:virality.component)

(v:define-component geometry ()
  ((%name :reader name
          :initarg :name
          :initform nil)
   (%geometry :reader geometry
              :initarg :geometry
              :initform nil)))

(defmethod v:on-component-initialize ((self geometry))
  (with-slots (%name %geometry) self
    (unless %name
      (error "A geometry component must have name specified."))
    (setf %geometry (v::make-geometry %name))))

(defmethod v:on-component-slave-render ((master render) (self geometry))
  (let ((instance-count (v::instances (comp:material master))))
    (v::draw-geometry (geometry self) instance-count)))
