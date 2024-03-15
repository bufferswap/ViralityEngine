(in-package #:virality.component)

(v:define-component camera ()
  ((%active-p :accessor active-p
              :initarg :active-p
              :initform nil)
   (%view :reader view
          :initform (m4:id))
   (%projection :reader projection
                :initform (m4:id))
   (%mode :reader mode
          :initarg :mode
          :initform :perspective)
   (%clip-near :reader clip-near
               :initarg :clip-near
               :initform 0.1f0)
   (%clip-far :reader clip-far
              :initarg :clip-far
              :initform 1024f0)
   (%fov-y :reader fov-y
           :initarg :fov-y
           :initform 45f0)
   (%zoom :accessor zoom
          :initarg :zoom
          :initform 1f0)
   (%transform :reader transform)
   (%free-look :accessor free-look
               :initarg :free-look
               :initform nil)
   (%free-look-state :reader free-look-state
                     :initform nil)))

(defun correct-camera-transform (camera)
  (when (v3:zero-p (v:get-translation camera))
    (let ((translation (ecase (mode camera)
                         (:orthographic (v3:vec 0f0 0f0 1f0))
                         (:perspective (v3:vec 0f0 0f0 50f0)))))
      (v:translate camera translation)
      (warn "Camera ~a was attached to an actor without a translation ~
                 transform.~%Using a sane default value for ~(~a~): ~s."
            (v:id (v:actor camera)) (mode camera) translation))))

(defmethod make-projection (camera (mode (eql :perspective)))
  (with-slots (%projection %fov-y %zoom %clip-near %clip-far) camera
    (m4:perspective! %projection
                     (/ %fov-y %zoom)
                     (float
                      (/ v:=window-width= v:=window-height=)
                      1f0)
                     %clip-near
                     %clip-far)))

(defmethod make-projection (camera (mode (eql :orthographic)))
  (with-slots (%projection %zoom %clip-near %clip-far) camera
    (let ((w (/ v:=window-width= %zoom 2f0))
          (h (/ v:=window-height= %zoom 2f0)))
      (m4:ortho!
       %projection (- w) w (- h) h %clip-near %clip-far))))

(defun compute-camera-view (camera)
  (with-slots (%active-p %transform %view) camera
    (when %active-p
      (let* ((model (v:get-model-matrix camera))
             (eye (m4:get-translation model))
             (target (v3:+ eye (v3:negate (m4:rotation-axis-to-vec3 model :z))))
             (up (m4:rotation-axis-to-vec3 model :y)))
        (m4:look-at! %view eye target up)))))

(defun find-active-camera (context)
  (dolist (camera (v::cameras (v::core context)))
    (when (active-p camera)
      (return-from find-active-camera camera))))

(defun zoom-camera (display direction)
  (let* ((context (v:context (v::core display)))
         (camera (find-active-camera context)))
    (with-slots (%zoom %mode) camera
      (setf %zoom (u:clamp (+ %zoom (/ direction 2f0)) 1f0 10f0))
      (make-projection %mode camera))))

;;; Component event hooks

(defmethod v:on-component-initialize ((self camera))
  (with-slots (%transform %fov-y %free-look %free-look-state) self
    (setf %transform (v:component-by-type (v:actor self) 'transform)
          %fov-y (* %fov-y (/ o:pi 180)))
    (when %free-look
      (setf %free-look-state (v::make-free-look-state (v::context self) self)))
    (correct-camera-transform self)
    (make-projection self (mode self))
    (push self (v::cameras (v::core self)))))

(defmethod v:on-component-update ((self camera))
  (when (free-look-state self)
    (v::set-initial-free-look-orientation
     (free-look-state self)
     (v:get-model-matrix self :copy t)))
  (when (free-look self)
    (v::update-free-look-state (free-look-state self))))

(defmethod v:on-component-destroy ((self camera))
  (let ((context (v:context self)))
    (u:deletef (v::cameras (v::core self)) self)
    (setf (v::active-camera context) nil)))
