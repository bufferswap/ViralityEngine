(in-package #:virality)

(defclass free-look-state ()
  ((%context :reader context
             :initarg :context)
   (%camera :reader camera
            :initarg :camera)
   (%initial-state :accessor initial-state
                   :initform nil)
   (%initial-orientation :accessor initial-orientation
                         :initform (m4:id))
   (%key-state :reader key-state
               :initform (u:dict #'eq))
   (%key-speed :reader key-speed
               :initform 30f0)
   (%mouse-sensitivity :reader mouse-sensitivity
                       :initform 10f0)))

(defun make-free-look-state (context camera)
  (make-instance 'free-look-state
                 :context context
                 :camera camera))

(defun set-initial-free-look-orientation (state model)
  (unless (initial-state state)
    (setf (initial-orientation state) model
          (initial-state state) t)))

(defun reset-free-look-state (state)
  (let* ((camera (camera state))
         (model (initial-orientation state))
         (translation (m4:get-translation model))
         (rotation (q:from-mat4 model)))
    (translate camera translation :replace t)
    (rotate camera rotation :replace t)))

(defun update-free-look-key-state (state)
  (let ((context (context state))
        (key-state (key-state state)))
    (when (on-button-enter context :key :backspace)
      (reset-free-look-state state))
    (cond
      ((on-button-enabled context :key :w)
       (setf (u:href key-state :forward) t))
      ((on-button-exit context :key :w)
       (setf (u:href key-state :forward) nil)))
    (cond
      ((on-button-enabled context :key :s)
       (setf (u:href key-state :backward) t))
      ((on-button-exit context :key :s)
       (setf (u:href key-state :backward) nil)))
    (cond
      ((on-button-enabled context :key :a)
       (setf (u:href key-state :strafe-left) t))
      ((on-button-exit context :key :a)
       (setf (u:href key-state :strafe-left) nil)))
    (cond
      ((on-button-enabled context :key :d)
       (setf (u:href key-state :strafe-right) t))
      ((on-button-exit context :key :d)
       (setf (u:href key-state :strafe-right) nil)))
    (cond
      ((on-button-enabled context :key :left)
       (setf (u:href key-state :turn-left) t))
      ((on-button-exit context :key :left)
       (setf (u:href key-state :turn-left) nil)))
    (cond
      ((on-button-enabled context :key :right)
       (setf (u:href key-state :turn-right) t))
      ((on-button-exit context :key :right)
       (setf (u:href key-state :turn-right) nil)))
    (cond
      ((on-button-enabled context :key :pageup)
       (setf (u:href key-state :strafe-up) t))
      ((on-button-exit context :key :pageup)
       (setf (u:href key-state :strafe-up) nil)))
    (cond
      ((on-button-enabled context :key :pagedown)
       (setf (u:href key-state :strafe-down) t))
      ((on-button-exit context :key :pagedown)
       (setf (u:href key-state :strafe-down) nil)))))

(defun free-look/key-move (camera key-state speed)
  (flet ((axis-dir (direction plus minus)
           (v3:scale (transform-direction camera direction)
                     (cond
                       ((u:href key-state plus) 1f0)
                       ((u:href key-state minus) -1f0)
                       (t 0f0)))))
    (let* ((x (axis-dir v3:+right+ :strafe-right :strafe-left))
           (y (axis-dir v3:+up+ :strafe-up :strafe-down))
           (z (axis-dir v3:+back+ :forward :backward))
           (vec (v3:+ (v3:+ x y) z))
           (vec (v3:normalize vec))
           (vec (v3:scale vec speed))
           (angle (axis-dir v3:+up+ :turn-left :turn-right))
           (angle (v3:scale angle speed)))
      (translate camera vec)
      (rotate/velocity camera angle 1f0))))

(defun free-look/mouse-move (state camera speed)
  (flet ((velocity (direction x)
           (v3:scale (transform-direction camera direction)
                     (* x speed))))
    (u:mvlet* ((context (context state))
               (mx my dx dy (get-mouse-position context))
               (x (velocity v3:+right+ dx))
               (y (velocity v3:+up+ dy))
               (z (velocity v3:+forward+ dy))

               ;; TODO: normalize?
               ;;(xxx (v2:length (v2:vec dx dy)))
               ;;
               ;; This addition will cause a faster movement along a diagonal
               ;; which is incorrect. This can be fixed by normalizing x, y, z.
               ;; But then we must correctly encode the speed contribution of
               ;; the mouse delta dx, dy AND the speed. This probably means we
               ;; need to compute (* xxx speed) and use that as a scale for vec,
               ;; angle, and z. The "velocity" function above probably should
               ;; only return direction vectors based on the signum of the val.
               ;; But, it is JUST complex enough that we need some paper to
               ;; figure it out later when we aren't on stream.
               (vec (v3:+ x y))
               (angle (v3:+ (velocity v3:+down+ dx)
                            (velocity v3:+right+ dy))))
      (when (or (on-button-enter context :key :lshift)
                (on-button-enter context :key :lctrl)
                (on-button-enter context :key :lalt))
        (enable-relative-motion context))
      (when (on-button-enabled context :key :lshift)
        (translate camera vec))
      (when (on-button-enabled context :key :lalt)
        (translate camera z))
      (when (on-button-enabled context :key :lctrl)
        (rotate/velocity camera angle 1f0))
      (when (or (on-button-exit context :key :lshift)
                (on-button-exit context :key :lctrl)
                (on-button-exit context :key :lalt))
        (disable-relative-motion context)))))

(defun update-free-look-state (state)
  (let* ((camera (camera state))
         (dt (frame-time (context state)))
         (key-speed (* (key-speed state) dt))
         (mouse-speed (* (mouse-sensitivity state) dt)))
    (update-free-look-key-state state)
    (free-look/key-move camera (key-state state) key-speed)
    (free-look/mouse-move state camera mouse-speed)))
