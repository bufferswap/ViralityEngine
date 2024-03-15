(in-package #:virality)

;;;; Implementation of datatypes: GAMEPAD, GAMEPAD-ATTACH-STATE,
;;;; GAMEPAD-ANALOG-STATE

(u:define-constant +gamepad-axis-names+
    #((:left-stick :x) (:left-stick :y) (:right-stick :x) (:right-stick :y)
      (:triggers :x) (:triggers :y))
  :test #'equalp)

(u:define-constant +gamepad-button-names+
    #(:a :b :x :y :back :guide :start :left-stick-button :right-stick-button
      :left-shoulder :right-shoulder :up :down :left :right)
  :test #'equalp)


(defun gamepad-attach-transition-in (data gamepad-id)
  (let ((input (list :attach gamepad-id)))
    (symbol-macrolet ((state (u:href (states data) input)))
      (if state
          (setf (gamepad-attach-state-enter state) t
                (gamepad-attach-state-enabled state) t
                (gamepad-attach-state-exit state) nil)
          (setf state (make-gamepad-attach-state :enter t :enabled t)))
      (push input (u:href (entering data) :gamepad)))))

(defun gamepad-attach-transition-out (data gamepad-id)
  (let ((input (list :attach gamepad-id)))
    (u:when-let ((state (u:href (states data) input)))
      (setf (gamepad-attach-state-enter state) nil
            (gamepad-attach-state-enabled state) nil
            (gamepad-attach-state-exit state) t)
      (push input (u:href (exiting data) :gamepad)))))

(defun gamepad-enable-entering (data)
  (symbol-macrolet ((entering (u:href (entering data) :gamepad)))
    (dolist (input entering)
      (let ((state (u:href (states data) input)))
        (setf (gamepad-attach-state-enter state) nil
              (gamepad-attach-state-enabled state) t
              (gamepad-attach-state-exit state) nil)))
    (setf entering nil)))

(defun gamepad-disable-exiting (data)
  (symbol-macrolet ((exiting (u:href (exiting data) :gamepad)))
    (dolist (input exiting)
      (let ((state (u:href (states data) input)))
        (setf (gamepad-attach-state-enter state) nil
              (gamepad-attach-state-enabled state) nil
              (gamepad-attach-state-exit state) nil)))
    (setf exiting nil)))

(defun get-gamepad-by-instance (data instance)
  (u:href (gamepad-instances data) instance))

(defun generate-gamepad-id (data)
  (with-slots (%gamepad-instances %detached-gamepads) data
    (or (pop %detached-gamepads)
        (u:format-symbol :keyword "GAMEPAD~d"
                         (1+ (hash-table-count %gamepad-instances))))))

(defun prepare-gamepads ()
  (let ((database (resolve-system-path "gamepad-db.txt")))
    (sdl2:game-controller-add-mappings-from-file (namestring database))
    (sdl2-ffi.functions:sdl-set-hint
     sdl2-ffi:+sdl-hint-joystick-allow-background-events+ "1")))

(defun shutdown-gamepads (core)
  (u:when-let* ((data (input-data core))
                (instances (gamepad-instances data)))
    (u:do-hash-values (v instances)
      (sdl2:game-controller-close (gamepad-handle v)))
    (clrhash instances)))

(defun normalize-gamepad-analog-value (sub-device axis value)
  (if (eq sub-device :triggers)
      (u:map-domain 0 32767 0 1 value)
      (let ((clamped (u:clamp value -32767 32767)))
        (ecase axis
          (:x (u:map-domain -32767 32767 -1 1 clamped))
          (:y (u:map-domain -32767 32767 1 -1 clamped))))))

(defun %on-gamepad-attach (data index)
  (with-slots (%gamepad-instances %gamepad-ids) data
    (when (sdl2:game-controller-p index)
      (let* ((handle (sdl2:game-controller-open index))
             (instance (sdl2:game-controller-instance-id handle))
             (id (generate-gamepad-id data))
             (gamepad (make-gamepad :id id
                                    :instance instance
                                    :name (sdl2:game-controller-name handle)
                                    :handle handle)))
        (setf (u:href %gamepad-instances instance) gamepad
              (u:href %gamepad-ids id) gamepad)
        (gamepad-attach-transition-in data (list :attach id))))))

(defun %on-gamepad-detach (data instance)
  (with-slots (%gamepad-instances %gamepad-ids %detached-gamepads) data
    (let* ((gamepad (u:href %gamepad-instances instance))
           (id (gamepad-id gamepad)))
      (sdl2:game-controller-close (gamepad-handle gamepad))
      (u:appendf %detached-gamepads (list id))
      (remhash id %gamepad-ids)
      (remhash instance %gamepad-instances)
      (gamepad-attach-transition-out data (list :attach id)))))

(defun on-gamepad-button-up (data instance button)
  (let ((id (gamepad-id (get-gamepad-by-instance data instance))))
    (button-transition-out data (list id button))
    (button-transition-out data (list id :any))
    (button-transition-out data '(:button :any))))

(defun on-gamepad-button-down (data instance button)
  (let ((id (gamepad-id (get-gamepad-by-instance data instance))))
    (button-transition-in data (list id button))
    (button-transition-in data (list id :any))
    (button-transition-in data '(:button :any))))

(defgeneric get-gamepad-analog (context deadzone input))

(defmethod get-gamepad-analog (context (deadzone (eql :axial)) input)
  (let ((data (input-data (core context))))
    (u:if-found (state (u:href (states data) input))
      (let ((x (gamepad-analog-state-x state))
            (y (gamepad-analog-state-y state))
            (deadzone (gamepad-analog-state-deadzone state)))
        (values (if (< (abs x) deadzone) 0f0 x)
                (if (< (abs y) deadzone) 0f0 y)))
      (values 0f0 0f0))))

(defmethod get-gamepad-analog (context (deadzone (eql :radial)) input)
  (let ((data (input-data (core context))))
    (u:if-found (state (u:href (states data) input))
      (let ((x (gamepad-analog-state-x state))
            (y (gamepad-analog-state-y state))
            (deadzone (gamepad-analog-state-deadzone state)))
        (v2:with-components ((v (v2:vec x y)))
          (if (< (v2:length v) deadzone)
              (values 0f0 0f0)
              (values vx vy))))
      (values 0f0 0f0))))

(defmethod get-gamepad-analog (context (deadzone (eql :radial-scaled)) input)
  (let ((data (input-data (core context))))
    (u:if-found (state (u:href (states data) input))
      (let ((x (gamepad-analog-state-x state))
            (y (gamepad-analog-state-y state))
            (deadzone (gamepad-analog-state-deadzone state)))
        (v2:with-components ((v (v2:vec x y)))
          (let ((length (v2:length v)))
            (if (< length deadzone)
                (values 0f0 0f0)
                (progn
                  (v2:scale! v
                             (v2:normalize v)
                             (/ (- length deadzone) (- 1 deadzone)))
                  (values vx vy))))))
      (values 0f0 0f0))))

(defun on-gamepad-analog-move (data instance axis value)
  (destructuring-bind (sub-device axis) axis
    (let* ((gamepad (get-gamepad-by-instance data instance))
           (key (list (gamepad-id gamepad) sub-device))
           (value (normalize-gamepad-analog-value sub-device axis value)))
      (symbol-macrolet ((state (u:href (states data) key)))
        (if (not state)
            (setf state (make-gamepad-analog-state :x 0f0 :y 0f0 :deadzone 0f0))
            (ecase axis
              (:x (setf (gamepad-analog-state-x state) value))
              (:y (setf (gamepad-analog-state-y state) value))))))))

(defun on-gamepad-attach (context gamepad-id)
  (u:when-let* ((data (input-data (core context)))
                (state (u:href (states data) (list :attach gamepad-id))))
    (gamepad-attach-state-enter state)))

(defun on-gamepad-enabled (context gamepad-id)
  (u:when-let* ((data (input-data (core context)))
                (state (u:href (states data) (list :attach gamepad-id))))
    (gamepad-attach-state-enabled state)))

(defun on-gamepad-detach (context gamepad-id)
  (u:when-let* ((data (input-data (core context)))
                (state (u:href (states data) (list :attach gamepad-id))))
    (gamepad-attach-state-exit state)))
