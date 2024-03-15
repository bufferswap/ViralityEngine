(in-package #:virality)

(defmacro event-case ((event) &body handlers)
  (let (events)
    (dolist (handler handlers)
      (destructuring-bind (type options . body) handler
        (let ((body (list*
                     `(declare (ignorable ,@(u:plist-values options)))
                     body)))
          (dolist (type (u:ensure-list type))
            (u:when-let ((x (sdl2::expand-handler event type options body)))
              (push x events))))))
    `(case (sdl2:get-event-type ,event)
       ,@(nreverse events))))

(defun dispatch-event (data event)
  (event-case (event)
    (:windowevent
     (:event event-type :data1 data1 :data2 data2)
     (case (aref +window-event-names+ event-type)
       (:show (on-window-show data))
       (:hide (on-window-hide data))
       (:move (on-window-move data :x data1 :y data2))
       (:resize (on-window-resize data :width data1 :height data2))
       (:minimize (on-window-minimize data))
       (:maximize (on-window-maximize data))
       (:restore (on-window-restore data))
       (:mouse-focus-enter (on-window-mouse-focus-enter data))
       (:mouse-focus-leave (on-window-mouse-focus-exit data))
       (:keyboard-focus-enter (on-window-keyboard-focus-enter data))
       (:keyboard-focus-leave (on-window-keyboard-focus-exit data))
       (:close (on-window-close data))))
    (:mousebuttonup
     (:button button)
     (on-mouse-button-up data (aref +mouse-button-names+ button)))
    (:mousebuttondown
     (:button button)
     (on-mouse-button-down data (aref +mouse-button-names+ button)))
    (:mousewheel
     (:x x :y y)
     (on-mouse-scroll data x y))
    (:mousemotion
     (:x x :y y :xrel dx :yrel dy)
     (on-mouse-move data x y dx dy))
    (:keyup
     (:keysym keysym :repeat repeat)
     (when (zerop repeat)
       (on-key-up data (aref +key-names+ (sdl2:scancode-value keysym)))))
    (:keydown
     (:keysym keysym :repeat repeat)
     (when (zerop repeat)
       (on-key-down data (aref +key-names+ (sdl2:scancode-value keysym)))))
    (:controllerdeviceadded
     (:which gamepad-id)
     (%on-gamepad-attach data gamepad-id))
    (:controllerdeviceremoved
     (:which gamepad-id)
     (%on-gamepad-detach data gamepad-id))
    (:controlleraxismotion
     (:which gamepad-id :axis axis :value value)
     (on-gamepad-analog-move
      data gamepad-id (aref +gamepad-axis-names+ axis) value))
    (:controllerbuttonup
     (:which gamepad-id :button button)
     (on-gamepad-button-up
      data gamepad-id (aref +gamepad-button-names+ button)))
    (:controllerbuttondown
     (:which gamepad-id :button button)
     (on-gamepad-button-down
      data gamepad-id (aref +gamepad-button-names+ button)))))

(defun perform-input-state-tasks (input-data)
  (button-enable-entering input-data)
  (button-disable-exiting input-data)
  (gamepad-enable-entering input-data)
  (gamepad-disable-exiting input-data)
  (reset-mouse-state input-data))

(defun handle-events (input-data)
  (perform-input-state-tasks input-data)
  (loop :with event = (sdl2:new-event)
        :until (zerop (sdl2:next-event event :poll))
        :do (dispatch-event input-data event)
        :finally (sdl2:free-event event)))
