(in-package :fl.host)

(defmethod initialize-host ((host (eql :sdl2)))
  (let ((flags '(:everything)))
    (unless (apply #'sdl2:was-init flags)
      (let ((flags (autowrap:mask-apply 'sdl2::sdl-init-flags flags)))
        (sdl2::check-rc (sdl2::sdl-init flags))))))

(defmethod shutdown-host ((host (eql :sdl2)))
  (let ((channel sdl2::*main-thread-channel*))
    (sdl2::sdl-quit)
    (setf sdl2::*main-thread-channel* nil
          sdl2::*lisp-message-event* nil)
    (when channel
      (sdl2::sendmsg channel nil))))

(defmethod create-window ((host (eql :sdl2)) title width height)
  (let ((flags '(:opengl)))
    (sdl2:create-window :title title :w width :h height :flags flags)))

(defmethod create-opengl-context ((host (eql :sdl2)) window major-version minor-version)
  (sdl2:gl-set-attrs :context-profile-mask sdl2-ffi::+sdl-gl-context-profile-core+
                     :context-major-version major-version
                     :context-minor-version minor-version)
  (sdl2:gl-create-context window))

(defmethod close-window ((host (eql :sdl2)) window)
  (sdl2:destroy-window window))

(defmethod get-refresh-rate ((host (eql :sdl2)) window)
  (declare (ignore window))
  (nth-value 3 (sdl2:get-current-display-mode 0)))

(defmethod redraw-window ((host (eql :sdl2)) window)
  (sdl2:gl-swap-window window))

(defmethod set-draw-mode ((host (eql :sdl2)) mode)
  (ecase mode
    (:immediate (sdl2:gl-set-swap-interval 0))
    (:sync (sdl2:gl-set-swap-interval 1))))

(defmethod get-window-title ((host (eql :sdl2)) window)
  (sdl2:get-window-title window))

(defmethod set-window-title ((host (eql :sdl2)) window title)
  (sdl2:set-window-title window title))

(defmethod get-window-size ((host (eql :sdl2)) window)
  (multiple-value-list (sdl2:get-window-size window)))

(defmethod set-window-size ((host (eql :sdl2)) window width height)
  (sdl2:set-window-size window width height))

(defmethod get-window-mode ((host (eql :sdl2)) window)
  (if (member :fullscreen-desktop (sdl2:get-window-flags window))
      :fullscreen
      :windowed))

(defmethod set-window-mode ((host (eql :sdl2)) window mode)
  (ecase mode
    (:fullscreen (sdl2:set-window-fullscreen window :desktop))
    (:windowed (sdl2:set-window-fullscreen window :windowed))))

(defmethod set-window-hidden ((host (eql :sdl2)) window)
  (sdl2:hide-window window))

(defmethod set-window-visible ((host (eql :sdl2)) window)
  (sdl2:show-window window))

(defmethod set-cursor-hidden ((host (eql :sdl2)))
  (sdl2:hide-cursor))

(defmethod set-cursor-visible ((host (eql :sdl2)))
  (sdl2:show-cursor))

(defmacro event-case ((event) &body handlers)
  `(case (sdl2:get-event-type ,event)
     ,@(au:collecting
         (dolist (handler handlers)
           (destructuring-bind (type options . body) handler
             (let ((body (list* `(declare (ignorable ,@(au:plist-values options))) body)))
               (dolist (type (au:ensure-list type))
                 (au:when-let ((x (sdl2::expand-handler event type options body)))
                   (collect x)))))))))

(defmethod on-event ((host (eql :sdl2)) event core-state)
  (event-case (event)
    ((:mousebuttondown :mousebuttonup)
     (:which id :timestamp ts :button button :state state :clicks clicks :x x :y y))

    (:mousewheel
     (:which id :timestamp ts :x x :y y))

    (:mousemotion
     (:which id :timestamp ts :state state :x x :y y :xrel xrel :yrel yrel))

    ((:keydown :keyup)
     (:timestamp ts :state state :repeat repeat :keysym keysym)
     (let ((key (fl.host:get-key-name (sdl2:scancode-value keysym))))
       (format t "~s~%" key)
       (when (eq key :escape)
         (fl.core:stop-engine core-state))))))

(defmethod handle-events ((host (eql :sdl2)) core-state)
  (loop :with event = (sdl2:new-event)
        :until (zerop (sdl2:next-event event :poll))
        :do (on-event host event core-state)
        :finally (sdl2:free-event event)))
