(in-package #:virality)

;;;; implementation of DISPLAY structure

(defun parse-opengl-version ()
  (mapcar #'parse-integer
          (split-sequence:split-sequence #\. =opengl-version=)))

(defun set-opengl-attributes ()
  (destructuring-bind (major minor) (parse-opengl-version)
    (sdl2:gl-set-attrs :context-major-version major
                       :context-minor-version minor
                       :context-profile-mask 1
                       :multisamplebuffers 1
                       :multisamplesamples 16)))

(defun make-opengl-context (display)
  (let ((context (sdl2:gl-create-context (window display))))
    (setf (context display) context)
    (apply #'gl:enable +enabled-capabilities+)
    (apply #'gl:disable +disabled-capabilities+)
    (apply #'gl:blend-func +blend-mode+)
    (gl:depth-func +depth-mode+)))

(defun make-window ()
  ;; opengl attributes must be set BEFORE SDL window creation.
  (set-opengl-attributes)
  (sdl2:create-window :title =window-title=
                      :w (truncate =window-width=)
                      :h (truncate =window-height=)
                      :flags '(:opengl)))

(defun make-display (core)
  ;; (sdl2:init :everything)
  ;; NOTE: We can't call (sdl2:init :everything) because it tries to manage the
  ;; main thread itself and when there is an ABORT restart in V, will get
  ;; confused and lock up. Since V does the thread management itself, we the
  ;; the lower level raw equvalent of it in the SDL2 CFFI.  TODO: Prolly should
  ;; export this in SDL2 and/or make it easier to use.
  (sdl2::sdl-init #xf231)
  (let* ((refresh-rate (nth-value 3 (sdl2:get-current-display-mode 0)))
         (resolution (v2:vec* =window-width= =window-height=))
         (display (make-instance 'display
                                 :window (make-window)
                                 :refresh-rate refresh-rate
                                 :resolution resolution)))
    ;; opengl context must be created AFTER SDL window creation.
    (make-opengl-context display)
    (sdl2:gl-set-swap-interval (if =vsync= 1 0))
    (if =allow-screensaver=
        (sdl2:enable-screensaver)
        (sdl2:disable-screensaver))
    (setf (slot-value core '%display) display)))

(defun kill-display (core)
  (u:when-let ((display (display core)))
    (sdl2:gl-delete-context (context display))
    (sdl2:destroy-window (window display)))
  ;; NOTE: Same issue with sdl2:quit as with sdl2:sdl-init above...
  (sdl2:sdl-quit))

;; TODO: The functions below are not finalized yet. The rendering pipeline need
;; to be reworked first. ~axion 4/8/2020

(defun clear-screen (core)
  (multiple-value-call #'gl:clear-color
    (if (eq =log-level= :debug)
        (values (* 0.25 (abs (sin (total-time (context core))))) 0 0 1)
        (values 0 0 0 1)))
  (gl:clear :color-buffer :depth-buffer))

(defun render-frame (core)
  (with-slots (%clock %display %running-p) core
    (when %running-p
      (clear-screen core)
      (execute-flow core
                    :default
                    'perform-one-frame
                    'entry/perform-one-frame
                    :come-from-state-name :ef)
      (sdl2:gl-swap-window (window %display))
      (incf (clock-frame-count %clock))
      (when *profile*
        (incf =profile-frame-counter=)))))

(defun get-resolution (context)
  (resolution (display (core context))))
