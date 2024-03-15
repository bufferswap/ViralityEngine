(in-package #:virality)

(u:define-constant +window-event-names+
    #(nil :show :hide nil :move :resize nil :minimize :maximize :restore
      :mouse-focus-enter :mouse-focus-exit :keyboard-focus-enter
      :keyboard-focus-exit :close nil nil)
  :test #'equalp)

(defun on-window-show (data)
  (declare (ignore data)))

(defun on-window-hide (data)
  (declare (ignore data)))

(defun on-window-move (data &key x y)
  (declare (ignore data x y)))

(defun on-window-resize (data &key width height)
  (declare (ignore data width height)))

(defun on-window-minimize (data)
  (declare (ignore data)))

(defun on-window-maximize (data)
  (declare (ignore data)))

(defun on-window-restore (data)
  (declare (ignore data)))

(defun on-window-mouse-focus-enter (data)
  (declare (ignore data)))

(defun on-window-mouse-focus-exit (data)
  (declare (ignore data)))

(defun on-window-keyboard-focus-enter (data)
  (declare (ignore data)))

(defun on-window-keyboard-focus-exit (data)
  (declare (ignore data)))

(defun on-window-close (data)
  (declare (ignore data)))

(defun get-window-title (window)
  (sdl2:get-window-title window))

(defun set-window-title (window title)
  (sdl2:set-window-title window title))

(defun get-window-size (window)
  (multiple-value-list (sdl2:get-window-size window)))

(defun set-window-size (window width height)
  (sdl2:set-window-size window width height))

(defun get-window-mode (window)
  (if (member :fullscreen-desktop (sdl2:get-window-flags window))
      :fullscreen
      :windowed))

(defun set-window-mode (window mode)
  (ecase mode
    (:fullscreen (sdl2:set-window-fullscreen window :desktop))
    (:windowed (sdl2:set-window-fullscreen window :windowed))))

(defun set-window-hidden (window)
  (sdl2:hide-window window))

(defun set-window-visible (window)
  (sdl2:show-window window))
