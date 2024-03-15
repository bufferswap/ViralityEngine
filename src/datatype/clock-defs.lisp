(in-package #:virality)

(defstruct (clock (:constructor %make-clock)
                  (:predicate nil)
                  (:copier nil))
  ;; Gaffer's "Fix Your Timestep" accumulator
  (accumulator 0d0 :type double-float)
  ;; Average frames per second (weighted average over last 10 seconds)
  (fps/average/10s 0d0 :type double-float)
  ;; Average frames per second (weighted average over last 30 seconds)
  (fps/average/30s 0d0 :type double-float)
  ;; Average frames per second (weighted average over last 60 seconds)
  (fps/average/60s 0d0 :type double-float)
  ;; Average frames per second over all frames since the game was started
  (fps/average 0d0 :type double-float)
  ;; Frames per second of the current frame
  (fps/current 0d0 :type double-float)
  ;; delta time smoothing variables
  (delta-buffer 0d0 :type double-float)
  ;; physics rate
  (delta-time (/ 30d0) :type double-float)
  ;; amount of time last frame took to render
  (frame-time 0d0 :type double-float)
  ;; number of frames since the game was started
  (frame-count 0 :type fixnum)
  ;; interpolation factor 0...1 from one physics step to the next
  (interpolation-factor 0d0 :type double-float)
  ;; amount of time spent in the last debugger invocation if any
  (pause-time 0d0 :type double-float)
  ;; the time in seconds the last periodic update has occurred
  (period-elapsed 0d0 :type double-float)
  ;; amount in seconds until the periodic update function is called, which
  ;; currently only includes unlocking the REPL
  (period-interval 0.25d0 :type double-float)
  ;; the time in seconds from the start of the game at the end of the previous
  ;; frame, excluding any time spent in any debugger invocation
  (previous-time 0d0 :type double-float)
  ;; the time in seconds from the start of the game at the end of the current
  ;; frame, excluding any time spent in any debugger invovation.
  ;; We present this as the total time of execution to the gamedev via the
  ;; total-time API call.
  (current-time 0d0 :type double-float)
  ;; a constant time recorded when the game first starts that every other time
  ;; variable is relative to
  (start-time 0 :type fixnum))
