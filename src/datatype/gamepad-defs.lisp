(in-package #:virality)

(defstruct (gamepad
            (:predicate nil)
            (:copier nil))
  id
  instance
  name
  handle)

(defstruct (gamepad-attach-state
            (:predicate nil)
            (:copier nil))
  enter
  enabled
  exit)

(defstruct (gamepad-analog-state
            (:predicate nil)
            (:copier nil))
  x
  y
  deadzone)
