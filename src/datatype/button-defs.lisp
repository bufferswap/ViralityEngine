(in-package #:virality)

(defstruct (button-state
            (:predicate nil)
            (:copier nil))
  enter
  enabled
  exit)
