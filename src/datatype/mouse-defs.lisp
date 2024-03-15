(in-package #:virality)

(defstruct (mouse-motion-state
            (:predicate nil)
            (:copier nil))
  relative
  (warp-x 0)
  (warp-y 0)
  (x 0)
  (y 0)
  (dx 0)
  (dy 0))
