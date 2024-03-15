(in-package #:vutils)

(defun initialize-rng ()
  "Initializes the PRNG's state, so that the same sequence is not generated each
time the image is started."
  (setf *random-state* (make-random-state t)))
