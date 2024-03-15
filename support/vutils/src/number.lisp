(in-package #:vutils)

(defun count-digits (integer)
  "Return the number of digits of `INTEGER`."
  (check-type integer integer)
  (if (zerop integer)
      1
      (1+ (floor (log (abs integer) 10)))))
