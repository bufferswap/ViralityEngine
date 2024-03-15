(in-package #:vutils)

(defun doc (string)
  "Convenience function for writing docstrings; intended to be read-evaluation with `#.`."
  (format nil string))

(defun noop (&rest args)
  "Do nothing."
  (declare (ignore args))
  (values))
