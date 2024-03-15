(in-package #:vutils)

(defun string-merge (&rest strings)
  "Merge `STRINGS` into a single string."
  (apply #'concatenate 'string strings))

(defun string-trim-whitespace (string)
  "Trim leading and trailing whitespace characters from `STRING`."
  (let ((chars '(#\space #\tab #\linefeed #\return #\newline #\page)))
    (string-trim chars string)))

(defun string-explode (string)
  "Explode `STRING` into a list of its characters."
  (map 'list #'identity string))

(defun string-starts-with-p (string prefix)
  "Check if `STRING` starts with `PREFIX`."
  (let ((prefix-length (length prefix)))
    (when (<= prefix-length (length string))
      (string= prefix (subseq string 0 prefix-length)))))

(defun string-ends-with-p (string suffix)
  "Check if `STRING` ends with `SUFFIX`."
  (let ((length (length string))
        (suffix-length (length suffix)))
    (when (<= suffix-length length)
      (string= suffix (subseq string (- length suffix-length) length)))))

(defun string->keyword (string)
  "Convert `STRING` to a keyword symbol."
  (alexandria:format-symbol :keyword "~@:(~a~)" string))

(defun split-string (string delimiter)
  "Split `STRING` into a list of two parts; everything before the first
`DELIMITER` character, and everything after it."
  (let ((pos (position delimiter string)))
    (list (subseq string 0 pos)
          (subseq string (1+ pos)))))
