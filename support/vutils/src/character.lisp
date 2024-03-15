(in-package #:vutils)

(defun ascii-lowercase-p (char)
  "Check if the character `CHAR` is a lowercase ASCII character."
  (char<= #\a char #\z))

(defun ascii-uppercase-p (char)
  "Check if the character `CHAR` is an uppercase ASCII character."
  (char<= #\A char #\Z))

(defun ascii-letter-p (char)
  "Check if the character `CHAR` is an ASCII letter character."
  (or (ascii-lowercase-p char)
      (ascii-uppercase-p char)))

(defun ascii-number-p (char)
  "Check if the character `CHAR` is an ASCII number character."
  (char<= #\0 char #\9))

(defun ascii-alphanumeric-p (char)
  "Check if the character `CHAR` is an ASCII alpha-numeric character."
  (or (ascii-letter-p char)
      (ascii-number-p char)))

(defun null-char-p (char)
  (char= char #\Nul))

(defun ascii-control-p (char)
  "Check if the character `CHAR` is an ASCII control character."
  (let ((code (char-code char)))
    (or (<= 0 code 31)
        (= code 127))))

(defun hex-char-p (char)
  "Check if the character `CHAR` is a hexadecimal character."
  (or (ascii-number-p char)
      (char<= #\A char #\F)))
