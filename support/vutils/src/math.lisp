(in-package #:vutils)

(declaim (inline degrees->radians))
(defun degrees->radians (degrees)
  "Convert `DEGREES` to radians."
  (* degrees #.(/ pi 180)))

(declaim (inline radians->degrees))
(defun radians->degrees (radians)
  "Convert `RADIANS` to degrees."
  (* radians #.(/ 180 pi)))

(defun map-domain (source-min source-max dest-min dest-max value)
  "Map `VALUE` from the domain denoted by `SOURCE-MIN` and `SOURCE-MAX` to the
domain denoted by `DEST-MIN` and `DEST-MAX`."
  (alexandria:lerp (/ (- value source-min)
                      (- source-max source-min))
                   dest-min
                   dest-max))

(defun average (&rest numbers)
  "Calculate the mean average of `NUMBERS`, a list of numbers."
  (/ (reduce #'+ numbers) (length numbers)))
