(in-package #:cl-user)

(defpackage #:vorigin.constants
  (:local-nicknames
   (#:u #:vutils))
  (:use #:cl)
  (:shadow
   #:pi)
  (:export
   #:+rad+
   #:+rad/double+
   #:+deg+
   #:+deg/double+
   #:2pi
   #:2pi/3
   #:2pi/12
   #:3pi/2
   #:3pi/4
   #:3pi/12
   #:4pi/3
   #:4pi/12
   #:5pi/3
   #:5pi/4
   #:5pi/6
   #:5pi/12
   #:6pi/12
   #:7pi/4
   #:7pi/6
   #:7pi/12
   #:8pi/12
   #:9pi/12
   #:10pi/12
   #:11pi/6
   #:11pi/12
   #:12pi/12
   #:13pi/12
   #:14pi/12
   #:15pi/12
   #:16pi/12
   #:17pi/12
   #:18pi/12
   #:19pi/12
   #:20pi/12
   #:21pi/12
   #:22pi/12
   #:23pi/12
   #:24pi/12
   #:pi
   #:pi/2
   #:pi/3
   #:pi/4
   #:pi/6
   #:pi/12))

(in-package #:vorigin.constants)

(u:define-constant pi/12 (float (/ cl:pi 12) 1f0))
(u:define-constant pi/6 (float (/ cl:pi 6) 1f0))
(u:define-constant 2pi/12 pi/6)
(u:define-constant pi/4 (float (/ cl:pi 4) 1f0))
(u:define-constant 3pi/12 pi/4)
(u:define-constant pi/3 (float (/ cl:pi 3) 1f0))
(u:define-constant 4pi/12 pi/3)
(u:define-constant 5pi/12 (float (/ (* cl:pi 5) 12) 1f0))
(u:define-constant pi/2 (float (/ cl:pi 2) 1f0))
(u:define-constant 6pi/12 pi/2)
(u:define-constant 7pi/12 (float (/ (* cl:pi 7) 12) 1f0))
(u:define-constant 2pi/3 (float (/ (* cl:pi 2) 3) 1f0))
(u:define-constant 8pi/12 2pi/3)
(u:define-constant 3pi/4 (float (/ (* cl:pi 3) 4) 1f0))
(u:define-constant 9pi/12 3pi/4)
(u:define-constant 5pi/6 (float (/ (* cl:pi 5) 6) 1f0))
(u:define-constant 10pi/12 5pi/6)
(u:define-constant 11pi/12 (float (/ (* cl:pi 11) 12) 1f0))
(u:define-constant pi (float cl:pi 1f0))
(u:define-constant 12pi/12 pi)
(u:define-constant 13pi/12 (float (/ (* cl:pi 13) 12) 1f0))
(u:define-constant 7pi/6 (float (/ (* cl:pi 7) 6) 1f0))
(u:define-constant 14pi/12 7pi/6)
(u:define-constant 5pi/4 (float (/ (* cl:pi 5) 4) 1f0))
(u:define-constant 15pi/12 5pi/4)
(u:define-constant 4pi/3 (float (/ (* cl:pi 4) 3) 1f0))
(u:define-constant 16pi/12 4pi/3)
(u:define-constant 17pi/12 (float (/ (* cl:pi 17) 12) 1f0))
(u:define-constant 3pi/2 (float (/ (* cl:pi 3) 2) 1f0))
(u:define-constant 18pi/12 3pi/2)
(u:define-constant 19pi/12 (float (/ (* cl:pi 19) 12) 1f0))
(u:define-constant 5pi/3 (float (/ (* cl:pi 5) 3) 1f0))
(u:define-constant 20pi/12 5pi/3)
(u:define-constant 7pi/4 (float (/ (* cl:pi 7) 4) 1f0))
(u:define-constant 21pi/12 7pi/4)
(u:define-constant 11pi/6 (float (/ (* cl:pi 11) 6) 1f0))
(u:define-constant 22pi/12 11pi/6)
(u:define-constant 23pi/12 (float (/ (* cl:pi 23) 12) 1f0))
(u:define-constant 2pi (float (* cl:pi 2) 1f0))
(u:define-constant 24pi12 2pi)
(u:define-constant +rad+ (float (/ 180 cl:pi) 1f0))
(u:define-constant +rad/double+ (/ 180 cl:pi))
(u:define-constant +deg+ (float (/ cl:pi 180) 1f0))
(u:define-constant +deg/double+ (/ cl:pi 180))
