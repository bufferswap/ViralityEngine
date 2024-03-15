(in-package #:vumbra.shaping)

;;;; Shaping functions
;;;; Various functions to modify a signal or interpolate a value non-linearly.
;;;; Golan Levin http://www.flong.com/texts/code/

;;; Exponential shaping functions
;;; Golan Levin http://www.flong.com/texts/code/shapers_exp/

(defun exponential-emphasis ((x :float)
                             (a :float))
  (let ((a (max +epsilon+ (min (- 1 +epsilon+) a))))
    (if (< a 0.5)
        (expt x (* 2 a))
        (expt x (/ (- 1 (* 2 (- a 0.5))))))))

(defun double-exponential-seat ((x :float)
                                (a :float))
  (let ((a (min (- 1 +epsilon+) (max +epsilon+ a))))
    (if (<= x 0.5)
        (/ (expt (* 2 x) (- 1 a)) 2)
        (- 1 (/ (expt (* 2 (- 1 x)) (- 1 a)) 2)))))

(defun double-exponential-sigmoid ((x :float)
                                   (a :float))
  (let ((a (- 1 (min (- 1 +epsilon+) (max +epsilon+ a)))))
    (if (<= x 0.5)
        (/ (expt (* 2 x) (/ a)) 2)
        (- 1 (/ (expt (* 2 (- 1 x)) (/ a)) 2)))))

(defun logistic-sigmoid ((x :float)
                         (a :float))
  (let* ((a (1- (/ (- 1 (max +epsilon+ (min (- 1 +epsilon+) a))))))
         (a2 (/ (1+ (exp (- (* (- x 0.5) a 2.0))))))
         (b (/ (1+ (exp a))))
         (c (/ (1+ (exp (- a))))))
    (/ (- a2 b) (- c b))))

;;; Circular & elliptical shaping functions
;;; Golan Levin http://www.flong.com/texts/code/shapers_circ/

(defun double-circle-seat ((x :float)
                           (a :float))
  (let* ((a (max 0 (min 1 a)))
         (b (* (- x a) (- x a)))
         (c (* (- 1 a) (- 1 a))))
    (if (<= x a)
        (sqrt (- (* a a) b))
        (- 1 (sqrt (- c b))))))

(defun double-circle-sigmoid ((x :float)
                              (a :float))
  (let* ((a (max 0 (min 1 a)))
         (b (* (- 1 a) (- 1 a)))
         (c (* (1- x) (1- x))))
    (if (<= x a)
        (- a (sqrt (- (* a a) (* x x))))
        (+ a (sqrt (- b c))))))

(defun double-elliptical-seat ((x :float)
                               (a :float)
                               (b :float))
  (let* ((a (max +epsilon+ (min (- 1 +epsilon+) a)))
         (b (max 0 (min 1 b)))
         (c (* (- x a) (- x a)))
         (d (* (- 1 a) (- 1 a))))
    (if (<= x a)
        (* (/ b a) (sqrt (- (* a a) c)))
        (- 1 (* (/ (- 1 b) (- 1 a)) (sqrt (- d c)))))))

(defun double-elliptical-sigmoid ((x :float)
                                  (a :float)
                                  (b :float))
  (let* ((a (max +epsilon+ (min (- 1 +epsilon+) a)))
         (b (max 0 (min 1 b)))
         (c (* (- 1 a) (- 1 a)))
         (d (* (1- x) (1- x))))
    (if (<= x a)
        (* b (- 1 (/ (sqrt (- (* a a) (* x x))) a)))
        (+ b (* (/ (- 1 b) (- 1 a)) (sqrt (- c d)))))))

;;; Polynomial shaping functions
;;; Golan Levin http://www.flong.com/texts/code/shapers_poly/

(defun blinn-wyvill-raised-inverted-cosine ((x :float))
  (let* ((x2 (* x x))
         (x4 (* x2 x2))
         (x6 (* x4 x2)))
    (+ (- (* (/ 4 9.0) x6) (* (/ 17 9.0) x4))
       (* (/ 22 9.0) x2))))

(defun double-cubic-seat ((x :float)
                          (a :float)
                          (b :float))
  (let ((a (min (- 1 +epsilon+) (max +epsilon+ a)))
        (b (min 1 (max 0 b))))
    (if (<= x a)
        (- b (* b (expt (- 1 (/ x a)) 3.0)))
        (+ b (* (- 1 b) (expt (/ (- x a) (- 1 a)) 3.0))))))

(defun double-cubic-seat/linear-blend ((x :float)
                                       (a :float)
                                       (b :float))
  (let ((a (min (- 1 +epsilon+) (max +epsilon+ a)))
        (b (- 1 (min 1 (max 0 b)))))
    (if (<= x a)
        (+ (* b x) (* (- 1 b) a (- 1 (expt (- 1 (/ x a)) 3.0))))
        (+ (* b x)
           (* (- 1 b) (+ a (* (- 1 a) (expt (/ (- x a) (- 1 a)) 3.0))))))))

(defun double-odd-polynomial-seat ((x :float)
                                   (a :float)
                                   (b :float)
                                   (n :uint))
  (let ((a (min (- 1 +epsilon+) (max +epsilon+ a)))
        (b (min 1 (max 0 b)))
        (p (1+ (* 2 n))))
    (if (<= x a)
        (- b (* b (expt (- 1 (/ x a)) p)))
        (+ b (* (- 1 b) (expt (/ (- x a) (- 1 a)) p))))))

(defun quadratic-point ((x :float)
                        (a :float)
                        (b :float))
  (let* ((a (min (- 1 +epsilon+) (max +epsilon+ a)))
         (b (min 1 (max 0 b)))
         (a2 (- (/ (- 1 b) (- 1 a)) (/ b a)))
         (b2 (/ (- (* a2 a a) b) a))
         (y (- (* a2 x x) (* b2 x))))
    (min 1 (max 0 y))))
