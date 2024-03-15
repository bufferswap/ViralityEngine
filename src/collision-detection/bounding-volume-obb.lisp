(in-package #:virality)

;;;; Implementation of datatype: ORIENTED-BOUNDING-BOX

(defun make-oriented-bounding-box (center axes half-widths)
  (make-instance 'oriented-bounding-box
                 :center center
                 :axes axes
                 :half-widths half-widths))

(defun get-closest-point/obb-point (obb point)
  (with-slots (%center %axes %half-widths) obb
    (let ((d (v3:- point %center))
          (q (v3:copy %center)))
      (dotimes (i 3)
        (let ((dist (v3:dot d (m3:get-column %axes i)))
              (e (aref %half-widths i)))
          (when (> dist e)
            (setf dist e))
          (when (< dist (- e))
            (setf dist (- e)))
          (v3:+! q q (v3:scale (m3:get-column %axes i) dist))))
      q)))

(defun make-obb-obb-rotation (obb1 obb2)
  ;; TODO: This and the male-obb-obb-translation probably boild down to an
  ;; inverted transform multiply in order to get B into A's space. If so, we
  ;; should convert this code to that cause it'll be like 4 lines.
  (let ((axes1 (axes obb1))
        (axes2 (axes obb2)))
    (m3:with-components ((a (m3:id))
                         (b (m3:id)))
      (psetf a00 (v3:dot (m3:get-column axes1 0) (m3:get-column axes2 0))
             a10 (v3:dot (m3:get-column axes1 1) (m3:get-column axes2 0))
             a20 (v3:dot (m3:get-column axes1 2) (m3:get-column axes2 0))
             a01 (v3:dot (m3:get-column axes1 0) (m3:get-column axes2 1))
             a11 (v3:dot (m3:get-column axes1 1) (m3:get-column axes2 1))
             a21 (v3:dot (m3:get-column axes1 2) (m3:get-column axes2 1))
             a02 (v3:dot (m3:get-column axes1 0) (m3:get-column axes2 2))
             a12 (v3:dot (m3:get-column axes1 1) (m3:get-column axes2 2))
             a22 (v3:dot (m3:get-column axes1 2) (m3:get-column axes2 2)))

      (psetf b00 (+ (abs a00) 1e-7)
             b10 (+ (abs a10) 1e-7)
             b20 (+ (abs a20) 1e-7)
             b01 (+ (abs a01) 1e-7)
             b11 (+ (abs a11) 1e-7)
             b21 (+ (abs a21) 1e-7)
             b02 (+ (abs a02) 1e-7)
             b12 (+ (abs a12) 1e-7)
             b22 (+ (abs a22) 1e-7))
      (values a b))))

(defun make-obb-obb-translation (obb1 obb2)
  ;; TODO: See TODO in make-obb-obb-rotation.
  (let ((axes1 (axes obb1))
        (translation (v3:- (center obb2) (center obb1))))
    (v3:vec (v3:dot translation (m3:get-column axes1 0))
            (v3:dot translation (m3:get-column axes1 1))
            (v3:dot translation (m3:get-column axes1 2)))))
