(in-package #:virality)

(defun map-font-glyphs (font-spec func string)
  (loop :with w = (float (font:scale-w font-spec) 1f0)
        :with h = (float (font:scale-h font-spec) 1f0)
        :with x = 0
        :with y = (font:line-height font-spec)
        :with kernings = (font:kernings font-spec)
        :with line-height = (font:line-height font-spec)
        :with line-widths = '(0)
        :with space-width = (font:space-size font-spec)
        :for i :from 0 :below (length string)
        :for char = (aref string i)
        :for char-data = (font:char-data char font-spec)
        :for p = nil :then char
        :for kerning = (font:%kerning kernings p char)
        :do (case char
              (#\newline
               (push x line-widths)
               (setf x 0)
               (incf y line-height))
              (#\space
               (incf x space-width))
              (#\tab
               (incf x (* 8 space-width)))
              (t
               (incf x kerning)
               (let* ((dxy (font:glyph-origin char-data))
                      (dx (aref dxy 0))
                      (dy (aref dxy 1))
                      (cw (font:glyph-width char-data))
                      (ch (font:glyph-height char-data))
                      (x- (+ x dx))
                      (y- (+ y dy))
                      (x+ (+ x- cw))
                      (y+ (+ y- ch))
                      (cx (font:glyph-x char-data))
                      (cy (font:glyph-y char-data))
                      (u- (/ cx w))
                      (v+ (/ cy h))
                      (u+ (/ (+ cx cw) w))
                      (v- (/ (+ cy ch) h)))
                 (psetf y- (- line-height y+)
                        y+ (- line-height y-))
                 (funcall func x- y- x+ y+ u- v- u+ v+))
               (incf x (font:glyph-xadvance char-data))))
        :finally (return (values (float (/ (max x (apply #'max line-widths))
					   2f0)
                                        1f0)
                                 (float (/ y 2f0) 1f0)))))

(defun calculate-font-position (spec position dimensions offset)
  (let* ((line-height (float (font:line-height spec) 1f0))
         (viewport-size (v2:vec 1920f0 1080f0))
         (dims (v2:/ (v2:scale dimensions 2f0) viewport-size))
         (offset (v2:scale (v2:/ offset viewport-size) line-height)))
    (case position
      (:top-left
       (v2:+ (v2:vec -1f0 1f0) offset))
      (:top-right
       (v2:+ (v2:vec (- 1f0 (v2:x dims)) 1f0) offset))
      (:bottom-left
       (v2:+ (v2:vec -1f0 (1- (v2:y dims))) offset))
      (:bottom-right
       (v2:+ (v2:vec (- 1f0 (v2:x dims)) (1- (v2:y dims))) offset))
      (:center-left
       (v2:+ (v2:vec -1f0 (/ (v2:y dims) 2f0)) offset))
      (:center-right
       (v2:+ (v2:vec (- 1f0 (v2:x dims)) (/ (v2:y dims) 2f0)) offset))
      (:center-top
       (v2:+ (v2:vec (/ (v2:x dims) -2f0) 1f0) offset))
      (:center-bottom
       (v2:+ (v2:vec (/ (v2:x dims) -2f0) (1- (v2:y dims))) offset))
      (t (v2:+ (v2:/ dims (v2:vec -2f0 2f0)) offset)))))
