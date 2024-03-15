(in-package #:vumbra.graphing)

;;;; Graphing
;;;; Credits:
;;;; Mikael Hvidtfeldt Christensen
;;;; http://blog.hvidtfeldts.net/index.php/2011/07/plotting-high-frequency-functions-using-a-gpu/

(defun graph ((func (function (:float) :float))
              (uv :vec2)
              (xy-range :vec4)
              (line-style :vec4)
              (samples :int))
  (decf (.xz xy-range) (.ww line-style))
  (let* ((diff (- (.yw xy-range) (.xz xy-range)))
         (uv (+ (* uv diff) (.xz xy-range)))
         (max-dist (* (.w line-style) diff))
         (step (/ max-dist samples))
         (count 0.0)
         (my-samples 0))
    (dotimes (i samples)
      (let ((f (funcall func (+ (.x uv) (* i (.x step))))))
        (dotimes (j samples)
          (let ((diff (- f (+ (.y uv) (* j (.y step))))))
            (incf my-samples)
            (incf count (1- (* (step 0 diff) 2)))))))
    (* (- 1 (/ (abs count) my-samples))
       (vec4 (.xyz line-style) 1))))
