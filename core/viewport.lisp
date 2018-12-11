(in-package :%fl)

(defmethod generate-viewport ((display display) position size)
  (with-cfg (window-width window-height) (context (core-state display))
    (let ((map-fn (fl.util:op (fl.util:map-domain 0 1 0 _ _))))
      (flm:with-vec2 ((p position) (s size))
        (gl:viewport (funcall map-fn window-width p.x)
                     (funcall map-fn window-height p.y)
                     (funcall map-fn window-width s.x)
                     (funcall map-fn window-height s.y))))))