(in-package #:virality-examples)

;; A barely adequate example component to demonstrate scaling a different
;; actor around a scaling pivot point.
(v:define-component demo-scale-around ()
  ((%scale-increment :accessor scale-increment
                     :initarg :scale-increment
                     :initform (v3:uniform .01f0))
   (%target-to-scale-around-pivot :accessor target-to-scale-around-pivot
                                  :initarg :target-to-scale-around-pivot)))

(defmethod v:on-component-update ((self demo-scale-around))
  (let ((context (v:context self))
        (self-world-coords
          (v:transform-point self (v3:zero))))

    ;; TODO: If you change the (v:ref "/scale-around/holder" to just "holder"
    ;; this will happen.
    (when (null (target-to-scale-around-pivot self))
      (error "Why is target null here?"))

    (v:scale-around (target-to-scale-around-pivot self)
                    self-world-coords
                    (v3:scale (scale-increment self)
                              (* 3f0 (sin (* (v:total-time context) 4f0)))))))

;;; Prefabs

(v:define-prefab "scale-around" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera :zoom 3))

  ;; Placed such that in world space it'll be in the holder actors in an
  ;; interesting place. The pivot could be deep in some other coordinate
  ;; space. It doesn't matter.
  (("pivot" :copy "/mesh")
   (comp:transform :translate (v3:vec -2f0 2f0 0f0)
                   :scale (v3:vec .5f0 .5f0 .5f0))
   (comp:mesh :name "sphere")
   (demo-scale-around :target-to-scale-around-pivot
                      ;; TODO: Why does this have to be fully qualified?
                      ;; Sometimes it must be, other times it doesn't, it is so
                      ;; very confusing. As in sometimes it can just be
                      ;; "holder" in other prefabs. Maybe it matters if it is a
                      ;; :link or :copy versus an inplace definition?
                      (v:ref "/scale-around/holder")))


  ("holder"
   (("cube0" :copy "/mesh")
    (comp:transform :translate (v3:vec -5f0 5f0 0f0)
                    :rotate/velocity (v3:velocity (v3:ones) o:pi/2))
    (comp:mesh :name "cube"))

   (("cube1" :copy "/mesh")
    (comp:transform :translate (v3:vec 0f0 5f0 0f0)
                    :rotate/velocity (v3:velocity (v3:ones) o:pi/2))
    (comp:mesh :name "cube"))

   (("cube2" :copy "/mesh")
    (comp:transform :translate (v3:vec 5f0 5f0 0f0)
                    :rotate/velocity (v3:velocity (v3:ones) o:pi/2))
    (comp:mesh :name "cube"))

   (("cube3" :copy "/mesh")
    (comp:transform :translate (v3:vec -5f0 0f0 0f0)
                    :rotate/velocity (v3:velocity (v3:ones) o:pi/2))
    (comp:mesh :name "cube"))
   (("cube4" :copy "/mesh")
    (comp:transform :translate (v3:vec 0f0 0f0 0f0)
                    :rotate/velocity (v3:velocity (v3:ones) o:pi/2))
    (comp:mesh :name "cube"))

   (("cube5" :copy "/mesh")
    (comp:transform :translate (v3:vec 5f0 0f0 0f0)
                    :rotate/velocity (v3:velocity (v3:ones) o:pi/2))
    (comp:mesh :name "cube"))

   (("cube6" :copy "/mesh")
    (comp:transform :translate (v3:vec -5f0 -5f0 0f0)
                    :rotate/velocity (v3:velocity (v3:ones) o:pi/2))
    (comp:mesh :name "cube"))

   (("cube7" :copy "/mesh")
    (comp:transform :translate (v3:vec 0f0 -5f0 0f0)
                    :rotate/velocity (v3:velocity (v3:ones) o:pi/2))
    (comp:mesh :name "cube"))

   (("cube8" :copy "/mesh")
    (comp:transform :translate (v3:vec 5f0 -5f0 0f0)
                    :rotate/velocity (v3:velocity (v3:ones) o:pi/2))
    (comp:mesh :name "cube"))


   ))
