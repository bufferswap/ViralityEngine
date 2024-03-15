(in-package #:virality-examples)

;;; Textures

(v:define-texture font (:texture-2d)
  (:data #((textures font))))

;;; Materials

(v:define-material font
  (:shader ex/shd:font
   :profiles (x:u-mvp)
   :uniforms
   ((:sampler 'font)
    (:color (v4:vec 0f0 1f0 0f0 0.75f0)))))

;;; Prefabs

;; The basic reusable text-display, :copy or :link this into other prefabs and
;; make changes as needed.
(v:define-prefab "default-text-display" (:library examples)
  ("text-container"
   (comp:geometry :name 'comp::text)
   (comp:font :asset '(metadata font)
              :text "Hello, World!")
   (comp:render :material 'font
                :slave (v:ref :self :component 'comp:geometry))))

;; Example use of the above prefab.
(v:define-prefab "text-wall-clock-time" (:library examples)
  (("camera" :copy "/cameras/ortho"))

  (("sign" :copy "/default-text-display")
   (comp:transform :scale (v3:uniform 5f0)
                   :translate (v3:vec 0f0 256f0 0f0))
   ;; override child component info
   ("text-container"
    (comp:font :text "Wall Clock Time")))

  (("wall-clock-time" :copy "/default-text-display")
   (comp:transform :scale (v3:uniform 5f0))
   ;; override child component info
   ("text-container"
    (comp:font :rate 0
               :text (lambda ()
                       (format nil "~5$"
                               (v:total-time (v:context (v:ref :self)))))))))

;; TODO: Make a "text-driver" component and put into toplevel of
;; "text-display" It has references to everything else and will delegate
;; setting :text :rate and whatever else. This is so all the machinery in
;; the "text-display" is hidden behind a single API for the user to use.
