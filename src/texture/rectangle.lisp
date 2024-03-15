(in-package #:virality.texture)

(defmethod load-texture-data ((texture-type (eql :texture-rectangle))
                              texture context)
  (error "load-texture-data: :texture-rectangle implement me")
  ;; Determine if loading :image or :planar
  nil)
