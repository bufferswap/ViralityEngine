(in-package #:virality.texture)

;;; NOTE: This code is likely to really change a lot, since texture buffer
;;; objects are implemented very differently than regular textures.

(defmethod load-texture-data ((texture-type (eql :texture-buffer)) texture
                              context)
  ;; NOTE: this one might be a little harder to get right, since the
  ;; resource-cache stuff might end up being wrong since this is a buffer
  ;; object, not a traditional texture. So pay attention while implementing
  ;; this one.
  (error "load-texture-data: :texture-buffer implement me")
  nil)
