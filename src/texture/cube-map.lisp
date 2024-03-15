(in-package #:virality.texture)

(defmethod load-texture-data ((texture-type (eql :texture-cube-map))
                              texture context)
  ;; TODO: Validate that all mip_0 cube faces are the same size. The width and
  ;; height must be identical (square) but need not be powers of two.
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (immutable-p (get-computed-applied-attribute texture :immutable))
         (texture-max-level (get-computed-applied-attribute
                             texture :texture-max-level))
         (texture-base-level (get-computed-applied-attribute
                              texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (data (get-computed-applied-attribute texture :data))
         (flip-y (get-computed-applied-attribute texture :flip-y))
         (images (read-mipmap-images
                  context data use-mipmaps-p :cube-map flip-y))
         (first-cube (aref images 0))
         (first-image (aref (second (aref first-cube 0)) 0))
         ;; TODO: This is not safe, need to check all of them.
         (num-mipmaps (length (second (aref first-cube 0)))))
    #++(:printv "Loading :texture-cube-map images = ~a" images)
    ;; Check to ensure they all fit into texture memory.
    ;; TODO: Refactor out of each method into validate-mipmap-images and
    ;; generalize.
    (loop :with max-size = v::=max-texture-size=
          :for (placement mipmaps) :across first-cube
          :for image = (aref mipmaps 0)
          :do (when (> (max (v::height image) (v::width image))
                       max-size)
                ;; TODO: print out the location of the failing image.
                (error "An Image for texture ~a is too big to be loaded onto ~
                        this card. Max resolution is ~a in either dimension."
                       (name texture)
                       max-size)))
    ;; Figure out the ideal mipmap count from the base resolution.
    (multiple-value-bind (expected-mipmaps expected-resolutions)
        ;; TODO: This might need work with cube-maps.
        (compute-mipmap-levels (v::width first-image) (v::height first-image))
      (declare (ignore expected-resolutions))
      ;; TODO: Fix this up for cube-maps
      #++(validate-mipmap-images images texture
                                 expected-mipmaps expected-resolutions)
      (potentially-degrade-texture-min-filter texture)
      ;; Allocate immutable storage if required.
      (when immutable-p
        (let ((num-mipmaps-to-generate
                (if use-mipmaps-p (min expected-mipmaps max-mipmaps) 1)))
          (%gl:tex-storage-2d texture-type num-mipmaps-to-generate
                              (v::internal-format first-image)
                              (v::width first-image)
                              (v::height first-image))))
      ;; Insert all cube faces plus mipmaps into the GPU.
      (loop :for cube :across images ;; only 1 cube available.
            :do (dotimes (idx (if use-mipmaps-p num-mipmaps 1))
                  (loop :with level = (+ texture-base-level idx)
                        :for (face-signifier mipmaps) :across cube
                        :do (let ((image (aref mipmaps idx)))
                              (if immutable-p
                                  (gl:tex-sub-image-2d
                                   face-signifier
                                   level
                                   0
                                   0
                                   (v::width image)
                                   (v::height image)
                                   (v::pixel-format image)
                                   (v::pixel-type image)
                                   (v::data image))
                                  (gl:tex-image-2d
                                   face-signifier
                                   level
                                   (v::internal-format image)
                                   (v::width image)
                                   (v::height image)
                                   0
                                   (v::pixel-format image)
                                   (v::pixel-type image)
                                   (v::data image)))))))
      (potentially-autogenerate-mipmaps texture-type texture))))
