(in-package #:virality.texture)

(defmethod load-texture-data ((texture-type (eql :texture-2d)) texture context)
  ;; TODO: This assumes no use of the general-data-descriptor or procedurally
  ;; generated content.
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (immutable-p (get-computed-applied-attribute texture :immutable))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (data (get-computed-applied-attribute texture :data))
         (flip-y (get-computed-applied-attribute texture :flip-y)))
    ;; load all of the images we may require.
    (let ((images (read-mipmap-images context data use-mipmaps-p :2d flip-y)))
      ;; Check to ensure they all fit into texture memory.
      ;; TODO: Refactor out of each method into validate-mipmap-images and
      ;; generalize.
      (loop :with max-size = v::=max-texture-size=
            :for image :across images
            :for location :across data
            :do (when (> (max (v::height image) (v::width image))
                         max-size)
                  (error "Image ~a for texture ~a is too big to be loaded onto ~
                          this card. Max resolution is ~a in either dimension."
                         location
                         (name texture)
                         max-size)))
      ;; Figure out the ideal mipmap count from the base resolution.
      (multiple-value-bind (expected-mipmaps expected-resolutions)
          (compute-mipmap-levels (v::width (aref images 0))
                                 (v::height (aref images 0)))
        (validate-mipmap-images
         images texture expected-mipmaps expected-resolutions)
        (potentially-degrade-texture-min-filter texture)
        ;; Allocate immutable storage if required.
        (when immutable-p
          (let ((num-mipmaps-to-generate
                  (if use-mipmaps-p (min expected-mipmaps max-mipmaps) 1)))
            (%gl:tex-storage-2d texture-type num-mipmaps-to-generate
                                (v::internal-format (aref images 0))
                                (v::width (aref images 0))
                                (v::height (aref images 0)))))
        ;; Upload all of the mipmap images into the texture ram.
        ;; TODO: Make this higher order.
        (loop :for idx :below (if use-mipmaps-p (length images) 1)
              :for level = (+ texture-base-level idx)
              :for image = (aref images idx)
              :do (if immutable-p
                      (gl:tex-sub-image-2d
                       texture-type
                       level
                       0
                       0
                       (v::width image)
                       (v::height image)
                       (v::pixel-format image)
                       (v::pixel-type image)
                       (v::data image))
                      (gl:tex-image-2d
                       texture-type
                       level
                       (v::internal-format image)
                       (v::width image)
                       (v::height image)
                       0
                       (v::pixel-format image)
                       (v::pixel-type image)
                       (v::data image))))
        ;; Determine if opengl should generate the mipmaps.
        (potentially-autogenerate-mipmaps texture-type texture)))))
