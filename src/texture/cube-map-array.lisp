(in-package #:virality.texture)

(defmethod load-texture-data ((texture-type (eql :texture-cube-map-array))
                              texture context)
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (immutable-p (get-computed-applied-attribute texture :immutable))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (data (get-computed-applied-attribute texture :data))
         (flip-y (get-computed-applied-attribute texture :flip-y))
         ;; load all of the images we may require.
         (images (read-mipmap-images
                  context data use-mipmaps-p :cube-map-array flip-y))
         (first-cube (aref images 0))
         (first-image (aref (second (aref first-cube 0)) 0))
         ;; TODO: This is not safe, need to check all of them.
         ;; TODO: We assume all cube maps have the same mipmap number. I don't
         ;; know if this is a reuirements or not.
         (num-mipmaps (length (second (aref first-cube 0)))))
    #++(:printv "Loading :texture-cube-map-array images = ~a" images)
    ;; Figure out the ideal mipmap count from the base resolution.
    (multiple-value-bind (expected-mipmaps expected-resolutions)
        ;; TODO: This might need work with cube-maps.
        (compute-mipmap-levels (v::width first-image)
                               (v::height first-image))
      (declare (ignore expected-resolutions))
      ;; TODO: Fix this up for cube-maps
      #++(validate-mipmap-images images texture
                                 expected-mipmaps expected-resolutions)
      (potentially-degrade-texture-min-filter texture)
      ;; Allocate immutable storage if required.
      (when immutable-p
        (let ((num-mipmaps-to-generate
                (if use-mipmaps-p (min expected-mipmaps max-mipmaps) 1)))
          #++(:printv "tex-storage-3d: texture-type = ~a, ~
                      num-mipmaps-to-generate = ~a, internal-format = ~a, ~
                      width = ~a, height = ~a, depth = ~a~%"
                      texture-type
                      num-mipmaps-to-generate
                      (v::internal-format first-image)
                      (v::width first-image)
                      (v::height first-image)
                      (* (length data) 6))
          (%gl:tex-storage-3d texture-type
                              num-mipmaps-to-generate
                              (v::internal-format first-image)
                              (v::width first-image)
                              (v::height first-image)
                              ;; For this target, we need number of layer-face,
                              ;; not number of layer like in cube-maps!
                              (* (length data) 6))))
      ;; Insert all cubes plus mipmaps into the GPU.
      (loop :for cube :across images
            :for cube-idx :below (length images)
            :do (dotimes (idx (if use-mipmaps-p num-mipmaps 1))
                  (let ((level (+ texture-base-level idx)))
                    (loop :for (face-signifier mipmaps) :across cube
                          ;; NOTE: face-idx works cause I sorted the faces
                          ;; earlier.
                          :for face-idx :by 1
                          :do
                          #++(:printv "inserting cube ~a face ~a[~a]~%"
                                      cube-idx face-signifier idx)
                             (let ((image (aref mipmaps idx)))
                               (if immutable-p
                                   (gl:tex-sub-image-3d
                                    texture-type
                                    level
                                    0
                                    0
                                    (+ (* cube-idx 6)
                                       face-idx)
                                    (v::width image)
                                    (v::height image)
                                    1
                                    (v::pixel-format image)
                                    (v::pixel-type image)
                                    (v::data image))
                                   (gl:tex-image-3d
                                    texture-type
                                    level
                                    (v::internal-format image)
                                    (v::width image)
                                    (v::height image)
                                    (+ (* cube-idx 6) face-idx)
                                    0
                                    (v::pixel-format image)
                                    (v::pixel-type image)
                                    (v::data image))))))))
      (potentially-autogenerate-mipmaps texture-type texture))))
