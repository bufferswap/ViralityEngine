(in-package #:virality.extension)

(v:define-geometry-layout 2d ()
  (:data (:format interleaved)
         (position :type float :count 2)
         (uv :type float :count 2)))
