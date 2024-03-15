(in-package #:virality)

(defun make-geometry (name)
  (funcall (u:href =meta/geometry= name)))

(defun update-geometry (geometry buffer-name data)
  (let ((data (or data (make-array (vertex-count geometry)
                                   :initial-element 0))))
    (fill-geometry-buffer geometry buffer-name data)
    (setf (primitive-count geometry) (length data))))

(defun draw-geometry (geometry instance-count)
  (gl:bind-vertex-array (id geometry))
  (%gl:draw-arrays-instanced (primitive geometry)
                             0
                             (* (primitive-count geometry)
                                (vertex-count geometry))
                             instance-count)
  (gl:bind-vertex-array 0))

(defun delete-geometry (geometry)
  (gl:delete-buffers (buffers geometry))
  (gl:delete-vertex-arrays (list (id geometry))))
