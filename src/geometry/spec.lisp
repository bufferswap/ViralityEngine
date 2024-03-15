(in-package #:virality)

;;;; Implementation of datatype GEOMETRY-SPEC

(defun make-geometry-spec (layout-name
                           &key (primitive :triangles) (vertex-count 0)
                             buffer-data)
  (lambda ()
    (let ((spec (make-instance 'geometry-spec
                               :id (gl:gen-vertex-array)
                               :layout (find-geometry-layout layout-name)
                               :vertex-count vertex-count
                               :primitive primitive)))
      (gl:bind-vertex-array (id spec))
      (make-geometry-buffers spec)
      (configure-geometry-buffers spec)
      (u:do-plist (k v buffer-data)
        (update-geometry spec k v))
      spec)))

(defmacro define-geometry (name options &body body)
  (declare (ignore options))
  (destructuring-bind (&key layout (primitive :triangles) (vertex-count 0)
                         buffers)
      (car body)
    `(setf (u:href =meta/geometry= ',name)
           (make-geometry-spec ',layout
                               :primitive ',primitive
                               :vertex-count ,vertex-count
                               :buffer-data ',buffers))))
