(in-package #:virality.component)

(v:define-component mesh ()
  ((%asset :reader asset
           :initarg :asset
           :initform nil)
   (%name :reader name
          :initarg :name
          :initform nil)
   (%index :reader index
           :initarg :index
           :initform 0)
   (%primitive :reader primitive)))

(defmethod v:on-component-initialize ((self mesh))
  (with-slots (%asset %name %index %primitive) self
    (unless %asset
      (error "A mesh component must have an asset."))
    (unless %name
      (error "A mesh component must have a name."))
    (let* ((context (v:context self))
           (path (v::resolve-path %asset))
           (gltf (v:with-asset-cache context :mesh path
                   (v::load-gltf path)))
           (mesh (u:href (v::meshes gltf) %name)))
      (unless mesh
        (error "Mesh name ~s not found in mesh file ~s." %name path))
      (setf %primitive (aref (v::primitives mesh) %index)))))

(defmethod v:on-component-slave-render ((master render) (self mesh))
  (let ((instance-count (v::instances (comp:material master))))
    (funcall (v::draw-func (primitive self)) instance-count)))
