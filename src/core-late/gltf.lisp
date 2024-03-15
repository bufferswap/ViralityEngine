(in-package #:virality)

;;;; Implementation of the GLTF* structures.

(u:define-constant +gltf-attribute-locations+
    '(("POSITION" . 0)
      ("NORMAL" . 1)
      ("TANGENT" . 2)
      ("COLOR_0" . 3)
      ("TEXCOORD_0" . 4)
      ("TEXCOORD_1" . 5)
      ("JOINTS_0" . 6)
      ("WEIGHTS_0" . 7))
  :test #'equal)


(defun get-gltf-property (gltf key &optional object)
  (let ((object (or object (json gltf))))
    (when (jsown:keyp object key)
      (jsown:val (or object (json gltf)) key))))

(defun get-gltf-chunk-type (chunk)
  (case (chunk-type chunk)
    (#x4e4f534a :json-content)
    (#x004e4942 :binary-buffer)
    (otherwise :unknown)))

(defun parse-gltf-header (gltf)
  (let* ((header (make-instance 'gltf-header))
         (buffer (fast-io:make-input-buffer
                  :vector (parse-bytes (buffer gltf) 12)))
         (magic (parse-string buffer :byte-count 4)))
    (if (string= magic "glTF")
        (setf (magic header) magic
              (version header) (parse-uint/le buffer 4)
              (format-length header) (parse-uint/le buffer 4))
        (error "Invalid glTF2 file."))
    header))

(defgeneric parse-gltf-chunk-data (gltf chunk-type chunk &key)
  (:method :around (gltf chunk-type chunk &key)
    (let ((buffer (fast-io:make-input-buffer
                   :vector (parse-bytes (buffer gltf)
                                        (chunk-length chunk)))))
      (call-next-method gltf chunk-type chunk :buffer buffer))))

(defmethod parse-gltf-chunk-data (gltf (chunk-type (eql :json-content)) chunk
                                  &key buffer)
  (let ((data (parse-string buffer :encoding :utf-8)))
    (setf (json gltf) (jsown:parse data))
    data))

(defmethod parse-gltf-chunk-data (gltf (chunk-type (eql :binary-buffer)) chunk
                                  &key buffer)
  (loop :with buffers = (get-gltf-property gltf "buffers")
        :with data = (make-array (length buffers))
        :for data-buffer :in buffers
        :for index :below (length buffers)
        :for size = (get-gltf-property gltf "byteLength" data-buffer)
        :do (setf (aref data index) (parse-bytes buffer size))
        :finally (setf (buffers gltf) data))
  nil)

(defmethod parse-gltf-chunk-data (gltf (chunk-type (eql :unknown)) chunk
                                  &key buffer)
  (declare (ignore buffer))
  (warn "Ignoring an unknown chunk type."))

(defun parse-gltf-chunk (gltf)
  (let* ((buffer (buffer gltf))
         (chunk (make-instance 'gltf-chunk
                               :length (parse-uint/le buffer 4)
                               :type (parse-uint/le buffer 4))))
    (setf (data chunk) (parse-gltf-chunk-data gltf
                                              (get-gltf-chunk-type chunk)
                                              chunk))))

(defun parse-gltf-chunks (gltf)
  (loop :with stream = (fast-io:input-buffer-stream (buffer gltf))
        :until (= (file-position stream) (file-length stream))
        :collect (parse-gltf-chunk gltf)))

(defun parse-gltf-datastream (gltf)
  (make-instance 'gltf-datastream
                 :header (parse-gltf-header gltf)
                 :chunks (parse-gltf-chunks gltf)))

(defun get-gltf-primitive-mode (gltf primitive)
  (case (get-gltf-property gltf "mode" primitive)
    (0 :points)
    (1 :lines)
    (2 :line-loop)
    (3 :line-strip)
    (4 :triangles)
    (5 :triangle-strip)
    (6 :triangle-fan)
    (otherwise :triangles)))

(defun get-gltf-component-type (gltf accessor)
  (ecase (get-gltf-property gltf "componentType" accessor)
    (5120 :byte)
    (5121 :unsigned-byte)
    (5122 :short)
    (5123 :unsigned-short)
    (5125 :unsigned-int)
    (5126 :float)))

(defun get-gltf-component-count (data-type)
  (ecase (u:make-keyword data-type)
    (:scalar 1)
    (:vec2 2)
    (:vec3 3)
    ((:vec4 :mat2) 4)
    (:mat3 9)
    (:mat4 16)))

(defun get-gltf-attribute-location (name)
  (u:alist-get +gltf-attribute-locations+ name :test #'string=))

(defun get-gltf-attribute-normalization (name component-type)
  (if (and (or (eq component-type :unsigned-byte)
               (eq component-type :unsigned-short))
           (not (string= name "JOINTS_0")))
      :true
      :false))

(defun make-gltf-buffer (gltf target accessor)
  (let ((buffer-view-index (get-gltf-property gltf "bufferView" accessor)))
    (unless (find buffer-view-index (allocated-views gltf))
      (let* ((buffer-view (elt (get-gltf-property gltf "bufferViews")
                               buffer-view-index))
             (index (get-gltf-property gltf "buffer" buffer-view))
             (offset (+ (or (get-gltf-property gltf "byteOffset" accessor) 0)
                        (or (get-gltf-property gltf "byteOffset" buffer-view)
                            0)))
             (size (get-gltf-property gltf "byteLength" buffer-view))
             (buffer (aref (buffers gltf) index))
             (data (static-vectors:make-static-vector
                    size
                    :element-type 'u:octet
                    :initial-contents (subseq buffer offset (+ offset size))))
             (pointer (static-vectors:static-vector-pointer data))
             (buffer-id (gl:gen-buffer)))
        (gl:bind-buffer target buffer-id)
        (%gl:buffer-data target size pointer :static-draw)
        (static-vectors:free-static-vector data)
        (push buffer-view-index (allocated-views gltf))
        buffer-id))))

(defun configure-gltf-attribute (gltf attribute accessor)
  (let* ((buffer-view (elt (get-gltf-property gltf "bufferViews")
                           (get-gltf-property gltf "bufferView" accessor)))
         (type (get-gltf-component-type gltf accessor))
         (count (get-gltf-component-count
                 (get-gltf-property gltf "type" accessor)))
         (stride (or (get-gltf-property gltf "byteStride" buffer-view) 0))
         (location (get-gltf-attribute-location attribute))
         (normalize (get-gltf-attribute-normalization attribute type)))
    (gl:enable-vertex-attrib-array location)
    (%gl:vertex-attrib-pointer location count type normalize stride 0)))

(defun make-gltf-vertex-buffers (gltf primitive data)
  (jsown:do-json-keys (attr accessor-id)
                      (get-gltf-property gltf "attributes" data)
    (let* ((accessor (elt (get-gltf-property gltf "accessors") accessor-id))
           (count (get-gltf-property gltf "count" accessor))
           (buffer (make-gltf-buffer gltf :array-buffer accessor)))
      (push buffer (vertex-buffers primitive))
      (configure-gltf-attribute gltf attr accessor)
      (when (string= attr "POSITION")
        (setf (element-count primitive) count)))))

(defun make-gltf-index-buffer (gltf primitive data)
  (u:when-let* ((indices (get-gltf-property gltf "indices" data))
                (accessor (elt (get-gltf-property gltf "accessors") indices)))
    (setf (element-count primitive) (get-gltf-property gltf "count" accessor)
          (component-type primitive) (get-gltf-component-type gltf accessor)
          (index-buffer primitive) (make-gltf-buffer
                                    gltf :element-array-buffer accessor))))

(defun draw-gltf-primitive/vertices (primitive instance-count)
  (declare (optimize speed))
  (gl:bind-vertex-array (vao primitive))
  (gl:draw-arrays-instanced
   (mode primitive) 0 (element-count primitive) instance-count))

(defun draw-gltf-primitive/indexed (primitive instance-count)
  (declare (optimize speed))
  (gl:bind-vertex-array (vao primitive))
  (gl:bind-buffer :element-array-buffer (index-buffer primitive))
  (%gl:draw-elements-instanced (mode primitive)
                               (element-count primitive)
                               (component-type primitive)
                               0
                               instance-count))

(defun make-gltf-draw-func (primitive)
  (setf (draw-func primitive)
        (if (index-buffer primitive)
            (lambda (x) (draw-gltf-primitive/indexed primitive x))
            (lambda (x) (draw-gltf-primitive/vertices primitive x)))))

(defun make-gltf-primitive (gltf data)
  (let ((primitive (make-instance 'gltf-primitive
                                  :vao (gl:gen-vertex-array)
                                  :mode (get-gltf-primitive-mode gltf data))))
    (gl:bind-vertex-array (vao primitive))
    (make-gltf-vertex-buffers gltf primitive data)
    (make-gltf-index-buffer gltf primitive data)
    (make-gltf-draw-func primitive)
    primitive))

(defun parse-gltf-meshes (gltf)
  (loop :for mesh :in (get-gltf-property gltf "meshes")
        :for index :from 0
        :for name = (or (get-gltf-property gltf "name" mesh)
                        (format nil "~a~d" (file-name gltf) index))
        :for primitives = (map
                           'vector
                           (lambda (x)
                             (make-gltf-primitive gltf x))
                           (get-gltf-property gltf "primitives" mesh))
        :do (setf (u:href (meshes gltf) name)
                  (make-instance 'gltf-mesh
                                 :name name
                                 :primitives primitives))))

(defun load-gltf (path)
  (u:with-binary-input (in path)
    (let* ((buffer (fast-io:make-input-buffer :stream in))
           (gltf (make-instance 'gltf
                                :file-name (pathname-name path)
                                :buffer buffer)))
      (setf (parse-tree gltf) (parse-gltf-datastream gltf))
      (parse-gltf-meshes gltf)
      (setf (buffers gltf) nil)
      gltf)))

(defun find-gltf-mesh (gltf index)
  (let ((meshes (get-gltf-property gltf "meshes")))
    (when (>= index (length meshes))
      (error "Mesh index ~d not found." index))
    (get-gltf-property gltf "primitives" (elt meshes index))))
