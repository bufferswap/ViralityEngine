(in-package #:virality)

(defclass gltf ()
  ((%file-name :reader file-name
               :initarg :file-name)
   (%buffer :reader buffer
            :initarg :buffer)
   (%parse-tree :accessor parse-tree)
   (%json :accessor json)
   (%buffers :accessor buffers)
   (%allocated-views :accessor allocated-views
                     :initform nil)
   (%meshes :reader meshes
            :initform (u:dict #'equalp))))

(defclass gltf-datastream ()
  ((%header :reader header
            :initarg :header)
   (%chunks :reader chunks
            :initarg :chunks)))

(defclass gltf-header ()
  ((%magic :accessor magic)
   (%version :accessor version)
   (%format-length :accessor format-length)))

(defclass gltf-chunk ()
  ((%length :reader chunk-length
            :initarg :length)
   (%type :reader chunk-type
          :initarg :type)
   (%data :accessor data)))

(defclass gltf-mesh ()
  ((%name :reader name
          :initarg :name)
   (%primitives :reader primitives
                :initarg :primitives)))

(defclass gltf-primitive ()
  ((%vao :reader vao
         :initarg :vao)
   (%mode :reader mode
          :initarg :mode)
   (%element-count :accessor element-count)
   (%component-type :accessor component-type)
   (%vertex-buffers :accessor vertex-buffers
                    :initform nil)
   (%index-buffer :accessor index-buffer)
   (%draw-func :accessor draw-func)))
