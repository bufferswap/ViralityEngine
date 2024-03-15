(in-package #:vshadow)

(defclass shader-buffer ()
  ((%id :reader id
        :initform (gl:gen-buffer))
   (%name :reader buffer-name
          :initarg :name)
   (%target :reader target
            :initarg :target)
   (%layout :reader layout
            :initarg :layout)))

(defun %make-buffer (name target layout)
  (make-instance 'shader-buffer :name name :target target :layout layout))

(defun buffer-type->target (buffer-type)
  (ecase buffer-type
    (:ubo :uniform-buffer)
    (:ssbo :shader-storage-buffer)))

(defun buffer-type->block-type (type)
  (ecase type
    (:ubo :uniform)
    (:ssbo :buffer)))

(defun block-type->buffer-type (block-type)
  (ecase block-type
    (:uniform :ubo)
    (:buffer :ssbo)))

(defun find-buffer (buffer-name)
  (u:href (meta :buffers) buffer-name))

(defun create-buffer (buffer-name block-alias)
  "Create a buffer of the given TYPE and NAME, using the block BLOCK-ID of PROGRAM-NAME."
  (u:if-let ((block (find-block block-alias)))
    (let* ((buffer-table (meta :buffers))
           (type (block-type->buffer-type (block-type block)))
           (target (buffer-type->target type))
           (buffer (%make-buffer buffer-name target (layout block))))
      (with-slots (%id %layout) buffer
        (%gl:bind-buffer target %id)
        (%gl:buffer-data target (size %layout) (cffi:null-pointer) :static-draw)
        (setf (u:href buffer-table buffer-name) buffer)
        (id buffer)))
    (error "Cannot find the block with alias ~s when attempting to create a ~
            buffer."
           block-alias)))

(defun bind-buffer (buffer-name binding-point)
  "Bind a buffer with name BUFFER-NAME to BINDING-POINT."
  (u:if-let ((buffer (find-buffer buffer-name)))
    (with-slots (%target %id) buffer
      (%gl:bind-buffer-base %target binding-point %id)
      (%gl:bind-buffer %target 0))
    (error "Cannot find buffer ~s." buffer-name)))

(defun unbind-buffer (buffer-name)
  "Unbind a buffer with name BUFFER-NAME."
  (bind-buffer buffer-name 0))

(defun clear-buffer (buffer-name)
  (with-slots (%id %target) (find-buffer buffer-name)
    (gl:bind-buffer %target %id)
    (%gl:clear-buffer-data %target :r8 :red :unsigned-byte (cffi:null-pointer))
    (unbind-buffer buffer-name)))

(defun delete-buffer (buffer-name)
  "Delete the buffer having a name of BUFFER-NAME."
  (let ((buffer (find-buffer buffer-name)))
    (with-slots (%id) buffer
      (clear-buffer buffer-name)
      (unbind-buffer buffer-name)
      (gl:delete-buffers (list %id))
      (remhash buffer-name (meta :buffers))
      %id)))

(defun %write-buffer-member (target member index value)
  (with-slots (%element-type %offset %element-stride %byte-stride) member
    (let* ((count (length value))
           (offset (+ %offset (* index %byte-stride)))
           (size (* count %byte-stride)))
      (static-vectors:with-static-vector
          (sv (* count %element-stride)
              :element-type %element-type
              :initial-element (coerce 0 %element-type))
        (let ((ptr (static-vectors:static-vector-pointer sv))
              (i 0))
          (map nil
               (lambda (x)
                 (if (typep x 'sequence)
                     (replace sv x :start1 i)
                     (setf (aref sv i) x))
                 (incf i %element-stride))
               value)
          (%gl:buffer-sub-data target offset size ptr))))))

(defun %write-buffer-member/matrix (target member index value)
  (with-slots (%element-type %offset %element-stride %byte-stride %dimensions) member
    (let* ((count (length value))
           (offset (+ %offset (* index %byte-stride)))
           (size (* count %byte-stride)))
      (destructuring-bind (columns rows) %dimensions
        (static-vectors:with-static-vector
            (sv (* count columns %element-stride)
                :element-type %element-type
                :initial-element (coerce 0 %element-type))
          (let ((ptr (static-vectors:static-vector-pointer sv))
                (i 0))
            (map nil
                 (lambda (x)
                   (loop :repeat columns
                         :for j :from i :by %element-stride
                         :for k :by rows
                         :do (replace
                              sv x :start1 j :start2 k :end2 (+ k rows)))
                   (incf i (* columns %element-stride)))
                 value)
            (%gl:buffer-sub-data target offset size ptr)))))))

(defun write-buffer-path (buffer-name &key path index value)
  "Write VALUE to the buffer with the name BUFFER-NAME, starting at the given PATH.

PATH: A \"dot-separated\" keyword symbol, where each part denotes a member in the buffer's block
layout.

VALUE: A value to write, such as a scalar or matrix depending on the type of the member PATH refers
to. To write to an array, use a sequence of values.

Note: Writing to arrays which contain other aggregate types (other arrays or structures) is not
possible. This is a design decision to allow this library to have a simple \"path-based\" buffer
writing interface."
  (with-slots (%type %id %target %layout) (find-buffer buffer-name)
    (unless path
      (error "Buffer path must be specified."))
    (let ((member (u:href (members %layout) path)))
      (check-type value sequence)
      (when (> (+ index (length value)) (element-count member))
        (error "Buffer index out of bounds when writing path: ~s." path))
      (gl:bind-buffer %target %id)
      (case (object-type member)
        (:mat (%write-buffer-member/matrix %target member index value))
        (t (%write-buffer-member %target member index value)))
      (gl:bind-buffer %target 0))))

(defun %read-buffer-member/scalar (member data count)
  (with-slots (%element-stride) member
    (if (= count 1)
        (aref data 0)
        (subseq data 0 (* count %element-stride)))))

(defun %read-buffer-member/vector (member data count)
  (with-slots (%dimensions %element-stride) member
    (let ((size (car %dimensions)))
      (flet ((make-vector (data index size)
               (let ((output (u:make-f32-array size)))
                 (replace output data :start2 index)
                 output)))
        (if (= count 1)
            (make-vector data 0 size)
            (loop :repeat count
                  :for i :by %element-stride
                  :collect (make-vector data i size)))))))

(defun %read-buffer-member/matrix (member data count)
  (with-slots (%dimensions %element-stride) member
    (destructuring-bind (columns rows) %dimensions
      (flet ((make-matrix (data index)
               (loop :repeat columns
                     :for i :from index :by %element-stride
                     :nconc (loop :for j :below rows
                                  :collect (aref data (+ i j)))
                       :into result
                     :do (incf index %element-stride)
                     :finally (return (make-array (* rows columns)
                                                  :element-type 'u:f32
                                                  :initial-contents result)))))
        (if (or (= columns rows 2)
                (= columns rows 3)
                (= columns rows 4))
            (if (= count 1)
                (make-matrix data 0)
                (loop :for i :below count
                      :for index = (* columns %element-stride i)
                      :collect (make-matrix data index)))
            (error "Only square matrices are supported."))))))

(defun %read-buffer-member (target member index count)
  (with-slots (%type %dimensions %count %element-stride %element-type %offset %byte-stride) member
    (let* ((count (or count %count))
           (offset (+ %offset (* index %byte-stride)))
           (size (* count %byte-stride))
           (element-count (reduce #'* %dimensions))
           (stride (if (and (eq %type :vec)
                            (= element-count 3))
                       4
                       %element-stride))
           (data (make-array (* stride (cadr %dimensions) count) :element-type %element-type)))
      (cffi:with-pointer-to-vector-data (ptr data)
        (%gl:get-buffer-sub-data target offset size ptr))
      (ecase %type
        (:scalar (%read-buffer-member/scalar member data count))
        (:vec (%read-buffer-member/vector member data count))
        (:mat (%read-buffer-member/matrix member data count))))))

(defun read-buffer-path (buffer-name &key path (index 0) count)
  (with-slots (%id %target %layout) (find-buffer buffer-name)
    (unless path
      (error "Buffer path must be specified."))
    (let ((member (u:href (members %layout) path)))
      (when (> (+ index (or count 1)) (element-count member))
        (error "Buffer index out of bounds when reading path: ~s." path))
      (gl:bind-buffer %target %id)
      (unwind-protect (%read-buffer-member %target member index count)
        (gl:bind-buffer %target 0)))))
