(in-package #:virality)

;;; spec

(defclass framebuffer-spec ()
  ((%name :reader name
          :initarg :name)
   (%mode :accessor mode)
   (%attachment-specs :reader attachment-specs
                      :initform (u:dict #'eq))
   (%materials :accessor materials
               :initform nil)))

(defclass framebuffer-attachment-spec ()
  ((%name :reader name
          :initarg :name)
   (%buffer :reader buffer
            :initarg :buffer)
   (%point :reader point
           :initarg :point)
   (%width :reader width
           :initarg :width)
   (%height :reader height
            :initarg :height)))

(u:define-printer (framebuffer-spec stream :identity t)
  (format stream "~s" (name framebuffer-spec)))

(defun find-framebuffer-spec (name)
  (or (u:href =meta/framebuffers= name)
      (error "Framebuffer ~s is not defined." name)))

(defun make-framebuffer-attachment-spec (spec)
  (flet ((generate-size-func (dimension value)
           (lambda ()
             (or value
                 (ecase dimension
                   (:width =window-width=)
                   (:height =window-height=))))))
    (destructuring-bind (name &key point (buffer :render-buffer) width height)
        spec
      (make-instance 'framebuffer-attachment-spec
                     :name name
                     :buffer (u:ensure-list buffer)
                     :point point
                     :width (generate-size-func :width width)
                     :height (generate-size-func :height height)))))

(defun find-framebuffer-attachment-spec (spec name)
  (u:href (attachment-specs spec) name))

(defun update-framebuffer-spec (name mode attachments)
  (let ((spec (find-framebuffer-spec name)))
    (setf (mode spec) mode)
    (dolist (x attachments)
      (destructuring-bind (name &key &allow-other-keys) x
        (let ((attachment-spec (make-framebuffer-attachment-spec x)))
          (setf (u:href (attachment-specs spec) name) attachment-spec))))
    (enqueue :recompile (list :framebuffer name))))

(defun make-framebuffer-spec (name mode attachments)
  (let ((spec (make-instance 'framebuffer-spec :name name)))
    (setf (u:href =meta/framebuffers= name) spec)
    (update-framebuffer-spec name mode attachments)
    spec))

(defmacro define-framebuffer (name (&key (mode :read/write)) &body body)
  `(if (u:href =meta/framebuffers= ',name)
       (update-framebuffer-spec ',name ',mode ',body)
       (make-framebuffer-spec ',name ',mode ',body)))

;;; implementation

(defclass framebuffer ()
  ((%spec :reader spec
          :initarg :spec)
   (%id :reader id
        :initarg :id)
   (%target :reader target
            :initarg :target)
   (%attachments :reader attachments
                 :initform (u:dict #'eq))))

(u:define-printer (framebuffer stream)
  (format stream "~s" (name (spec framebuffer))))

(defun framebuffer-mode->target (mode)
  (ecase mode
    (:read :read-framebuffer)
    (:write :draw-framebuffer)
    (:read/write :framebuffer)))

(defun find-framebuffer (core name)
  (u:href (framebuffers core) name))

(defun framebuffer-attachment-point->gl (point)
  (destructuring-bind (type &optional (index 0)) (u:ensure-list point)
    (ecase type
      (:color (u:format-symbol :keyword "~a-ATTACHMENT~d" type index))
      (:depth :depth-attachment)
      (:stencil :stencil-attachment)
      (:depth/stencil :depth-stencil-attachment))))

(defun framebuffer-attachment-names->points (framebuffer attachment-names)
  (mapcar
   (lambda (x)
     (let* ((spec (spec framebuffer))
            (attachment (find-framebuffer-attachment-spec spec x)))
       (unless attachment
         (error "Attachment name ~s not found for framebuffer ~s."
                x (name spec)))
       (framebuffer-attachment-point->gl (point attachment))))
   attachment-names))

(defun framebuffer-attachment-point->render-buffer-format (point)
  (destructuring-bind (type &optional index) (u:ensure-list point)
    (declare (ignore index))
    (ecase type
      (:color :rgb)
      (:depth :depth-component)
      (:stencil :stencil-index)
      (:depth/stencil :depth24-stencil8))))

(defun ensure-framebuffer-complete (framebuffer target buffer attachment)
  (let ((result (gl:check-framebuffer-status  target)))
    (unless (find result '(:framebuffer-complete :framebuffer-complete-oes))
      (error "Error attaching ~a as attachment ~s of framebuffer ~s: ~a"
             buffer attachment (name (spec framebuffer)) result))))

(defun framebuffer-attach/render-buffer (framebuffer attachment)
  (let* ((point (point attachment))
         (gl-point (framebuffer-attachment-point->gl point))
         (internal-format (framebuffer-attachment-point->render-buffer-format
                           point))
         (buffer-id (gl:gen-renderbuffer))
         (width (funcall (width attachment)))
         (height (funcall (height attachment)))
         (target (target framebuffer)))
    (gl:bind-renderbuffer :renderbuffer buffer-id)
    (gl:renderbuffer-storage :renderbuffer internal-format width height)
    (gl:bind-renderbuffer :renderbuffer 0)
    (gl:bind-framebuffer target (id framebuffer))
    (gl:framebuffer-renderbuffer target gl-point :renderbuffer buffer-id)
    (ensure-framebuffer-complete framebuffer target buffer-id point)
    (gl:bind-framebuffer target 0)
    (setf (u:href (attachments framebuffer) point) buffer-id)
    buffer-id))

;; TODO: stub until textures are refactored. ~axion 4/16/2020
(defun load-texture (&rest args)
  (declare (ignore args)))

(defun framebuffer-attach/texture (core framebuffer attachment)
  (destructuring-bind (type &optional texture-name) (buffer attachment)
    (declare (ignore type))
    (unless texture-name
      (error "Framebuffer ~s attachment ~s uses a texture buffer without a ~
                texture name."
             (name framebuffer)
             (name attachment)))
    (let* ((width (funcall (width attachment)))
           (height (funcall (height attachment)))
           (buffer-id (id (load-texture (context core)
                                        texture-name
                                        :width width
                                        :height height)))
           (point (framebuffer-attachment-point->gl (point attachment)))
           (target (target framebuffer)))
      (gl:bind-framebuffer target (id framebuffer))
      (%gl:framebuffer-texture target point buffer-id 0)
      (ensure-framebuffer-complete framebuffer target buffer-id point)
      (gl:bind-framebuffer target 0)
      (setf (u:href (attachments framebuffer) point) buffer-id)
      buffer-id)))

(defun framebuffer-attach (core framebuffer attachment-name)
  (let* ((spec (spec framebuffer))
         (attachment (find-framebuffer-attachment-spec spec attachment-name)))
    (ecase (car (buffer attachment))
      (:render-buffer (framebuffer-attach/render-buffer framebuffer attachment))
      (:texture (framebuffer-attach/texture core framebuffer attachment)))))

(defun framebuffer-attach-all (core framebuffer)
  (let ((spec (spec framebuffer)))
    (u:do-hash-values (attachment (attachment-specs spec))
      (framebuffer-attach core framebuffer (name attachment)))))

(defun make-framebuffer (core spec)
  (let* ((target (framebuffer-mode->target (mode spec)))
         (framebuffer (make-instance 'framebuffer
                                     :spec spec
                                     :id (gl:gen-framebuffer)
                                     :target target)))
    (framebuffer-attach-all core framebuffer)
    (setf (u:href (framebuffers core) (name spec)) framebuffer)
    framebuffer))

(defun load-framebuffer (context name)
  (let ((spec (find-framebuffer-spec name))
        (core (core context)))
    (or (find-framebuffer core name)
        (make-framebuffer core spec))))

(defmacro with-framebuffer (framebuffer (&key mode attachments) &body body)
  (u:with-gensyms (id target)
    `(if ,framebuffer
         (let ((,id (id ,framebuffer))
               (,target ,(if mode
                             `(framebuffer-mode->target ,mode)
                             `(target ,framebuffer))))
           (gl:bind-framebuffer ,target ,id)
           ,@(when attachments
               `((gl:draw-buffers ,attachments)))
           (progn ,@body)
           (gl:bind-framebuffer ,target 0))
         (progn ,@body))))

;; TODO: Framebuffers aren't used yet, but this is how it was done in Pyx.
;; ~axion
#++(on-recompile :framebuffer data ()
     (u:when-let ((spec (find-framebuffer-spec data))
                  (data (find-framebuffer data)))
       (framebuffer-attach-all data)
       (dolist (material (materials spec))
         (enqueue :recompile (list :material material)))))
