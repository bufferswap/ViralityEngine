(in-package #:virality.texture)

;;;; Implementation of datatype TEXTURE-TABLE

(defun make-textures-table (&rest init-args)
  (apply #'make-instance 'textures-table init-args))

;; TODO: Candidate for public API
(defun add-texture-profile (profile core)
  (setf (u:href (profiles (v::textures core)) (name profile)) profile))

;; TODO: Candidate for public API
(defun remove-texture-profile (profile-name core)
  (remhash profile-name (profiles (v::textures core))))

;; TODO: Candidate for public API
(defun add-semantic-texture-descriptor (texdesc core)
  (setf (u:href (semantic-texture-descriptors (v::textures core))
                (name texdesc))
        texdesc))

;; TODO: Candidate for public API
(defun remove-semantic-texture-descriptor (texdesc core)
  (remhash texdesc (semantic-texture-descriptors (v::textures core))))

;; TODO: Candidate for public API
(defun add-unrealized-texture (texture context)
  "Add a TEXTURE, which only has defined an opengl texid, to core. This book
keeps unrealized textures for the user to finalize before the game starts."
  (setf (u:href (unrealized-procedural-textures (v::textures (v::core context)))
                (name texture))
        texture))

;; TODO: Candidate for public API
(defun remove-unrealized-texture (texture context)
  "Remove the unrealized TEXTURE from the texture tables. It is assumed that you
have just realized it by setting its opengl parameters and uploading data or
declaring storage of some kind to the GPU."
  (remhash (name texture)
           (unrealized-procedural-textures (v::textures (v::core context)))))

;; TODO: Candidate for public API
(defun get-unrealized-textures (context)
  "Return a list of textures that are specified in materials, but haven't been
completed (no opengl parameters set for them, or data loaded into GPU).
NOTE: These are already in the resource-cache."
  (u:hash-values
   (unrealized-procedural-textures
    (v::textures (v::core context)))))

;; TODO: Candidate for public API.
(defun get-procedural-texture-descriptors (context)
  "Return a list of all procedural texture descriptors."
  (let (results)
    (u:do-hash-values (texdesc (semantic-texture-descriptors
                                (v::textures (v::core context))))
      (when (eq (texture-type texdesc) :procedural)
        (push texdesc results)))
    (nreverse results)))

;; TODO: Candidate for public API
(defun find-semantic-texture-descriptor (semantic-texture-name context)
  (u:href (semantic-texture-descriptors (v::textures (v::core context)))
          semantic-texture-name))

;; TODO: Candidate for public API
(defclass texture-profile ()
  ((%name :reader name
          :initarg :name)
   (%attributes :reader attributes
                :initarg :attributes
                :initform (u:dict #'eq))))

;; TODO: Candidate for public API
(defun make-texture-profile (&rest init-args)
  (apply #'make-instance 'texture-profile init-args))

;; TODO: Candidate for public API
(defclass texture-descriptor ()
  ((%name :accessor name
          :initarg :name)
   ;; Procedural textures have :procedural as a texture-type!
   (%texture-type :accessor texture-type
                  :initarg :texture-type)
   (%profile-overlay-names :accessor profile-overlay-names
                           :initarg :profile-overlay-names)
   ;; Attribute specified in the define-texture form
   (%attributes :accessor attributes
                :initarg :attributes
                :initform (u:dict #'eq))
   ;; Up to date attributes once the profiles have been applied.
   (%applied-attributes :accessor applied-attributes
                        :initarg :applied-attributes
                        :initform (u:dict #'eq))))

;; TODO candidate for public API
(defun make-texture-descriptor (&rest init-args)
  (apply #'make-instance 'texture-descriptor init-args))

;; TODO: Candidate for public API.
(defun copy-texture-descriptor (texdesc)
  (let ((new-texdesc (make-texture-descriptor)))
    (setf
     ;; These are currently symbols.
     (name new-texdesc) (name texdesc)
     (texture-type new-texdesc) (texture-type texdesc)
     ;; This is a list
     (profile-overlay-names new-texdesc)
     (copy-seq (profile-overlay-names texdesc)))
    ;; Then copy over the attributes, we support SIMPLE values such as: string,
    ;; array, list, vector, and symbol.
    (u:do-hash (key value (attributes texdesc))
      (setf (u:href (attributes new-texdesc) key) (u:copy-sequence-tree value)))
    (u:do-hash (key value (applied-attributes texdesc))
      (setf (u:href (applied-attributes new-texdesc) key) (u:copy-sequence-tree value)))
    new-texdesc))

;; The texture descriptor as read from the define-texture DSL form. This records
;; the original values for that texture definition which may include that it is
;; procedural or not. This texdesc is a reference to the only copy of it as read
;; from the DSL and stored in the texture-table in core.
(defclass texture ()
  ((%semantic-texdesc :accessor semantic-texdesc
                      :initarg :semantic-texdesc)
   ;; The texture descriptor as finally computed from the semantic texture
   ;; descriptor before we load/procedurally create the texture data or storage
   ;; on the GPU. Here, the texture-type must be valid among other aspects of
   ;; the texdesc. In the case of a non procedural texture, this can be computed
   ;; automatically, but in the case of a procedural texture, the user
   ;; ultimately must set the slots to valid things. This texdesc is unique to
   ;; this texture instance. This means, suppose in the case of a procedural
   ;; texture, that each texture instance contains a unique computed copy of
   ;; that the semantic-texdesc that directly describes this version of that
   ;; texture.
   (%computed-texdesc :accessor computed-texdesc
                      :initarg :computed-texdesc)
   ;; The name of the texture might be generated off the texdesc name for
   ;; procedural textures.
   (%name :reader name
          :initarg :name)
   ;; The allocated opengl texture id.
   (%texid :accessor texid
           :initarg :texid)))

(u:define-printer (texture stream :type t)
  (format stream "id: ~s" (texid texture)))

(defun get-semantic-applied-attribute (texture attribute-name)
  (u:href (applied-attributes (semantic-texdesc texture)) attribute-name))

(defun (setf get-semantic-applied-attribute) (new-value texture attribute-name)
  (setf (u:href (applied-attributes (semantic-texdesc texture)) attribute-name)
        new-value))

(defun get-computed-applied-attribute (texture attribute-name)
  (u:href (applied-attributes (computed-texdesc texture)) attribute-name))

(defun (setf get-computed-applied-attribute) (new-value texture attribute-name)
  (setf (u:href (applied-attributes (computed-texdesc texture)) attribute-name)
        new-value))

;; TODO: Candidate public API
(defun set-opengl-texture-parameters (texture)
  (with-accessors ((texture-type texture-type)
                   (applied-attributes applied-attributes))
      (computed-texdesc texture)
    (let ((texture-parameters
            '(:depth-stencil-texture-mode :texture-base-level
              :texture-border-color :texture-compare-func
              :texture-compare-mode :texture-lod-bias
              :texture-min-filter :texture-mag-filter
              :texture-min-lod :texture-max-lod
              :texture-max-level :texture-swizzle-r
              :texture-swizzle-g :texture-swizzle-b
              :texture-swizzle-a :texture-swizzle-rgba
              :texture-wrap-s :texture-wrap-t :texture-wrap-r)))
      (dolist (putative-parameter texture-parameters)
        (u:when-found (value (u:href applied-attributes putative-parameter))
          (gl:tex-parameter texture-type
                            putative-parameter
                            value))))))

;;; TODO: These are cut into individual functions for their context. Maybe later
;;; I'll see about condensing them to be more concise.

(defgeneric load-texture-data (texture-type texture context)
  (:documentation "Load actual data described in the TEXTURE's texdesc of ~
TEXTURE-TYPE into the texture memory."))


(defun read-mipmap-images (context data use-mipmaps-p kind flip-y)
  "Read the images described in the mipmap location array DATA into main memory.
If USE-MIPMAPS-P is true, then load all of the mipmaps, otherwise only load the
base image, which is the first one in the array. CONTEXT is the core context
slot value. Return a vector of image structure from the function V:LOAD-IMAGE.
If KIND is :1d or :2d, then DATA must be an array of location descriptors like:
#((:project \"a/b/c/foo.png\") (:local \"a/b/c/foo.png\")) If KIND is :1d-array,
:2d-array, :3d, then DATA must be an array of slices of mipmap images:
#(#((:project \"a/b/c/slice0-mip0.png\") (:local \"a/b/c/slice1-mip0.png\")))
The same vector structure is returned but with the local descriptor lists
replaced by actual IMAGE instances of the loaded images."
  (flet ((read-image-contextually (asset)
           (v:with-asset-cache context :texture asset
             (v::load-image (v::resolve-path asset) :flip-y flip-y)))
         (process-cube-map-mipmaps (cube-data choice-func)
           ;; Process only one cube map right now... when this works, edit it to
           ;; process many cube maps.
           (unless (equal '(:layout :six) (first cube-data))
             (error "read-mipmap-images: :texture-cub-map, invalid :layout ~s, ~
                     it must be :six at this time."
                    (first cube-data)))
           ;; canonicalize the face name.
           (map 'vector
                (lambda (x)
                  (list (canonicalize-cube-map-face-signfier (first x))
                        (second x)))
                ;; sort the faces for loading in the right order.
                (stable-sort
                 ;; convert vector of path-locations to image instances.
                 (map 'vector
                      (lambda (x)
                        (let ((face-signifier (first x))
                              (mipmaps (second x)))
                          (list face-signifier (funcall choice-func mipmaps))))
                      (second cube-data))
                 #'sort-cube-map-faces-func :key #'first))))
    (if use-mipmaps-p
        ;; Processing contained mipmaps
        (ecase kind
          ((:1d :2d)
           (map 'vector #'read-image-contextually data))
          ((:1d-array :2d-array :3d)
           (map 'vector
                (lambda (x)
                  (map 'vector #'read-image-contextually x))
                data))
          ((:cube-map :cube-map-array)
           (map 'vector
                (lambda (x)
                  (process-cube-map-mipmaps
                   x
                   (lambda (x)
                     (map 'vector #'read-image-contextually x))))
                data)))
        ;; Processing only the first image location (mipmap 0)
        (ecase kind
          ((:1d :2d)
           (vector (read-image-contextually (aref data 0))))
          ((:1d-array :2d-array :3d)
           (vector (map 'vector #'read-image-contextually (aref data 0))))
          ((:cube-map :cube-map-array)
           ;; TODO: Test this path.
           (map 'vector
                (lambda (x)
                  (process-cube-map-mipmaps
                   x
                   (lambda (x)
                     (map 'vector #'read-image-contextually
                          (vector (aref x 0))))))
                data))))))

(defun validate-mipmap-images (images texture expected-mipmaps
                               expected-resolutions)
  "Given the settings in the TEXTURE, validate the real image objects in the
IMAGES vector to see if we have the expected number and resolution of mipmaps."
  (declare (ignorable expected-resolutions))
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (num-mipmaps (length images))
         (texture-name (name texture)))
    ;; We need at least one base image.
    ;; TODO: When dealing with procedurally generated textures, this needs to be
    ;; evolved.
    (unless (plusp num-mipmaps)
      (error "Texture ~a specifies no images! Please specify an image!"
             texture-name))
    (when use-mipmaps-p
      (cond
        ;; We have a SINGLE base mipmap (and we're generating them all).
        ((= num-mipmaps 1)
         ;; TODO: Check resolution.
         nil)
        ;; We have the exact number of expected mipmaps.
        ((and (= num-mipmaps expected-mipmaps)
              (<= num-mipmaps max-mipmaps))
         ;; TODO: Check resolutions.
         nil)
        ;; We have the exact number of mipmaps required that fills the entire
        ;; range expected by this texture.
        ((= num-mipmaps max-mipmaps)
         ;; TODO: Check resolutions
         nil)
        ;; Otherwise, something went wrong.
        ;; TODO: Should do a better error message which diagnoses what's wrong.
        (t
         (error "Texture ~a mipmap levels are incorrect:~% ~
                 ~2Ttexture-base-level = ~a~%~2Ttexture-max-level = ~a~% ~
                 ~2Tnumber of mipmaps specified in texture = ~a~% ~
                 ~2Texpected number of mipmaps = ~a~%Probably too many or to ~
                 few specified mipmap images."
                texture-name
                texture-base-level
                texture-max-level
                num-mipmaps
                expected-mipmaps))))))

(defun potentially-degrade-texture-min-filter (texture)
  "If the TEXTURE is not using mipmaps, fix the :texture-min-filter attribute on
the texture to something appropriate if it is currently set to using mipmap
related interpolation."
  (let ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
        (texture-name (name texture)))
    (symbol-macrolet ((current-tex-min-filter
                        (get-computed-applied-attribute
                         texture
                         :texture-min-filter)))
      (unless use-mipmaps-p
        (ecase current-tex-min-filter
          ((:nearest-mipmap-nearest :nearest-mipmap-linear)
           (warn "Down converting nearest texture min mipmap filter due to ~
                  disabled mipmaps. Please specify an override ~
                  :texture-min-filter for texture ~a"
                 texture-name)
           (setf current-tex-min-filter :nearest))
          ((:linear-mipmap-nearest :linear-mipmap-linear)
           (warn "Down converting linear texture min mipmap filter due to ~
                  disabled mipmaps. Please specify an override ~
                  :texture-min-filter for texture ~a"
                 texture-name)
           (setf current-tex-min-filter :linear)))))))

(defun potentially-autogenerate-mipmaps (texture-type texture)
  "If, for this TEXTURE, we are using mipmaps, and only supplied a single base
image, then we use GL:GENERASTE-MIPMAP to auto create all of the mipmaps in the
GPU memory."
  (let* ((use-mipmaps-p (get-computed-applied-attribute texture :use-mipmaps))
         (texture-max-level
           (get-computed-applied-attribute texture :texture-max-level))
         (texture-base-level
           (get-computed-applied-attribute texture :texture-base-level))
         (max-mipmaps (- texture-max-level texture-base-level))
         (data (get-computed-applied-attribute texture :data))
         (num-mipmaps
           (ecase texture-type
             ((:texture-1d :texture-2d :texture-3d)
              (length data))
             ((:texture-1d-array :texture-2d-array)
              (length (aref data 0)))
             ((:texture-cube-map :texture-cube-map-array)
              ;; TODO: This assumes no errors in specification of the number of
              ;; mipmaps.
              (let* ((cube (second (aref data 0)))
                     (face (aref cube 0))
                     (mipmaps (second face)))
                (length mipmaps))))))
    (when (and use-mipmaps-p
               (= num-mipmaps 1)  ; We didn't supply any but the base image.
               (> max-mipmaps 1)) ; And we're expecting some to exist.
      (gl:generate-mipmap texture-type))))

;; TODO: Candidate for user API.
(defun procedural-texture-descriptor-p (texdesc)
  (eq (texture-type texdesc) :procedural))

;; TODO: Candidate for user API.
(defun procedural-texture-p (texture)
  (procedural-texture-descriptor-p (semantic-texdesc texture)))

(defun generate-computed-texture-descriptor (texture)
  "Perform an identity copy of the semantic texture descriptor in TEXTURE and
assign it to the computed texture descriptor slot in TEXTURE."
  (setf (computed-texdesc texture)
        (copy-texture-descriptor (semantic-texdesc texture))))

;; TODO 1. make missing texture and missing material not recompiliable.

(defun find-debug-texture (core)
  (let* ((texture-name (canonicalize-texture-name 'x:debug-texture))
         (texture (v::resource-cache-peek
                   (v::context core) :texture texture-name)))
    (unless texture
      (error "FIND-DEBUG-TEXTURE: It is impossible that the debug-texture ~
              doesn't not exist. Fixme."))
    texture))

(defun unload-texture (context texture)
  (gl:delete-texture (texid texture))
  (let ((debug-texture (find-debug-texture (v::core context))))
    (setf (texid texture) (texid debug-texture))))

(defun refresh-texture (context texture texture-name)
  ;; Unload the current texid and shove back into a new texid whatever the
  ;; new texture descriptor for the texture-name indicates.
  (with-accessors ((texid texid)) texture
    (unload-texture context texture)
    (let((semantic-texdesc
           (find-semantic-texture-descriptor
            (semantic-texture-name texture-name) context))
         (id (gl:gen-texture)))
      (setf (semantic-texdesc texture) semantic-texdesc
            texid id)
      (upload-texture context texture))))

(defun load-texture (context texture-name)
  "Return a TEXTURE instance that represents the loading of the images required
for TEXTURE-NAME from disk (or elsewhere) and the uploading of it to the GPU.
If TEXTURE-NAME does not resolve to a known semantic texture descriptor then
return the TEXTURE instance for the debug-texture."
  (u:if-let ((semantic-texdesc
              (find-semantic-texture-descriptor
               (semantic-texture-name texture-name) context)))
    (let* ((id (gl:gen-texture))
           ;; TODO: tex-name is wrong here. It shoudl be what the material
           ;; believed it to be or the canonicalname of it if only a symbol.
           (texture (make-instance 'texture
                                   :name texture-name
                                   :semantic-texdesc semantic-texdesc
                                   :texid id)))
      (values (upload-texture context texture) t))

    (values (v::resource-cache-lookup
             context :texture
             (canonicalize-texture-name 'x:debug-texture))
            nil)))

(defun upload-texture (context texture)
  (cond
    ;; Non procedural textures have their semantic texdescs automatically
    ;; converted to computed-texdesc and their information is loaded
    ;; immediately onto the GPU.
    ((not (procedural-texture-p texture))
     (generate-computed-texture-descriptor texture)
     (with-accessors ((computed-texdesc computed-texdesc)) texture
       (gl:bind-texture (texture-type computed-texdesc) (texid texture))
       (set-opengl-texture-parameters texture)
       (load-texture-data (texture-type computed-texdesc) texture context)))
    ;; Procedural textures are completely left alone EXCEPT for the texid
    ;; assigned above. It is up to the user to define all the opengl
    ;; parameters and GPU data for this texture.
    (t
     (add-unrealized-texture texture context)
     ;; TODO: Possibly record a note that someone asked for this specific
     ;; texture name, so in the prologue function, the gamedev can know the
     ;; entire set they need to create that is eagerly going to be used.
     nil))
  texture)

(defun parse-texture-profile (name body-form)
  (u:with-gensyms (texprof)
    `(let* ((,texprof (make-texture-profile :name ',name)))
       (setf ,@(loop :for (attribute value) :in body-form
                     :appending `((u:href (attributes ,texprof) ,attribute)
                                  ,value)))
       ,texprof)))

(defmacro define-texture-profile (name &body body)
  "Define a set of attribute defaults that can be applied while defining a
texture."
  (u:with-gensyms (profile)
    `(let ((,profile ,(parse-texture-profile name body)))
       (setf (u:href v::=meta/texture-profiles= (name ,profile)) ,profile))))

(defun update-texture (context old-descriptor new-descriptor)
  (v::push-queue
   :recompile
   (list
    :texture
    (lambda (core)
      (if old-descriptor
          (let* ((old-name (canonicalize-texture-name (name old-descriptor)))
                 (old-texture (v::resource-cache-peek
                               (v::context core)
                               :texture old-name)))
            (remove-semantic-texture-descriptor old-descriptor core)
            ;; NOTE: Can't refactor the two lines below cause it needs the
            ;; remove-semantic-texture-descriptor to have been called.
            (add-semantic-texture-descriptor new-descriptor core)
            (resolve-semantic-texture-descriptor core new-descriptor)
            (refresh-texture context old-texture old-name))
          (let ((new-name (canonicalize-texture-name (name new-descriptor))))
            (add-semantic-texture-descriptor new-descriptor core)
            (resolve-semantic-texture-descriptor core new-descriptor)
            (v::resource-cache-lookup context :texture new-name)))))))

(defun update-texture/interactively (old-descriptor new-descriptor)
  (when (boundp 'v::*core-debug*)
    (let ((context (v:context v::*core-debug*)))
      (update-texture context old-descriptor new-descriptor))))

(defmacro define-texture (name (textype &rest profile-overlay-names) &body body)
  "Construct a semantic TEXTURE-DESCRIPTOR. "
  (u:with-gensyms (desc-lookup old-desc new-desc)
    `(symbol-macrolet ((,desc-lookup (u:href v::=meta/textures= ',name)))
       (let ((,new-desc (make-texture-descriptor
                         :name ',name
                         :texture-type ',textype
                         :profile-overlay-names ',profile-overlay-names))
             (,old-desc ,desc-lookup))
         ;; Record the parameters we'll overlay on the profile at use time.
         (setf ,@(loop :for (key value) :in body
                       :append `((u:href (attributes ,new-desc) ,key)
                                 ,value)))
         (setf ,desc-lookup ,new-desc)
         (update-texture/interactively ,old-desc ,new-desc)))))

(defun resolve-semantic-texture-descriptor (core texture-descriptor)
  (symbol-macrolet ((profiles (profiles (v::textures core)))
                    (default-profile-name 'x:default-profile))
    ;; 1. Check for x:default-profile
    (unless (u:href profiles default-profile-name)
      (error "Default-profile for texture descriptors is not defined."))
    ;; 2. For each texture-descriptor, apply all the profiles in order.
    ;; 3. Check that the specified profiles are valid.
    ;; TODO: 4
    ;; TODO: 5
    (let* ((profile-overlays
             ;; First, gather the specified profile-overlays
             (loop :for profile-overlay-name :in (profile-overlay-names
                                                  texture-descriptor)
                   :collect
                   (u:if-found (concrete-profile
                                (u:href profiles profile-overlay-name))
                     concrete-profile
                     (error "Texture profile ~a does not exist."
                            profile-overlay-name))))
           (attributes (attributes texture-descriptor))
           (applied-attributes (applied-attributes texture-descriptor))
           (profile-overlays
             ;; Then, if we don't see a profile-default in there, we put it
             ;; first automatically.
             (if (member default-profile-name (profile-overlay-names
                                               texture-descriptor))
                 profile-overlays
                 (list* (u:href profiles default-profile-name)
                        profile-overlays))))
      ;; Now, overlay them left to right into the applied-attributes table in
      ;; texdesc.
      (dolist (profile-overlay profile-overlays)
        (maphash
         (lambda (key value)
           (setf (u:href applied-attributes key)
                 value))
         (attributes profile-overlay)))
      ;; And finally fold in the texdesc attributes last
      (maphash
       (lambda (key value)
         (setf (u:href applied-attributes key) value))
       attributes))))

(defun resolve-all-semantic-texture-descriptors (core)
  " Ensure that these aspects of texture profiles and desdcriptors are ok:
1. The X:DEFAULT-PROFILE exists.
2. Each texture-descriptor has an updated applied-profile set of attributes.
3. All currently known about texture descriptors have valid profile references.
4. All images specified by paths actually exist at that path.
5. The texture type is valid."
  (u:do-hash-values (v (semantic-texture-descriptors (v::textures core)))
    (resolve-semantic-texture-descriptor core v)))

;; public API
(defun general-data-format-descriptor (&key width height depth internal-format
                                         pixel-format pixel-type data)
  "Produce a descriptor for generalized volumetric data to be loaded into a
:texture-3d type texture. If :data has the value :empty, allocate the memory of
the size and types specified on the GPU."
  ;; TODO: Implement me!
  (declare (ignore width height depth internal-format pixel-format pixel-type
                   data))
  #())

;;; Interim use of the resource-cache API.

;; TODO: candidate for public API.
(defmethod v::resource-cache-layout ((entry-type (eql :texture)))
  '(equalp))

;; TODO: candidate for public API.
(defmethod v::resource-cache-construct (context (entry-type (eql :texture))
                                        &rest keys)
  (destructuring-bind (texture-location) keys
    (load-texture context texture-location)))

;; TODO: candidate for public API.
(defmethod v::resource-cache-dispose (context (entry-type (eql :texture))
                                      texture)
  (gl:delete-texture (texid texture)))

;;; Utility functions for texturing, maybe move elsewhere.

(defun canonicalize-texture-name (texture-name)
  "If the TEXTURE-NAME is a symbol, we canoniclalize it to the list:
 (TEXTURE-NAME 0). If it is a consp, we return it unmodified."
  (typecase texture-name
    (symbol (list texture-name 0))
    (cons texture-name)
    (t (error "Unable to canonicalize the texture name: ~a" texture-name))))

;; TODO: Maybe reifiy the semantic-name, texture-instance-name specification
;; into a CLOS object.
(defun semantic-texture-name (texture-name)
  "Given a TEXTURE-NAME (which could be either a symbol as the semantic name of
a texture, or a list when it is an instance of a texture), return just the
semantic name of it which was specified with a DEFINE-TEXTURE."
  (if (consp texture-name)
      (first texture-name)
      texture-name))

(defun load-texture-descriptors (core)
  (gl:pixel-store :unpack-alignment 1)
  (u:do-hash-values (profile v::=meta/texture-profiles=)
    (add-texture-profile profile core))
  (u:do-hash-values (desc v::=meta/textures=)
    (add-semantic-texture-descriptor desc core))
  (resolve-all-semantic-texture-descriptors core))
