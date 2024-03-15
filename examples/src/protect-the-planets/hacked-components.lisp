(in-package #:virality-examples)

;; NOTE: This file is here because I need draw order to work for PtP but the V
;; engine doesn't yet have a proper system for representing sorting of
;; rendering components. So, these two components take the place of the
;; rendering component in PtP. They will be removed and this hackish solution
;; deleted when rendering components can properly sort. This method is also
;; crappy and not intended for default behavior in V. It is too nasty to use.
;; The code duplication alone is vomitous.

;; NOTE: I should be able to specify these components in the typedag, but
;; that functionality seems terribly broken right now.


(defclass render-sort ()
  ((%render-layer :accessor render-layer
                  :initarg :render-layer
                  ;; NIL means draw right now.
                  :initform nil)))

;; This is effectively the render component but instead of rendering itself it
;; sketches a reference of itself into a hash table in another components keyed
;; by the render-layer. Then, when that other component is forced to execute
;; after this one, it actually performs the render in the right order.
(v:define-component sketch (comp:render render-sort)
  ((%delayed-render-ref :accessor delayed-render-ref
                        :initarg :delayed-render-ref
                        :initform nil)))

;; TODO: Make this constant time
(defmacro with-depth-function (material &body body)
  `(destructuring-bind (&key depth) (v::attributes ,material)
     (if depth
         (let ((old-depth (v::get-gpu-parameter :depth-func)))
           (unwind-protect
                (progn
                  (gl:depth-func depth)
                  ,@body)
             (gl:depth-func old-depth)))
         (progn ,@body))))

(defmacro with-material (material (&rest bindings) &body body)
  "Bind uniforms in BINDINGS before evaluating the BODY.
If the uniform doesn't exist, silently ignore the setting of it.  NOTE: This
means if the BODY sets a uniform it will be IGNORED for this render, and if a
shared material may affect the NEXT rendering call!"
  (u:with-gensyms (material-ref)
    `(let ((,material-ref ,material))
       (shadow:with-shader (v::shader ,material-ref)
         ;; TODO: This behavior here implies a policy about uniform usage for
         ;; materials, in that, using a material that doesn't define this
         ;; uniform will silently ignore the issue if you try setting it.
         ;; The todo here is figure out how to get this better described so
         ;; we can do compile time checks on uniforms.
         (progn
           ,@(loop :for (k v) :on bindings :by #'cddr
                   :collect `(when (v:uniform-ref-p ,material-ref ,k)
                               (setf (v:uniform-ref ,material-ref ,k) ,v))))
         (v::bind-material ,material-ref)
         (with-depth-function ,material-ref
           ,@body)))))

(defmethod v:on-component-initialize ((self sketch))
  (with-slots (comp::%transform) self
    (setf comp::%transform (v:component-by-type (v:actor self) 'transform))))

(defmethod v:on-component-render ((self sketch))
  (with-accessors ((render-layer render-layer)
                   (delayed-render-ref delayed-render-ref)
                   (context v:context))
      self

    ;; First thing we do is try and find the delayed-render component and
    ;; set my slot to point to it. If we don't findit, yeesh, I guess we don't
    ;; render and keep going. This is a bad hack. :)
    (unless delayed-render-ref
      (let ((actor
              (first (tags-find-actors-with-tag context
                                                :delayed-render-system))))
        (if (null actor)
            (return-from v:on-component-render nil)
            (setf delayed-render-ref
                  (v:component-by-type actor 'delayed-render)))))

    (if render-layer
        ;; add self into delayed-render and do it later.
        (%add-sketch delayed-render-ref self)
        ;; else render self right now.
        (%really-render self))))

(defun %really-render (self)
  (u:when-let ((camera (v::active-camera (v:context self)))
               (slave (comp::slave self))
               (material (comp::material self)))

    (with-material material
        (:model (v:get-model-matrix self)
         :view (comp:view camera)
         :proj (comp:projection camera))

      ;; TODO: For now, if this uniform exists, set it up. We don't want to
      ;; unecessarily set this all the time, since it could compute work for a
      ;; lot of situations where we don't need it and as a general rule
      ;; materials might not have it. Another solution is that the user sets a
      ;; function for the normal-matrix and when it gets called it figures out
      ;; the actor and passes it in (along with the material). Need to think on
      ;; it a little bit more. Currently the function only knows of the context
      ;; and the material. Since materials are shared by different renderers,
      ;; materials can't have backreferences to their components/actors.
      (when (v:uniform-ref-p material :normal-matrix)
        (setf (v:uniform-ref material :normal-matrix)
              ;; TODO: Implement a m3:invert and reorganize this expressions
              ;; to reduce operations and not cons memory.
              (m4:rotation-to-mat3
               (m4:transpose (m4:invert
                              (m4:set-translation
                               (m4:* (comp:view camera)
                                     (v:get-model-matrix self))
                               (v3:zero)))))))



      (v:on-component-slave-render self slave))))

;; ------------------------------------------------------------------------

;; This component is executed after the above in the type graph.
;; It renders everything in the right order and then clears the layer-table
;; for the next frame's work. This goes on ONE actor in the world, and it is
;; found by the sketch component when needed.
(v:define-component delayed-render ()
  ((%sketch-table :reader sketch-table
                  :initform (u:dict #'eq))
   (%layer-order :accessor layer-order
                 :initarg :layer-order)))

(defmethod v:on-component-render ((self delayed-render))
  (with-accessors ((sketch-table sketch-table)
                   (layer-order layer-order))
      self

    (dolist (layer layer-order)
      (let ((sketches (u:href sketch-table layer)))
        (loop :for sketch :in sketches
              :do (when (v:actor sketch) ;; <- TODO: Big Damn Hack.
                    (%really-render sketch)))))
    (clrhash sketch-table)))

(defun %add-sketch (delayed-render sketch)
  (push sketch (u:href (sketch-table delayed-render) (render-layer sketch))))
