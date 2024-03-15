(in-package #:virality)

;;;; Implementation of datatypes ANNOTATION-VALUE, COMPONENT-CLASS,
;;;; ANNOTATABLE-SLOT

(defun identity/annotation (value component)
  (declare (ignore component))
  value)


(defun make-annotation-value (name snid state
                              &key
                                (getter #'identity/annotation)
                                (setter #'identity/annotation))
  (make-instance 'annotation-value
                 :name name
                 :serialnum snid
                 :state state
                 :getter getter
                 :setter setter))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotation DB API
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; called at the end of define-annotation. it'll also give back the appropriate
;; annotation entry.
(defun register-annotation (component-metaclass-name annotation-name state
                            &key (getter #'identity/annotation)
                              (setter #'identity/annotation))
  (let* ((db (find-class component-metaclass-name))
         (annodb (annotations db))
         (entry (u:href annodb annotation-name)))
    ;; first look it up.
    ;; use AU to fix this.
    (unless entry
      (let ((snid (prog1 (annotation-serialnum db)
                    (incf (annotation-serialnum db)))))
        ;; make a new generic one with defaults.
        (setf entry (make-annotation-value
                     annotation-name snid :forward-reference)
              (u:href annodb annotation-name) entry
              (annotations-dirty-p db) t)))
    ;; then debate what to do based upon state.
    (ecase state
      (:initialized
       (setf (state entry) state
             (setter entry) setter
             (getter entry) getter))
      (:forward-reference
       nil))

    entry))

;; called by slot-value-using-class for a specific component.
(defun lookup-annotation (component-metaclass-name annotation-name)
  (let ((entry (register-annotation
                component-metaclass-name annotation-name :forward-reference)))
    entry))

(defun optimize-annotations (component-metaclass-name)
  (let ((db (find-class component-metaclass-name)))
    (when (annotations-dirty-p db)
      (let* ((annodb (annotations db))
             (num-annotations (hash-table-count annodb))
             (optiarray (make-array num-annotations)))
        ;; now, fill the array with the annotations at their snid spots.
        (u:do-hash-values (v annodb)
          (setf (aref optiarray (serialnum v)) v))
        ;; and replace the previous one in the db.
        (setf (annotation-array db) optiarray
              ;; and we're not dirty anymore.
              (annotations-dirty-p db) nil)))))

;; Ability to reset the annotations.
(defun clear-annotations (component-metaclass-name)
  (let ((db (find-class component-metaclass-name)))
    (setf (annotations db) (u:dict #'eq)
          (annotation-serialnum db) 0
          (annotations-dirty-p db) nil
          (annotation-array db) #()
          (annotated-slots db) (u:dict #'eq))))

;; Define a set of slot definition classes that understand the concept of being
;; annotated.


(defmethod annotation (non-annotatable-slot)
  nil)

(defmethod annotation-indexes (non-annotatable-slot)
  nil)

;; Is any direct/effective slot instance (of any type) annotated?
(defgeneric annotated-slot-p (slot))

(defmethod annotated-slot-p (slot)
  nil)

(defmethod annotated-slot-p ((slot component-annotated-direct-slot-definition))
  t)

(defmethod annotated-slot-p
    ((slot component-annotated-effective-slot-definition))
  t)

;; The entry point of the finalization of the component classes.
(defmethod c2mop:compute-slots ((class component-class))
  (call-next-method))

;; We override this in order that direct slots can have :annotation added to the
;; slot initializer arguments.
(defmethod c2mop:direct-slot-definition-class ((class component-class)
                                               &rest initargs)
  (cond
    ((getf initargs :annotation)
     (find-class 'component-annotated-direct-slot-definition))
    (t
     (find-class 'component-direct-slot-definition))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return NIL if no annotations, otherwise, get the union of them from
;; all the slots, in the same order as supplied
(defun dslotds-annotated-p (dslotds)
  ;; Note: dslotds are in most-specific-first order.
  (let (annotations)
    (dolist (dslotd dslotds)
      (when (annotated-slot-p dslotd)
        (push (annotation dslotd) annotations)))
    (u:flatten-tree (nreverse annotations))))

;; Lifted from sb-pcl::compute-effective-slot-definition-initargs
;; We add knowledge of the :annotation slot and how it works across
;; all the direct slots contributing to the definition of the annotated
;; effective slots.
;; direct-slotds SHOULD BE most-specific-to-least-specific order.
(defun compute-annotated-effective-slot-definition-initargs
    (class direct-slotds)
  (let* ((name nil)
         (initfunction nil)
         (initform nil)
         (initargs nil)
         (allocation nil)
         #+sbcl (allocation-class nil)
         (type t)
         (namep  nil)
         (initp  nil)
         (allocp nil)
         (annotation nil))
    (dolist (slotd direct-slotds)
      (when slotd
        (unless namep
          (setq name (c2mop:slot-definition-name slotd)
                namep t))
        ;; Our annotation policy is to merge all annotations going towards the
        ;; root for this slot.
        (when (annotation slotd)
          (push (annotation slotd) annotation))
        (unless initp
          (let ((it (c2mop:slot-definition-initfunction slotd)))
            (setq initform (c2mop:slot-definition-initform slotd)
                  initfunction it
                  initp t)))
        (unless allocp
          (setq allocation (c2mop:slot-definition-allocation slotd)
                ;; Support SBCL who needs to know what class provided this slot.
                ;; TODO CCL might need something like this too.
                #+sbcl allocation-class
                #+sbcl (sb-pcl::slot-definition-class slotd)
                allocp t))
        (setq initargs
              (append (c2mop:slot-definition-initargs slotd) initargs))
        (let ((slotd-type (c2mop:slot-definition-type slotd)))
          (setq type (cond
                       ((eq type t) slotd-type)
                       (t `(and ,type ,slotd-type)))))))
    `(:name ,name
      :initform ,initform
      :initfunction ,initfunction
      :initargs ,initargs
      :allocation ,allocation
      ;; SBCL requires the knowlede of the class that initially defined this
      ;; slot.
      #+sbcl :allocation-class
      #+sbcl ,allocation-class
      :type ,type
      :class ,class
      ;; documentation is not supported cause no MOP function to get it.
      :documentation nil
      ;; FL new stuff, but only if the any direct slot was actually annotated.
      ;; If there are no annotations, then return value will produce a
      ;; component-effective-slot-definition. If there were annotations, then
      ;; this return value will produce a
      ;; component-annotated-direct-slot-definition. Hence, this is why it is
      ;; optionally in this output.
      ,@(when annotation
          (list :annotation (remove-duplicates
                             (u:flatten-tree (nreverse annotation))
                             :from-end t))))))

(defun compute-component-initargs (component-type)
  (let ((methods nil)
        (class (c2mop:ensure-finalized (find-class component-type))))
    (push (find-method #'initialize-instance '(:before) (list class) nil)
          methods)
    (push (find-method #'initialize-instance '(:after) (list class) nil)
          methods)
    (push (find-method #'initialize-instance '(:around) (list class) nil)
          methods)
    (push (find-method #'reinitialize-instance '(:before) (list class) nil)
          methods)
    (push (find-method #'reinitialize-instance '(:after) (list class) nil)
          methods)
    (push (find-method #'reinitialize-instance '(:around) (list class) nil)
          methods)
    (push (find-method #'shared-initialize '(:before) (list class t) nil)
          methods)
    (push (find-method #'shared-initialize '(:after) (list class t) nil)
          methods)
    (push (find-method #'shared-initialize '(:around) (list class t) nil)
          methods)
    (remove-duplicates
     (union (u:mappend #'c2mop:slot-definition-initargs
                       (c2mop:class-slots class))
            (mapcan
             (lambda (x)
               (mapcar #'u:make-keyword
                       (rest (member '&key (c2mop:method-lambda-list x)))))
             (remove nil methods))))))

;; Ok, this processes a LIST of dslotds that can all be different types and are
;; melded together into a single effective slot.
(defmethod c2mop:compute-effective-slot-definition ((class component-class)
                                                    name
                                                    dslotds)
  ;; If none of the slots have any annotations, just do what we were going to do
  ;; normally. This preserves :documentation on the slot when possible and
  ;; anything else the vendor lisp might be doing.
  (unless (dslotds-annotated-p dslotds)
    (return-from c2mop:compute-effective-slot-definition
      (call-next-method)))
  ;; All righty, if we have *any* annotated direct slots we're combining into
  ;; the effective slot, we must do the whole of the work ourselves.
  (let* ((initargs
           ;; This function knows what to do with the annotations. Sadly, it
           ;; drops :documentation since it isn't portable to get.
           (compute-annotated-effective-slot-definition-initargs class dslotds))
         (effective-class (apply #'c2mop:effective-slot-definition-class
                                 class initargs))
         (slotd (apply #'make-instance effective-class initargs)))
    ;; NOTE: This only happens when we finalize the class, so all
    ;; define-component expansions also expand to c2mop:ensure-finalized.
    (dolist (annotation (getf initargs :annotation))
      (register-annotation 'component annotation :forward-reference))
    ;; NOTE: this code is executed (num-components * num-annotations) given the
    ;; number of components...Since for each component, we rebuild this array.
    ;; However, there are probably going to not be that many annotations (<100
    ;; maybe?) so we're probably ok. Also, this expects the class to have
    ;; already been finalized (which we force in DEFINE-COMPONENT).
    (optimize-annotations 'component)
    ;; NOTE: Produce the compiled optimization indexes for this slot so I can
    ;; execute them properly in slot-value-using-class for this component-type.
    (setf (annotation-indexes slotd)
          (make-array (length (annotation slotd))
                      :initial-contents
                      (mapcar
                       (lambda (x)
                         (serialnum
                          (u:href (annotations (find-class 'component)) x)))
                       (annotation slotd))))

    slotd))

(defmethod c2mop:effective-slot-definition-class ((class component-class)
                                                  &rest initargs)
  (let ((chosen-class
          (if (getf initargs :annotation)
              (find-class 'component-annotated-effective-slot-definition)
              (find-class 'component-effective-slot-definition))))
    chosen-class))

;; Utility function to get me knowledge about all annotated effective slots in a
;; class. Assumes class is finalized.
(defun collect-all-annotated-effective-slot-data (component-class-name)
  (remove-if #'null
             (mapcar
              (lambda (x)
                (when (annotated-slot-p x)
                  (list (c2mop:slot-definition-name x)
                        (c2mop:slot-definition-initargs x)
                        (annotation x))))
              (c2mop::class-slots (find-class component-class-name)))))

;; Here we collect all the annotated slot data from a component and put it into
;; the COMPONENT meta-class slots.
(defun track-annotations (component-name)
  (setf (u:href (annotated-slots (find-class 'component)) component-name)
        (collect-all-annotated-effective-slot-data component-name)))

;;; Stuff used to make DEFINE-COMPONENT work.

(defun %collect-annotated-component-slot-forms (slots)
  (loop :for (name . options) :in slots
        :for anno = (getf options :annotation)
        :when anno
          ;; We need the ACTUAL slot name, so the % prefixed one.
          :collect `(,name ,anno)))

;; TODO: This is sort of a scary location to put DEFINE-COMPONENT. Can we break
;; up this component and MOP code to not be so scary? Or we could at least move
;; DEFINE-COMPONENT into kernel/component.lisp. :)

(defmacro define-component (name super-classes &body body)
  (destructuring-bind (slots &optional storage-metadata) body
    ;; At macro-expansion time actually forward-declare the annotations into
    ;; the 'component class and store a reference to the registered entries.
    ;; We use this to generate the optimized lookups in the read/write
    ;; function. This works because all DEFINE-COMPONENT invocations must
    ;; happen AFTER the metaclass COMPONENT-CLASS and the base component type
    ;; of COMPONENT have been declared and finalized.
    (dolist (anno-slot (%collect-annotated-component-slot-forms slots))
      (dolist (anno (second anno-slot))
        (when anno
          (register-annotation 'component anno :forward-reference))))
    `(progn
       ;; NOTE: We check the super classes to ensure that if COMPONENT isn't
       ;; in there, we know to add it at the beginning. All components
       ;; themselves are ensure-finalized, so this should work for the most
       ;; part. If anything, we'll have to finalize everything in the
       ;; super-classes and then check the subtypep.
       (defclass ,name (,@(if (some (lambda (super-class)
                                      (subtypep super-class 'component))
                                    super-classes)
                              super-classes
                              (append '(component) super-classes)))
         ,slots
         (:metaclass component-class))
       (c2mop:ensure-finalized (find-class ',name))
       (track-annotations ',name)
       ;; Reader for annotated slots at this class inheritance level.
       (defmethod c2mop:slot-value-using-class
           ((class component-class)   ; the component metaclass
            (instance ,name)          ; the specific component
            (slotd component-annotated-effective-slot-definition))
         (let ((annoarray (annotation-array (find-class 'component))))
           (loop :with original-slot-value = (c2mop:standard-instance-access
                                              instance
                                              (c2mop:slot-definition-location
                                               slotd))
                 :with composing-value = original-slot-value
                 :for index :across (annotation-indexes slotd)
                 :for annotation-value = (aref annoarray index)
                 :do (setf composing-value
                           (funcall (getter annotation-value)
                                    composing-value instance))
                 :finally (return composing-value))))
       ;; writer for annotated slots at this class inheritance level.
       (defmethod (setf c2mop:slot-value-using-class)
           (new-value
            (class component-class) ;; the component metaclass
            (instance ,name)        ;; the specific component
            (slotd component-annotated-effective-slot-definition))
         (let ((annoarray (annotation-array (find-class 'component))))
           (setf (c2mop:standard-instance-access
                  instance (c2mop:slot-definition-location slotd))
                 (loop :with composing-value = new-value
                       :for index :across (annotation-indexes slotd)
                       :for annotation-value = (aref annoarray index)
                       :do (setf composing-value
                                 (funcall (setter annotation-value)
                                          composing-value instance))
                       :finally (return composing-value)))))
       ;; A method to locate the metadata in the storage form in this
       ;; class.
       (defmethod ,(intern "STORAGE-METADATA" :virality)
           ((component-name (eql ',name)) &optional namespace)
         (declare (ignore component-name))
         (let ((ss-meta ',storage-metadata))
           (if namespace
               ;; TODO: make this better/faster
               (find namespace ss-meta :key #'first)
               ss-meta))))))

(defun get-computed-component-precedence-list (component-type)
  (u:when-let ((class (find-class component-type nil)))
    (loop :for class :in (c2mop:compute-class-precedence-list class)
          :for name = (class-name class)
          :until (eq name 'component)
          :when (subtypep name 'component)
            :collect name)))
