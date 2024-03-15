(in-package #:virality-examples)

(v:define-component example-selector ()
  ((%selector-prefabs :accessor selector-prefabs
                      :initarg :selector-prefabs
                      :initform nil)
   (%selector-camera :reader selector-camera
                     :initarg :selector-camera
                     :initform nil)
   (%selector-text :accessor selector-text
                   :initarg :selector-text)
   (%selector-text-renderer :accessor selector-text-renderer
                            :initarg :selector-text-renderer)
   (%selector-initialized :accessor selector-initialized
                          :initarg :selector-initialized
                          :initform nil)
   (%selector-selected :accessor selector-selected
                       :initarg :selector-selected
                       :initform 0)
   (%selector-safe-actors :accessor selector-safe-actors
                          :initarg :selector-safe-actors
                          :initform nil)
   (%selector-state :accessor selector-state
                    :initarg :selector-state
                    :initform :selecting-example) ;; or :playing-example
   ))

(defun update-selector-text (self)
  (with-accessors ((selector-prefabs selector-prefabs)
                   (selector-text selector-text)
                   (selector-selected selector-selected))
      self
    (let* ((header
             (mapcar (lambda (line) (format nil line))
                     (list
                      "    Examples Menu~%"
                      "      UP/DOWN - Select next/prev example.~%"
                      "      ENTER - Start the example.~%"
                      "      HOME - Reset currently running example.~%"
                      "      END - Exit example and return to Menu.~%"
                      "      ESC - Exit Engine Right Now.~%"
                      )))
           (header (apply #'concatenate 'string header))
           (new-text
             (apply #'concatenate 'string
                    header
                    (loop :for (prefab lib) :across selector-prefabs
                          :for idx :from 0
                          :collect
                          (format nil "[~:[ ~;X~]] ~A - ~A in ~(~S~)~%"
                                  (= idx selector-selected)
                                  idx prefab lib))))
           (new-text (if (plusp (length new-text))
                         new-text
                         "No examples to run!")))

      (setf (comp::text selector-text) new-text))))

(defun kill-non-selector-actors (self)
  (with-accessors ((selector-safe-actors selector-safe-actors)
                   (context v:context))
      self
    (comp:map-actors (lambda (actor)
                       (unless (member actor selector-safe-actors)
                         (v:destroy actor)))
                     :universe context)))

(defun spawn-selected-prefab (self)
  (with-accessors ((selector-prefabs selector-prefabs)
                   (selector-selected selector-selected)
                   (context v:context))
      self
    (v:make-prefab-instance
     (v::core context)
     ;; TODO: So far, only starts ONE prefab, augment to take
     ;; a list.
     (list (aref selector-prefabs selector-selected)))))

(defmethod v:on-component-update ((self example-selector))
  (with-accessors ((selector-prefabs selector-prefabs)
                   (selector-camera selector-camera)
                   (selector-text selector-text)
                   (selector-text-renderer selector-text-renderer)
                   (selector-initialized selector-initialized)
                   (selector-selected selector-selected)
                   (selector-safe-actors selector-safe-actors)
                   (selector-state selector-state)
                   (context v:context))
      self
    ;; TODO: This is an idiom that really requires a whole protocol function
    ;; (or "something" assigned to THIS prefab that we can execute) to be run
    ;; after the entire prefab is set up correctly. It cannot be done in
    ;; on-component-initialize because that is defined to happen before any
    ;; attachments nor in any attachment function because they run out of
    ;; order. Since we don't yet have such a thing, we mimic one like this
    ;; (which of course does the check every frame, eeww.)
    (unless selector-initialized
      ;; Find all actors associated with the selector ONCE...
      (comp::map-actors (lambda (actor)
                          (push actor selector-safe-actors))
                        (v:actor self))

      (update-selector-text self)
      (kill-non-selector-actors self)

      (setf selector-initialized t)

      (return-from v:on-component-update))


    ;; Run the state machine to move back and forth between selector and
    ;; examples.

    (ecase selector-state
      (:selecting-example
       (let ((num-prefabs (length selector-prefabs))
             (next-p (v:on-button-enter context :key :down))
             (prev-p (v:on-button-enter context :key :up))
             (start-example-p (v:on-button-enter context :key :return)))

         ;; TODO: Make this case better.
         (unless (plusp num-prefabs)
           (return-from v:on-component-update))

         (cond
           ;; Move the indicator forwards or backwards
           (next-p
            (setf selector-selected
                  (let ((new-idx (1+ selector-selected)))
                    (mod new-idx num-prefabs)))

            (update-selector-text self))

           (prev-p
            (setf selector-selected
                  (let ((new-idx (1- selector-selected)))
                    (if (< new-idx 0)
                        (1- num-prefabs)
                        new-idx)))

            (update-selector-text self))

           (start-example-p

            (setf (comp:active-p selector-camera) nil
                  (comp:render-p selector-text-renderer) nil)

            (spawn-selected-prefab self)

            (setf selector-state :playing-example)))))

      (:playing-example
       (let ((reset-p (v:on-button-enter context :key :home))
             (exit-p (v:on-button-enter context :key :end)))

         (cond
           (exit-p
            (kill-non-selector-actors self)
            (setf (comp:active-p selector-camera) t
                  (comp:render-p selector-text-renderer) t
                  selector-state :selecting-example))
           (reset-p
            (kill-non-selector-actors self)
            (spawn-selected-prefab self))))))
    nil))

(v:define-prefab "selector-text-display" (:library examples)
  ("text-container"
   (comp:geometry :name 'comp::text)
   (comp:font :asset '(metadata font)
              :rate 0
              :text "Default Text")
   (comp:render :material 'font
                :slave (v:ref :self :component 'comp:geometry))))

(v:define-prefab "example-selector" (:library examples)
  (example-selector
   :selector-camera (v:ref "/example-selector/camera"
                           :component 'comp:camera)
   :selector-text (v:ref "/example-selector/options/text-container"
                         :component 'comp:font)
   :selector-text-renderer (v:ref "/example-selector/options/text-container"
                                  :component 'comp:render)
   :selector-prefabs #(("damaged-helmet" examples)
                       ("damaged-helmet-turn-table" examples)
                       ("flying-helmet" examples)
                       ("collision-smoke-test" examples)
                       ("collision-transform-test-0" examples)
                       ("[BROKEN] collision-transform-test-1" examples)
                       ("collision-test-0" examples)
                       ("collision-test-1" examples)
                       ("collision-test-2" examples)
                       ("collision-test-3" examples)
                       ("collision-test-4" examples)
                       ("collision-test-5" examples)
                       ("collision-test-6" examples)
                       ("dynamic-geometry" examples)
                       ("geometric-volumes" examples)
                       ("graph" examples)
                       ("3d-graph-1" examples)
                       ("3d-graph-2" examples)
                       ("isometric-view" examples)
                       ("noise" examples)
                       ("art1" examples)
                       ("art2" examples)
                       ("art3" examples)
                       ("art4" examples)
                       ("art5" examples)
                       ("art6" examples)
                       ("sprite" examples)
                       ("texture" examples)
                       ("scale-around" examples)
                       ("text-wall-clock-time" examples)
                       ("artblob" examples)
                       ("artblob-interactive" examples)
                       ("artblob-group" examples)
                       ("ship01" examples)
                       ("ship01-interactive" examples)
                       ("ship-group" examples)
                       ("protect-the-planets" ptp)
                       ))

  ;; default camera will get activated and deactivated to make room for the one
  ;; in the example.
  (("camera" :link "/cameras/ortho")
   (comp:transform :translate (v3:vec 0f0 0f0 50f0)))

  ;; The text display will show all the time (unless I add features to
  ;; turn it on and off.
  (("options" :link "/selector-text-display")
   (comp:transform :scale (v3:uniform .5f0))))
