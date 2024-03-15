(in-package #:virality)

(defun load-initial-scene (core scene-name)
  (let ((scene-name (or scene-name =initial-scene=)))
    (make-prefab-instance core scene-name)))

(defun initialize (core project scene-name)
  (load-config project)
  (setup-repl)
  (u:initialize-rng)
  (prepare-gamepads)
  (make-display core)
  (make-input-data core)
  (load-hardware-info)
  (make-thread-pool)
  (load-graphs core)
  (load-call-flows core)
  (initialize-shaders core)
  (tex::load-texture-descriptors core)
  (make-clock core)
  (load-materials core)
  (initialize-collider-system core)
  (make-scene-tree core)
  (load-initial-scene core scene-name)
  (run-prologue core)
  (start-game-loop core))

(defun deinitialize (core)
  (run-epilogue core)
  (shutdown-gamepads core)
  (kill-display core)
  (destroy-thread-pool)
  (makunbound '*core-debug*))

(defun start-game-loop (core)
  ;; Note: We let-bind the following variables so that we don't have to
  ;; dynamically access core's slot values each step of the main game loop.
  (let ((context (context core))
        (input-data (input-data core)))
    (with-profiling core
      (u:while (running-p core)
        (with-continuable
          (handle-events input-data)
          (render-frame core)
          ;; TODO: Remove this later when possible.
          (when (on-button-enter context :key :escape)
            (stop core)))))))

(defun start (&key config scene)
  (let ((core (make-core config)))
    (unwind-protect (initialize core config scene)
      (deinitialize core))))

(defun stop (core)
  (setf (running-p core) nil))
