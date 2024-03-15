(in-package #:virality)

(define-call-flow :default ()
  (flow initialize-phase
        (flow-state entry/initialize-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition pending-tasks))
        ;; High level description of this flow:
        ;; 0. If pre-init is empty, go to 7.
        ;; 1. straight up move components from pre-init to init.
        ;; 2. straight up move actors from pre-init to init.
        ;; 3. run initalize-component by-type in init.
        ;; 4. realize-components from init to active.
        ;; 5. realize-actors from init to active.
        ;; 6. realize-phase-commit (new name: WHILE-INITIALIZE-PHASE)
        ;;    make a decision:
        ;;    If still stuff in pre-init go to 1
        ;;    If nothing in pre-init, go to 7.
        ;; 7. exit flow
        ;; 0
        (flow-state pending-tasks :reset ()
                    (selector nil)
                    (action nil)
                    (transition
                     (lambda (core)
                       (if (pending-preinit-tasks-p core)
                           init-components
                           exit/initialize-phase))))
        ;; 1
        (flow-state init-components :reset ()
                    (selector
                     (lambda (core)
                       (values :type-policy
                               (component-preinit-by-type-view (tables core)))))
                    (action #'component/preinit->init)
                    (transition init-actors))
        ;; 2
        (flow-state init-actors :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               (actor-preinit-db (tables core)))))
                    (action #'actor/preinit->init)
                    (transition protocol-initialize-components))
        ;; 3
        (flow-state protocol-initialize-components :reset ()
                    (selector
                     (lambda (core)
                       (values :type-policy
                               (component-init-by-type-view (tables core)))))
                    (action #'on-component-initialize)
                    (transition realize-components))
        ;; 4
        (flow-state realize-components :reset ()
                    (selector
                     (lambda (core)
                       (values :type-policy
                               (component-init-by-type-view (tables core)))))
                    (action #'component/init->active)
                    (transition realize-actors))
        ;; 5
        (flow-state realize-actors :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               (actor-init-db (tables core)))))
                    (action #'actor/init->active)
                    (transition while-initialize-phase))
        ;; 6
        (flow-state while-initialize-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition
                     (lambda (core)
                       ;; Running PROTOCOL-INITIALIZE-COMPONENTS may have caused
                       ;; additional actors/components to be created, so we
                       ;; check for that here and repeat as needed.
                       (if (pending-preinit-tasks-p core)
                           ;; Then do the process over again.
                           init-components
                           ;; Or exit this phase, we're done initializing.
                           exit/initialize-phase))))
        ;; 7
        (flow-state exit/initialize-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition nil)))
  (flow active-phase
        (flow-state entry/active-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition physics-loop))
        ;; NOTE: We don't call comp::process-deferred-instant-transform-updates
        ;; in the "physics-loop" flow-state below because that should happen at
        ;; the end of the transform updating, OR the end of the frame after the
        ;; user code has ran (and in this case, before component destruction).
        (flow-state physics-loop :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               core)))
                    (action
                     (lambda (core)
                       (clock-tick core)
                       (comp::interpolate-transforms core)))
                    (transition make-active-camera-view))
        ;; NOTE: This flow-state is invoked deep inside of TICK in the
        ;; flow-state physics loop. Its purpose is to be called every time the
        ;; physics has been actually computed.
        (flow-state protocol-physics-update :reset ()
                    (selector
                     (lambda (core)
                       (values :type-policy
                               (component-active-by-type-view (tables core)))))
                    (action #'on-component-physics-update)
                    (transition nil))
        ;; TODO: PHYSICS-COLLISIONS has a tentative implementation, there may be
        ;; more work here than this single state. This will compute collisions
        ;; and then inform the recipients of those collisions as desired in the
        ;; boundary regions components (yet to be written).
        ;; NOTE: This flow-state is invoked deep inside of TICK, hence no
        ;; transitions in and out.
        (flow-state physics-collisions :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               core)))
                    (action (lambda (core)
                              (let ((cs (collider-system core)))
                                ;; NOTE: This order is required.
                                (compute-stable-collisions cs)
                                (compute-registering-collisions cs))))
                    (transition nil))
        ;; TODO: Should I run flow destroy-phase just before this next
        ;; flow-state so that those actors/components aren't even drawn?
        ;; Currently, I defer all actor/component destruction until the end of
        ;; frame, far after this. I don't know what is better yet.
        (flow-state make-active-camera-view :reset ()
                    (selector
                     ;; TODO: Move this code into a real function inside of
                     ;; the engine.
                     (lambda (core)
                       (let ((context (context core)))
                         (symbol-macrolet ((camera (active-camera context)))
                           (unless (and camera (comp:active-p camera))
                             (setf camera (comp:find-active-camera
                                           context)))
                           (values :identity-policy
                                   camera)))))
                    (action #'comp:compute-camera-view)
                    (transition protocol-attach/detach-components))
        (flow-state protocol-attach/detach-components :reset ()
                    (selector
                     (lambda (core)
                       (values :type-policy
                               (component-active-by-type-view
                                (tables core)))))
                    (action #'component/invoke-attach/detach-events)
                    (transition protocol-update-component))
        (flow-state protocol-update-component :reset ()
                    (selector
                     (lambda (core)
                       (values :type-policy
                               (component-active-by-type-view (tables core)))))
                    (action #'on-component-update)
                    (transition protocol-render-component))
        (flow-state protocol-render-component :reset ()
                    (selector
                     (lambda (core)
                       (values :type-policy
                               (component-active-by-type-view (tables core)))))
                    (action #'on-component-render)
                    (transition exit/active-phase))
        (flow-state exit/active-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition nil)))
  (flow destroy-phase
        (flow-state entry/destroy-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition pending-predestroy-tasks))
        ;; TODO: Fix me. This should handle if I spawn an actor/component and
        ;; then destroy it in the same function, etc, etc.
        ;; 0. If pre-destroy (actor and components) is empty, go to 5
        ;; 1. move components (<= ttl 0) in pre-destroy from init/active to
        ;;    destroy.
        ;; 2. move root actors (<= ttl 0) (and their components) in pre-destroy
        ;;    from init/active to destroy.
        ;; 3. Recursively move all actor children found in destroy from
        ;;    init/active into destroy, Set any actor/component ttl in any
        ;;    discovered instances to 0, since the root with ttl <= 0 overrides
        ;;    all pending ttls for anything that may be in the process of being
        ;;    destroyed.
        ;; 4. A) decrement ttl by frame-time for predestroying components B)
        ;;    decrement ttl by frame-time for predestroying actors.
        ;; 5. If destroy is empty, go to 11.
        ;; 5.5 run ON-ATTACH-COMPONENT & ON-DETACH-COMPONENT by-type.
        ;; 5.75 HACKISH: run any deregistration of colliders. Maybe too late?
        ;; 6. run ON-COMPONENT-DESTROY by-type in destroy.
        ;; 7. disconnect all destroyed actors from the scene heirarchy.
        ;; 8. release-components (and remove from actors) from destroy.
        ;; 9. release-actors (which now should be empty) from destroy.
        ;; 10. restart to see if anything else got marked to be destroyed during
        ;;     this entire process, goto 0.
        ;; 11. exit flow.
        ;; 0
        (flow-state pending-predestroy-tasks :reset ()
                    (selector nil)
                    (action nil)
                    (transition
                     (lambda (core)
                       (if (pending-predestroy-tasks-p core)
                           prepare-predestroy-components
                           pending-destroy-tasks))))
        ;; 1
        (flow-state prepare-predestroy-components :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               (component-predestroy-view (tables core)))))
                    (action #'component/init-or-active->destroy)
                    (transition prepare-predestroy-actors))
        ;; 2
        (flow-state prepare-predestroy-actors :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               (actor-predestroy-view (tables core)))))
                    (action #'actor/init-or-active->destroy)
                    (transition destroy-actor-children))
        ;; 3
        (flow-state destroy-actor-children :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               (u:hash-keys (actor-destroy-db (tables core))))))
                    ;; NOTE: See selector for this flow-state.
                    (action #'actor/destroy-descendants)
                    (transition decrement-component-destroy-timer))
        ;; 4 A
        (flow-state decrement-component-destroy-timer :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               (component-predestroy-view (tables core)))))
                    (action #'component/countdown-to-destruction)
                    (transition decrement-actor-destroy-timer))
        ;; 4 B
        (flow-state decrement-actor-destroy-timer :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               (actor-predestroy-view (tables core)))))
                    (action #'actor/countdown-to-destruction)
                    (transition pending-destroy-tasks))
        ;; 5
        (flow-state pending-destroy-tasks :reset ()
                    (selector nil)
                    (action nil)
                    (transition
                     (lambda (core)
                       (if (pending-destroy-tasks-p core)
                           protocol-attach/detach-components
                           exit/destroy-phase))))
        ;; 5.5
        (flow-state protocol-attach/detach-components :reset ()
                    (selector
                     (lambda (core)
                       (values :type-policy
                               (component-destroy-by-type-view (tables core)))))
                    (action #'component/invoke-attach/detach-events)
                    (transition deregister-colliders))
        ;; 5.75 HACKISH
        ;; TODO: Might call on-collision-* on components that have been
        ;; detached? Might have to have a much finer grained understanding of
        ;; something that is about to be destroyed and a barrier that all the
        ;; about to be destroyed colliders can send their message before any
        ;; actual detaches/destroys happen. Confirm that the new flow design
        ;; will do the right thing!
        (flow-state deregister-colliders :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               core)))
                    (action
                     (lambda (core)
                       (let ((cs (collider-system core)))
                         (compute-deregistering-collisions cs))))
                    (transition protocol-destroy-component))
        ;; 6
        (flow-state protocol-destroy-component :reset ()
                    (selector
                     (lambda (core)
                       (values :type-policy
                               (component-destroy-by-type-view (tables core)))))
                    (action #'on-component-destroy)
                    (transition disconnect-destroyed-actors))
        ;; 7
        (flow-state disconnect-destroyed-actors :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               (actor-destroy-db (tables core)))))
                    (action #'actor/disconnect)
                    (transition release-components))
        ;; 8
        (flow-state release-components :reset ()
                    (selector
                     (lambda (core)
                       (values :type-policy
                               (component-destroy-by-type-view (tables core)))))
                    (action #'component/destroy->released)
                    (transition release-actors))
        ;; 9
        (flow-state release-actors :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               (actor-destroy-db (tables core)))))
                    (action #'actor/destroy->released)
                    (transition restart-predestroy-phase))
        ;; 10
        (flow-state restart-predestroy-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition pending-predestroy-tasks))
        ;; 11
        (flow-state exit/destroy-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition nil)))
  (flow maintenance-phase
        (flow-state entry/maintenance-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition shader-refresh))
        (flow-state shader-refresh :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               core)))
                    (action nil)
                    (transition exit/maintenance-phase))
        (flow-state exit/maintenance-phase :reset ()
                    (selector nil)
                    (action nil)
                    (transition nil)))
  (flow perform-one-frame
        (flow-state entry/perform-one-frame :reset ()
                    (selector nil)
                    (action nil)
                    (transition perform-recompilations))
        ;; NOTE: If we have anything to recompile in the queue, do all of it
        ;; now. This causes the frame we're just about the render to see the
        ;; updated changes as an atomic change. This is the earliest we can see
        ;; these atomic changes.
        (flow-state perform-recompilations :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               core)))
                    (action #'recompile-queued-items)
                    (transition initialize-phase))
        (flow-state initialize-phase :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               core)))
                    (action
                     (lambda (core)
                       (execute-flow core
                                     :default
                                     'initialize-phase
                                     'entry/initialize-phase
                                     :come-from-state-name
                                     :ef-realize-phase)))
                    (transition active-phase))
        (flow-state active-phase :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               core)))
                    (action
                     (lambda (core)
                       (execute-flow core
                                     :default
                                     'active-phase
                                     'entry/active-phase
                                     :come-from-state-name
                                     :ef-active-phase)))
                    (transition eof-work))
        (flow-state eof-work :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy core)))
                    (action #'comp::process-deferred-instant-transform-updates)
                    (transition destroy-phase))
        (flow-state destroy-phase :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               core)))
                    (action
                     (lambda (core)
                       (execute-flow core
                                     :default
                                     'destroy-phase
                                     'entry/destroy-phase
                                     :come-from-state-name
                                     :ef-destroy-phase)))
                    (transition maintenance-phase))
        (flow-state maintenance-phase :reset ()
                    (selector
                     (lambda (core)
                       (values :identity-policy
                               core)))
                    (action
                     (lambda (core)
                       (execute-flow core
                                     :default
                                     'maintenance-phase
                                     'entry/maintenance-phase
                                     :come-from-state-name
                                     :ef-maintenance-phase)))
                    (transition exit/do-next-frame))
        (flow-state exit/do-next-frame :reset ()
                    (selector nil)
                    (action nil)
                    (transition nil))
        (flow-state exit/game-over :reset ()
                    (selector nil)
                    (action nil)
                    (transition nil))))
