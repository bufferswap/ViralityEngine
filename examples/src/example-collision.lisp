(in-package #:virality-examples)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is not really a general purpose component. It is just here to help out
;; testing how destruction and colliders work together.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(v:define-component destroy-my-actor ()
  ((%time-to-destroy :accessor time-to-destroy
                     :initarg :time-to-destroy
                     :initform 5f0)))

(defmethod v:on-collision-enter ((self destroy-my-actor) other-collider)
  #++(:printv "DESTROY-MY-ACTOR: Actor ~a entered collision with collider ~
             ~a(on actor ~a)"
              (v:actor self) other-collider (v:actor other-collider))
  (when (string= (v:display-id other-collider) "Ground")
    #++(:printv "===>>> DESTROY-MY-ACTOR: It was specifically the \"Ground\" ~
               object, so destroy myself!")
    (v:destroy (v:actor self))))

(defmethod v:on-collision-exit ((self destroy-my-actor) other-collider)
  #++(:printv "DESTROY-MY-ACTOR: Actor ~a is exiting collision with ~
             ~a(on actor: ~a)."
              (v:actor self) other-collider (v:actor other-collider)))

(defmethod v:on-component-update ((self destroy-my-actor))
  (decf (time-to-destroy self) (v:frame-time (v:context self)))
  (when (<= (time-to-destroy self) 0)
    (v:destroy (v:actor self))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing getting the directions from a transform
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(v:define-component unit-test-transform-api ()
  ((%test-type :reader test-type
               :initarg :test-type
               :initform nil)
   (%test-performed :reader test-performed
                    :initform (u:dict #'eq))))

(defmethod v:on-component-physics-update ((self unit-test-transform-api))
  (ecase (test-type self)
    (:test-direction-vectors
     (test-axis-directions self))
    (:test-transform-api
     (test-transform-api self))))

(defun test-axis-directions (self)
  (let ((test-type (test-type self)))
    (unless (u:href (test-performed self) test-type)
      (let ((forward (v:transform-forward self))
            (backward (v:transform-backward self))
            (up (v:transform-up self))
            (down (v:transform-down self))
            (right (v:transform-right self))
            (left (v:transform-left self)))
        #++(:printv "FORWARD Vector -> ~a" forward)
        #++(:printv "BACKWARD Vector -> ~a" backward)
        #++(:printv "UP Vector -> ~a" up)
        #++(:printv "DOWN Vector -> ~a" down)
        #++(:printv "RIGHT Vector -> ~a" right)
        #++(:printv "LEFT Vector -> ~a" left)
        ;; NOTE: This expects the actor to be unrotated wrt the universe.
        (unless (and (v3:= forward (v3:vec 0f0 0f0 -1f0))
                     (v3:= backward (v3:vec 0f0 0f0 1f0))
                     (v3:= up (v3:vec 0f0 1f0 0f0))
                     (v3:= down (v3:vec 0f0 -1f0 0f0))
                     (v3:= right (v3:vec 1f0 0f0 0f0))
                     (v3:= left (v3:vec -1f0 0f0 0f0)))
          (error "The Transform Axis Direction API didn't match expectations!"))
        (setf (u:href (test-performed self) test-type) t)))))

(defun test-transform-api (self)
  (let* ((test-type (test-type self)))
    (unless (u:href (test-performed self) test-type)
      (test-transform-point-api self)
      (test-transform-vector-api self)
      (test-transform-direction-api self)
      ;; And ensure we don't run this again.
      (setf (u:href (test-performed self) test-type) t))))

(defun test-transform-point-api (self)
  "Test if TRANSFORM-POINT works."
  (let* ((object-space-point (v3:vec 1f0 0f0 0f0))
         (world-space-point (v3:vec 1f0 3f0 1f0))
         (local->world (v:transform-point self object-space-point))
         (world->local (v:transform-point self
                                          world-space-point
                                          :space :model)))
    ;; See if transform-point works.
    (let ((result-0 (v3:= local->world world-space-point))
          (result-1 (v3:= world->local object-space-point)))
      (unless (and result-0 result-1)
        (unless result-0
          #++(:printv
              "FAILED: (v3:~~ local->world:~a world-space-point: ~a) -> ~a"
              local->world world-space-point result-0))
        (unless result-1
          #++(:printv
              "FAILED: (v3:~~ world->local:~a object-space-point: ~a) -> ~a"
              world->local object-space-point result-1))
        (error "TRANSFORM-POINT API Failed!")))))

(defun test-transform-vector-api (self)
  "Test if TRANSFORM-VECTOR works."
  (let* ((object-space-vector (v3:vec 2f0 2f0 0f0))
         (world-space-vector (v3:vec -4f0 4f0 0f0))
         (local->world (v:transform-vector self object-space-vector))
         (world->local (v:transform-vector self
                                           world-space-vector
                                           :space :model)))
    ;; See if transform-vector works.
    (let ((result-0
            (v3:= local->world world-space-vector))
          (result-1
            (v3:= world->local object-space-vector)))
      (unless (and result-0 result-1)
        (unless result-0
          #++(:printv
              "FAILED: (v3:= local->world:~a world-space-vector: ~a) -> ~a"
              local->world world-space-vector result-0))
        (unless result-1
          #++(:printv
              "FAILED: (v3:= world->local:~a object-space-vector: ~a) -> ~a"
              world->local object-space-vector result-1))
        (error "TRANSFORM-VECTOR API Failed!")))))

(defun test-transform-direction-api (self)
  (let* (;; NOTE: these must be normalized for the test. I specified it this way
         ;; so it would be easier to see in your mind's eye.
         (object-space-direction (v3:normalize (v3:vec 1f0 1f0 0f0)))
         (world-space-direction (v3:normalize (v3:vec -1f0 1f0 0f0)))
         (local->world (v:transform-direction self object-space-direction))
         (world->local (v:transform-direction self
                                              world-space-direction
                                              :space :model)))
    ;; See if transform-direction works.
    (let ((result-0
            (v3:= local->world world-space-direction))
          (result-1
            (v3:= world->local object-space-direction)))
      (unless (and result-0 result-1)
        (unless result-0
          #++(:printv
              "FAILED: (v3:= local->world:~a world-space-direction: ~a) -> ~a"
              local->world world-space-direction result-0))
        (unless result-1
          #++(:printv
              "FAILED: (v3:= world->local:~a object-space-direction: ~a) -> ~a"
              world->local object-space-direction result-1))
        (error "TRANSFORM-DIRECTION API Failed!")))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pick testing, which is testing ray casting with the mouse.
;; This interface is TBD.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(v:define-component pick-test () ())

(defmethod v:on-component-update ((self pick-test))
  (let ((context (v:context self))
        (line-segment (make-instance 'v::line-segment))
        (picked nil))
    (when (v:on-button-enter context :mouse :left)
      (setf picked (v::pick-actor context line-segment)))
    (when picked
      (:printv picked)
      ;; TODO: Uncomment this when sly has send-to-repl in master
      #++(v::send-to-repl (list picked) :comment "Picked actor"))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some basic models to help us out.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(v:define-prefab "default-rock" (:library examples)
  ("model"
   (comp:transform :scale (v3:uniform 2f0))
   (comp:mesh :asset '(meshes rocks)
              :name "Rock-1")
   (comp:render :material `(x:matcap ceramic-dark
                                     :uniforms
                                     ((:sampler x:matcap/ceramic-dark)))
                :slave (v:ref :self :component 'comp:mesh))))

(v:define-prefab "rock-0" (:library examples)
  (("rock-0" :link "/default-rock")
   ("model"
    (comp:mesh :name "Rock-0"))))

(v:define-prefab "rock-1" (:library examples)
  (("rock-1" :link "/default-rock")
   ("model"
    (comp:mesh :name "Rock-1"))))

(v:define-prefab "rock-2" (:library examples)
  (("rock-2" :link "/default-rock")
   ("model"
    (comp:mesh :name "Rock-2"))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The test prefabs.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(v:define-prefab "collision-smoke-test" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args) :zoom 6f0))
  ("rot-0-center"
   (comp:transform :translate (v3:vec -2f0 0f0 0f0)
                   :rotate/velocity (v3:velocity v3:+forward+ o:pi))
   (("stone0" :link "/rock-1")
    (comp:transform :translate (v3:vec -2f0 0f0 0f0))
    (comp:sphere :display-id "Player"
                 :visualize t
                 :on-layer :player
                 :center (v3:zero)
                 :radius 1f0)))
  ("rot-1-center"
   (comp:transform :translate (v3:vec 2f0 0f0 0f0)
                   :rotate/velocity (v3:velocity v3:+forward+ (- o:pi)))
   (("stone1" :link "/rock-1")
    (comp:transform :translate (v3:vec 2f0 0f0 0f0))
    (comp:sphere :display-id "Enemy"
                 :visualize t
                 :on-layer :enemy
                 :center (v3:zero)
                 :radius 1f0))))

(v:define-prefab "collision-transform-test-0" (:library examples)
  "This test just prints out the directions of the actor transform. Since
the actor is at universe 0,0,0 and has no rotations, we should see the
unit world vector representations of the axis directions as:
  forward:  (0 0 -1)
  backward: (0 0 1)
  up:       (0 1 0)
  down:     (0 -1 0)
  right:    (1 0 0)
  left:     (-1 0 0)
"
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args) :zoom 3f0))

  (("thingy" :link "/rock-1")
   ;; NOTE: The 5 0 0 is specific to the unit-test-transform-api tests.
   (comp:transform :translate (v3:vec 5f0 0f0 0f0))
   (unit-test-transform-api :test-type :test-direction-vectors)))

;; TODO: This currently fails. Compute this math by hand and verify if the math
;; or the test is correct.
(v:define-prefab "collision-transform-test-1" (:library examples)
  "This test checks to see if we can move in and out of object space and
world space for a particular transform."

  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args) :zoom 3.5f0))

  ("right"
   (comp:transform :translate (v3:vec 1f0 0f0 0f0))
   ("up"
    (comp:transform :translate (v3:vec 0f0 1f0 0f0))
    ("back"
     (comp:transform :translate (v3:vec 0f0 0f0 1f0))
     (("mark" :link "/rock-1")
      ;; Origin sitting at 1,1,1 wrt the universe, but +90deg rotation around
      ;; "mark" Z axis.
      (comp:transform :rotate (q:orient :local :z o:pi/2) :scale 2f0)
      (unit-test-transform-api :test-type :test-transform-api))))))



(v:define-prefab "stone-destroy-when-collide-with-ground" (:library examples)
  (destroy-my-actor :display-id "destroy-my-actor: stone")
  (("stone" :link "/default-rock")
   (comp:sphere :display-id "Stone Collider"
                :visualize t
                :on-layer :player
                ;; TODO: This "../" _happens_ to get the parent which _happens_
                ;; to be the root of >this< prefab. This works surprisingly and
                ;; needs cleanup and definition.  It makes sense, but isn't
                ;; properly defined and tested. I was expecting to use
                ;; "/stone-destroy-when-collide-with-ground" here for this
                ;; reference. Also, just like :self, we could have :prefab-root
                ;; which means "this prefab root actor".
                ;;
                ;; PARSE-REFERENCE-PATH/PARENT is prolly the culprit here.
                :referent (v:ref "../" :component 'destroy-my-actor)
                :center (v3:zero)
                :radius 1f0)))

(v:define-prefab "stone-destroy-after-time" (:library examples)
  (destroy-my-actor :time-to-destroy 2f0)
  (("stone" :link "/default-rock")
   (comp:sphere :display-id "Stone Collider"
                :visualize t
                :on-layer :player
                :referent (v:ref "../" :component 'destroy-my-actor)
                :center (v3:zero)
                :radius 1f0)))


(v:define-prefab "collision-test-0" (:library examples)
  "In this test, you should see two actors with a narrow gap between them and
an another actor near the bottom of the screen. These three are unmoving. The
green spiral (if VISUALIZE defaults to T in the sphere component) is a sphere
collider being rendered as a spiral wound around it (it is 3d and you're seeing
the top of it in the ortho projection). A small actor shows up and moves
downwards. When it hits the narrow gape, when the colliders touch they will
highlight and there will be output to the repl. After it leaves the narrow gap
and hits the bottom actor, it will disappear, having destroyed itself when
hitting the bottom actor. The DESTROY-MY-ACTOR component specifically checks for
the \"Ground\" collider via its display-id. This is a hack, but good enough for
a test. After it disappears, nothing else happens. TODO: Make a spawner object
that just spawns stone prefabs so they rain down onto the ground, which should
be made bigger. to accomodate it. Maybe some fragments too when it hits..."
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args) :zoom 4f0))
  (("left-gate" :link "/rock-1")
   (comp:transform :translate (v3:vec -1.15f0 2f0 -.1f0))
   (comp:sphere :display-id "Left-Gate"
                :visualize t
                :on-layer :ground))
  (("right-gate" :link "/rock-2")
   (comp:transform :translate (v3:vec 1.15f0 2f0 -.1f0))
   (comp:sphere :display-id "Right-Gate"
                :visualize t
                :on-layer :ground))

  (("stone" :link "/stone-destroy-when-collide-with-ground")
   (comp:transform :translate (v3:vec 0f0 4.5f0 0f0)
                   :scale 0.5f0
                   :translate/velocity (v3:vec 0f0 -3f0 0f0)
                   :rotate/velocity (v3:velocity v3:+up+ o:pi)))

  (("ground" :link "/default-rock")
   (comp:transform :translate (v3:vec 0f0 -2f0 0.1f0))
   (comp:sphere :display-id "Ground"
                :visualize t
                :on-layer :ground
                :center (v3:zero)
                :radius 1f0)))

(v:define-prefab "collision-test-1" (:library examples)
  "This test demonstrates that at frame 0 colliders that should be colliding
actually are. You have to view the results to see the colliders lighting up."
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args) :zoom 4f0))
  (("upper-left" :link "/rock-0")
   (comp:transform :translate (v3:vec -2f0 2f0 -0.1f0))
   (comp:sphere :display-id "Upper-Left"
                :visualize t
                :on-layer :ground))
  (("upper-right" :link "/rock-1")
   (comp:transform :translate (v3:vec 2f0 2f0 -0.1f0))
   (comp:sphere :display-id "Upper-Right"
                :visualize t
                :on-layer :ground))
  (("lower-left" :link "/rock-2")
   (comp:transform :translate (v3:vec -2f0 -2f0 -0.1f0))
   (comp:sphere :display-id "Lower-Left"
                :visualize t
                :on-layer :ground))
  (("lower-right" :link "/rock-0")
   (comp:transform :translate (v3:vec 2f0 -2f0 -0.1f0))
   (comp:sphere :display-id "Lower-Right"
                :visualize t
                :on-layer :ground))

  (("stone" :link "/stone-destroy-after-time")
   (comp:transform :scale 2f0
                   :rotate/velocity (v3:velocity v3:+up+ o:pi))))

(v:define-prefab "collision-test-2" (:library examples)
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args) :zoom 3f0))
  ("a"
   (comp:transform :translate (v3:vec -4f0 0f0 0f0)
                   :translate/velocity (v3:zero)
                   :scale 2f0)
   (("stone-cuboid" :link "/rock-1")
    (comp:transform :scale 2f0
                    :rotate (q:orient :local :x o:pi/2)
                    :rotate/velocity (v3:velocity (v3:ones) o:pi/6))
    (comp:cuboid :display-id "Stone"
                 :visualize t
                 :on-layer :ground
                 :center (v3:zero)
                 :minx -.75f0
                 :maxx .75f0
                 :miny -.75f0
                 :maxy .75f0
                 :minz -.75f0
                 :maxz .75f0)))
  ("b"
   (comp:transform :translate (v3:vec 4f0 0f0 0f0)
                   :translate/velocity (v3:zero)
                   :scale 2f0)
   (("stone-sphere" :link "/rock-1")
    (comp:transform :scale 2f0
                    :rotate (q:orient :local :x o:pi/2)
                    :rotate/velocity (v3:velocity (v3:ones) o:pi/6))
    (comp:sphere :display-id "Stone"
                 :visualize t
                 :on-layer :ground
                 :center (v3:zero)
                 :radius 1f0))))

(v:define-prefab "collision-test-3" (:library examples)
  (("camera" :copy "/cameras/ortho")
   (comp:camera (:policy :new-args) :zoom 140f0))
  ("test-case-always"
   (comp:transform :translate (v3:vec -2f0 0f0 0f0))
   ("a"
    ;; As cuboid 1 rotates it should always be hitting (red) cuboid 2 since they
    ;; penetrate and then share a plane when both are parallel to each other.
    (comp:transform :translate (v3:vec -.50f0 0f0 0f0)
                    :translate/velocity (v3:vec 0f0 0f0 0f0))
    ("cuboid1"
     (comp:transform :rotate (q:orient :local :z o:pi/4)
                     :rotate/velocity (v3:velocity (v3:vec 0f0 0f0 1f0) o:pi/12))
     (comp:cuboid :visualize t
                  :on-layer :ground
                  :center (v3:zero))))
   ("cuboid2"
    (comp:transform :translate (v3:vec 0.5f0 0f0 0f0))
    (comp:cuboid :visualize t
                 :on-layer :ground
                 :center (v3:zero))))
  ("test-case-gap"
   (comp:transform :translate (v3:vec 2f0 0f0 0f0))
   ("a"
    ;; As cuboid 1 rotates, when they become parallel, there will be a slight
    ;; gap and so both should turn green to represent no collision during that
    ;; small gap.
    (comp:transform :translate (v3:vec -.51f0 0f0 0f0)
                    :translate/velocity (v3:zero))
    ("cuboid1"
     (comp:transform :rotate (q:orient :local :z o:pi/4)
                     :rotate/velocity (v3:velocity (v3:vec 0f0 0f0 1f0) o:pi/12))
     (comp:cuboid :visualize t
                  :on-layer :ground
                  :center (v3:zero))))
   ("cuboid2"
    (comp:transform :translate (v3:vec 0.5f0 0f0 0f0))
    (comp:cuboid :visualize t
                 :on-layer :ground
                 :center (v3:zero)))))

(v:define-prefab "collision-test-4" (:library examples)
  "In this test, we test that the center of the collision sphere is away from
the actual model and picking still works."
  (("camera" :copy "/cameras/perspective")
   (pick-test)
   (comp:camera (:policy :new-args) :zoom 1f0))
  ("a"
   (comp:transform :translate (v3:vec 5f0 0f0 0f0)
                   :scale 1f0)
   (("stone-sphere1" :link "/rock-1")
    (comp:transform :scale 2f0
                    :rotate (q:orient :local :x o:pi/2)
                    :rotate/velocity (v3:velocity (v3:ones) o:pi/6))
    (comp:sphere :display-id "Stone"
                 :visualize t
                 :on-layer :ground
                 :center (v3:vec 5f0 0f0 0f0)
                 :radius 1f0)))
  ("b"
   (comp:transform :translate (v3:vec -5f0 0f0 0f0)
                   :scale 1f0)
   (("stone-sphere2" :link "/rock-2")
    (comp:transform :scale 2f0
                    :rotate (q:orient :local :x o:pi/2)
                    :rotate/velocity (v3:velocity (v3:ones) o:pi/6))
    (comp:sphere :display-id "Stone"
                 :visualize t
                 :on-layer :ground
                 :center (v3:zero)
                 :radius 1f0))))

(v:define-prefab "collision-test-5" (:library examples)
  "This test has very careful geometry which aligns one obb's plane to the
camera so we see parallel to the plane. Also, the particular variation of
this math is something that once broke in the internal OBB math."
  (("camera" :copy "/cameras/perspective")
   (pick-test)
   (comp:camera (:policy :new-args) :zoom 1f0))
  ("a"
   (comp:transform :translate (v3:vec 15f0 0f0 0f0)
                   :translate/velocity (v3:vec 0f0 0f0 0f0)
                   :scale 1f0)
   (("stone-obb1" :link "/rock-2")
    (comp:transform :scale 15f0
                    :rotate (q:orient :local :x o:pi/2))
    (comp:cuboid :display-id "Stone"
                 :visualize t
                 :on-layer :ground
                 :center (v3:zero)
                 :minx -1f0
                 :maxx 1f0
                 :miny -1f0
                 :maxy 1f0
                 :minz -1f0
                 :maxz 1f0)))
  ("b"
   (comp:transform :translate (v3:vec -15f0 0f0 0f0)
                   :translate/velocity (v3:zero)
                   :scale 1f0)
   (("stone-obb2" :link "/rock-0")
    (comp:transform :scale 15f0
                    :rotate (q:orient :local :z o:pi/4 :x o:pi/2))
    (comp:cuboid :display-id "Stone"
                 :visualize t
                 :on-layer :ground
                 :center (v3:zero)
                 :minx -1f0
                 :maxx 1f0
                 :miny -1f0
                 :maxy 1f0
                 :minz -1f0
                 :maxz 1f0))))

(v:define-prefab "collision-test-6" (:library examples)
  "Test that the top stays in collision, and the bottom stay colliding when
they separate, and while the big one is alive stay collided, but after it goes
away then become non-collided."
  (("camera" :copy "/cameras/perspective")
   (comp:camera (:policy :new-args) :zoom 4f0))
  (("upper-left" :link "/rock-1")
   (comp:transform :translate (v3:vec -.5f0 2f0 -0.1f0))
   (comp:sphere :display-id "Upper-Left"
                :visualize t
                :on-layer :ground))
  (("upper-right" :link "/rock-1")
   (comp:transform :translate (v3:vec .5f0 2f0 -0.1f0))
   (comp:sphere :display-id "Upper-Right"
                :visualize t
                :on-layer :ground))
  (("lower-left" :link "/rock-1")
   (comp:transform :translate (v3:vec -.5f0 -2f0 -0.1f0)
                   :translate/velocity (v3:velocity v3:+left+ .5f0))
   (comp:sphere :display-id "Lower-Left"
                :visualize t
                :on-layer :ground))
  (("lower-right" :link "/rock-1")
   (comp:transform :translate (v3:vec .5f0 -2f0 -0.1f0)
                   :translate/velocity (v3:velocity v3:+right+ .5f0))
   (comp:sphere :display-id "Lower-Right"
                :visualize t
                :on-layer :ground))

  (("stone" :link "/stone-destroy-after-time")
   (comp:transform :scale 2f0
                   :rotate/velocity (v3:velocity v3:+up+ o:pi))))
