(in-package #:virality-examples)

;; Working Title: Ball Breaker.

;; Powerups:
;;
;; Decription:  powerup falls from a busted brick, doesn't collide with
;; anything except the paddle. It draws over everything else except the ball.
;; Types:
;;   1: Increase Paddle Size
;;   2: Additional Balls (Multiball, cumulative)
;;     (one ball must always be onscreen, no additional powerups while
;;     multiball)
;;   3: Paddle into Laser Canon Fires independently of the ball.
;;   4: Sticky Paddle to control when to fire ball.
;;   5: Slows the ball (cumulative, slowly (or not) dissipates).
;;   6: Deus Ex Machina (wins the level immediately)
;;   7: Additional Life

;; Additional for this game:
;;   A: Increase Ball Speed (cumulative)

;; Brick:
;;
;; Types:
;;   1: White 50 pts
;;   2: Orange 60 pts
;;   3: Cyan 70 pts
;;   4: Green 80 pts
;;   5: Red 90 pts
;;   6: Blue 100 pts
;;   7: Magenta 110 pts
;;   8: Yellow 120 pts
;;   9: Silver (Within a level range, takes X hits to destroy, X increases).
;;      (50 * Level for pts)
;;  10: Gold (Indestructible)

;; The paddle
;;
;; 3 Segments to paddle:
;;   Middle segment: sharp angle (relative to paddle) bounce
;;   End cap segment: 45 degree bounce
;;   Extreme edge of paddle: ~180 degree bounce

;; Aspects of game making:
;;
;; Level design:
;;  Probably need a compact and easy to edit level format to allow fast adjustment
;;  of game making. Maybe Tiled as a level editor?
;;
;; All walls are gold bricks.

;; V Twists to the game:
;;   Layout the board like pinball game (with that perspective).
;;   In a 3d mode, when you destroy a brick it breaks and the stack falls.
;;   Tinker with angle of incidence and paddle, maybe a power up will always
;;     cause the ball to bounce to a perfect spot given the board.
;;   When moving the paddle, you can apply english to the ball to get it to go
;;     faster or maybe curve?
;;   Additional bricks types:
;;    Glass bricks
;;    Bricks explode and takes out bricks next to it.
;;   Ball may gain powers like break more bricks than just one, etc.
;;   Multiple levels:
;;    If a level has multiple levels, then when you hit a hole/etc and fall
;;    back down most or all of the level respawns so you have to defeat it
;;    again before advancing.
;;   Bosses:
;;    How does this work? Think about what a "Boss" means. MCP in Tron maybe?
;;    Advancing wall (Like space invaders?)
;;    Shrinking board: all sides shrink and crush bricks.
;;   Game Scaffolding:
;;    Make the game harder as it progresses with new levels.
;;    Possible "enemy" paddles an AI controls to frustrate the player.
;;    Bricks that bounce differently.
;;    Enemies that frustrate the player?
;;    What if we mixed shmup, galaxy conquerers, pinball, ball/brick together?
;;    Cylindrical worlds.
;;    Imagine a house floor plan, one wall in each room is "death", the
;;     bricks are in the center of the room and if the ball hits it and
;;     goes into another room, then the game slows down and the camera
;;     translates (and possibly rotates) to the new view where the player
;;     continues to play. Leaving the camera aligned to the new room or
;;     not is a choice for when we get there and see which one is more fun.
;;   The player might hit switches to open doors or do other things, the
;;    ball might hit other environmental things.
;;   Mixing pinball and ball/brick mechanics:
;;    Ball can go up ramp to another section of the level, or to a different
;;     level where the player assumes control of a different paddle in that
;;     new space. Ball could hit a hole, or whatever, and fall back down.
;;   When you defeat a level, you go (literally) forward to the next level.
;;    "dying" just means that you go back to the previous level. You can
;;    only actually die when you run out of lives, or the ball falls behind
;;    you on the bottom level.
;;   Levels are distinct with (possible) multi-tier stages. Possible
;;    non-linear multi-tired stages: go up to stage 3 in level 2, turn on a
;;    switch, open gate in stage 1 level 2, put ball through it, advance to
;;    next level.
;;   MUCH longer (or continuous) corridor that you use WS to move
;;    forward/backward (with rocket thrusters on the paddle, and you have to
;;    eat (like pacman) the entire level to get to the end. AD rotates to
;;    affect bounce, but WS moves in inertial frame. Maybe the corridors
;;    can fork and the user chooses a particular direction and can't go back.
;;    If the ball goes behind you, you lose a life. You always move forward.
;;    If the ball goes behind you a self-destruct timer starts on it and you
;;    need to quickly zoom backwards with WD and bounce it the FRONT of your
;;    paddle which will reset it and allow you to continue. Maybe this long
;;    corridor is segmented with a barrier (which bounces the ball) that
;;    must be destroyed by completing the level in order to move forward
;;    (This prevents the ball from unluckily destroying and bouncing bricks
;;    infront of you until you can't see it anymore.
;;   Play with brick/ball size in relation to each other and the
;;    environment/paddle.
;;   Brainstorming (unknown quality of mechanics or implementation difficulty):
;;     Ball take on brick color, player must click mouse to change paddle
;;      color to brick. (Maybe tediuos?)
;;     Segmenting space (vague idea currently).
;;     At the highest level, marble madness (tilt world to move paddle).
;;     Procedural infinite segmented levels? (Need LOTS of thinking for
;;      viable content and fun.)

;; Stuff to watch out for:
;;   Ball/brick collisions might tunnel or do sordid things.
;;    so watch out for it in the math.

;; Simple map mode idea in ASCII for easy level (or sections of levels)
;; specification.
#++
"
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!           #####################        !
!         #########################      !
!           #####################        !
!           HHHHHHDDDDHHHHHHHH           !
!                                        !
!                                        !
!                                        !
!                                        !
!                                        !
!                                        !
!                                        !
!                                        !
!                                        !
!                                        !
!         B                              !
!                                        !
!                                        !
!                                        !
!                                        !
!                                        !
!                                        !
!     PPPPPPP                            !
!                                        !
"

(v:define-component brick ()
  ((%hp :accessor hp
        :initarg :hp
        :initform 1)
   ))

(v:define-component ball ()
  ((%velocity :accessor velocity
              :initarg :velocity
              :initform (v3:zero))
   (%contacts :accessor contacts
              :initarg :contacts
              :initform nil)))

(defmethod v:on-collision-enter ((self ball) face)
  (format t "Ball entered collision: ball ~A with actor ~A~%"
          self (v:actor face))
  (push (v:actor face) (contacts self)))

(defmethod v:on-collision-exit ((self ball) face)
  (format t "Ball exited collision: ball ~A with actor ~A~%"
          self (v:actor face)))

;; Out of all bricks hit, react properly, but only destroy one of them.
(defmethod v:on-component-physics-update ((self ball))
  (when (contacts self)
    (format t "Ball: Would have processed: ~A~%" (contacts self))
    (setf (contacts self) nil))

  )

(v:define-prefab "brick" (:library examples)
  (brick)
  (comp:cuboid :visualize t
               :referent (v:ref :self :component 'brick)
               :on-layer :enemy
               :center (v3:zero)
               :minx -1f0
               :maxx 1f0
               :miny -.5f0
               :maxy .5f0
               :minz -1f0
               :maxz 1f0))


(v:define-prefab "ball" (:library examples)
  (ball)
  (comp:sphere :visualize t
               :referent (v:ref :self :component 'ball)
               :on-layer :player
               :center (v3:zero)
               :radius .2f0))


;; Layout: 01234
(v:define-prefab "brick-group" (:library examples)
  (("brick-0" :copy "/brick")
   (comp:transform :translate (v3:vec -4f0 0f0 0f0)))
  (("brick-1" :copy "/brick")
   (comp:transform :translate (v3:vec -2f0 0f0 0f0)))
  (("brick-2" :copy "/brick")
   (comp:transform :translate (v3:zero)))
  (("brick-3" :copy "/brick")
   (comp:transform :translate (v3:vec 2f0 0f0 0f0)))
  (("brick-4" :copy "/brick")
   (comp:transform :translate (v3:vec 4f0 0f0 0f0)))
  )

(v:define-prefab "level-0" (:library examples)
  (("bg-0" :copy "/brick-group")
   (comp:transform :translate (v3:vec 0f0 1f0 0f0)))
  (("bg-1" :copy "/brick-group")
   (comp:transform :translate (v3:vec 1f0 0f0 0f0)))
  (("bg-2" :copy "/brick-group")
   (comp:transform :translate (v3:vec 0f0 -1f0 0f0)))
  )

;; The toplevel prefab to load the example.
(v:define-prefab "bricked" (:library examples)
  (("camera" :copy "/cameras/ortho")
   (comp:camera :zoom 25f0))
  (("level-0" :link "/level-0"))
  (("ball-0" :link "/ball")
   (comp:transform
    :translate (v3:vec 0f0 -10f0 0f0)
    :translate/velocity (v3:velocity v3:+up+ 10f0))))
