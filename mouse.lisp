(ql:quickload '(:sdl2 :sdl2-ttf :bordeaux-threads :alexandria :sdl2-util :jmutil :sdl2-util :sdl2-image))

(require :sdl2)
(require :sdl2-image)
(require :sdl2-ttf)
(require :bordeaux-threads)
(require :alexandria)
(require :sdl2-util)
(require :jmutil)


(import 'alexandria:clamp)
(import 'alexandria:curry)
(import 'sdl2-util:with-sdl-thread)
(import 'sdl2-util:rgb)
(import 'jmutil:clampedp)



(defvar *game-objects* nil
  "List of game objects, which will be updated during gameplay.")
(defvar *player* nil
  "The player object (mouse), to avoid searching *game-objects* each time it is required.")
(defvar *loaded* nil
  "Indicates whether a level has been loaded or is in progress of being loaded,
   for purposes of suspending rendering and game checks while data integrity
   would be compromised.")
(defvar *lives* 1
  "Number of lives a player has remaining")
(defvar *score* 0
  "Current game score")
(defvar *remaining-cats* 0
  "Number of cats before next level")

(defvar *sprites* nil
  "Table of sprites for rendering purposes. Currently a plist.")

(defparameter *game-size* 10
  "Size of game area, in squares. A value of 10 means a player could move at
   most 10 spaces before colliding with a wall.")
(defparameter *window-size* 800
  "Size of SDL window the game will be played in, integer in pixels.")
(defparameter *steppable-types*
  '(:cat (:player :cheese)
    :player (:cat :cheese)
    :box (:cheese))
  "Game object types that can move, paired with object types they can move on top of.")
(defparameter *pushable-types* '(:player :box)
  "Kinds of objects that can be pushed or pushed onto")
(defparameter *movable-types* '(:player :box :cat)
  "Kinds of objects that can move")

(defparameter *idle-lock* (bordeaux-threads:make-lock "Game Idle Lock")
  "Lock for any idle activity")

(defvar *levels* nil)

(sdl2:register-user-event-type :mvplayer)   ; Event for movement keys
(sdl2:register-user-event-type :updatecats) ; Event for cat timer
(sdl2:register-user-event-type :startgame)  ; Event for starting a game/level after loading.


(defclass game-object ()
  ((rect :initarg :rect
         :initform (sdl2:make-rect 0 0 1 1)
         :accessor gobj-rect
         :documentation "Current location and size of object")
   (prev-rect :initarg :prev-rect
              :initform (sdl2:make-rect 0 0 1 1)
              :accessor gobj-prev-rect
              :documentation "Tracks previous rect after a move, for animation purposes.")
   (obj-type :initarg :obj-type
             :initform nil
             :accessor gobj-type 
             :documentation "Type of object, e.g., :PLAYER or :CAT")
   (x :initarg :x
      :initform 0
      :accessor gobj-x 
      :documentation "Logical game horizontal location, smaller than *game-size*")
   (y :initarg :y
      :initform 0
      :accessor gobj-y
      :documentation "Logical game vertical location, smaller than *game-size*"))
  (:documentation "Generic container for game objects"))

(defun make-game-object (obj-type x y)
  (let* ((tile-size (truncate *window-size* *game-size*))
         (x-px (* tile-size x))
         (y-px (* tile-size y))
         (rect (sdl2:make-rect x-px y-px tile-size tile-size))
         (prev-rect (sdl2:copy-rect rect)))
    (make-instance 'game-object
                   :rect rect
                   :prev-rect prev-rect
                   :obj-type obj-type
                   :x x
                   :y y)))

(defmethod move-game-object ((obj game-object) x y)
  "Move a game object to a new (logical) location."
  (let* ((tile-size (truncate *window-size* *game-size*))
         (x-prime (clamp x 0 (1- *game-size*)))
         (y-prime (clamp y 0 (1- *game-size*)))
         (x-px (* tile-size x-prime))
         (y-px (* tile-size y-prime)))
    (with-slots (x y rect prev-rect) obj
      (sdl2:copy-into-rect prev-rect rect)
      (setf x x-prime
            y y-prime
            (sdl2:rect-x rect) x-px
            (sdl2:rect-y rect) y-px))))

(defun game-objects-at (x y)
  (remove-if-not (lambda (obj)
                   (and
                     (= (gobj-x obj) x)
                     (= (gobj-y obj) y)))
                 *game-objects*))
(defun collide-object (objs)
  "Called when game objects collide
   A collision is when two objects enter the same position on the board as a result of movement.
   objs should be a list of length two."
  (let ((type1 (gobj-type (first objs)))
        (type2 (gobj-type (second objs))))
    (macrolet ((type-cond (&rest cases)
                 (let ((case-clauses
                         (mapcar (lambda (c)
                                   (destructuring-bind (t1 t2 &rest body) c
                                     `((or (and (eql type1 ,t1)
                                                (eql type2 ,t2))
                                           (and (eql type1 ,t2)
                                                (eql type2 ,t1)))
                                       ,@body)))
                                 cases)))
                   `(cond ,@case-clauses))))
      (type-cond 
        (:cat :player 
          (decf *lives*)
          ; Move to a random empty position when we have lives left
          (when (> *lives* 0)
            (loop for x = (random *game-size*) then (random *game-size*)
                  for y = (random *game-size*) then (random *game-size*)
                  until (= 0 (length (game-objects-at x y)))
                  finally (move-game-object *player* x y)))
          ; Stop timers when we're out of lives
          ; Cats will random-walk under the mouse otherwise
          (when (<= *lives* 0)
            (sdl2-util:remove-timers :updatecats)))
        (:cheese :box
          (setf *game-objects*
                (remove (if (eql type1 :cheese)
                            (first objs)
                            (second objs))
                        *game-objects*)))
        (:cheese :player
          (setf *game-objects*
                (remove (if (eql type1 :cheese)
                            (first objs)
                            (second objs))
                        *game-objects*)
                *score* (+ *score* 100)))))))

(defmethod move-relative ((obj game-object) +x +y)
  (with-slots (x y) obj
    (move-game-object obj (+ x +x) (+ y +y))
    ; Check for collisions
    (let ((objs (game-objects-at x y)))
      (when (>= (length objs) 2)
        (alexandria:map-combinations #'collide-object
                                     objs
                                     :length 2)))))

(defmethod in-bounds-p (x y)
  "Return whether x,y is in bounds, nil if out of bounds"
  (and (clampedp x 0 (1- *game-size*))
       (clampedp y 0 (1- *game-size*))))


(defun free-game-objects ()
  "Cleans up game objects, particularly SDL resources which they consume"
  (dolist (o *game-objects*)
    (sdl2:free-rect (gobj-rect o))
    (sdl2:free-rect (gobj-prev-rect o)))
  (setf *game-objects* nil)
  (setf *player* nil)
  (values))

(defun game-obj-type (obj-type)
  (lambda (obj)
    (eql obj-type (gobj-type obj))))

(defun get-objects (obj-type)
  (remove-if-not (game-obj-type obj-type) *game-objects*))




(defun game-status ()
  "Return a symbol representing the game status. It is one of:
   :gameover - Game has been played to completion.
   :playing - Game is in progress"
  ; Game over condition: Player is not null and overlaps with a cat object.
  (cond ((and *player*
              (<= *lives* 0))
         :gameover)
        (t :playing)))

(defun cat-stuck-p (x y)
  "Return if a cat at location x,y would be stuck"
  (not (steppable-directions x y)))

(defun gobj-sprite (obj)
  "Return SDL surface corresponding to a game object, loading it from disk if required."
  (let* ((obj-type (gobj-type obj))
         (sprite-type (case obj-type
                        (:cat (if (cat-stuck-p (gobj-x obj)
                                               (gobj-y obj))
                                  :cat-stuck
                                  :cat))
                        (t obj-type)))
         (sprite (getf *sprites* sprite-type)))
    (unless sprite
      (let ((bitmap (sdl2-image:load-image (concatenate 'string
                                                       (symbol-name sprite-type)
                                                       ".png"))))
        (setf sprite bitmap
              (getf *sprites* sprite-type) bitmap)))
    sprite))

(defun render-game-object (surf win obj)
  "Draw game object on the given surface"
  (declare (ignore win))
  (let ((sprite (gobj-sprite obj)))
    ;Redraw game objects
    (sdl2:blit-scaled sprite nil surf (gobj-rect obj))))


(defvar *last-ticks* 0)
(defun event-idle (surf win)
  (bordeaux-threads:with-lock-held (*idle-lock*)
    ; framerate limiter
#|    (if (<= 10 (- (sdl2:get-ticks) *last-ticks*))
        (return-from event-idle)
        (setf *last-ticks* (sdl2:get-ticks))) |#
    ; Game timers
    (sdl2-util:do-timers)
    ; Clear buffer
    (sdl2-util:clear-surface surf)
    ;Redraw game objects
    (dolist (o *game-objects*)
      (render-game-object surf win o))
    ;Draw current game info
    (sdl2-util:draw-text win surf
                         (format nil "LIVES: ~A       SCORE: ~A" *lives* *score*)
                         0 0
                         255 255 255 255)
    ;Perform status-specific updates
    (case (game-status)
      (:gameover
        (sdl2-util:draw-text win surf 
                             "GAME OVER" 100 100 
                             255 255 255 255)))
    (sdl2:update-window win)))

(defmethod catp ((o game-object))
  (eq (gobj-type o) :cat))

(defmethod playerp ((o game-object))
  (eq (gobj-type o) :player))

(defun movablep (o)
  (find (gobj-type o) *movable-types*))

(defun pushablep (o)
  (find (gobj-type o) *pushable-types*))


(defun steppablep (x y +x +y)
  "Return t if the movable objects at x,y can step on +x,+y, i.e., move without pushing
   Also returns t if no objects at x,y at all.
   Return nil if +x,+y is out of bounds or equals x,y"
  (if (and (in-bounds-p (+ x +x) (+ y +y))
           (or (not (= +x 0)) ; not +0,+0
               (not (= +y 0))))
      (dolist (o1 (game-objects-at x y) t)
        (dolist (o2 (game-objects-at (+ x +x) (+ y +y)))
          (let ((t1 (gobj-type o1))
                (t2 (gobj-type o2)))
            (unless (find t2 (getf *steppable-types* t1 nil))
              (return-from steppablep nil)))))
      nil))

(defun step-object-at (x y +x +y)
  "Attempt to move, without pushing, from x,y in direction +x,+y.
   Return t if move succeeded, or nil otherwise."
  (when (and (steppablep x y +x +y)
             (<= (abs +x) 1) (<= (abs +y) 1)) ; only allow one-move steps
        (dolist (o (game-objects-at x y) t)
          (when (movablep o)
            (move-relative o +x +y)))))

(defun push-object-at (x y +x +y)
  "Push the object at x,y in direction +x,+y if possible.
   Return T if successful or if (x,y) is empty."
  (let ((o (game-objects-at x y))
        (xp (+ x +x))
        (yp (+ y +y)))
    ; If x,y is empty, nothing to do - push succeeds de facto.
    (unless o (return-from push-object-at t))
    ; If any object here is not pushable then fail.
    (when (find-if-not #'pushablep o)
      (return-from push-object-at nil))
    ; If x,y->+x,+y is steppable, then step.
    (when (steppablep x y +x +y)
      (step-object-at x y +x +y)
      (return-from push-object-at t))
    ; If target coordinates are illegal, push fails.
    (unless (and (jmutil:clampedp xp 0 (1- *game-size*))
                 (jmutil:clampedp yp 0 (1- *game-size*)))
      (return-from push-object-at nil))
    ; Attempt to push next object
    (unless (push-object-at xp yp +x +y)
      (return-from push-object-at nil))
    ; Move all movable objects to the new location
    (dolist (ob o)
      (when (movablep ob)
        (move-relative ob +x +y)))
    ; Return true when moves have all succeeded.
    t))

(defun event-keyup (keysym)
  (sdl2-util:scancode-case (sdl2:scancode-value keysym)
    ; Start game
    (:scancode-return
      (sdl2:push-user-event :startgame))
    ; Exit keys
    ((:scancode-escape :scancode-capslock)
     (sdl2:push-quit-event))
    ; Movement keys
    (:scancode-kp-1 
      (sdl2:push-user-event :mvplayer '(-1  1)))
    ((:scancode-down :scancode-kp-2)
      (sdl2:push-user-event :mvplayer '( 0  1)))
    (:scancode-kp-3
      (sdl2:push-user-event :mvplayer '( 1  1)))
    ((:scancode-left :scancode-kp-4)
      (sdl2:push-user-event :mvplayer '(-1  0)))
    ((:scancode-right :scancode-kp-6)
      (sdl2:push-user-event :mvplayer '( 1  0)))
    (:scancode-kp-7
      (sdl2:push-user-event :mvplayer '(-1  -1)))
    ((:scancode-up :scancode-kp-8)
      (sdl2:push-user-event :mvplayer '( 0 -1)))
    (:scancode-kp-9
      (sdl2:push-user-event :mvplayer '( 1 -1)))))

(defun event-mvplayer (datum)
  (when (eq (game-status) :gameover)
    (return-from event-mvplayer))
  (let ((+x (first datum))
        (+y (second datum)))
    (push-object-at (gobj-x *player*)
                    (gobj-y *player*)
                    +x
                    +y)))

(defun player-distance-from (x y)
  "Return the distance in game units to the player from coordinates x,y"
  (let* ((player-x (gobj-x *player*))
         (player-y (gobj-y *player*))
         (diff-x (abs (- x player-x))) ; x,y always positive
         (diff-y (abs (- y player-y))))
    (sqrt (+ (expt diff-x 2)
             (expt diff-y 2)))))

(defun step-distances (x y) 
  "List of (list (player-distance-from xp,yp) dir-x dir-y) for each steppable direction from x,y"
  (let ((result nil))
    (dolist (+x '(-1 0 1) result)
      (dolist (+y '(-1 0 1))
        (when (steppablep x y +x +y)
          (push (list (player-distance-from (+ x +x) (+ y +y))
                      +x
                      +y)
                result))))))

(defun close-to (max-diff num1 num2)
  "t if num1 and num2 are within max-diff of each other"
  (>= max-diff
      (abs (- (max num1 num2)
              (min num1 num2)))))

(defun cat-step-direction (x y)
  "Returns the direction a cat should step, or nil if the cat is stuck."
  (let* ((all-distances (sort (step-distances x y) #'< :key #'first))
         (min-distance (or (first (first all-distances))
                           0))
         (distances (remove-if-not (curry #'close-to 0.1 min-distance)
                                   all-distances
                                   :key #'first)))
    (if distances
        (rest (elt distances (random (length distances))))
        nil)))



(defun steppable-directions (x y) 
  "Return a list of directions '(x y) that the objects at x y can step.
   Nil if there are no objects at x,y or if there is no direction in which
   they can step."
  (let ((objs (game-objects-at x y))
        (dirs nil))
    (unless objs (return-from steppable-directions nil))
    (dolist (+x '(-1 0 1) dirs)
      (dolist (+y '(-1 0 1)) 
        (when (steppablep x y +x +y)
          (push (list +x +y) dirs))))))


(defun next-level ()
  "Load the next level, or otherwise act to progress the game"
  (cond ((> 0 *remaining-cats*) ; Levels continue as long as more cats are needed.
         (dotimes (c (random (min 4 *remaining-cats*))) ; Generate up to *remaining-cats* cats
           (loop for x = (random *game-size*) then (random *game-size*)
                 for y = (random *game-size*) then (random *game-size*)
                 until (null (game-objects-at x y))
                 finally (push (make-game-object :cat x y) *game-objects*))))
       (*levels*  ; Load the next level.
        (load-level (first *levels*))
        (setf *levels* (rest *levels*)))))


(defun update-cats ()
  "Check the status of cat objects, moving them if possible or turning them to cheese if not."
  (let ((cats (remove-if-not #'catp *game-objects*))
        (stuck-cats 0))
    (dolist (c cats)
      (let* ((cx (gobj-x c))
             (cy (gobj-y c))
             (dir (cat-step-direction cx cy))
             (+x (first dir))
             (+y (second dir)))
        (if dir
            (step-object-at cx cy +x +y) ; move in 'best' direction
            (incf stuck-cats))))         ; mark cat as stuck
    ; Check if all cats are stuck (level win condition)
    (when (or (null cats)
              (= (length cats) stuck-cats))
      (dolist (c cats) ; turn cats to cheese
        (setf (gobj-type c) :cheese))
      (next-level))))

(defun event-updatecats ()
  "Run cat update"
  (update-cats))

(defun event-startgame ()
      ; Timers for cats
      (sdl2-util:remove-timers :updatecats)
      (sdl2-util:make-sdl-userevent-timer 1000 :updatecats :ident :updatecats)
      ; Reset player lives
      (setf *lives* 1))


(defun run-game ()
  (sb-posix:chdir #P"/home/jakykong/lisp/mouse")
  (finish-output)
  (setf *last-ticks* (sdl2:get-ticks))
  (with-sdl-thread (:title "Mouse Game" :h *window-size* :w *window-size*)
    (sdl2-image:init '(:png)) ; Enable loading of images for tiles
    (let ()
      ; Default minimum data that satisfies assumptions later in code.
      ; Not playable data. Just a mouse you can move around.
      (unless *loaded*
        (when *game-objects* (free-game-objects))
        (setf *player* (make-game-object :player 0 0))
        (push *player* *game-objects*))

      ; Event loop
      (sdl2:with-event-loop (:method :poll)
        (:idle () 
         (event-idle surf win))
        (:quit ()
         t)
        (:keyup (:keysym keysym) 
         (event-keyup keysym))
        (:mvplayer (:user-data datum)
         (event-mvplayer datum))
        (:updatecats ()
         (event-updatecats))
        (:startgame ()
         (event-startgame))))))

#|
(defvar *mywin*)
(defvar *mysurf*)
(sdl2-util:with-sdl-thread (:title "Testing memory" :h 200 :w 200)
  (setf *mywin* win)
  (setf *mysurf* surf)
  (sdl2:with-event-loop (:method :poll)
    (:idle ())
    (:quit () t)))
|#




(defun load-level (game-data)
  (bordeaux-threads:with-lock-held (*idle-lock*)
    (let ((game-size (getf game-data :game-size))
          (contents  (getf game-data :contents))
          (num-cats  (getf game-data :num-cats))
          (bonus-lives (getf game-data :bonus-lives)))
      (free-game-objects)
      (setf *loaded* :in-progress
            *game-objects* nil
            *player* nil
            *game-size* game-size
            *remaining-cats* (or num-cats 0)
            *lives* (+ *lives* (or bonus-lives 0)))
      (loop for i = 0 then (1+ i)
            for e in contents
            do (let ((x (mod i game-size))
                     (y (truncate i game-size))
                     (k (alexandria:make-keyword e)))
                 (unless (eql k :*) ; Asterisks denote blank space, no objects.
                   (case (alexandria:make-keyword e)
                     (:b (push (make-game-object :box x y) *game-objects*))
                     (:c (push (make-game-object :cat x y) *game-objects*))
                     (:x (push (make-game-object :brick x y) *game-objects*))
                     (:m (setf *player* (make-game-object :player x y))
                         (push *player* *game-objects*))
                     (:player (setf *player* (make-game-object :player x y)) ; synonym
                              (push *player* *game-objects*))
                     ; Default case - assume a valid game object was named.
                     (t (push (make-game-object (alexandria:make-keyword e) x y) *game-objects*))))))
      (setf *loaded* t))))

(defun play-level (level-data)
  (load-level level-data)
  (sdl2:push-user-event :startgame))

(defparameter *example-level-1*
  '(:game-size 10
    :contents
        (*   *   *   *   *   *   *   *   *   *
         *   *   *   *   *   *   *   *   *   *
         *   *   b   b   b   b   b   b   *   *
         *   *   b   b   b   b   b   b   *   *
         *   *   b   b   m   b   b   b   *   *
         *   *   b   b   b   b   b   b   *   *
         *   *   b   b   b   b   b   b   *   *
         *   *   b   b   b   b   b   b   *   *
         *   *   *   *   *   *   *   *   *   c
         *   *   *   *   *   *   *   *   *   *)))

(defparameter *example-level-2*
  '(:game-size 20
    :contents
        (* * * * * * * c * * * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * c *
         c * * b b b b b b m b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * * * * * * * c * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *)))


(defparameter *example-level-3*
  '(:game-size 20
    :contents
        (* * * * * * * c * * * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b x b b b b b b b x b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * c *
         c * * b b b b b b m b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b x b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b x b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * * * * * * * c * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *)))

(defun edit-level (size &key (game-objects nil) (lives nil) (cats 0))
  "A basic level editor; click to cycle through object types in each grid square."
  (sb-posix:chdir #P"/home/jakykong/lisp/mouse")
  (free-game-objects)
  (setf *game-objects* game-objects)
  (setf *game-size* size)
  (let ((result nil))
    (sdl2-util::with-initialized-sdl (:title "Mouse Game" :h *window-size* :w *window-size*)
      (sdl2-image:init '(:png)) ; Enable loading of images for tiles
      ; Event loop
      (sdl2:with-event-loop (:method :poll)
        (:idle () 
         ; Render game objects on a blank canvas
         (sdl2-util:clear-surface surf)
         (dolist (obj *game-objects*)
           (render-game-object surf win obj))
         (sdl2:update-window win))
        (:quit () 
          (setf result *game-objects*)
          t)
        #|(:keyup (:keysym keysym) 
                  t) |#
        (:mousebuttondown (:x xpx :y ypx)
         (let* ((x (truncate xpx (truncate *window-size* size)))
                (y (truncate ypx (truncate *window-size* size)))
                (existing-objects (game-objects-at x y))
                (obj (first existing-objects)))
           (if existing-objects
               (setf (gobj-type obj)
                     (case (gobj-type obj)
                       (:box :cat)
                       (:cat :player)
                       (:player :brick)
                       (:brick :nothing)
                       (t :box)))
               (push (make-game-object :box x y) *game-objects*))))))
    (setf *game-objects*
          (remove-if (curry #'eql :nothing) *game-objects* :key #'gobj-type))
    ; Convert resultant data into level set suitable for #'load-level
    `(:game-size ,size 
      :contents ,(loop for y from 0 to (1- size)
                       appending (loop for x from 0 to (1- size) 
                                       collect (let ((obj (first (game-objects-at x y))))
                                                 (if obj 
                                                     (gobj-type obj)
                                                     '*)))))))


(defun run-edited-level (size)
  (load-level (edit-level size))
  (run-game)
  (event-startgame))
