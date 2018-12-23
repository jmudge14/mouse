(in-package :mouse)


; Resources stored in script folder, return filenames from there for loading purposes.
(defun resource-path (resource-name &key (type "png"))
  (asdf:system-relative-pathname :mouse resource-name :type type))

(defparameter *hole-stuck-time* 3000
             "Milliseconds for a player to be stuck once in a hole") 
(defparameter *cat-random-chance* 9
             "One in *cat-random-chance* chance of cats moving randomly instead of
              toward the player.")

(defvar *next-cat-spawn-time* nil)
(defvar *next-cat-spawn-delay* 10000)




(defvar *game-objects* nil
  "List of game objects, which will be updated during gameplay.")
(defvar *player* nil
  "The player object (mouse), to avoid searching *game-objects* each time it is required.")
(defvar *player-stuck* nil
  "If not nil, disable movement keys until (>= *player-stuck* (sdl2:get-tickts)) .")
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
(defvar *game-state* nil
  "Tracks the current play state of the game overall. NIL = not started, WON/LOST as expected.")

(defvar *sprites* nil
  "Table of sprites for rendering purposes. Currently a plist.")

(defparameter *game-size* 10
  "Size of game area, in squares. A value of 10 means a player could move at
   most 10 spaces before colliding with a wall.")
(defparameter *window-size* 800
  "Size of SDL window the game will be played in, integer in pixels.")
(defparameter *banner-size* 50
  "Extra window height for the top info banner")
(defparameter *steppable-types*
  '(:cat (:player :cheese :hole)
    :player (:cat :cheese :hole)
    :box (:cheese :hole))
  "Game object types that can move, paired with object types they can move on top of.")
(defparameter *pushable-types* '(:player :box :cat)
  "Kinds of objects that can be pushed or pushed onto.
   Special case of cat, which can be pushed but not itself push, handeld in #'push-object-at")
(defparameter *movable-types* '(:player :box :cat)
  "Kinds of objects that can move")

(defparameter *idle-lock* (bordeaux-threads:make-lock "Game Idle Lock")
  "Lock for any idle activity")

(defparameter *font* (get-font (resource-path "DejaVuSansMono" :type "ttf") 28))

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
         (y-px (+ *banner-size* (* tile-size y)))
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
         (y-px (+ *banner-size* (* tile-size y-prime))))
    ; Update location
    (with-slots (x y rect prev-rect) obj
      (sdl2:copy-into-rect prev-rect rect)
      (setf x x-prime
            y y-prime
            (sdl2:rect-x rect) x-px
            (sdl2:rect-y rect) y-px))
    ; Handle collisions
    (let ((targ-objs (game-objects-at x-prime y-prime)))
      (when (>= (length targ-objs) 2)
        (labels ((local-collide (olist)
                   (collide (first olist)
                            (second olist))))
          (alexandria:map-combinations #'local-collide targ-objs :length 2))))))

(defun game-objects-at (x y)
  (remove-if-not (lambda (obj)
                   (and
                     (= (gobj-x obj) x)
                     (= (gobj-y obj) y)))
                 *game-objects*))



(defmethod collide ((obj1 game-object)
                    (obj2 game-object))
  "Called when game objects collide
   A collision is when two objects enter the same position on the board as a result of movement.
   objs should be a list of length two."
  (let ((type1 (gobj-type obj1))
        (type2 (gobj-type obj2)))
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
                  finally (move-game-object *player* x y))
            (setf *player-stuck* nil))
          ; Stop timers when we're out of lives
          ; Cats will random-walk under the mouse otherwise
          (when (<= *lives* 0)
            (setf *game-state* :lost) ; Out of lives, game is lost.
            (remove-timers :updatecats)))
        (:cheese :box
          (setf *game-objects*
                (remove (if (eql type1 :cheese)
                            obj1 
                            obj2)
                        *game-objects*)))
        (:cheese :player
          (setf *game-objects*
                (remove (if (eql type1 :cheese)
                            obj1 
                            obj2)
                        *game-objects*)
                *score* (+ *score* 100)))
        (:hole :box
          (print "Removing box for hole:Box collision~%")
          (setf *game-objects*
                (remove (if (eql type1 :hole)
                            obj2 ; remove box, not hole
                            obj1)
                        *game-objects*)))
        (:hole :player
          (setf *player-stuck*
                (+ (sdl2:get-ticks) *hole-stuck-time*))
          (format t "Player stuck until ~A" *hole-stuck-time*))))))

(defmethod move-relative ((obj game-object) +x +y)
  (with-slots (x y) obj
    (move-game-object obj (+ x +x) (+ y +y))))

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
  (setf *loaded* nil)
  (setf *sprites* nil)
  (values))

(defun game-obj-type (obj-type)
  (lambda (obj)
    (eql obj-type (gobj-type obj))))

(defvar *renderer* nil
  "Reference to the current renderer, used in run-game")

(defun cat-stuck-p (x y)
  "Return if a cat at location x,y would be stuck"
  (not (steppable-directions x y)))

(defun gobj-sprite (obj)
  "Return image texture corresponding to a game object, loading it from disk if required."
  (let* ((obj-type (gobj-type obj))
         (sprite-type (case obj-type
                        (:cat (if (cat-stuck-p (gobj-x obj)
                                               (gobj-y obj))
                                  :cat-stuck
                                  :cat))
                        (:player (if *player-stuck*
                                     :player-stuck
                                     :player))
                        (t obj-type)))
         (sprite (getf *sprites* sprite-type)))
    (unless sprite
      (let* ((filename (resource-path (symbol-name sprite-type) :type "png"))
             (surface (sdl2-image:load-image filename))
             (texture (sdl2:create-texture-from-surface *renderer* surface)))
        (setf sprite texture
              (getf *sprites* sprite-type) texture)))
    sprite))

(defun render-game-object (rend obj)
  "Render game object"
  (let ((sprite (gobj-sprite obj)))
    ;Redraw game objects
    (sdl2:render-copy rend sprite :dest-rect (gobj-rect obj))))


(defun format-ticks (tick-count)
  (if tick-count
      (let* ((mins (truncate tick-count 60000))
             (secs (mod (round tick-count 1000)
                        60)))
        (format nil "~A:~A" mins secs))
      ""))

(defparameter *game-object-sort-order* 
  '(:player :cat :box :cheese :hole :nothing)
  "Stacking order for rendering game objects")

(defun gobj-sort-order (obj1 obj2)
  (let ((p1 (position (gobj-type obj1)
                      *game-object-sort-order*))
        (p2 (position (gobj-type obj2)
                      *game-object-sort-order*)))
    (> (or p1 2)
       (or p2 2))))

(defvar *temp-debug* nil)
(defun event-idle (win rend)
  "Called for the :idle event in SDL event loop"
  (declare (ignore win))
  (bordeaux-threads:with-lock-held (*idle-lock*)
    ; Game timers
    (do-timers)
    ; Spawn cats
    (when (and *next-cat-spawn-time*
               (>= (sdl2:get-ticks) *next-cat-spawn-time*))
      (spawn-cats)
      (increment-cat-spawn-timer))
    ; Check if player is unstuck from hole
    (when (and *player-stuck* 
               (>= (sdl2:get-ticks) *player-stuck*))
      (setf *player-stuck* nil))
    ; Clear buffer
    (sdl2:render-clear rend)
    ;Redraw game objects
    (setf *temp-debug* (copy-list *game-objects*))
    (setf *game-objects* (sort *game-objects* #'gobj-sort-order))
    (dolist (o *game-objects*)
      (render-game-object rend o))
    ;Draw current game info
    (draw-text rend
               (format nil "LIVES: ~A  SCORE: ~A  CATS LEFT: ~A  ~A"
                       *lives*
                       *score*
                       *remaining-cats*
                       (format-ticks (if *next-cat-spawn-time*
                                         (- *next-cat-spawn-time* (sdl2:get-ticks))
                                         nil)))
               *font*
               0 0
               255 255 255 255)
    ;Border between game info and game objects
    ; TODO
    ;Perform status-specific updates
    (case *game-state*
      (:lost
        (draw-text rend
                   "GAME OVER"
                   *font*
                   10 50
                   255 255 255 255))
      (:won
        (draw-text rend
                   "YOU WON!"
                   *font*
                   10 50
                   255 255 255 255)))
    (sdl2:render-present rend)))

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

(defun step-objects-at (x y +x +y)
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
    ; If any object here is not pushable and not steppable then fail.
    (when (find-if-not (lambda (o) 
                         (or (pushablep o)
                             (steppablep (gobj-x o)
                                         (gobj-y o)
                                         +x
                                         +y)))
                       o)
      (return-from push-object-at nil))
    ; If x,y->+x,+y is steppable, then step.
    (when (steppablep x y +x +y)
      (step-objects-at x y +x +y)
      (return-from push-object-at t))
    ; If target coordinates are illegal, push fails.
    (unless (and (jmutil:clampedp xp 0 (1- *game-size*))
                 (jmutil:clampedp yp 0 (1- *game-size*)))
      (return-from push-object-at nil))
    ; In case of a cat, if target coordinates are not steppable, fail to push.
    ; This allows a cat to be pushed, but not to push anything else.
    (when (find :cat (game-objects-at x y) :key #'gobj-type)
        (unless (steppablep x y xp yp)
          (return-from push-object-at nil)))
    ; Attempt to push next object which might be in the way.
    (unless (push-object-at xp yp +x +y)
      (return-from push-object-at nil))
    ; Move all movable objects to the new location
    (step-objects-at x y +x +y)
    #|
    (dolist (ob o)
      (when (movablep ob)
        (move-relative ob +x +y)))
    |#
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
  (when (find *game-state* '(:won :lost))
    (return-from event-mvplayer))
  (when *player-stuck* 
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
    ; Small chance of picking a completely random direction
    ; This allows cats to get "unstuck" from some obstacles.
    (if (= 0 (random *cat-random-chance*))
        (setf distances all-distances))
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

(defun random-cat ()
  (loop for x = (random *game-size*) then (random *game-size*)
        for y = (random *game-size*) then (random *game-size*)
        until (null (game-objects-at x y))
        finally (push (make-game-object :cat x y) *game-objects*)))

(defun load-level (game-data)
  (bordeaux-threads:with-lock-held (*idle-lock*)
    (let ((game-size (getf game-data :game-size))
          (contents  (getf game-data :contents))
          (num-cats  (getf game-data :num-cats))
          (bonus-lives (getf game-data :bonus-lives))
          (cat-spawn-delay (getf game-data :cat-spawn-delay)))
      (free-game-objects)
      (setf *loaded* :in-progress
            *game-objects* nil
            *player* nil
            *game-size* game-size
            *remaining-cats* (or num-cats 0)
            *lives* (+ *lives* (or bonus-lives 0))
            *next-cat-spawn-delay* (if cat-spawn-delay
                                       (* 1000 cat-spawn-delay)
                                       nil))
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

(defun spawn-cats ()
  (when (> *remaining-cats* 0)
    (dotimes (c (min 2 *remaining-cats*)) ; Generate up to *remaining-cats* cats, two at a time.
      (random-cat)
      (decf *remaining-cats*))))

(defun increment-cat-spawn-timer ()
  (setf *next-cat-spawn-time* 
        (if (and *next-cat-spawn-delay*
                 (> *remaining-cats* 0))
            (+ (sdl2:get-ticks)
               *next-cat-spawn-delay*)
            nil)))

(defun next-level ()
  "Load the next level, or otherwise act to progress the game"
  (cond ((> *remaining-cats* 0) ; Levels continue as long as more cats are needed.
         (spawn-cats)
         (increment-cat-spawn-timer))
        (*levels*  ; Load the next level.
          (load-level (first *levels*))
          (setf *levels* (rest *levels*)
                *player-stuck* nil)
          (incf *score* 200)) ; Bonus points for each level
        ; Out of levels - player won the game!
        (t (setf *game-state* :won))))


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
            (step-objects-at cx cy +x +y) ; move in 'best' direction
            (incf stuck-cats))))         ; mark cat as stuck
    ; Check if all cats are stuck (level win condition)
    (when (or (null cats)
              (= (length cats) stuck-cats))
      (dolist (c cats) ; turn cats to cheese
        (decf *remaining-cats*)  
        (setf (gobj-type c) :cheese))
      (incf *score* (* stuck-cats 50))
      (next-level))))

(defun event-updatecats ()
  "Run cat update"
  (update-cats))

(defun event-startgame ()
      ; Timers for cats
      (sdl2-util:remove-timers :updatecats)
      (sdl2-util:make-sdl-userevent-timer 1000 :updatecats :ident :updatecats)
      ; Reset player lives
      (setf *lives* 3)
      (setf *game-state* nil)
      (setf *score* 0)
      (increment-cat-spawn-timer))


(defun run-game ()
  (finish-output)
  (sdl2-util:with-sdl-thread
        (:title "Mouse Game" :h (+ *window-size* *banner-size*) :w *window-size*)
    (setf *last-ticks* (sdl2:get-ticks))
    (setf *renderer* rend)
    (setf *sprites* nil) ; Can't re-use sprites between runs.
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
         (event-idle win rend))
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



(defun play-level (level-data)
  "Plays a level defined by level-data"
  (free-game-objects)
  (load-level level-data)
  (run-game))




(defun edit-level (size &key (game-objects nil) (lives nil) (cats 0) (cat-spawn-delay nil))
  "A basic level editor; click to cycle through object types in each grid square."
  (free-game-objects)
  (setf *game-objects* game-objects)
  (setf *game-size* size)
  (let ((result nil))
    (sdl2-util:with-initialized-sdl (:title "Mouse Game" :h (+ *banner-size* *window-size*) :w *window-size*)
      (setf *renderer* rend)
      (sdl2-image:init '(:png)) ; Enable loading of images for tiles
      ; Event loop
      (sdl2:with-event-loop (:method :poll)
        (:idle () 
         ; Clear to black
         (sdl2:set-render-draw-color rend 0 0 0 255)
         (sdl2:render-clear rend)
         ; Render a grid
         (sdl2:set-render-draw-color rend 255 255 255 255)
         (dotimes (x size)
           (dotimes (y size)
             (sdl2:render-draw-rect rend
                                    (sdl2:make-rect (* x (truncate *window-size* size))
                                                    (+ *banner-size* 
                                                       (* y (truncate *window-size* size)))
                                                    (truncate *window-size* size)
                                                    (truncate *window-size* size)))))
         ; Render game objects on a blank canvas
         (dolist (obj *game-objects*)
           (render-game-object rend obj))
         (sdl2:render-present rend))
        (:quit () 
          (setf result *game-objects*)
          t)
        #|(:keyup (:keysym keysym) 
                  t) |#
        (:mousebuttondown (:x xpx :y ypx)
         (let* ((x (truncate xpx (truncate *window-size* size)))
                (y (truncate (- ypx *banner-size*)
                             (truncate *window-size* size)))
                (existing-objects (game-objects-at x y))
                (obj (first existing-objects)))
           (if existing-objects
               (setf (gobj-type obj)
                     (case (gobj-type obj)
                       (:nothing :box)
                       (:box :cat)
                       (:cat :player)
                       (:player :brick)
                       (:brick  :hole)
                       (t :nothing)))
               (push (make-game-object :box x y) *game-objects*))))))
    (setf *game-objects*
          (remove-if (curry #'eql :nothing) *game-objects* :key #'gobj-type))
    ; Convert resultant data into level set suitable for #'load-level
    `(:game-size ,size 
      :num-cats ,cats
      :bonus-lives ,lives
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

(defun play-levelset (levelset)
  (free-game-objects)
  (load-level (first levelset))
  (setf *levels* (rest levelset))
  (run-game)
  (event-startgame))



