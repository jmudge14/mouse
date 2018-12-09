(let* ((cat-x (gobj-x cat*))
       (cat-y (gobj-y cat*))
       (cat-dir (cat-step-direction cat-x cat-y))
       (dir-x (first cat-dir))
       (dir-y (second cat-dir)))
  (when cat-dir (move-relative cat* dir-x dir-y)))

(dolist (d '((0 0) (0 1) (0 2) (1 0) (1 2) (2 0) (2 1) (2 2)))
  (destructuring-bind (x y) d
    (push (make-game-object :box x y "box.png")
          *game-objects*)))

(dolist (y '(3 4)) 
  (dolist (x (alexandria:iota 10))
    (push (make-game-object :box x y "box.png")
          *game-objects*)))

(move-game-object cat* 1 1)

