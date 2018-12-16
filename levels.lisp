; Level data 

(in-package :mouse)


(defparameter *example-level-1*
  '(:game-size 10
    :num-cats 2
    :cat-spawn-delay 5
    :bonus-lives 0
    :contents
    (* * * * * * * * * *
     * * * * * * * * * *
     * * b b b b b b * *
     * * b b b b b b * *
     * * b b m b b b * *
     * * b b b b b b * *
     * * b b b b b b * *
     * * b b b b b b * *
     * * * * * * * * * c
     * * * * * * * * * *)))

(defparameter *example-level-2*
  '(:game-size 20
    :num-cats 7
    :cat-spawn-delay 60
    :bonus-lives 0
    :contents
        (* * * * * * * * * * * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * c *
         * * * b b b b b b m b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *)))


(defparameter *example-level-3*
  '(:game-size 20
    :num-cats 7
    :cat-spawn-delay 60
    :bonus-lives 0
    :contents
        (* * * * * * * * * * * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b x b b b b b b b x b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * c *
         * * * b b b b b b m b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b x b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b x b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * b b b b b b b b b b b b b b * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *
         * * * * * * * * * * * * * * * * * * * *)))
(defparameter *example-levelset*
  (list *example-level-1* *example-level-2* *example-level-3*))


(defparameter *levelset*
  (list
 '(:GAME-SIZE 21 ; Level 1 (Win31 Version)
   :NUM-CATS 7 
   :BONUS-LIVES NIL 
   :CONTENTS (* * * * * * * * * * * * * * * * * * * * * * :CAT * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :PLAYER :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *))

 '(:GAME-SIZE 21 ; Level 2 (Win31 Version)
   :NUM-CATS 7 
   :BONUS-LIVES NIL 
   :CONTENTS (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * :BRICK * * * * * * * * * * * * * * * :BRICK * * * * * * * * * * :BOX :BOX :BRICK :BOX :BOX :BOX :BOX :BOX :BRICK :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BRICK :BOX :BOX :BRICK :BOX :BOX :BOX :BRICK :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * :BRICK * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BRICK :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :PLAYER :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BRICK :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BRICK :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BRICK :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BRICK :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BRICK :BOX :BOX :BOX :BOX :BOX :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX * :BRICK * :BRICK * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BRICK :BOX :BOX * * * * * * :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BOX :BRICK :BOX :BOX * * * * * * * * * * * * * * * * * * * * * * * :CAT * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)) 

 '(:GAME-SIZE 21 ; Level 3 (Win31 Version)
   :NUM-CATS 7 
   :BONUS-LIVES NIL 
   :CONTENTS (* * :BOX :BOX :BOX * * * * :BOX * * * * * * * :BOX * * * * * :BOX :BOX :CAT * * * * * * * * :BOX * * * * :BOX :BRICK * * * * * :BOX :BOX * * * * * :BRICK * * * * * * * * * * :BOX * * * :BOX :BOX :BOX * * * * * * :BOX * :BOX * * * * :BOX :BRICK * :BOX * * * * * * :BOX :BOX * * * :BOX :BOX * :BOX :BOX * * * * * * :BOX * * :BOX * :BOX * * * * :BRICK * * * :BOX * * :BOX * * * * :BOX * * * :BRICK * * :BOX :BOX :BOX * :BOX * :BOX * * * :BOX * :BOX :BOX * :BOX * * :BOX * * * :BRICK * * :BOX * * * * :BOX * * * :BOX :BOX :BRICK :BOX * :BRICK * * * * * :BOX :BRICK :BOX * * * :BOX * * :BOX * * * * * :BOX :BOX * * * :BOX * * * * * :BOX :BOX :BOX :BOX :BRICK * :BOX * :BOX * :PLAYER :BOX * :BOX :BOX * * * * :BOX * * :BOX :BOX * * * :BOX * * * * :BOX * :BRICK :BOX * * * * :BOX * * * * * * * :BOX * * * * * * * * :BOX :BOX * * :BOX * * :BRICK * * :BOX :BRICK * * * :BOX * * :BOX * * * :BOX * :BOX * * * :BOX * :BOX * :BOX * * * * * :BOX :BOX * :BOX :BRICK :BOX :BOX * :BOX * * * * * * * * * * :BOX * * * * :BOX :BOX :BRICK :BOX * :BOX * :BRICK * * * * * * * * * * * * :BOX :BOX :BOX :CAT :BOX :BOX :BOX * * :BRICK :BOX :BOX * :BOX * * :BOX :BRICK * :BOX :BOX * :BOX :BOX * * :BRICK * * * :BRICK :BOX :BOX :BOX * :BOX :BOX * * * :CAT * :BOX * * * :BOX * :BOX * :BOX * * :BOX * :BOX :BOX :BOX * :BOX :BOX * * * * * :BOX * * :BOX * * * * * * * * * * * * * * * * * * * * * *))

 '(:GAME-SIZE 21 ; Level 4 (Win31 Version)
   :NUM-CATS 7 
   :BONUS-LIVES NIL 
   :CONTENTS (* * * * * * * :CAT * * * * * * * * * * * * * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX :HOLE :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :HOLE * :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX :PLAYER :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :HOLE * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :HOLE * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :HOLE * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX :HOLE :BOX * :BOX * :BOX * * * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * :BOX * * * * * * * * * * * * * * * * * * * * * * *))

 '(:GAME-SIZE 21 ; Level 5 (Win31 Version) - Note - yarn not yet implemented, appears in level.
   :NUM-CATS 7 
   :BONUS-LIVES NIL 
   :CONTENTS (* :BOX * * * * * * * * * * * :BOX :BOX * * * * * * * :BOX * * * :BRICK * :BOX :BRICK :BRICK * * * * :BOX :BOX * * :BRICK * * * * * * :BRICK * :BOX * * * :BRICK * * * :BOX * * :BRICK * :BRICK * * * * * * * * :BOX * * * * * :BOX * :BOX * :BRICK :BOX :BRICK * :CAT * * * :BOX * :BRICK :BRICK * * * * * * * * :BOX :BOX :BRICK :BRICK * * :BRICK * * * * * * :BOX :HOLE :BRICK * * * :BOX * :BOX * * :BOX * * :BOX * * * * * * * :HOLE * :BOX * :BRICK * * * * * :BRICK * * * :BOX :BRICK :BRICK * * :BOX * :HOLE * * :BOX * :BOX :BRICK * * :BOX :BOX * * :BRICK :BRICK * * * :BOX * * * :BRICK :BRICK * * * * :BOX * * * * * :BOX :BOX * * :HOLE :BRICK * :BOX :BOX * * :HOLE * * * :BRICK * :BOX :BOX * :BRICK * * * * * * :BOX :BRICK * :PLAYER :BRICK * * * :HOLE :BRICK * * :BRICK * :BOX :BOX * * * * :BRICK :BRICK * :BRICK :HOLE * * * * * * :BOX :BOX :BRICK * * :BOX * * * * * * * * * * * * :BOX :BRICK * :HOLE * * * * * * * :BOX :BRICK :BOX * * :BOX * :BOX :BOX * * * * * * * * * :BRICK * * :BOX :BOX * :BOX * * * * * * * * :BOX :BOX * :BRICK * :HOLE * * * * :BRICK * * * :BRICK :BOX * * :BRICK :BRICK * :BRICK :CAT * * * * * :BOX :BOX :BRICK * * * * :BRICK * * :BOX * :BRICK * * :BOX * :BRICK * :BRICK :BOX * * * :BRICK :BOX * * * * * :BOX :BOX * * * * :BRICK * * * * * * * :BRICK * * * * :BRICK * * * * * * * :BOX * * :BOX * * :HOLE :HOLE * :BRICK * * * * :BOX * :BOX * * :BRICK * * * * * * * * * * * * * * * * * * * * * * * * *))
 
   )) ; end of list 
