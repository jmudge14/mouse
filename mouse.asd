;;;; mouse.asd

(asdf:defsystem #:mouse
  :description "Sort-of clone of Rodent's Revenge"
  :author "Jack Mudge <jakykong@theanythingbox.com>"
  :license  "GPLv3+ for source code; Public Domain for images."
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2 #:sdl2-ttf #:sdl2-image
               #:bordeaux-threads #:alexandria 
               ; Local libraries from Jack Mudge (not in main quicklisp dist):
               #:sdl2-util #:jmutil)
  :components ((:file "package")
               (:file "mouse")
               (:file "levels")))
