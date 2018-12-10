;;;; package.lisp

(defpackage #:mouse
  (:use #:cl)
  (:shadowing-import-from :sdl2-util
                          :get-font
                          :draw-text
                          :rgb
                          :with-initialized-sdl
                          :with-sdl-thread
                          :remove-timers
                          :scancode-case
                          :do-timers)
  (:shadowing-import-from :alexandria
                          :curry
                          :clamp)
  (:shadowing-import-from :jmutil
                          :clampedp))

