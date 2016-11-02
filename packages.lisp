(defpackage :cl-ca
  (:documentation "cellular automaton library")
  (:use :common-lisp)
  (:export
    #:set-automaton
    #:get-automaton
    #:get-dimensions
    #:set-dimensions
    #:run-step
    #:create-automata
    #:print-automata
    #:von-neumann-neighbors
    #:moore-neighbors))
