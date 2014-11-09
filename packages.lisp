(defpackage :cl-ca
  (:documentation "cellular automaton library")
  (:use :common-lisp)
  (:export
	#:set-automaton
	#:get-automaton
	#:run-step
	#:create-automata
	#:von-neumann-neighbors
	#:moore-neighbors))
