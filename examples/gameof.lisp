;;; gameof.lisp
;;; example program implementing Conway's game of life

(ql:quickload :cl-ca)

(setf *random-state* (make-random-state t)) ; we want more random stuff /o/
(defparameter *from* '(0 . 0) "starting point of the simulation")
(defparameter *to* '(80 . 24) "ending point of the simulation")

(defun liferules (automaton neighbors)
  "implements the rules of Conway's Game of Life for cl-ca"
  (let ((alive-neighbors (reduce (lambda (val el) 
                                   (if (car el)
                                     (1+ val)
                                     val))
                                 neighbors :initial-value 0)))
    (list 
      ; the cond construct determinates the new state
      (cond
        ((and (car automaton)       (> alive-neighbors 3))                         'nil)
        ((and (car automaton)       (< alive-neighbors 2))                         'nil)
        ((and (car automaton)       (>= alive-neighbors 2) (<= alive-neighbors 3)) 't)
        ((and (not (car automaton)) (= alive-neighbors 3))                         't)
        ((and (not (car automaton)) (not (= alive-neighbors 3)))                   'nil)
        )
      (cadr automaton))))

(defun init-random (pos)
  "returns the initial state of each automaton"
  (if (= 0 (random 2))
    'nil
    't))

(defun only-glider (pos)
  "inits the simulation so that there's only one hacker glider :)"
  (cond
    ((equal pos '(1 . 0)) 't)
    ((equal pos '(2 . 1)) 't)
    ((equal pos '(2 . 2)) 't)
    ((equal pos '(0 . 2)) 't)
    ((equal pos '(1 . 2)) 't)
    (t 'nil)))

(cl-ca:create-automata *from* *to* #'init-random #'liferules)

(loop do (progn
	   (cl-ca:print-automata *from* *to* (lambda (x)
                                           (if x
                                             #\#
                                             #\space)))
	   (sleep 1)
	   (cl-ca:run-step *from* *to* #'cl-ca:moore-neighbors)))
