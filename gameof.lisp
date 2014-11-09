(ql:quickload :cl-ca)

(setf *random-state* (make-random-state t)) ; we want more random stuff /o/
(defparameter *width* 80 "width of the simulation")
(defparameter *height* 24 "height of the simulation")

(defun liferules (automaton neighbors)
  "implements the rules of Conway's Game of Life for cl-ca"
  (let ((alive-neighbors (reduce (lambda (val el) 
				   (+ val (car el))) 
				 neighbors :initial-value 0)))
    (cond
      ((and (= (car automaton) 1) (> alive-neighbors 3))                          (list 0 (cadr automaton)))
      ((and (= (car automaton) 1) (< alive-neighbors 2))                          (list 0 (cadr automaton)))
      ((and (= (car automaton) 1) (>= alive-neighbors 2) (<= alive-neighbors 3))  (list 1 (cadr automaton)))
      ((and (= (car automaton) 0) (= alive-neighbors 3))                          (list 1 (cadr automaton)))
      ((and (= (car automaton) 0) (not (= alive-neighbors 3)))                    (list 0 (cadr automaton)))
      )))

(defun printgame ()
  "prints the simulation"
  #+clisp
  (ext:shell "clear")
  #+sbcl
  (sb-ext:run-program "/bin/sh" (list "-c" "clear") :input nil :output *standard-output*)
  (loop for y
	upto *height*
	do (progn 
	     (loop for x
		 upto *width*
		 do (format t "~a" (if (= 1 (car (cl-ca:get-automaton (cons x y))))
				     #\#
				     #\space)))
	     (format t "~%"))))

(defun init-random (pos)
  "returns the initial state of each automaton"
  (random 2))

(defun only-glider (pos)
  "inits the simulation so that there's only one hacker glider :)"
  (cond
    ((equal pos '(1 . 0)) 1)
    ((equal pos '(2 . 1)) 1)
    ((equal pos '(2 . 2)) 1)
    ((equal pos '(0 . 2)) 1)
    ((equal pos '(1 . 2)) 1)
    (t 0)))

(cl-ca:create-automata (cons 0 0) (cons *width* *height*) #'init-random #'liferules)

(loop do (progn
	   (printgame)
	   (sleep 1)
	   (cl-ca:run-step (cons 0 0) (cons *width* *height*) #'cl-ca:moore-neighbors)))
