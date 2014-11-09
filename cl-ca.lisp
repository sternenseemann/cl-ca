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

(in-package :cl-ca)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-functional part managing the automata ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((automata (make-hash-table :test 'equal)))
  (defun set-automaton (pos state fun)
	"sets an automaton in the hash table (in the closure)"
	(setf (gethash pos automata) (list state fun)))

  (defun get-automaton (pos)
	"gets an automaton from the hash table (in the closure)"
	(gethash pos automata))

  (defun run-step (from-pos to-pos neighborhood)
	"runs the the simulation for one step. it invokes the
	automata between from-pos and to-pos in a 2d grid"
	(let ((new-hash-table (make-hash-table :test 'equal)))
	  (loop for x
			from (car from-pos)
			to   (car to-pos)
			do (loop for y
					 from (cdr from-pos)
					 to   (cdr to-pos)
					 do (let* ((pos (cons x y))
							   (automaton (get-automaton pos))
							   (neighbors (mapcar #'get-automaton (funcall neighborhood pos))))
						  (setf (gethash pos new-hash-table)
								(funcall (cadr automaton) automaton neighbors)))))
	  (setf automata new-hash-table)))
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neighborhood functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moore-neighbors (pos)
  "calculates the moore neighbors of an automaton
  and returns the positions of those that exist"
  (let ((neighbors '()))
	(loop for y
		  from (1- (cdr pos))
		  to  (1+ (cdr pos))
		  do (loop
			   for x
			   from (1- (car pos))
			   to   (1+ (car pos))
			   do (let ((neighborpos (cons x y)))
					(if (and 
						  (cadr (multiple-value-list (get-automaton neighborpos)))
						  (not (equal pos neighborpos)))
					  (push neighborpos neighbors)))))
	neighbors))

(defun von-neumann-neighbors (pos)
  "calculates the von Neumann neighbors of an automaton
  and returns the positions of those that exist"
  (let ((x (car pos))
		(y (cdr pos)))
	(delete-if-not (lambda (pos)
					 (cadr (multiple-value-list (get-automaton pos))))
				   (list (cons (1- x) y)
						 (cons (1+ x) y)
						 (cons x (1- y))
						 (cons x (1+ y))))))

;;;;;;;;;;;;;;;;;;;;;;
;; helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun create-automata (from-pos to-pos gen-state fun)
  "adds the automata between from-pos to to-pos in a
  2d grid. For the initial state it calls gen-state each
  time with the position as argument"
  (loop for x
		from (car from-pos)
		to   (car to-pos)
		do (loop for y
				 from (cdr from-pos)
				 to   (cdr to-pos)
				 do (set-automaton (cons x y) (funcall gen-state (cons x y)) fun))))
