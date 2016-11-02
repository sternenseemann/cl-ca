(in-package :cl-ca)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; non-functional part managing the automata   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((automata (make-hash-table :test 'equal))
      (dimensions (cons (cons 0 0) (cons 0 0))))

  (defun set-dimensions (dim)
    "Sets the dimensions (bondaries) of the
    simulation (cons upperleft lowerright)"
    (setf dimensions dim))

  (defun get-dimensions ()
    dimensions)

  (defun set-automaton (pos state fun)
   "sets an automaton in the hash table (in the closure)"
    (setf (gethash pos automata) (list state fun)))

  (defun get-automaton (pos)
    "gets an automaton from the hash table (in the closure)"
    (gethash pos automata))

  (defun run-step (neighborhood)
    "runs the the simulation for one step. it invokes the
    automata in the 2d grid defined by set-dimensions"
    (let ((new-hash-table (make-hash-table :test 'equal)))
      (loop for x
            from (caar dimensions)
            to   (cadr dimensions)
            do (loop for y
                     from (cdar dimensions)
                     to   (cddr dimensions)
                     do (let* ((pos (cons x y))
                               (automaton (get-automaton pos))
                               (neighbors-pos (delete-if-not
                                                (lambda (pos)
                                                  (cadr (multiple-value-list (get-automaton pos))))
                                                (funcall neighborhood pos dimensions)))
                               (neighbors (mapcar #'get-automaton neighbors-pos)))
                          (setf (gethash pos new-hash-table)
                                (funcall (cadr automaton) automaton neighbors)))))
      (setf automata new-hash-table)))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; neighborhood functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moore-neighbors (pos dim)
  "calculates the moore neighbors of an automaton"
  (let ((neighbors '()))
  (loop for y
      from (1- (cdr pos))
      to  (1+ (cdr pos))
      do (loop
         for x
         from (1- (car pos))
         to   (1+ (car pos))
         do (let ((neighborpos (cons x y)))
          (if (not (equal pos neighborpos))
            (push (wrap-if-necessary neighborpos dim) neighbors)))))
  neighbors))

(defun von-neumann-neighbors (pos dim)
  "calculates the von Neumann neighbors of an automaton"
  (let ((x (car pos))
        (y (cdr pos)))
    (mapcar (lambda (pos) (wrap-if-necessary pos dim))
         (list (cons (1- x) y)
               (cons (1+ x) y)
               (cons x (1- y))
               (cons x (1+ y))))))

(defun wrap-one-dim (n min max)
  "makes sure that a position is
  always in the boundary min - max
  and wraps it around if necessary"
  (+ min (mod (- n min) max)))

(defun wrap-if-necessary (pos dim)
  "Makes sure that pos is always between
  the boundaries of (car dim) and (cdr dim).
  Wraps pos if necessary"
  (cons
    (wrap-one-dim (car pos) (caar dim) (cadr dim))
    (wrap-one-dim (cdr pos) (cdar dim) (cddr dim))))

;;;;;;;;;;;;;;;;;;;;;;
;; helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun create-automata (gen-state fun)
  "adds the automata between from-pos to to-pos in a
  2d grid. For the initial state it calls gen-state each
  time with the position as argument"
  (let* ((dimensions (get-dimensions))
         (from-pos   (car dimensions))
         (to-pos     (cdr dimensions)))
    (loop for x
          from (car from-pos)
          to   (car to-pos)
          do (loop for y
                   from (cdr from-pos)
                   to   (cdr to-pos)
                   do (set-automaton (cons x y) (funcall gen-state (cons x y)) fun)))))

(defun print-automata (conversion-fun)
  "prints the automata between two positions. It uses a conversion
  function to determine which char to use"
  (let* ((dimensions (get-dimensions))
         (from-pos   (car dimensions))
         (to-pos     (cdr dimensions)))
    ; perform a shell clear (unix and sbcl/clisp only working)
    #+clisp
    (ext:shell "clear")
    #+sbcl
    (sb-ext:run-program "/usr/bin/clear" '() :input nil :output *standard-output*)

    (loop for y
          from (cdr from-pos)
          upto (cdr to-pos)
          do (progn
               (loop for x
                     from (car from-pos)
                     upto (car to-pos)
                     do (format t "~a" (funcall conversion-fun (car (get-automaton (cons x y))))))
               (format t "~%")))))
