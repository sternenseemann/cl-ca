;;; pattern.lisp
;;; simple example program
;;; showing the capability to generate a pattern using
;;; cl-ca:create-automata
(ql:quickload :cl-ca)

(defparameter *from* '(0 . 0))
(defparameter *to* '(20 . 10))


(defun print-automata (from to)
           #+clisp
           (ext:shell "clear")
           #+sbcl
           (sb-ext:run-program "/bin/sh" (list "-c" "clear") :input nil :output *standard-output*)
           (loop for y
                 from (cdr from)
                 upto (cdr to)
                 do (progn 
                      (loop for x
                            from (car from)
                            upto (car to) 
                            do (format t "~a" (if (car (cl-ca:get-automaton (cons x y)))
                                                #\#
                                                #\space)))
                      (format t "~%"))))

(defun pattern (pos)
  (if (= 0 (mod (+ (car pos) (cdr pos)) 2))
    'nil
    't))

(defun toggle (me neighbors)
  (list (if (car me)
          'nil
          't) (cadr me)))

(cl-ca:create-automata *from* *to* #'pattern #'toggle)

(loop do (progn
           (print-automata *from* *to*)
           (cl-ca:run-step *from* *to* (lambda (pos) '()))
           (sleep 0.5)))
