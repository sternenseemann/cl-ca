;;; pattern.lisp
;;; simple example program
;;; showing the capability to generate a pattern using
;;; cl-ca:create-automata
(ql:quickload :cl-ca)

(defparameter *from* '(0 . 0))
(defparameter *to* '(20 . 10))

(defun pattern (pos)
  (if (= 0 (mod (+ (car pos) (cdr pos)) 2))
    'nil
    't))

(defun toggle (me neighbors)
  (list (not (car me)) (cadr me)))

(cl-ca:create-automata *from* *to* #'pattern #'toggle)

(loop do (progn
           (cl-ca:print-automata *from* *to* (lambda (x)
                                               (if x
                                                 #\#
                                                 #\space)))
           (cl-ca:run-step *from* *to* (lambda (pos) '()))
           (sleep 0.5)))
