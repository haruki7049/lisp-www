(in-package :cl-user)
(defpackage lisp-www
  (:use :cl
        :woo)
  (:export :main))
(in-package :lisp-www)

(defun main
  ()
  (woo:run
    (lambda (env)
      (declare (ignore env))
      '(200 (:content-type "text/plain") ("Hello, World")))))
