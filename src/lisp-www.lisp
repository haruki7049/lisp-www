(in-package :cl-user)
(defpackage lisp-www
  (:use :cl
        :woo)
  (:export :main))
(in-package :lisp-www)

(defun main
  ()
  (write-line "Web server, serves 'Hello, World' to http://localhost:5000")
  (woo:run
    (lambda (env)
      (declare (ignore env))
      '(200 (:content-type "text/plain") ("Hello, World")))))
