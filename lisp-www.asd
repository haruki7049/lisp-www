(defsystem "lisp-www"
  :description "My test code for Nix package manager and SBCL"
  :version "0.1.0"
  :author "haruki7049"
  :license "MIT"
  :depends-on ("woo")
  :components ((:module "src"
                        :components
                        ((:file "lisp-www")))))
