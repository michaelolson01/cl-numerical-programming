(in-package :asdf-user)

(defsystem "cl-numerical-programming"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("alexandria")
  :components ((:module "src"
                :components ((:file "arrays")
                             (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-numerical-programming/tests"))))

(defsystem "cl-numerical-programming/tests"
  :author ""
  :license ""
  :depends-on ("cl-numerical-programming"
               "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :description "Test system for cl-numerical-programming"
  :perform (test-op (op c) (symbol-call :rove :run c)))
