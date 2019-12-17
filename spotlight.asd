(defsystem spotlight
  :author "Conjunctive"
  :maintainer "Conjunctive"
  :license "AGPL-3.0"
  :version "0.2.0"
  :homepage "https://github.com/conjunctive/spotlight"
  :source-control (:git "git@github.com:conjunctive/spotlight.git")
  :description "Functional references for Common Lisp"
  :depends-on ("alexandria")
  :components ((:module "src"
                :serial t
                :components ((:file "spotlight"))))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.org"))
  :in-order-to ((test-op (test-op spotlight-test))))
