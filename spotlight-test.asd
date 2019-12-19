(defsystem spotlight-test
  :author "Conjunctive"
  :maintainer "Conjunctive"
  :license "AGPL-3.0"
  :version "0.2.1"
  :homepage "https://github.com/conjunctive/spotlight"
  :source-control (:git "git@github.com:conjunctive/spotlight.git")
  :description "Test system for Spotlight"
  :depends-on ("spotlight"
               "alexandria"
	       "prove")
  :components ((:module "t"
                :serial t
                :components
                ((:file "spotlight"))))
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
