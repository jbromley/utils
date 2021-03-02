#-asdf3.1 (error "ASDF 3.1 or higher is required.")

(defsystem "utils"
  :version "0.0.1"
  :description "Common Lisp scripts"
  :license "MIT"
  :author "J. Bromley"
  :class :package-inferred-system
  :depends-on ((:version "cl-scripting" "0.1")
               (:version "inferior-shell" "2.0.3.3")
               (:version "fare-utils" "1.0.0.5")
               "utils/main"
               "utils/colors"))
