(defsystem :cl-sector-lispasc
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3) (safety 3) (speed 0)))
                    (funcall next))
  :components ((:module "source"
                :pathname "./"
                :components ((:file "package")
                             (:file "cl-sector-lisp" :depends-on ("package"))
                             (:file "atom-memory" :depends-on ("cl-sector-lisp"))
                             (:file "intern" :depends-on ("atom-memory"))
                             (:file "read" :depends-on ("intern"))
                             (:file "context" :depends-on ("macros"))
                             (:file "macros" :depends-on ("package"))
                             (:file "support" :depends-on ("context"))
                             
                             ;(:file ".lookupasc" :depends-on ("intern" "support"))
                             ;(:file "lookup" :depends-on ("../dasl2/asca/lookupasc"))

			     (:file "proto" :depends-on ("intern" "support"))
                             (:file "lookup" :depends-on ("proto"))

                             ))))

