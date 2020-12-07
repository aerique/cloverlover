;;;; cloverlover.asd

(in-package :asdf)

(defsystem :cloverlover
  :version "0.0.2"
  :description "A library for Pushover's Open Client API"
  :author "Erik Winkels <aerique@xs4all.nl>"
  :serial t
  :components ((:file "credentials")
               (:module "src"
                        ;:serial t
                        :components ((:file "main"))))
  :depends-on (:drakma
               :flexi-streams
               :jsown))
