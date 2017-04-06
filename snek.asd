(in-package #:asdf-user)

(asdf:defsystem #:snek
  :serial t
  :description "Snek"
  :author "Oblivia Simplex <oblivia@paranoici.org>"
  :depends-on (#:usocket)
  :components ((:file "package")
	       (:file "mersenne")
	       (:file "snek")))
