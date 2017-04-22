(defpackage :mersenne
  (:use :cl)
  (:export :mt
	   :mt-gen
	   :make-mt))

(defpackage :snek
  (:use :cl
	:usocket
  :cl-ansi-text
	:mersenne))


