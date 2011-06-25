(defpackage #:awesome-mode-line-asd
  (:use :cl :asdf))

(in-package :awesome-mode-line-asd)

(defsystem awesome-mode-line
  :name "awesome-mode-line"
  :version "git"
  :maintainer "Raffael Mancini <raffael.mancini@hcl-club.lu>"
  :author "Raffael Mancini <raffael.mancini@hcl-club.lu>"
  :licence "GNU General Public License"
  :description "A replacement mode-line for stupwm."
  :serial t
  :depends-on (:clx)
  :components ((:file "package")
               (:file "awesome-mode-line")
	       (:file "cell")))
