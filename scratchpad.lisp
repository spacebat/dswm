;; Copyright (C) 2010-2011 Alexander aka CosmonauT Vynnyk
;;
;;  This file is part of dswm.
;;
;; dswm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; dswm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Commentary:
;;
;; All group related code resides here
;;
;; Code:

;; TODO: make it good
(in-package :dswm)

(defvar *scratchpad-group* '()
  "Group of the scratchpad." )

(defun scratchpad-init ()
  "Initializing scratchpad support"
  (let ((cg (current-group)))
	 (unless *scratchpad-group*
	   ;; Add the (hidden) scratchpad group and give it the special number 0
	   (setf *scratchpad-group* (add-group (current-screen) "scratchpad")
		 (group-number *scratchpad-group*) 0))
	 (switch-to-group cg))
  ;; TODO: It`s just dirty hack. Replace it to good code!
  t)

(defcommand scratchpad () ()
  (let ((groups (screen-groups (current-screen))))
    (if
	(eq (current-group) *scratchpad-group*)
	(when (> (length groups) 1)
	  (switch-to-group (nth 1 groups)))
      (switch-to-group *scratchpad-group*))))
