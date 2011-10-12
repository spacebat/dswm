;; Copyright (C) 2007-2008 Jonathan Liles, Shawn Betts
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

;; Code:

(in-package #:dswm)

(export '(ddump
          ddump-current
          ddump-screens
          dump-desktop-to-file
          dump-group-to-file
          dump-screen-to-file
          fdump
          fdump-current
          fdump-height
          fdump-number
          fdump-width
          fdump-windows
          fdump-x
          fdump-y
	  fgdump
	  fgdump-name
	  fgdump-number
          gdump
          gdump-current
          gdump-name
          gdump-number
          gdump-tree
          place-existing-windows
          restore
	  restore-all-rules
	  restore-desktop-snapshot
	  save-all-rules
	  save-frame+group-rules
          sdump
          sdump-current
          sdump-groups
          sdump-number
	  snapshot-desktop))

(defstruct fdump
  number x y width height windows current)

;; group dump
(defstruct gdump
  number name tree current)

(defstruct fgdump
  number name)

;; screen dump
(defstruct sdump
  number groups current)

;; desktop dump
(defstruct ddump
  screens current)

(defun dump-structure (structure to-fs file &optional backup-p)
  "Dump some code, values etc to file or just to output"
  (if to-fs
      (progn
	(when (and backup-p (file-exists-p file))
	  (copy-file
	   file
	   (merge-pathnames
	    (make-pathname :type (concat (pathname-type file) "~")) file)
	   :overwrite t))
	(with-open-file (fp file :direction :output :if-exists :supersede)
			(with-standard-io-syntax
			 (let ((*package* (find-package :dswm))
			       (*print-pretty* t))
			   (prin1 structure fp))))
	structure)
    structure))

(defun dump-group (group &key (window-dump-fn 'window-id)
			 (to-fs nil)
			 (file
			  (data-dir-file
			   (concat "group-" (prin1-to-string (group-name group))) "rules"))
			 (backup-p nil))
  (labels ((dump (f)
		 (make-fdump
		  :windows (mapcar window-dump-fn (frame-windows group f))
		  :current (and (frame-window f)
				(funcall window-dump-fn (frame-window f)))
		  :number (frame-number f)
		  :x (frame-x f)
		  :y (frame-y f)
		  :width (frame-width f)
		  :height (frame-height f)))
           (copy (tree)
		 (cond ((null tree) tree)
		       ((typep tree 'frame)
			(dump tree))
		       (t
			(mapcar #'copy tree)))))
    (let ((group-dump
	   (cond ((eq (type-of group) 'tile-group)
		  (make-gdump
		   ;; we only use the name and number for screen and desktop restores
		   :number (group-number group)
		   :name (group-name group)
		   :tree (copy (tile-group-frame-tree group))
		   :current (frame-number (tile-group-current-frame group))))
		 ((eq (type-of group) 'float-group)
		  (make-fgdump
		   :number (group-number group)
		   :name (group-name group))))))
      (dump-structure group-dump to-fs file backup-p))))

(defun dump-screen (screen &key
			   (to-fs nil)
			   (file (data-dir-file
				  (concat "screen-" (prin1-to-string
						     (screen-id screen))) "rules"))
			   (backup-p nil))
  "Makes dump of given screen"
  (let
      ((screen-dump
	(make-sdump :number (screen-id screen)
		    :current (group-number (screen-current-group screen))
		    :groups (mapcar 'dump-group (sort-groups screen)))))
    (dump-structure screen-dump to-fs file backup-p)))

(defun dump-desktop (&key
		     (to-fs nil)
		     (file (data-dir-file "desktop" "rules"))
		     (backup-p nil))
  "Makes full dump of desktop"
  (let ((desktop-dump
	 (make-ddump :screens (mapcar 'dump-screen *screen-list*)
		     :current (screen-id (current-screen)))))
    (dump-structure desktop-dump to-fs file backup-p)))


;;;

(defun read-dump-from-file (file)
  (with-open-file (fp file :direction :input)
    (with-standard-io-syntax
      (let ((*package* (find-package :dswm)))
        (read fp)))))

(defun restore-group (group gdump &optional auto-populate (window-dump-fn 'window-id))
  ;; TODO: make from-fs option, like in dump-group
  (let ((windows (group-windows group)))
    (labels ((give-frame-a-window (f)
               (unless (frame-window f)
                 (setf (frame-window f) (find f windows :key 'window-frame))))
             (restore (fd)
               (let ((f (make-frame
                         :number (fdump-number fd)
                         :x (fdump-x fd)
                         :y (fdump-y fd)
                         :width (fdump-width fd)
                         :height (fdump-height fd))))
                 ;; import matching windows
                 (if auto-populate
                     (choose-new-frame-window f group)
                     (progn
                       (dolist (w windows)
                         (when (equal (fdump-current fd) (funcall window-dump-fn w))
                           (setf (frame-window f) w))
                         (when (find (funcall window-dump-fn w) (fdump-windows fd) :test 'equal)
                           (setf (window-frame w) f)))))
                 (when (fdump-current fd)
                   (give-frame-a-window f))
                 f))
             (copy (tree)
               (cond ((null tree) tree)
                     ((typep tree 'fdump)
                      (restore tree))
                     (t
                      (mapcar #'copy tree))))
	     (migrate (group gdump)
		      (progn
			(setf (group-number (current-group)) (find-free-hidden-group-number (current-screen)))
			(let ((old-group (current-group))
			      (new-group
			       (cond ((eq (type-of gdump) 'fgdump)
				      (add-group (current-screen)
						 (concat (group-name group "~"))
						 :type 'float-group
						 :background t))
				     ((eq (type-of gdump) 'gdump)
				      (add-group (current-screen)
						 (concat (group-name group "~"))
						 :type 'tile-group
						 :background t))
				     (t
				      (error "Incorrect group dump type")))))
			  (progn
			    (kill-group (current-group) new-group)
			    (switch-to-group new-group)
			    (setf (group-name (current-group)) (group-name old-group)))
			  (restore-group (current-group) gdump)
			  ))))
      (cond
       ((and (eq (type-of group) 'tile-group)
	     (eq (type-of gdump) 'gdump))
	;; clear references to old frames
	(dolist (w windows)
	  (setf (window-frame w) nil))
	(setf (tile-group-frame-tree group) (copy (gdump-tree gdump))
	      (tile-group-current-frame group) (find (gdump-current gdump) (group-frames group) :key 'frame-number))
	;; give any windows still not in a frame a frame
	(dolist (w windows)
	  (unless (window-frame w)
	    (setf (window-frame w) (tile-group-current-frame group))))
	;; FIXME: if the current window was blank in the dump, this does not honour that.
	(give-frame-a-window (tile-group-current-frame group))
	;; raise the curtains
	(dolist (w windows)
	  (if (eq (frame-window (window-frame w)) w)
	      (unhide-window w)
	    (hide-window w)))
	(sync-all-frame-windows group)
	(focus-frame group (tile-group-current-frame group)))
       ;; If group is float and dump is float
       ((and (eq (type-of group) 'float-group)
	     (eq (type-of gdump) 'fgdump))
	(setf (group-name group) (fgdump-name gdump)))
       ;; If group is float and dump is tile or group is tile and dump is float
       (t
       	(migrate group gdump))
       ))))

(defun restore-screen (screen sdump)
  "Restore all frames in all groups of given screen. Create groups if
 they don't already exist."
  (dolist (gdump (sdump-groups sdump))
    (cond ((eq (type-of gdump) 'gdump)
	   (format t "~a~%" (gdump-name gdump))
	   (restore-group
	    (or
	     (find-group screen (gdump-name gdump))
	     ;; FIXME: if the group doesn't exist then
	     ;; windows won't be migrated from existing
	     ;; groups
	     (add-group screen (gdump-name gdump)))
	    gdump))
	  ((eq (type-of gdump) 'fgdump)
	   (format t "~a~%" (fgdump-name gdump))
	   (format t "~a"
		   (restore-group
		    (or
		     (find-group screen (fgdump-name gdump))
		     (add-group screen (fgdump-name gdump) :type 'float-group))
		    gdump))))))

(defun restore-desktop (ddump)
  "Restore all frames, all groups, and all screens."
  (dolist (sdump (ddump-screens ddump))
    (let ((screen (find (sdump-number sdump) *screen-list*
                        :key 'screen-id :test '=)))
      (when screen
        (restore-screen screen sdump)))))

(defun restore-all ()
  "Restore all rules. Useful at startup"
  (progn
    (when (file-exists-p (data-dir-file "desktop" "rules"))
      (restore-from-file
       (data-dir-file "desktop" "rules")))
    (setf *window-placement-rules*
	  (read-dump-from-file
	   (data-dir-file "window-placement" "rules")))
    ;; Add function for restore all programs, running in last session
    ))

(defcommand restore-from-file (file) ((:rest "Restore From File: "))
  "Restores screen, groups, or frames from named file, depending on file's contents."
  (let ((dump (read-dump-from-file file)))
    (typecase dump
      ((or gdump fgdump)
       (restore-group (current-group) dump)
       (message "Group restored."))
      (sdump
       (restore-screen (current-screen) dump)
       (message "Screen restored."))
      (ddump
       (restore-desktop dump)
       (message "Desktop restored."))
      (t
       (message "Don't know how to restore ~a" dump)))))

;; (defcommand-alias restore restore-from-file)

(defcommand place-existing-windows () ()
  "Re-arrange existing windows according to placement rules."
  (sync-window-placement))

(defcommand dump-group-to-file (file) ((:rest "Dump To File: "))
  "Dumps the frames of the current group of the current screen to the
named file."
  (eval-with-message :body
		     (dump-group (current-group) :to-fs t :file file)
		     :message-if-done "Group dumped"
		     :message-if-false "Cannot dump group"))

(defcommand remember-group () ()
  "Dumps the frames of the current group of the current screen to the
default dump file."
  (dump-group-to-file *desktop-dump-file*))

(defcommand dump-screen-to-file (file) ((:rest "Dump To File: "))
  "Dumps the frames of all groups of the current screen to the named
file"
  (eval-with-message :body
		     (dump-screen (current-screen) :to-fs t :file file)
		     :message-if-done "Screen dumped"
		     :message-if-false "Can't dump screen"))

(defcommand remember-screen () ()
  "Dumps the frames of all groups of the current screen to the default
dump file"
  (dump-screen-to-file *desktop-dump-file*))

(defcommand dump-desktop-to-file (file) ((:rest "Dump To File: "))
  "Dumps the frames of all groups of all screens to the named file"
  (eval-with-message :body
		     (dump-desktop :to-fs t :file file)
		     :message-if-done "Desktop dumped"
		     :message-if-false "Can't dump desktop"))

(defcommand remember-desktop () ()
  "Dumps the frames of all groups of all screens to the default dump file"
  (dump-desktop-to-file *desktop-dump-file*))

;; TODO: defcommand forgot-desktop () ()
  
(defcommand snapshot-desktop () ()
  "Make rules of all existing windows, bind it to groups and frames,
where they located now and dump all groups frames and window placement
rules to frame-froup-placement.rules and window-placement.rules in
data dir"
  (eval-with-message :body
		     (progn
		       (remember-all-windows '("y") '("n"))
		       (dump-desktop :to-fs t))
		     :message-if-done "Snapshot created"
		     :message-if-false "Can't create snapshot"))

(defcommand restore-desktop-snapshot () ()
  "Restores frame and group placement rules (like numbers, names,
splitting rules etc) and window placement rules of all groups and
frames of the current desktop from frame-froup-placement.rules and
window-placement.rules file in data dir"
  (eval-with-message :body
		     (restore-all)
		     :message-if-done "Snapshot restored"
		     :message-if-false "Can't restore snapshot"))