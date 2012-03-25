;; Copyright (C) 2003-2008 Shawn Betts
;; Copyright (C) 2010-2012 Alexender aka CosmonauT Vynnyk
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
;; workarounds for bugs in clx
;;
;; Code:

(in-package :xlib)

;;; SBCL workaround for a clx caching bug. This is taken from portable-clx's display.lisp.

;;; NOTE! The latest clx in Rhodes' repository has fixed this in a far
;;; better way by only caching XIDs created by the client.

;; Define functions to find the CLX data types given a display and resource-id
;; If the data type is being cached, look there first.
#+sbcl
(macrolet ((generate-lookup-functions (useless-name &body types)
	    `(within-definition (,useless-name generate-lookup-functions)
	       ,@(mapcar
		   #'(lambda (type)
		       `(defun ,(xintern 'lookup- type)
			       (display id)
			  (declare (type display display)
				   (type resource-id id))
			  (declare (clx-values ,type))
			  ,(if (member type +clx-cached-types+)
			       `(let ((,type (lookup-resource-id display id)))
				  (cond ((null ,type) ;; Not found, create and save it.
					 (setq ,type (,(xintern 'make- type)
						      :display display :id id))
					 (save-id display id ,type))
					;; Found.  Check the type
                                        ((type? ,type ',type) ,type)
                                        (t 
                                         (restart-case
                                             (x-error 'lookup-error
                                                      :id id
                                                      :display display
                                                      :type ',type
                                                      :object ,type)
                                           (:one ()
                                             :report "Invalidate this cache entry"
                                             (save-id display id (,(xintern 'make- type) :display display :id id)))
                                           (:all ()
                                             :report "Invalidate all display cache"
                                             (clrhash (display-resource-id-map display))
                                             (save-id display id (,(xintern 'make- type) :display display :id id)))))))
			       ;; Not being cached.  Create a new one each time.
			       `(,(xintern 'make- type)
				 :display display :id id))))
		   types))))
  (generate-lookup-functions ignore
    drawable
    window
    pixmap
    gcontext
    cursor
    colormap
    font))

;;; Both clisp and SBCL can't handle incompliant (and in clisp's case,
;;; even compliant) wm-class strings. See test-wm-class in test-wm.lisp.

#+sbcl
(defun get-wm-class (window)
  (declare (type window window))
  (declare (clx-values (or null name-string) (or null class-string)))
  (let ((value (get-property window :WM_CLASS :type :STRING :result-type '(vector card8))))
    (declare (type (or null (vector card8)) value))
    (when value
      ;; Buggy clients may not comply with the format, so deal with
      ;; the unexpected.
      (let* ((first-zero (position 0 (the (vector card8) value)))
             (second-zero (and first-zero
                               (position 0 (the (vector card8) value) :start (1+ first-zero))))
	     (name (subseq (the (vector card8) value) 0 first-zero))
	     (class (and first-zero
                         (subseq (the (vector card8) value) (1+ first-zero) second-zero))))
	(values (and (plusp (length name)) (map 'string #'card8->char name))
		(and (plusp (length class)) (map 'string #'card8->char class)))))))

#+clisp
(defun get-wm-class (window)
  (let ((value (get-property window :WM_CLASS :type :STRING :result-type 'string :transform #'card8->char)))
    (when value
      ;; Buggy clients may not comply with the format, so deal with
      ;; the unexpected.
      (let* ((first-zero (position (load-time-value (card8->char 0)) (the string value)))
             (second-zero (and first-zero
                               (position (load-time-value (card8->char 0)) (the string value) :start (1+ first-zero))))
             (name (subseq (the string value) 0 first-zero))
             (class (and first-zero
                         (subseq (the string value) (1+ first-zero) second-zero))))
        (values (and (plusp (length name)) name)
                (and (plusp (length class)) class))))))

#+clisp
(when (fboundp '%gcontext-key->mask)
(defmacro WITH-GCONTEXT ((gcontext &rest options) &body body)
  (let ((saved (gensym)) (gcon (gensym)) (g0 (gensym)) (g1 (gensym))
        (comps 0)
        (setf-forms nil)
        dashes? clip-mask?)
    (do ((q options (cddr q)))
        ((null q))
      (cond ((eq (car q) :dashes)    (setf dashes? t))
            ((eq (car q) :clip-mask) (setf clip-mask? t)))
      (setf comps      (logior comps (%gcontext-key->mask (car q)))
            setf-forms (nconc setf-forms
                              (list (list (find-symbol (ext:string-concat "GCONTEXT-" (symbol-name (car q))) :xlib)
                                          gcon)
                                    (cadr q)))))
    `(LET* ((,gcon ,gcontext)
            (,saved (%SAVE-GCONTEXT-COMPONENTS ,gcon ,comps))
            ,@(if dashes?    (list `(,g0 (GCONTEXT-DASHES    ,gcon))))
            ,@(if clip-mask? (list `(,g1 (GCONTEXT-CLIP-MASK ,gcon)))))
       (UNWIND-PROTECT
            (PROGN
              (SETF ,@setf-forms)
              ,@body)
         (PROGN
           (%RESTORE-GCONTEXT-COMPONENTS ,gcon ,saved)
           ,@(if dashes?    (list `(SETF (GCONTEXT-DASHES ,gcon) ,g0)))
           ,@(if clip-mask? (list `(SETF (GCONTEXT-CLIP-MASK ,gcon) ,g1)))))))))
