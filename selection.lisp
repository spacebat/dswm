;; Copyright (C) 2003-2008 Shawn Betts
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
;; Handle the X selection.
;;
;; Code:

(in-package #:dswm)

(export '(get-x-selection
          set-x-selection))

(defun export-selection ()
  (let* ((screen (current-screen))
         (selwin (screen-focus-window (current-screen)))
         (root (screen-root screen)))
    (xlib:set-selection-owner *display* :primary selwin)
    (unless (xlib:window-equal (xlib:selection-owner *display* :primary) selwin)
      (error "Can't set selection owner"))
    ;; also set the cut buffer for completeness
    (xlib:change-property root :cut-buffer0 *x-selection* :string 8 :transform #'xlib:char->card8
                          :mode :replace)))

(defun set-x-selection (text)
  "Set the X11 selection string to @var{string}."
  (setf *x-selection* text)
  (export-selection))

(defun send-selection (requestor property selection target time)
  (dformat 1 "send-selection ~s ~s ~s ~s ~s~%" requestor property selection target time)
  (cond
    ;; they're requesting what targets are available
    ((eq target :targets)
     (xlib:change-property requestor property (list :targets :string) target 8 :mode :replace))
    ;; send them a string
    ((find target '(:string ))
     (xlib:change-property requestor property *x-selection* :string 8 :mode :replace :transform #'xlib:char->card8))
    ;; we don't know how to handle anything else
    (t
     (setf property nil)))
  (xlib:send-event requestor :selection-notify nil
                   :display *display*
                   :window requestor
                   :selection selection
                   :property property
                   :target target
                   :time time)
  (xlib:display-finish-output *display*))

(defun get-x-selection (&optional timeout)
  "Return the x selection no matter what client own it."
  (labels ((wait-for-selection (&rest event-slots &key display event-key &allow-other-keys)
             (declare (ignore display))
             (when (eq event-key :selection-notify)
               (destructuring-bind (&key window property &allow-other-keys) event-slots
                 (if property
                     (xlib:get-property window property :type :string :result-type 'string :transform #'xlib:card8->char :delete-p t)
                     "")))))
    (if *x-selection*
        *x-selection*
        (progn
          (xlib:convert-selection :primary :string (screen-input-window (current-screen)) :dswm-selection)
          ;; Note: this may spend longer than timeout in this loop but it will eventually return.
          (let ((time (get-internal-real-time)))
            (loop for ret = (xlib:process-event *display* :handler #'wait-for-selection :timeout timeout :discard-p nil)
                  when (or ret
                           (> (/ (- time (get-internal-real-time)) internal-time-units-per-second)
                              timeout))
                  ;; make sure we return a string
                  return (or ret "")))))))

;;; Commands

(defcommand putsel (string) ((:rest "Enter text to put it: "))
  "Stuff the string @var{string} into the X selection."
  (set-x-selection string))

;; FIXME: this function is basically useless atm.
(defcommand getsel () ()
  "Echo the X selection."
  (message "~a" (get-x-selection)))

(defcommand copy-last-message () ()
  "Copy the last message displayed into the X selection"
  (when (screen-last-msg (current-screen))
    (set-x-selection (uncolorify (format nil "~{~a~^~%~}" (car (screen-last-msg (current-screen))))))))
