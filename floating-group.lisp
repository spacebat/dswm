;; Copyright (C) 2003-2008 Shawn Betts
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
;; This file contains primitive data structures and functions used
;; throughout dswm.
;;
;; Code:

(in-package :dswm)

;;; floating window

(defclass float-window (window)
  ((last-width :initform 0 :accessor float-window-last-width)
   (last-height :initform 0 :accessor float-window-last-height)
   (last-x :initform 0 :accessor float-window-last-x)
   (last-y :initform 0 :accessor float-window-last-y))
  )

;; (defvar +default-frame-outline-width+ 1)
(defvar *float-window-title-height* 10)

;; some book keeping functions
(defmethod (setf window-x) :before (val (window float-window))
  (unless (eql (window-x window) val)
    (setf (float-window-last-x window) (window-x window))))

(defmethod (setf window-y) :before (val (window float-window))
  (unless (eql (window-y window) val)
    (setf (float-window-last-y window) (window-y window))))

(defmethod (setf window-width) :before (val (window float-window))
  (unless (eql (window-width window) val)
    (setf (float-window-last-width window) (window-width window))))

(defmethod (setf window-height) :before (val (window float-window))
  (unless (eql (window-height window) val)
    (setf (float-window-last-height window) (window-height window))))

(defun float-window-move-resize (win &key x y width height (border +default-frame-outline-width+))
  ;; x and y position the parent window while width, height resize the
  ;; xwin (meaning the parent will have a larger width).
  (with-slots (xwin parent) win
    (xlib:with-state (parent)
      (xlib:with-state (xwin)
        (when x
          (setf (xlib:drawable-x parent) x
                (window-x win) x))
        (when y
          (setf (xlib:drawable-y parent) y
                (window-y win) y))
        (when width
          (setf (xlib:drawable-width parent) (+ (xlib:drawable-x xwin) width border)
                (xlib:drawable-width xwin) width
                (window-width win) width))
        (when height
          (setf (xlib:drawable-height parent) (+ (xlib:drawable-y xwin) height border)
                (xlib:drawable-height xwin) height
                (window-height win) height))))))

(defmethod update-decoration ((window float-window))
  (let ((group (window-group window)))
    (setf (xlib:window-background (window-parent window))
	  (if (eq (group-current-window group) window)
	      (screen-focus-color (window-screen window))
	    (screen-unfocus-color (window-screen window))))
    (xlib:clear-area (window-parent window))))

(defmethod window-sync ((window float-window) hint)
  (declare (ignore hint))
  )

(defmethod window-head ((window float-window))
  (dolist (head (screen-heads (group-screen (window-group window))))
    (when (and
           (>= (window-x window) (frame-x head))
           (>= (window-y window) (frame-y head))
           (<= (+ (window-x window) (window-width window))
               (+ (frame-x head) (frame-width head)))
           (<= (+ (window-y window) (window-height window))
               (+ (frame-y head) (frame-height head))))
      (return head))))

(defmethod window-visible-p ((win float-window))
  (eql (window-state win) +normal-state+))

(defmethod (setf window-fullscreen) :after (val (window float-window))
  (with-slots (last-x last-y last-width last-height parent) window
    (if val
        (let ((head (window-head window)))
          (with-slots (x y width height) window
            (format t "major on: ~a ~a ~a ~a~%" x y width height))
          (set-window-geometry window :x 0 :y 0)
          (float-window-move-resize window
                                    :x (frame-x head)
                                    :y (frame-y head)
                                    :width (frame-width head)
                                    :height (frame-height head)
                                    :border 0)
          (format t "loot after: ~a ~a ~a ~a~%" last-x last-y last-width last-height))
        (progn
          (format t "fullscreenage: ~a ~a ~a ~a~%" last-x last-y last-width last-height)
          ;; restore the position
          (set-window-geometry window :x +default-frame-outline-width+ :y *float-window-title-height*)
          (float-window-move-resize window
                                    :x last-x
                                    :y last-y
                                    :width last-width
                                    :height last-height)))))

;;; floating group

(defclass float-group (group)
  ((current-window :accessor float-group-current-window))
  )

(defmethod group-startup ((group float-group))
  )

(defmethod group-add-window ((group float-group) window &key &allow-other-keys)
  (change-class window 'float-window)
  (float-window-align window)
  (focus-window window))

(defun &float-focus-next (group)
  (if (group-windows group)
      (focus-window (first (group-windows group)))
      (no-focus group nil)))

(defmethod group-delete-window ((group float-group) window)
  (declare (ignore window))
  (&float-focus-next group))

(defmethod group-wake-up ((group float-group))
  (&float-focus-next group))

(defmethod group-suspend ((group float-group))
  )

(defmethod group-current-window ((group float-group))
  (screen-focus (group-screen group)))

(defmethod group-current-head ((group float-group))
  (first (screen-heads (group-screen group))))

(defun float-window-align (window)
  (with-slots (parent xwin width height) window
    (set-window-geometry window :x +default-frame-outline-width+ :y *float-window-title-height*)
    (xlib:with-state (parent)
      (setf (xlib:drawable-width parent) (+ width (* 2 +default-frame-outline-width+))
            (xlib:drawable-height parent) (+ height *float-window-title-height* +default-frame-outline-width+)
            (xlib:window-background parent) (xlib:alloc-color (xlib:screen-default-colormap (screen-number (window-screen window)))
                                                              +default-window-background-color+)))
    (xlib:clear-area (window-parent window))))
  
(defmethod group-resize-request ((group float-group) window width height)
  (float-window-move-resize window :width width :height height))

(defmethod group-move-request ((group float-group) window x y relative-to)
  (declare (ignore relative-to))
  (float-window-move-resize window :x x :y y))

(defmethod group-raise-request ((group float-group) window type)
  (declare (ignore type))
  (focus-window window))

(defmethod group-lost-focus ((group float-group))
  (&float-focus-next group))

(defmethod group-indicate-focus ((group float-group))
  )

(defmethod group-focus-window ((group float-group) window)
  (focus-window window))

(defmethod group-root-exposure ((group float-group))
  )

(defmethod group-add-head ((group float-group) head)
  (declare (ignore head)))

(defmethod group-remove-head ((group float-group) head)
  (declare (ignore head)))

(defmethod group-resize-head ((group float-group) oh nh)
  (declare (ignore oh nh)))

(defmethod group-sync-all-heads ((group float-group))
  )

(defmethod group-sync-head ((group float-group) head)
  (declare (ignore head))
  )

(defmethod group-button-press ((group float-group) x y (window float-window))
  (let ((screen (group-screen group)))
    (cond
      ((or (< x (xlib:drawable-x (window-xwin window)))
           (> x (+ (xlib:drawable-width (window-xwin window))
                   (xlib:drawable-x (window-xwin window))))
           (< y (xlib:drawable-y (window-xwin window)))
           (> y (+ (xlib:drawable-height (window-xwin window))
                   (xlib:drawable-y (window-xwin window)))))
       (multiple-value-bind (relx rely same-screen-p child state-mask)
           (xlib:query-pointer (window-parent window))
         (declare (ignore same-screen-p child))
         (let ((initial-width (xlib:drawable-width (slot-value window 'parent)))
               (initial-height (xlib:drawable-height (slot-value window 'parent))))
           (labels ((move-window-event-handler (&rest event-slots &key event-key &allow-other-keys)
                      (case event-key
                        (:button-release
                         :done)
                        (:motion-notify
                         (with-slots (parent) window
                           (xlib:with-state (parent)
                             ;; Either move or resize the window
                             (cond
                               ((find :button-1 (xlib:make-state-keys state-mask))
                                (setf (xlib:drawable-x parent) (- (getf event-slots :x) relx)
                                      (xlib:drawable-y parent) (- (getf event-slots :y) rely)))
                               ((find :button-3 (xlib:make-state-keys state-mask))
                                (let ((w (+ initial-width
                                            (- (getf event-slots :x)
                                               relx
                                               (xlib:drawable-x parent))))
                                      (h (+ initial-height
                                            (- (getf event-slots :y)
                                               rely
                                               (xlib:drawable-y parent)
                                               *float-window-title-height*))))
                                  ;; Don't let the window become too small
                                  (float-window-move-resize window
                                                            :width (max w *min-frame-width*)
                                                            :height (max h *min-frame-height*)))))))
                         t)
                        ;; We need to eat these events or they'll ALL
                        ;; come blasting in later. Also things start
                        ;; lagging hard if we don't (on clisp anyway).
                        (:configure-notify t)
                        (:exposure t)
                        (t
                         nil))))
             (xlib:grab-pointer (screen-root screen) '(:button-release :pointer-motion))
             (unwind-protect
                  ;; Wait until the mouse button is released
                  (loop for ev = (xlib:process-event *display*
                                                     :handler #'move-window-event-handler
                                                     :timeout nil
                                                     :discard-p t)
                     until (eq ev :done))
               (ungrab-pointer))
	     (update-configuration window)
             ;; don't forget to update the cache
             (setf (window-x window) (xlib:drawable-x (window-parent window))
                   (window-y window) (xlib:drawable-y (window-parent window)))))))
      (t
       (unless (eq *mouse-focus-policy* :sloppy)
         (focus-window window))))))

(defmethod group-button-press ((group float-group) x y where)
  (declare (ignore x y where))
  )

(defcommand gnew-float (name) ((:string "Input group name: "))
"Create a floating window group with the specified name and switch to it."
  (add-group (current-screen) name :type 'float-group))

(defcommand gnewbg-float (name) ((:string "Input group name: "))
"Create a floating window group with the specified name, but do not switch to it."
  (add-group (current-screen) name :background t :type 'float-group))

(defcommand gnew-float-with-window (name) ((:string "Input group name: "))
  "Move current window to new float group"
  (let ((group (gnewbg-float name)))
    (gmove group)
    (gselect group)))

(defcommand gnew-float-with-marked (name) ((:string "Input group name: "))
  "Move marked windows to new float group"
  (let ((group (gnewbg-float name)))
    (gmove-marked group)
    (gselect group)))

(defcommand grun-new-float (command) ((:shell "Input command to run program: "))
  "Run shell command in new float group with same name with command"
  (check-type command string)
  (gnew-float command)
  (run-shell-command command))
