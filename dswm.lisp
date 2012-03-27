;; Copyright (C) 2003-2008 Shawn Betts
;; Copyright (C) 2010-2012 Alexander aka CosmonauT Vynnyk
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
;; Code:

(in-package :dswm)

(export '(cancel-timer
	  run-with-timer
	  dswm
	  timer-p))


;;; Main

(defmacro startup-only (&rest body)
  "function, which runs code, just when DSWM starting.
Not when command loadrc runs, reinit runs after crash etc.
Useful for run programs on startup etc."
  `(defun run-code-just-when-start ()
     (progn
       ,@body)))

(defun load-rc-file (&optional (catch-errors t) (reload nil))
  "Load the user's .dswm file or the system wide one if that
doesn't exist. Returns a values list: whether the file loaded (t if no
rc files exist), the error if it didn't, and the rc file that was
loaded. When CATCH-ERRORS is nil, errors are left to be handled further up. "
  (let* ((user-rc (file-exists-p (data-dir-file "init" "lisp")))
	 (user-initrc (file-exists-p (merge-pathnames (user-homedir-pathname)
						   #p".dswm")))
         (etc-rc (file-exists-p #p"/etc/dss/dswm/dswm.lisp"))
         (rc (or user-initrc user-rc)))
    (progn
      (startup-only)
      (if etc-rc
	  (if catch-errors
	      (handler-case (load etc-rc)
			    (error (c) (values nil (format nil "~a" c) etc-rc))
			    (:no-error (&rest args) (declare (ignore args)) (values t nil etc-rc)))
	    (progn
	      (load etc-rc)
	      (values t nil etc-rc)))
	(values t nil t))
      (if rc
	  ;; TODO: Should we compile the file before we load it?
	  (if catch-errors
	      (handler-case (load rc)
			    (error (c) (values nil (format nil "~a" c) rc))
			    (:no-error (&rest args) (declare (ignore args)) (values t nil rc)))
	    (progn
	      (load rc)
	      (values t nil rc)))
	(values t nil nil))
      (when (not reload)
	(run-code-just-when-start)))))

(defun error-handler (display error-key &rest key-vals &key asynchronous &allow-other-keys)
  "Handle X errors"
  (cond 
    ;; ignore asynchronous window errors
    ((and asynchronous
          (find error-key '(xlib:window-error xlib:drawable-error xlib:match-error)))
     (dformat 4 "Ignoring error: ~s~%" error-key))
    ((eq error-key 'xlib:access-error)
     (write-line "Another window manager is running.")
     (throw :top-level :quit))
     ;; all other asynchronous errors are printed.
     (asynchronous
      (message "Caught Asynchronous X Error: ~s ~s" error-key key-vals))
     (t
      (apply 'error error-key :display display :error-key error-key key-vals))))

;;; Timers

(defvar *timer-list* nil
  "List of active timers.")

(defstruct timer
  time repeat function args)

(defun run-with-timer (secs repeat function &rest args)
  "Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if repeat is non-nil.
SECS and REPEAT may be reals.
The action is to call FUNCTION with arguments ARGS."
  (check-type secs (real 0 *))
  (check-type repeat (or null (real 0 *)))
  (check-type function (or function symbol))
  (let ((timer (make-timer
                :repeat repeat
                :function function
                :args args)))
    (schedule-timer timer secs)
    (setf *timer-list* (sort-timers (cons timer *timer-list*)))
    timer))

(defun cancel-timer (timer)
  "Remove TIMER from the list of active timers."
  (check-type timer timer)
  (setf *timer-list* (remove timer *timer-list*)))

(defun schedule-timer (timer when)
  (setf (timer-time timer) (+ (get-internal-real-time)
                              (* when internal-time-units-per-second))))

(defun sort-timers (timers)
  "Return a new list of timers sorted by time to time out."
  (sort (copy-list timers)
        (lambda (a b)
          (< (timer-time a) (timer-time b)))))

(defun run-expired-timers (timers)
  "Return a new list of valid timers and run the timer functions
of those expired."
  (let ((now (get-internal-real-time)))
    (sort-timers (loop for i in timers
                       with keepers = nil do
                       (if (< (timer-time i) now)
                           (progn
                             (apply (timer-function i) (timer-args i))
                             (when (timer-repeat i)
                               (schedule-timer i (timer-repeat i))
                               (push i keepers)))
                           (push i keepers))
                       finally (return keepers)))))

(defun get-next-timeout (timers)
  "Return the number of seconds until the next timeout or nil if there are no timers."
  (when timers
    (max (/ (- (timer-time (car timers)) (get-internal-real-time))
            internal-time-units-per-second)
         0)))

(defun perform-top-level-error-action (c)
  (ecase *top-level-error-action*
    (:message
     (let ((s (format nil "~&Caught '~a' at the top level. Please report this." c)))
       (write-line s)
       (print-backtrace)
       (message "^1*^B~a" s)))
    (:break (invoke-debugger c))
    (:abort
     (throw :top-level (list c (backtrace-string))))))

(defun dswm-internal-loop ()
  "The internal loop that waits for events and handles them."
  (loop
     (run-hook *internal-loop-hook*)
     (handler-bind
         ((xlib:lookup-error (lambda (c)
                               (if (lookup-error-recoverable-p)
                                   (recover-from-lookup-error)
                                   (error c))))
          (warning #'muffle-warning)
          ((or serious-condition error)
           (lambda (c)
             (run-hook *top-level-error-hook*)
             (perform-top-level-error-action c)))
          (t
           (lambda (c)
             ;; some other wacko condition was raised so first try
             ;; what we can to keep going.
             (cond ((find-restart 'muffle-warning)
                    (muffle-warning))
                   ((find-restart 'continue)
                    (continue)))
             ;; and if that fails treat it like a top level error.
             (perform-top-level-error-action c))))
       ;; Note: process-event appears to hang for an unknown
       ;; reason. This is why it is passed a timeout in hopes that
       ;; this will keep it from hanging.
       (let ((timeout (get-next-timeout *timer-list*)))
         (dformat 10 "timeout: ~a~%" timeout)
         (if timeout
             (let* ((nevents (xlib:event-listen *display* (ceiling timeout))))
               (setf *timer-list* (run-expired-timers *timer-list*))
               (when nevents
                 (xlib:process-event *display* :handler #'handle-event)))
             ;; Otherwise, simply wait for an event
             (xlib:process-event *display* :handler #'handle-event))
         ;; flush any pending output. You'd think process-event would, but
         ;; it seems not.
         (xlib:display-finish-output *display*)
         ;;(dformat 10 "toplevel focus: ~a~%" (multiple-value-list (xlib:input-focus *display*)))
         ))))

(defun parse-display-string (display)
  "Parse an X11 DISPLAY string and return the host and display from it."
  (ppcre:register-groups-bind (protocol host ('parse-integer display screen))
			      ("^(?:(.*?)/)?(.*?)?:(\\d+)(?:\\.(\\d+))?" display :sharedp t)
    (values 
     ;; clx doesn't like (vector character *)
     (coerce (or host "")
	     '(simple-array character (*)))
     display screen
     (cond (protocol
	    (intern1 protocol :keyword))
	   ((or (string= host "")
		(string-equal host "unix"))
	    :local)
	   (t :internet)))))

(defun engage-on-startup ()
  "Runs many intresting things on startup (dswm-spacific)"
  (progn
    ;; Switch on mode-line
    (if (not (head-mode-line (current-head)))
	(toggle-mode-line (current-screen) (current-head)))
    ;; FIXME: fix tip-of-the-day and (first-start)
    (first-start)
    ;; (if (and *show-tip-of-the-day-p*
    ;; 	     (file-exists-p (data-dir-file "started" "p")))
    ;;     (tip-of-the-day))
    ))

(defun dswm-internal (display-str)
  (multiple-value-bind (host display screen protocol) (parse-display-string display-str)
    (declare (ignore screen))
    (setf *display* (xlib:open-display host :display display :protocol protocol)
          (xlib:display-error-handler *display*) 'error-handler)
    (with-simple-restart (quit-dswm "Quit DSWM")
      ;; In the event of an error, we always need to close the display
      (unwind-protect
           (progn
             (let ((*initializing* t))
               ;; we need to do this first because init-screen grabs keys
               (update-modifier-map)
               ;; Initialize all the screens
               (setf *screen-list* (loop for i in (xlib:display-roots *display*)
                                      for n from 0
                                      collect (init-screen i n host)))
               (xlib:display-finish-output *display*)
               ;; Load startup components
               (let ((*package* (find-package *default-package*)))
                 (multiple-value-bind
		     (success err rc)
		     (scratchpad-init)
		   (load-rc-file)
		   (run-with-timer
		    *mode-line-timeout* *mode-line-timeout* 'toggle-mode-line-blink)
		   ;; Switch on mode-line
		   (if (not (head-mode-line (current-head)))
		       (toggle-mode-line (current-screen) (current-head)))
                   (if success
		       (engage-on-startup)
                       (message "^B^1*Error loading ^b~A^B: ^n~A" rc err))))
               (when *last-unhandled-error*
                 (message-no-timeout "^B^1*DSWM Crashed With An Unhandled Error!~%Copy the error to the clipboard with the 'copy-unhandled-error' command.~%^b~a^B^n~%~%~a"
                          (first *last-unhandled-error*) (second *last-unhandled-error*)))
               (mapc 'process-existing-windows *screen-list*)
               ;; We need to setup each screen with its current window. Go
               ;; through them in reverse so the first screen's frame ends up
               ;; with focus.
               (dolist (s (reverse *screen-list*))
                 ;; map the current group's windows
                 (mapc 'unhide-window (reverse (group-windows (screen-current-group s))))
                 ;; update groups
                 (dolist (g (reverse (screen-groups s)))
                   (dformat 3 "Group windows: ~S~%" (group-windows g))
                   (group-startup g))
                 ;; switch to the (old) current group.
                 (let ((netwm-id (first (xlib:get-property (screen-root s) :_NET_CURRENT_DESKTOP))))
                   (when (and netwm-id (< netwm-id (length (screen-groups s))))
                     (switch-to-group (elt (sort-groups s) netwm-id))))
                 (redraw-current-message (current-screen))))
             ;; Let's manage.
             (let ((*package* (find-package *default-package*)))
               (run-hook *start-hook*)
               (dswm-internal-loop)))
        (xlib:close-display *display*))))
  ;; what should the top level loop do?
  :quit)

;; Usage: (dswm)
(defun dswm (&optional (display-str (or (getenv "DISPLAY") ":0")))
  "Start the deep space window manager."
  (loop
     (let ((ret (catch :top-level
                  (dswm-internal display-str))))
       (setf *last-unhandled-error* nil)
       (cond ((and (consp ret)
                   (typep (first ret) 'condition))
              (format t "~&Caught '~a' at the top level. Please report this.~%~a" 
                      (first ret) (second ret))
              (setf *last-unhandled-error* ret))
             ;; we need to jump out of the event loop in order to hup
             ;; the process because otherwise we get errors.
             ((eq ret :hup-process)
                  (apply 'execv (first (argv)) (argv)))
             ((eq ret :restart))
             (t 
              ;; the number is the unix return code
              (return-from dswm 0))))))
