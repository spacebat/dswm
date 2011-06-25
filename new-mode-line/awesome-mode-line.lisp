(in-package :awesome-mode-line)
(load "cell.lisp")

(require 'clx)

(defvar *display*)
(defvar *display-number* 0)

(defstruct awesome-mode-line
  "A mode-line for stumpwm. It holds everything necessary to display
the mode-line."
  display
  screen
  window
  gcontext
  border
  border-width
  root-cell)

(defun create-awesome-mode-line (&key (parent-window nil) (place 'top) (height 20))
  "Returns an initialized awesome-mode-line"
  (let* ((display *display*)
	 (screen (nth *display-number* (xlib:display-roots display)))
	 (my-parent-window (or parent-window
			       (xlib:screen-root screen)))
	 (foreground-color (xlib:screen-black-pixel screen))
	 (background-color (xlib:screen-white-pixel screen))
	 (text-font (xlib:open-font display "fixed"))
	 (screen-width (xlib:screen-width screen))
	 (screen-height (xlib:screen-height screen))
	 (window
	  (xlib:create-window
	   :parent       my-parent-window
	   :class        :input-output
	   :x            0
	   :y            (if (eq place 'top)
			     0
			     (- screen-height height))
	   :width        screen-width
	   :height       height
	   :border-width 0
	   :save-under   :on
	   :override-redirect :off
	   :event-mask   (xlib:make-event-mask :leave-window
					       :exposure
					       :button-press)))
	 (gcontext
	  (xlib:create-gcontext
	   :drawable   my-parent-window
	   :foreground foreground-color
	   :background background-color
	   :font       text-font))
	 (aml (make-awesome-mode-line
	       :display display
	       :screen screen
	       :gcontext gcontext
	       :window window
	       :root-cell nil)))
         ;; Make the window manager accept the mode-line as dock
	 (xlib:change-property window :_NET_WM_WINDOW_TYPE
			       (list (xlib:intern-atom display :_NET_WM_WINDOW_TYPE_DOCK))
			       :atom  32)
	 ;; Reserve some space for the mode-line
	 ;; TODO: Adjust for 'bottom placement
	 (xlib:change-property window :_NET_WM_STRUT_PARTIAL
			       (list 0 0 screen-height 0 0 0 0 0 0 screen-width 0 0)
			       :cardinal  32)
	 aml))

(defun awesome-mode-line-width (aml)
  (xlib:drawable-width (awesome-mode-line-window aml)))

(defun awesome-mode-line-height (aml)
  (xlib:drawable-height (awesome-mode-line-window aml)))

(defun destroy-awesome-mode-line (aml)
  (xlib:unmap-window (awesome-mode-line-window aml))
  (xlib:free-gcontext (awesome-mode-line-gcontext aml))
  (xlib:destroy-window (awesome-mode-line-window aml)))

(defun draw-and-map-awesome-mode-line (aml)
  (let ((window (awesome-mode-line-window aml)))
    ;; Draw root-cell, which in turn will draw all it's children
    (draw-cell (awesome-mode-line-root-cell aml))
    ;; Map everything
    (xlib:map-window window)
    (xlib:map-subwindows window)))

(defun utf8-to-string (octets)
  "Convert the list of octets to a string."
  #+sbcl (handler-bind
             ((sb-impl::octet-decoding-error #'(lambda (c) (invoke-restart 'use-value "?"))))
           (sb-ext:octets-to-string 
            (coerce octets '(vector (unsigned-byte 8)))
            :external-format :utf-8))
  #+clisp (ext:convert-string-from-bytes (coerce octets '(vector (unsigned-byte 8)))
                                         charset:utf-8)
  #-(or sbcl clisp)
  (map 'string #'code-char octets))

(defun window-name (win)
  "Return the netwm wm name"
  (let ((net-wm-name (xlib:get-property win :_NET_WM_NAME))
	(net-wm-visible-name (xlib:get-property win :_NET_WM_VISIBLE_NAME))
	(wm-name (xlib:get-property win :WM_NAME)))
    (cond
      ((plusp (length net-wm-name))
       (utf8-to-string net-wm-name))
      ((plusp (length net-wm-visible-name))
       (utf8-to-string net-wm-visible-name))
      ((plusp (length wm-name))
       (utf8-to-string wm-name))
      (t "no name"))))

(defun windows-on-desktop (root-window desktop-number)
  "List of windows on desktop desktop-number"
  (let ((windows (mapcar (lambda (window-id)
			   (window-from-id (xlib:window-display root-window) window-id))
			 (net-client-list root-window))))
    ;; remove windows not on desktop
    (remove-if-not (lambda (w)
		     (eql
		      (first (xlib:get-property w :_NET_WM_DESKTOP))
		      desktop-number))
		   windows)))

(defun windows-on-current-desktop (root-window)
  "List of windows on current desktop"
  (windows-on-desktop root-window (first (xlib:get-property root-window :_NET_CURRENT_DESKTOP))))

(defun window-names (root-window)
  "Returns the names of the currently mapped windows."
  (mapcar (lambda (w)
	    (window-name (window-from-id (xlib:window-display root-window) w)))
	  (net-client-list root-window)))

(defun window-from-id (display id)
  "Create a window from a resource id"
  (xlib::make-window :id id :display display))

(defun net-client-list (window)
  "Return a list of window id's with window as parent and in _NET_CLIENT_LIST"
  (xlib:get-property window :_NET_CLIENT_LIST))

(defun time-string ()
  "Returns a string containing the current time"
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (format nil "~2,'0d:~2,'0d" hour minute)))

(defun show-awesome-mode-line ()
  (setf *display* (xlib:open-default-display))
  (let* ((aml (create-awesome-mode-line :place 'top :height 24))
	 (display (awesome-mode-line-display aml))
	 (window (awesome-mode-line-window aml))
	 (gcontext (awesome-mode-line-gcontext aml))
	 ;; Holds everything
	 (root-cell
	  (make-instance 'list-cell
			 :parent-window window
			 :gcontext gcontext
			 :x 0 :y 0
			 :width (xlib:drawable-width window)
			 :height (xlib:drawable-height window)))
	 ;; Holds a logo
	 (logo-cell
	  (make-instance 'image-cell
	 		 :parent-window window
	 		 :gcontext gcontext
	 		 :x 0 :y 0
	 		 :width 31 :height 20
	 		 :image-path "stumpwm.xbm"))
	 ;; Holds the taskbar
	 (taskbar-cell
	  (make-instance 'taskbar-cell
			 :parent-window window
			 :gcontext gcontext
			 :x 35 :y 0
			 :width (- (awesome-mode-line-width aml) (cell-width logo-cell) 7) :height 20)))

    ;; This will hold all the content of the awesome-mode-line
    (setf (awesome-mode-line-root-cell aml) root-cell)
    
    ;; Add stuff to the root-cell
    (add-to-cells (awesome-mode-line-root-cell aml) logo-cell)
    (add-to-cells (awesome-mode-line-root-cell aml) taskbar-cell)

    ;; Event loop
    (unwind-protect
	 (loop
	    (sleep 0.5)
	    (draw-and-map-awesome-mode-line aml)
	    ;; Event handeling
	    (xlib:event-case (display :force-output-p t)
	    		     (:exposure (count)
	    				(draw-and-map-awesome-mode-line aml))
	    		     (:button-press (x y)
					    )
	    		     (otherwise ()
	    				t)))
      ;; Clean up
      (destroy-awesome-mode-line aml)
      (xlib:close-display display))))
