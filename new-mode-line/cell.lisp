(in-package :awesome-mode-line)

(require 'clx)

(defvar *display*)

;;;
;;; Cell
;;;

(defclass cell ()
  ((x
    :initarg :x
    :accessor cell-x)
   (y
    :initarg :y
    :accessor cell-y)
   (width
    :initarg :width
    :accessor cell-width)
   (height
    :initarg :height
    :accessor cell-height)
   (window
    :initarg :window
    :accessor cell-window
    :documentation "Every cell has it's own window to draw on.")
   (gcontext
    :initarg :gcontext
    :accessor cell-gcontext)
   (cells
    :initarg :cells
    :accessor cell-cells
    :initform nil
    :documentation "Holds all the subcells, thus building up a cell tree."))
  (:documentation "A place to put content. It resides in the awesome-mode-line."))

(defmethod initialize-instance :after
    ((self cell) &key parent-window x y width height &allow-other-keys)
  (let ((window (xlib:create-window
		 :parent parent-window
		 :class :input-output
		 :x x
		 :y y
		 :width width
		 :height height
		 :save-under :on
		 :override-redirect :off
		 :event-mask (xlib:make-event-mask :leave-window
						   :exposure
						   :button-press))))
    (setf (cell-cells self) '()
	  (cell-window self) window)))

(defgeneric draw-cell (cell)
  (:documentation "Draw a cell and it's sub-cells."))

;; Prepare cell and draw frame around
(defmethod draw-cell :before ((self cell))
  (let ((gcontext (cell-gcontext self)))
    ;; Fill background of cell
    (xlib:with-gcontext (gcontext :foreground (xlib:gcontext-background (cell-gcontext self)))
      (xlib:draw-rectangle (cell-window self) gcontext 0 0 (cell-width self) (cell-height self) t))
    ;; Draw frame around cell
    (xlib:draw-rectangle (cell-window self)
			 gcontext
			 0 0
			 (- (cell-width self) 1)
			 (- (cell-height self) 1))))

;; TODO is this a fucking hack?
(defmethod draw-cell :after ((self cell))
  (xlib:map-window (cell-window self))
  (xlib:map-subwindows (cell-window self))
  )

(defgeneric add-to-cells (cell new-cell)
  (:documentation "Add a new cell to the celllist."))

(defmethod add-to-cells ((self cell) (new-cell cell))
  (setf (cell-cells self)
	(append (cell-cells self) 
		(list new-cell))))

(defgeneric remove-from-cells (cell pred)
  (:documentation "Remove a cell form cell's cellist and deallocate the resources."))

;; TODO: debug this fuck
(defmethod remove-from-cells (pred (self cell))
  (setf (cell-cells self)
	(remove-if pred (cell-cells self))))

;;;
;;; Container cell
;;;

(defclass container-cell (cell)
  ()
  (:documentation "A container cell displays it's sub cells."))

;; Draw subcells after a subclass object initializes and places it's subcells
(defmethod draw-cell :after ((self container-cell))
  (dolist (cell (cell-cells self))
    (draw-cell cell)))

;;;
;;; List cell
;;; 

(defclass list-cell (container-cell)
  ()
  (:documentation "A list cell displays it's sub-cells horizontally next to each other."))

;; Place subcells
(defmethod draw-cell :before ((self list-cell))
  (let ((x 2)
	(y 2))
    (loop for cell in (cell-cells self) do
	 (setf (xlib:drawable-x (cell-window cell)) x
	       (xlib:drawable-y (cell-window cell)) y)
	 (incf x (+ (xlib:drawable-width (cell-window cell)) 2)))))

(defmethod draw-cell ((self list-cell))
  (if (next-method-p) (call-next-method)))

;;;
;;; Taskbar cell
;;;

(defclass taskbar-cell (list-cell)
  ()
  (:documentation "Lists windows"))

(defmethod draw-cell ((self taskbar-cell))
  "Create subcells on the fly for each window on desktop"
  (dolist (cell (cell-cells self))
    (xlib:unmap-window (cell-window cell))
    (xlib:destroy-window (cell-window cell)))
  (setf (cell-cells self) nil)
  (let ((windows (windows-on-current-desktop
  		  (xlib:screen-root (xlib:display-default-screen *display*)))))
    (dolist (w windows)
      (add-text-cell-from-window-to-container-cell w self)))
  )
	 
(defun add-text-cell-from-window-to-container-cell (window container-cell)
  (add-to-cells container-cell
		;; TODO: WTF, I can't give this cell a position yet.
		(make-instance 'text-cell
			       :parent-window (cell-window container-cell)
			       :gcontext (cell-gcontext container-cell)
			       :x 0
			       :y 0
			       :width 110
			       :height 16
			       :text (window-name window))))

;;;
;;; Image cell
;;;

(defclass image-cell (cell)
  ((image
    :initarg :image
    :accessor cell-image
    :documentation "The image to be displayed"))
  (:documentation "Displays a given X Pixmap"))

(defmethod initialize-instance :after
    ((self image-cell) &key image-path &allow-other-keys)
  (setf (slot-value self 'image) (xlib:read-bitmap-file image-path)))

(defmethod draw-cell ((self image-cell))
  (xlib:put-image (cell-window self)
		  (cell-gcontext self)
		  (cell-image self)
		  :x 3 :y 3
		  :bitmap-p t))

;;;
;;; Text cell
;;;

(defclass text-cell (cell)
  ((text
    :initarg :text
    :accessor text
    :documentation "Text to be displayed in the cell"))
  (:documentation "Displays text in the mmode-line"))

(defmethod draw-cell ((self text-cell))
  ;; Draw text
  (if (text self)
      (let* ((font (xlib:gcontext-font (cell-gcontext self)))
	     (font-ascent (xlib:font-ascent font))
	     (font-descent (xlib:font-descent font))
	     (font-offset (- (/ (+ (cell-height self) font-descent font-ascent) 2) font-descent)))
	(xlib:draw-glyphs (cell-window self)
				(cell-gcontext self)
				(ceiling (/ font-ascent 2))
				(ceiling font-offset)
				(text self)))))
