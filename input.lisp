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
;; This file handles input stuff
;;
;; Code:
(in-package :dswm)

(export '(*input-history-ignore-duplicates*
          *input-map*
	  completing-read
	  input-delete-region
	  input-goto-char
	  input-insert-char
	  input-insert-string
	  input-point
	  input-substring
	  input-validate-region
	  read-one-char
	  read-one-line))

(defstruct input-line
  string position history history-bk password)

(defvar *input-map* nil
  "This is the keymap containing all input editing key bindings.")

(when (null *input-map*)
  (setf *input-map*
	(let ((map (make-sparse-keymap)))
          (define-key map (kbd "DEL") 'input-delete-backward-char)
          (define-key map (kbd "M-DEL") 'input-backward-kill-word)
          (define-key map (kbd "C-d") 'input-delete-forward-char)
          (define-key map (kbd "M-d") 'input-forward-kill-word)
          (define-key map (kbd "Delete") 'input-delete-forward-char)
          (define-key map (kbd "C-f") 'input-forward-char)
          (define-key map (kbd "Right") 'input-forward-char)
          (define-key map (kbd "M-f") 'input-forward-word)
          (define-key map (kbd "C-b") 'input-backward-char)
          (define-key map (kbd "Left") 'input-backward-char)
          (define-key map (kbd "M-b") 'input-backward-word)
          (define-key map (kbd "C-a") 'input-move-beginning-of-line)
          (define-key map (kbd "Home") 'input-move-beginning-of-line)
          (define-key map (kbd "C-e") 'input-move-end-of-line)
          (define-key map (kbd "End") 'input-move-end-of-line)
          (define-key map (kbd "C-k") 'input-kill-line)
          (define-key map (kbd "C-u") 'input-kill-to-beginning)
          (define-key map (kbd "C-p") 'input-history-back)
          (define-key map (kbd "Up") 'input-history-back)
          (define-key map (kbd "C-n") 'input-history-forward)
          (define-key map (kbd "Down") 'input-history-forward)
          (define-key map (kbd "RET") 'input-submit)
          (define-key map (kbd "C-g") 'input-abort)
          (define-key map (kbd "ESC") 'input-abort)
          (define-key map (kbd "C-y") 'input-yank-selection)
          (define-key map (kbd "TAB") 'input-complete-forward)
          (define-key map (kbd "ISO_Left_Tab") 'input-complete-backward)
	  (define-key map t 'input-self-insert)
	  map)))

(defvar *input-history* nil
  "History for the input line.")

(defvar *input-last-command* nil
  "The last input command.")

(defvar *input-completions* nil
  "The list of completions")

(defvar *input-current-completions* nil
  "The list of matching completions.")

(defvar *input-current-completions-idx* nil
  "The current index in the current completions list.")

(defvar *input-history-ignore-duplicates* nil
  "Do not add a command to the input history if it's already the first in the list.")

;;; keysym functions

(defun is-modifier (keycode)
  "Return t if keycode is a modifier"
  (or (find keycode *all-modifiers* :test 'eql)
      ;; Treat No Symbol keys as modifiers (and therefore ignorable)
      (= (xlib:keycode->keysym *display* keycode 0) 0)))

(defun keycode->character (code mods)
  (let ((idx (if (member :shift mods) 1 0)))
    (xlib:keysym->character *display* (xlib:keycode->keysym *display* code idx) 0)))

;;; line and key reading functions

(defun setup-input-window (screen prompt input)
  "Set the input window up to read input"
  (let* ((height (+ (xlib:font-descent (screen-font screen))
                    (xlib:font-ascent (screen-font screen))))
         (win (screen-input-window screen)))
    ;; Window dimensions
    (xlib:with-state (win)
      (setf (xlib:window-priority win) :above
            (xlib:drawable-height win) height))
    (xlib:map-window win)
    ;; Draw the prompt
    (draw-input-bucket screen prompt input)
    ;; Ready to recieve input
    
))

(defun shutdown-input-window (screen)
  (xlib:ungrab-keyboard *display*)
  (xlib:unmap-window (screen-input-window screen)))

(defun input-handle-key-press-event (&rest event-slots &key event-key root code state &allow-other-keys)
  (declare (ignore event-slots root))
  ;; FIXME: don't use a cons
  (list* event-key code state))

(defun input-handle-selection-event (&key window selection property &allow-other-keys)
  (declare (ignore selection))
  (if property
      (xlib:get-property window property :type :string :result-type 'string :transform #'xlib:card8->char :delete-p t)
      ""))

(defun read-key-handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
  (case event-key
    ((or :key-release :key-press)
     (apply 'input-handle-key-press-event event-slots))
    (t nil)))

(defun read-key-or-selection-handle-event (&rest event-slots &key display event-key &allow-other-keys)
  (declare (ignore display))
  (case event-key
    ((or :key-release :key-press)
     (apply 'input-handle-key-press-event event-slots))
    (:selection-notify
     (apply 'input-handle-selection-event event-slots))
    (t nil)))

(defun read-key ()
  "Return a dotted pair (code . state) key."
  (loop for ev = (xlib:process-event *display* :handler #'read-key-handle-event :timeout nil) do
       (when (and (consp ev)
                  (eq (first ev) :key-press))
           (return (cdr ev)))))

(defun read-key-no-modifiers ()
  "Like read-key but never returns a modifier key."
  (loop for k = (read-key)
       while (is-modifier (car k))
       finally (return k)))

(defun read-key-or-selection ()
  (loop for ev = (xlib:process-event *display* :handler #'read-key-or-selection-handle-event :timeout nil) do
       (cond ((stringp ev)
              (return ev))
             ((and (consp ev)
                   (eq (first ev) :key-press))
              (return (cdr ev))))))

(defun make-input-string (initial-input)
  (make-array (length initial-input) :element-type 'character :initial-contents initial-input
              :adjustable t :fill-pointer t))

(defun completing-read (screen prompt completions &key (initial-input "") require-match)
  "Read a line of input through dswm and return it with TAB
completion. completions can be a list, an fbound symbol, or a
function. if its an fbound symbol or a function then that function is
passed the substring to complete on and is expected to return a list
of matches. If require-match argument is non-nil then the input must
match with an element of the completions."
  (check-type completions (or list function symbol))
  (let ((*input-completions* completions)
        (*input-current-completions* nil)
        (*input-current-completions-idx* nil))
    (let ((line (read-one-line screen prompt :initial-input initial-input :require-match require-match)))
      (when line (string-trim " " line)))))

(defun read-one-line (screen prompt &key (initial-input "") require-match password)
  "Read a line of input through dswm and return it. returns nil if the user aborted."
  (let ((*input-last-command* nil)
        (input (make-input-line :string (make-input-string initial-input)
                                :position (length initial-input)
                                :history -1
				:password password)))
    (labels ((match-input ()
               (let* ((in (string-trim " " (input-line-string input)))
                      (compls (input-find-completions in *input-completions*)))
                 (and (consp compls)
                      (string= in (car compls)))))
             (key-loop ()
               (loop for key = (read-key-or-selection) do
                     (cond ((stringp key)
                            ;; handle selection
                            (input-insert-string input key)
                            (draw-input-bucket screen prompt input))
                           ;; skip modifiers
                           ((is-modifier (car key)))
                           ((process-input screen prompt input (car key) (cdr key))
                            (if (or (not require-match)
                                    (match-input))
                                (return (input-line-string input))
                                (draw-input-bucket screen prompt input "[No match]" t)))))))
      (setup-input-window screen prompt input)
      (catch :abort
        (unwind-protect
             (with-focus (screen-input-window screen)
               (key-loop))
          (shutdown-input-window screen))))))

(defun read-one-char (screen)
  "Read a single character from the user."
  (with-focus (screen-key-window screen)
    (let ((k (read-key-no-modifiers)))
      (keycode->character (car k) (xlib:make-state-keys (cdr k))))))


(defun draw-input-bucket (screen prompt input &optional (tail "") errorp)
  "Draw to the screen's input window the contents of input."
  (let* ((gcontext (screen-message-gc screen))
         (win (screen-input-window screen))
         (prompt-width (xlib:text-width (screen-font screen) prompt :translate #'translate-id))
	 (line-content (input-line-string input))
         (string (if (input-line-password input)
                     (make-string (length line-content) :initial-element #\*)
                     line-content))
         (string-width (xlib:text-width (screen-font screen) string :translate #'translate-id))
         (space-width  (xlib:text-width (screen-font screen) " "    :translate #'translate-id))
         (tail-width   (xlib:text-width (screen-font screen) tail   :translate #'translate-id))
         (full-string-width (+ string-width space-width))
         (pos (input-line-position input))
         (width (+ prompt-width
                   (max 100 (+ full-string-width space-width tail-width)))))
    (xlib:with-state (win)
      (xlib:clear-area win :x (+ *message-window-padding*
                                 prompt-width
                                 string-width))
      (setf (xlib:drawable-width win) (+ width (* *message-window-padding* 2)))
      (setup-win-gravity screen win *input-window-gravity*))
    (xlib:with-state (win)
      (xlib:draw-image-glyphs win gcontext
                              *message-window-padding*
                              (xlib:font-ascent (screen-font screen))
                              prompt
                              :translate #'translate-id
                              :size 16)
      (xlib:draw-image-glyphs win gcontext
                              (+ *message-window-padding* prompt-width)
                              (xlib:font-ascent (screen-font screen))
                              string
                              :translate #'translate-id
                              :size 16)
      (xlib:draw-image-glyphs win gcontext
                              (+ *message-window-padding* prompt-width full-string-width space-width)
                              (xlib:font-ascent (screen-font screen))
                              tail
                              :translate #'translate-id
                              :size 16)
      ;; draw a block cursor
      (invert-rect screen win
                   (+ *message-window-padding*
                      prompt-width
                      (xlib:text-width (screen-font screen) (subseq string 0 pos) :translate #'translate-id))
                   0
                   (xlib:text-width (screen-font screen) (if (>= pos (length string))
                                                             " "
                                                             (string (char string pos)))
                                    :translate #'translate-id)
                   (+ (xlib:font-descent (screen-font screen))
                      (xlib:font-ascent (screen-font screen))))
      ;; draw the error
      (when errorp
        (invert-rect screen win 0 0 (xlib:drawable-width win) (xlib:drawable-height win))
        (xlib:display-force-output *display*)
        (sleep 0.05)
        (invert-rect screen win 0 0 (xlib:drawable-width win) (xlib:drawable-height win))))))

(defun code-state->key (code state)
  (let* ((mods    (xlib:make-state-keys state))
         (shift-p (and (find :shift mods) t))
         (altgr-p (and (intersection (modifiers-altgr *modifiers*) mods) t))
         (base    (if altgr-p 2 0))
         (sym     (xlib:keycode->keysym *display* code base))
         (upsym   (xlib:keycode->keysym *display* code (+ base 1))))
    ;; If a keysym has a shift modifier, then use the uppercase keysym
    ;; and remove remove the shift modifier.
    (make-key :keysym (if (and shift-p (not (eql sym upsym)))
                          upsym
                          sym)
              :control (and (find :control mods) t)
              :shift (and shift-p (eql sym upsym))
              :meta (and (intersection mods (modifiers-meta *modifiers*)) t)
              :alt (and (intersection mods (modifiers-alt *modifiers*)) t)
              :hyper (and (intersection mods (modifiers-hyper *modifiers*)) t)
              :super (and (intersection mods (modifiers-super *modifiers*)) t))))


;;; input string utility functions

(defun input-submit (input key)
  (declare (ignore input key))
  :done)

(defun input-abort (input key)
  (declare (ignore input key))
  (throw :abort nil))

(defun input-goto-char (input point)
  "Move the cursor to the specified point in the string"
  (setf (input-line-position input) (min (max 0 point)
                                         (length (input-line-string input)))))

(defun input-insert-string (input string)
  "Insert @var{string} into the input at the current
position. @var{input} must be of type @var{input-line}. Input
functions are passed this structure as their first argument."
  (check-type string string)
  (loop for c across string
	do (input-insert-char input c)))

(defun input-point (input)
  "Return the position of the cursor."
  (check-type input input-line)
  (input-line-position input))

(defun input-validate-region (input start end)
  "Return a value pair of numbers where the first number is < the
second and neither excedes the bounds of the input string."
  (values (max 0 (min start end))
          (min (length (input-line-string input))
               (max start end))))

(defun input-delete-region (input start end)
  "Delete the region between start and end in the input string"
  (check-type input input-line)
  (check-type start fixnum)
  (check-type end fixnum)
  (multiple-value-setq (start end) (input-validate-region input start end))
  (replace (input-line-string input) (input-line-string input)
           :start2 end :start1 start)
  (decf (fill-pointer (input-line-string input)) (- end start))
  (cond
    ((< (input-line-position input) start))
    ((< (input-line-position input) end)
     (setf (input-line-position input) start))
    (t
     (decf (input-line-position input) (- end start)))))

(defun input-insert-char (input char)
  "Insert @var{char} into the input at the current
position. @var{input} must be of type @var{input-line}. Input
functions are passed this structure as their first argument."
  (vector-push-extend #\_ (input-line-string input))
  (replace (input-line-string input) (input-line-string input)
           :start2 (input-line-position input) :start1 (1+ (input-line-position input)))
  (setf (char (input-line-string input) (input-line-position input)) char)
  (incf (input-line-position input)))

(defun input-substring (input start end)
  "Return a the substring in INPUT bounded by START and END."
  (subseq (input-line-string input) start end))


;;; "interactive" input functions

(defun input-find-completions (str completions)
  (if (or (functionp completions)
          (and (symbolp completions)
               (fboundp completions)))
      (funcall completions str)
      (remove-if-not (lambda (elt)
                       (when (listp elt)
                         (setf elt (car elt)))
                       (and (<= (length str) (length elt))
                            (string= str elt
                                     :end1 (length str)
                                     :end2 (length str))))
                     completions)))

(defun input-complete (input direction)
  ;; reset the completion list if this is the first time they're
  ;; trying to complete.
  (unless (find *input-last-command* '(input-complete-forward
                                       input-complete-backward))
    (setf *input-current-completions* (input-find-completions (input-substring input 0 (input-point input)) *input-completions*)
          *input-current-completions-idx* -1))
  (if *input-current-completions*
      (progn
        ;; Insert the next completion
        (input-delete-region input 0 (input-point input))
        (if (eq direction :forward)
            (progn
              (incf *input-current-completions-idx*)
              (when (>= *input-current-completions-idx* (length *input-current-completions*))
                (setf *input-current-completions-idx* 0)))
            (progn
              (decf *input-current-completions-idx*)
              (when (< *input-current-completions-idx* 0)
                (setf *input-current-completions-idx* (1- (length *input-current-completions*))))))
        (let ((elt (nth *input-current-completions-idx* *input-current-completions*)))
          (input-insert-string input (if (listp elt) (first elt) elt))
          (input-insert-char input #\Space)))
      :error))

(defun input-complete-forward (input key)
  (declare (ignore key))
  (input-complete input :forward))

(defun input-complete-backward (input key)
  (declare (ignore key))
  (input-complete input :backward))

(defun input-delete-backward-char (input key)
  (declare (ignore key))
  (let ((pos (input-line-position input)))
    (cond ((or (<= (length (input-line-string input)) 0)
               (<= pos 0))
           :error)
          (t
           (replace (input-line-string input) (input-line-string input)
                    :start2 pos :start1 (1- pos))
           (decf (fill-pointer (input-line-string input)))
           (decf (input-line-position input))))))

(defun input-delete-forward-char (input key)
  (declare (ignore key))
  (let ((pos (input-line-position input)))
    (cond ((>= pos
               (length (input-line-string input)))
           :error)
          (t
           (replace (input-line-string input) (input-line-string input)
                    :start1 pos :start2 (1+ pos))
           (decf (fill-pointer (input-line-string input)))))))

(defun input-forward-kill-word (input key)
  (declare (ignore key))
  (let* ((p1 (position-if 'alphanumericp (input-line-string input) :start (input-line-position input)))
         (p2 (and p1 (position-if-not 'alphanumericp (input-line-string input) :start p1))))
    (input-delete-region input (input-point input) (or p2 (length (input-line-string input))))))

(defun input-backward-kill-word (input key)
  (declare (ignore key))
  (let* ((p1 (position-if 'alphanumericp (input-line-string input) :end (input-line-position input) :from-end t))
         (p2 (and p1 (position-if-not 'alphanumericp (input-line-string input) :end p1 :from-end t))))
    (input-delete-region input (input-point input) (or (and p2 (1+ p2)) 0))))

(defun input-forward-word (input key)
  (declare (ignore key))
  (let* ((p1 (position-if 'alphanumericp (input-line-string input) :start (input-line-position input)))
         (p2 (and p1 (position-if-not 'alphanumericp (input-line-string input) :start p1))))
    (setf (input-line-position input) (or p2 (length (input-line-string input))))))

(defun input-backward-word (input key)
  (declare (ignore key))
  (let* ((p1 (position-if 'alphanumericp (input-line-string input) :end (input-line-position input) :from-end t))
         (p2 (and p1 (position-if-not 'alphanumericp (input-line-string input) :end p1 :from-end t))))
    (setf (input-line-position input) (or (and p2 (1+ p2))
                                          0))))

(defun input-forward-char (input key)
  (declare (ignore key))
  (incf (input-line-position input))
  (when (> (input-line-position input)
           (length (input-line-string input)))
    (setf (input-line-position input) (length (input-line-string input)))))

(defun input-backward-char (input key)
  (declare (ignore key))
  (decf (input-line-position input))
  (when (< (input-line-position input) 0)
    (setf (input-line-position input) 0)))

(defun input-move-beginning-of-line (input key)
  (declare (ignore key))
  (setf (input-line-position input) 0))

(defun input-move-end-of-line (input key)
  (declare (ignore key))
  (setf (input-line-position input) (length (input-line-string input))))

(defun input-kill-line (input key)
  (declare (ignore key))
  (unless (= (input-line-position input) (length (input-line-string input)))
    (set-x-selection (subseq (input-line-string input) (input-line-position input))))
  (setf (fill-pointer (input-line-string input)) (input-line-position input)))

(defun input-kill-to-beginning (input key)
  (declare (ignore key))
  (unless (= (input-line-position input) 0)
    (set-x-selection (subseq (input-line-string input) 0 (input-line-position input))))
  (replace (input-line-string input) (input-line-string input)
           :start2 (input-line-position input) :start1 0)
  (decf (fill-pointer (input-line-string input)) (input-line-position input))
  (setf (input-line-position input) 0))

(defun input-history-back (input key)
  (declare (ignore key))
  (when (= (input-line-history input) -1)
    (setf (input-line-history-bk input) (input-line-string input)))
  (incf (input-line-history input))
  (if (>= (input-line-history input)
          (length *input-history*))
      (progn
        (decf (input-line-history input))
        :error)
      (setf (input-line-string input) (make-input-string (elt *input-history* (input-line-history input)))
            (input-line-position input) (length (input-line-string input)))))

(defun input-history-forward (input key)
  (declare (ignore key))
  (decf (input-line-history input))
  (cond ((< (input-line-history input) -1)
         (incf (input-line-history input))
         :error)
        ((= (input-line-history input) -1)
         (setf (input-line-string input) (input-line-history-bk input)
               (input-line-position input) (length (input-line-string input))))
        (t
         (setf (input-line-string input) (make-input-string (elt *input-history* (input-line-history input)))
               (input-line-position input) (length (input-line-string input))))))

(defun input-self-insert (input key)
  (let ((char (xlib:keysym->character *display* (key-keysym key))))
    (if (or (key-mods-p key) (null char)
            (not (characterp char)))
        :error
        (input-insert-char input char))))

(defun input-yank-selection (input key)
  (declare (ignore key))
  ;; if we own the selection then just insert it.
  (if *x-selection*
      (input-insert-string input *x-selection*)
      (xlib:convert-selection :primary :string (screen-input-window (current-screen)) :dswm-selection)))


;;; Misc functions

(defun process-input (screen prompt input code state)
  "Process the key (code and state), given the current input
buffer. Returns a new modified input buffer."
  (labels ((process-key (code state)
             "Call the appropriate function based on the key
pressed. Return 'done when the use has signalled the finish of his
input (pressing Return), nil otherwise."
             (let* ((key (code-state->key code state))
                    (command (and key (lookup-key *input-map* key t))))
               (if command
                   (prog1
                       (funcall command input key)
                     (setf *input-last-command* command))
                   :error))))
    (case (process-key code state)
      (:done

       (unless (or (input-line-password input)
                   (and *input-history-ignore-duplicates*
                        (string= (input-line-string input) (first *input-history*))))
         (push (input-line-string input) *input-history*))
       :done)
      (:abort
       (throw :abort t))
      (:error
       ;; FIXME draw inverted text
       (draw-input-bucket screen prompt input "" t)
       nil)
      (t
       (draw-input-bucket screen prompt input)
       nil))))

(defun all-modifier-codes ()
  (multiple-value-bind
        (shift-codes lock-codes control-codes mod1-codes mod2-codes mod3-codes mod4-codes mod5-codes)
      (xlib:modifier-mapping *display*)
    (append shift-codes
            lock-codes
            control-codes
            mod1-codes
            mod2-codes
            mod3-codes
            mod4-codes
            mod5-codes)))

(defun get-modifier-map ()
  (labels ((find-mod (mod codes)
             (find (xlib:keysym->keycodes *display* (keysym-name->keysym mod)) codes)))
    (let ((modifiers (make-modifiers)))
      (multiple-value-bind
            (shift-codes lock-codes control-codes mod1-codes mod2-codes mod3-codes mod4-codes mod5-codes)
          (xlib:modifier-mapping *display*)
        (declare (ignore shift-codes lock-codes control-codes))
        (loop for mod in '(:mod-1 :mod-2 :mod-3 :mod-4 :mod-5)
              for codes in (list mod1-codes mod2-codes mod3-codes mod4-codes mod5-codes)
              do
              (cond ((or (find-mod "Meta_L" codes)
                         (find-mod "Meta_R" codes))
                     (push mod (modifiers-meta modifiers)))
                    ((or (find-mod "Alt_L" codes)
                         (find-mod "Alt_R" codes))
                     (push mod (modifiers-alt modifiers)))
                    ((or (find-mod "Super_L" codes)
                         (find-mod "Super_R" codes))
                     (push mod (modifiers-super modifiers)))
                    ((or (find-mod "Hyper_L" codes)
                         (find-mod "Hyper_R" codes))
                     (push mod (modifiers-hyper modifiers)))
                    ((find-mod "Num_Lock" codes)
                     (push mod (modifiers-numlock modifiers)))
                    ((find-mod "ISO_Level3_Shift" codes)
                     (push mod (modifiers-altgr modifiers)))))
        ;; If alt is defined but meta isn't set meta to alt and clear alt
        (when (and (modifiers-alt modifiers)
                   (null (modifiers-meta modifiers)))
          (setf (modifiers-meta modifiers) (modifiers-alt modifiers)
                (modifiers-alt modifiers) nil))
        modifiers))))

(defun update-modifier-map ()
  (setf *modifiers* (get-modifier-map)
        *all-modifiers* (all-modifier-codes)))

;; (defun x11mod->dsmod (screen state)
;;   (let ((mod nil))
;;     (when (member state (modifiers-alt (screen-modifiers screen)))
;;       (push :alt mod))
;;     (when (member state (modifiers-meta (screen-modifiers screen)))
;;       (push :meta mod))
;;     (when (member state (modifiers-hyper (screen-modifiers screen)))
;;       (push :hyper mod))
;;     (when (member state (modifiers-super (screen-modifiers screen)))
;;       (push :super mod))
;;     (when (member state :control)
;;       (push :control mod))
;;     mod))

(defun mod->string (state)
  "Convert a ds modifier list to a string"
  (let ((alist '((:alt . "A-") (:meta . "M-") (:hyper . "H-") (:super . "S-"))))
    (apply #'concatenate 'string (mapcar (lambda (x) (cdr (assoc x alist))) state))))

;; (defun keycode->string (code state)
;;   (concatenate 'string (mod->string state)
;;             (string (keysym->character *display*
;;                                        (xlib:keycode->keysym *display* code 0)
;;                                        state))))

;; (defun cook-keycode (code state)
;;   (values (xlib:keycode->keysym *display* code 0) (x11mod->dsmod state)))

(defun y-or-n-p (message)
  "ask a \"y or n\" question on the current screen and return T if the
user presses 'y'"
  (message "~a(y or n) " message)
  (char= (read-one-char (current-screen))
        #\y))

(defun yes-or-no-p (message)
  "ask a \"yes or no\" question on the current screen and return T if the
user presses 'yes'"
  (loop for line = (read-one-line (current-screen)
                                  (format nil "~a(yes or no) " message))
        until (find line '("yes" "no") :test 'string-equal)
        do (message "Please answer yes or no")
           (sleep 1)
        finally (return (string-equal line "yes"))))
