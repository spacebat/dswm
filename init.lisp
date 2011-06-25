;; -*-lisp-*-
;;
;; Here is a sample .dswmrc file

;; (in-package :dswm)

;;;; change the prefix key to something else
;; (set-prefix-key (kbd "C-z"))

;;;; Default terminal emulator
;; (setq *terminal* "urxvt")

;;;; Default Browser
;; (setq *browser* "firefox")

;;;; Default emacs realisation
;; (setq *emacs* "xemacs")

;;;; Message window gravity
;; ; :top-left :top-right :bottom-left :bottom-right :center
;; (setq *message-window-gravity* :top-right)

;;;; Input window gravity
;; ; :top-left :top-right :bottom-left :bottom-right :center
;; (setq *input-window-gravity* :bottom-left)

;;;; From which source window name will be formed
;; ; :title  :class :resource-name
;; (setq *window-name-source* :title)

;;;; Window format
;; (setq *window-format* "|%m%s%n %c %10t...")

;;;; Startup message
;; (setq *startup-message* "^2*Welcome to The ^BD^beep ^BS^bpace ^BW^bindow ^BM^banager!")

;; in seconds, how long a message will appear for. This must be an integer.
;; (setf *timeout-wait* 5)

;; In what frame system try to open next window
;; (setf *new-window-preferred-frame* '(:empty :focused))

;;;; Mouse focus policy
;; ; :ignore, :sloppy, and :click
;; (setq *mouse-focus-policy* :sloppy)

;;;; Name of default group
;; (setq *default-group-name* "Main")

;;;; Style of window borsers
;; ; :thick :thin :tight :none
;; (defvar *window-border-style* :thick)

;;;;
;;;; Custom keybindings:
;;;;
;;;; Read some doc
;; (define-key *root-map* (kbd "d") "exec gv")
;;;; Lock screen
;; (define-key *root-map* (kbd "C-l") "exec xlock")

;;;; Message window font
;; (set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")
;; (set-font "-*-terminus-medium-r-normal-*-14-*-*-*-*-*-iso10646-1")
(set-font "-*-fixed-medium-r-*-*-"15-*-*-*-*-*-iso10646-1")

;;;; Loading some external modules
(load-module "amixer" "web" "s-bindings")

;; Definition of some useful keybindings
(defkeys-top
  ("XF86Calculator" "exec emacs -Q -f calc")
  ("XF86HomePage" "exec conkeror http://google.com")
;;  ("XF86Search" "google")
  ("XF86Mail" "exec thunderbird")
  ;; ("XF86AudioLowerVolume" "exec amixer -q set Master 5- unmute")
  ;; ("XF86AudioRaiseVolume" "exec amixer -q set Master 5+ unmute")
  ;; ("XF86AudioMute" "exec amixer -q set Master toggle")

  ("XF86AudioLowerVolume" "amixer-Master-1-")
  ("XF86AudioRaiseVolume" "amixer-Master-1+")
  ("XF86AudioMute" "exec amixer -q set Master toggle")

  ("XF86Forward" "gnext")
  ("XF86Back" "gprev")
  ;; Control music playing. Need installed mpd and mpc
  ;; ("XF86AudioPlay"  "exec mpc toggle")
  ;; ("XF86Launch5"  "exec mpc next")
  ;; ("XF86Launch1"  "exec mpc prev")
)

;;;; Debug level: 0-5
(setf *debug-level* 0)

;;;; set up X cursor color.
;; (run-shell-command (format nil "xsetroot -cursor_name left_ptr -fg \"~a\"" "red" BACKGROUND-COLOR))
