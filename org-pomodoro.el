;;; org-pomodoro.el --- Pomodoro implementation for org-mode.

;; Author: Arthur Leonard Andersen <leoc.git@gmail.com>, Marcin Koziej <marcin at lolownia dot org>
;; Maintainer: Aayush Bajaj <aayushbajaj7@gmail.com>
;; URL: https://github.com/abaj8494/org-pomodoro
;; Created: May 10, 2013
;; Version: 3.1.0
;; Package-Requires: ((alert "0.5.10") (cl-lib "0.5"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Org-pomodoro introduces an easy way to clock time in org-mode with
;; the pomodoro technique.  You can clock into tasks with starting a
;; pomodoro time automatically.  Each finished pomodoro is followed by
;; a break timer.  If you completed 4 pomodoros in a row the break is
;; longer that the shorter break between each pomodoro.
;;
;; For a full explanation of the pomodoro technique, have a look at:
;;   http://www.pomodorotechnique.com
;;
;; Version 3.1.0 changes (abaj8494 fork):
;; - Support for multiple simultaneous named pomodoros
;; - Prompt for pomodoro name when starting
;; - Verbose Messages buffer logging for all events
;; - C-u prefix argument support for custom work/break lengths
;;   - C-u: prompt for work length
;;   - C-u C-u: prompt for work and break lengths

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'timer)
(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'org-timer)
(require 'alert)

;;; Custom Variables

(defgroup org-pomodoro nil
  "Org pomodoro customization"
  :tag "Org Pomodoro"
  :group 'org-progress)

(defcustom org-pomodoro-long-break-frequency 4
  "The maximum number of pomodoros until a long break is started."
  :group 'org-pomodoro
  :type 'integer)

(defcustom org-pomodoro-ask-upon-killing t
  "Determines whether to ask upon killing a pomodoro or not."
  :group 'org-pomodoro
  :type 'boolean)

(defcustom org-pomodoro-play-sounds t
  "Determines whether sounds are played or not."
  :group 'org-pomodoro
  :type 'boolean)

(defcustom org-pomodoro-manual-break nil
  "Whether the user needs to exit manually from a running pomodoro to enter a break.

If non-nil, after the time is up for a pomodoro, an \"overtime\"
state is entered until 'org-pomodoro' is invoked, which then
finishes the pomodoro and enters the break period."
  :group 'org-pomodoro
  :type 'boolean)

;; Pomodoro Values

(defcustom org-pomodoro-length 25
  "The length of a pomodoro in minutes."
  :group 'org-pomodoro
  :type 'integer)

(defcustom org-pomodoro-time-format "%.2m:%.2s"
  "Defines the format of the time representation in the modeline."
  :group 'org-pomodoro
  :type 'string)

(defcustom org-pomodoro-format "Pomodoro~%s"
  "The format of the mode line string during a pomodoro session."
  :group 'org-pomodoro
  :type 'string)


(defcustom org-pomodoro-audio-player (or (executable-find "aplay")
                                         (executable-find "afplay"))
  "Music player used to play sounds."
  :group 'org-pomodoro
  :type 'string)

;;; POMODORO START SOUND
(defcustom org-pomodoro-start-sound-p nil
  "Determines whether to play a sound when a pomodoro started.

Use `org-pomodoro-start-sound' to determine what sound that should be."
  :group 'org-pomodoro
  :type 'boolean)

(defcustom org-pomodoro-start-sound (when load-file-name
                                      (concat (file-name-directory load-file-name)
                                              "resources/bell.wav"))
  "The path to a sound file that´s to be played when a pomodoro is started."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-start-sound-args nil
  "Arguments used when playing the `org-pomodoro-start-sound'."
  :group 'org-pomodoro
  :type 'string)


;;; POMODORO FINISHED SOUND
(defcustom org-pomodoro-finished-sound-p t
  "Determines whether to play a sound when a pomodoro finished.

Use `org-pomodoro-finished-sound' to determine what sound that should be."
  :group 'org-pomodoro
  :type 'boolean)

(defcustom org-pomodoro-finished-sound (when load-file-name
                                         (concat (file-name-directory load-file-name)
                                                 "resources/bell.wav"))
  "The path to a sound file that´s to be played when a pomodoro was finished."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-finished-sound-args nil
  "Arguments used when playing the `org-pomodoro-finished-sound'."
  :group 'org-pomodoro
  :type 'string)

;;; POMODORO OVERTIME SOUND
(defcustom org-pomodoro-overtime-sound-p t
  "Determines whether to play a sound when a pomodoro starts to run overtime.

Use `org-pomodoro-overtime-sound' to determine what sound that should be."
  :group 'org-pomodoro
  :type 'boolean)

(defcustom org-pomodoro-overtime-sound (when load-file-name
                                         (concat (file-name-directory load-file-name)
                                                 "resources/bell.wav"))
  "The path to a sound file that´s to be played when a pomodoro runs overtime."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-overtime-sound-args nil
  "Arguments used when playing the `org-pomodoro-overtime-sound'."
  :group 'org-pomodoro
  :type 'string)

;;; POMODORO KILLED SOUND
(defcustom org-pomodoro-killed-sound-p nil
  "Determines whether to play a sound when a pomodoro killed.

Use `org-pomodoro-killed-sound' to determine what sound that should be."
  :group 'org-pomodoro
  :type 'boolean)

(defcustom org-pomodoro-killed-sound nil
  "The path to a sound file, that´s to be played when a pomodoro is killed."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-killed-sound-args nil
  "Arguments used when playing the `org-pomodoro-killed-sound'."
  :group 'org-pomodoro
  :type 'string)

;;; POMODORO SHORT-BREAK SOUND
(defcustom org-pomodoro-short-break-sound-p t
  "Determines whether to play a sound when a short-break finished.

Use `org-pomodoro-short-break-sound' to determine what sound that should be."
  :group 'org-pomodoro
  :type 'boolean)

(defcustom org-pomodoro-clock-break nil
  "When t, also clocks time during breaks."
  :group 'org-pomodoro
  :type 'boolean)

(defcustom org-pomodoro-short-break-sound (when load-file-name
                                            (concat (file-name-directory load-file-name)
                                                    "resources/bell.wav"))
  "The path to a sound file that´s to be played when a break was finished."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-short-break-sound-args nil
  "Arguments used when playing the `org-pomodoro-short-break-sound'."
  :group 'org-pomodoro
  :type 'string)

;;; POMODORO LONG-BREAK SOUND
(defcustom org-pomodoro-long-break-sound-p t
  "Determines whether to play a sound when a long-break finished.

Use `org-pomodoro-long-break-sound' to determine what sound that should be."
  :group 'org-pomodoro
  :type 'boolean)

(defcustom org-pomodoro-long-break-sound (when load-file-name
                                           (concat (file-name-directory load-file-name)
                                                   "resources/bell_multiple.wav"))
  "The path to a sound file that´s to be played when a long break is finished."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-long-break-sound-args nil
  "Arguments used when playing the `org-pomodoro-long-break-sound'."
  :group 'org-pomodoro
  :type 'string)

;;; POMODORO TICKING SOUND
(defcustom org-pomodoro-ticking-sound-p nil
  "Determines whether ticking sounds are played or not."
  :group 'org-pomodoro
  :type 'boolean)

(defcustom org-pomodoro-ticking-sound (when load-file-name
                                        (concat (file-name-directory load-file-name)
                                                "resources/tick.wav"))
  "The path to a sound file that´s to be played while a pomodoro is running."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-ticking-sound-args nil
  "Arguments used when playing the `org-pomodoro-ticking-sound'."
  :group 'org-pomodoro
  :type 'string)

(defcustom org-pomodoro-ticking-sound-states '(:pomodoro :short-break :long-break)
  "The states in which to play ticking sounds."
  :group 'org-pomodoro
  :type 'list)

(defcustom org-pomodoro-ticking-frequency 1
  "The frequency at which to playback the ticking sound."
  :group 'org-pomodoro
  :type 'list)

;;; OVERTIME VALUES
(defcustom org-pomodoro-overtime-format "+%s"
  "The format of the mode line during a pomodoro running overtime."
  :group 'org-pomodoro
  :type 'string)

;;; BREAK VALUES
(defcustom org-pomodoro-short-break-length 5
  "The length of a short break in minutes."
  :group 'org-pomodoro
  :type 'integer)

(defcustom org-pomodoro-short-break-format "Short Break~%s"
  "The format of the mode line string during a short break."
  :group 'org-pomodoro
  :type 'string)

(defcustom org-pomodoro-long-break-length 20
  "The length of a long break in minutes."
  :group 'org-pomodoro
  :type 'integer)

(defcustom org-pomodoro-long-break-format "Long Break~%s"
  "The format of the mode line string during a long break."
  :group 'org-pomodoro
  :type 'string)

(defcustom org-pomodoro-expiry-time 120
  "The time in minutes for which a pomodoro group is valid.
The size of a pomodoro group is defined by `org-pomodoro-long-break-frequency'.

If you do not clock in for this period of time you will be prompted
whether to reset the pomodoro count next time you call `org-pomodoro'."
  :group 'org-pomodoro
  :type 'integer)

(defcustom org-pomodoro-keep-killed-pomodoro-time nil
  "Keeps the clocked time of killed pomodoros."
  :group 'org-pomodoro
  :type 'boolean)

;; Hooks

(defvar org-pomodoro-started-hook nil
  "Hooks run when a pomodoro is started.
Hook functions receive the pomodoro name as an argument.")

(defvar org-pomodoro-finished-hook nil
  "Hooks run when a pomodoro is finished.
Hook functions receive the pomodoro name as an argument.")

(defvar org-pomodoro-overtime-hook nil
  "Hooks run when a pomodoro enters overtime.
Hook functions receive the pomodoro name as an argument.")

(defvar org-pomodoro-killed-hook nil
  "Hooks run when a pomodoro is killed.
Hook functions receive the pomodoro name as an argument.")

(defvar org-pomodoro-break-finished-hook nil
  "Hook run after any break has finished.
Run before a break's specific hook.
Hook functions receive the pomodoro name as an argument.")

(defvar org-pomodoro-long-break-finished-hook nil
  "Hooks run when a long break is finished.
Hook functions receive the pomodoro name as an argument.")

(defvar org-pomodoro-short-break-finished-hook nil
  "Hooks run when short break is finished.
Hook functions receive the pomodoro name as an argument.")

(defvar org-pomodoro-tick-hook nil
  "Hooks run every second during a pomodoro.
Hook functions receive the pomodoro instance as an argument.")

;; Faces

(defface org-pomodoro-mode-line
  '((t (:foreground "tomato1")))
  "Face of a pomodoro in the modeline."
  :group 'faces)

(defface org-pomodoro-mode-line-overtime
  '((t (:foreground "tomato3" :weight bold)))
  "Face of a pomodoro running overtime in the modeline."
  :group 'faces)

(defface org-pomodoro-mode-line-break
  '((t (:foreground "#2aa198")))
  "Face of a pomodoro break in the modeline ."
  :group 'faces)

;; Temporary Variables

(defvar org-pomodoro-mode-line "")
(put 'org-pomodoro-mode-line 'risky-local-variable t)

;; Multi-pomodoro support: list of active instances
(defvar org-pomodoro--instances '()
  "List of active pomodoro instances.
Each instance is a plist with:
  :name - user-given name
  :state - :pomodoro, :short-break, :long-break, :overtime, :none
  :timer - the timer object
  :end-time - when this phase ends
  :count - number of completed pomodoros for this instance
  :length - pomodoro length for this instance
  :short-break-length - short break length
  :long-break-length - long break length
  :last-clock-in - last clock-in time")

;; Legacy variables for backwards compatibility
(defvar org-pomodoro-timer nil
  "The timer while a pomodoro or a break (legacy, use instances).")

(defvar org-pomodoro-end-time nil
  "The end time of the current pomodoro phase (legacy).")

(defvar org-pomodoro-state :none
  "The current state of `org-pomodoro` (legacy).
For multi-pomodoro, check `org-pomodoro--instances'.")

(defvar org-pomodoro-count 0
  "The number of pomodoros since the last long break (legacy).")

(defvar org-pomodoro-last-clock-in nil
  "The last time the pomodoro was set (legacy).")

;;; Internal

;; Logging

(defun org-pomodoro--log (format-string &rest args)
  "Log a message to *Messages* with timestamp and org-pomodoro prefix.
FORMAT-STRING and ARGS are passed to `format'."
  (let ((msg (apply #'format format-string args))
        (timestamp (format-time-string "%H:%M:%S")))
    (message "[org-pomodoro %s] %s" timestamp msg)))

;; Instance management

(defun org-pomodoro--make-instance (name &optional length short-break long-break)
  "Create a new pomodoro instance with NAME.
Optional LENGTH, SHORT-BREAK, LONG-BREAK override defaults."
  (list :name name
        :state :none
        :timer nil
        :end-time nil
        :count 0
        :length (or length org-pomodoro-length)
        :short-break-length (or short-break org-pomodoro-short-break-length)
        :long-break-length (or long-break org-pomodoro-long-break-length)
        :last-clock-in nil))

(defun org-pomodoro--get-instance (name)
  "Get the pomodoro instance with NAME, or nil if not found."
  (cl-find-if (lambda (inst)
                (string= (plist-get inst :name) name))
              org-pomodoro--instances))

(defun org-pomodoro--instance-active-p (instance)
  "Return t if INSTANCE is active (not :none state)."
  (not (eq (plist-get instance :state) :none)))

(defun org-pomodoro--any-active-p ()
  "Return t if any pomodoro instance is active."
  (cl-some #'org-pomodoro--instance-active-p org-pomodoro--instances))

(defun org-pomodoro--active-instances ()
  "Return list of all active pomodoro instances."
  (cl-remove-if-not #'org-pomodoro--instance-active-p org-pomodoro--instances))

(defun org-pomodoro--instance-remaining-seconds (instance)
  "Return remaining seconds for INSTANCE."
  (let ((end-time (plist-get instance :end-time)))
    (if end-time
        (float-time (time-subtract end-time (current-time)))
      0)))

(defun org-pomodoro--instance-format-time (instance)
  "Format remaining time for INSTANCE."
  (let ((remaining (org-pomodoro--instance-remaining-seconds instance))
        (state (plist-get instance :state)))
    (format-seconds org-pomodoro-time-format
                    (if (eq state :overtime)
                        (- remaining)
                      remaining))))

;; Helper Functions

(defun org-pomodoro-active-p ()
  "Retrieve whether org-pomodoro is active or not."
  (org-pomodoro--any-active-p))

(defun org-pomodoro-sound-p (type)
  "Return whether to play sound of given TYPE."
  (cl-case type
    (:start org-pomodoro-start-sound-p)
    (:pomodoro org-pomodoro-finished-sound-p)
    (:overtime org-pomodoro-overtime-sound-p)
    (:killed org-pomodoro-killed-sound-p)
    (:short-break org-pomodoro-short-break-sound-p)
    (:long-break org-pomodoro-long-break-sound-p)
    (:tick org-pomodoro-ticking-sound-p)
    (t (error "Unknown org-pomodoro sound: %S" type))))

(defun org-pomodoro-sound (type)
  "Return the sound file for given TYPE."
  (cl-case type
    (:start org-pomodoro-start-sound)
    (:pomodoro org-pomodoro-finished-sound)
    (:overtime org-pomodoro-overtime-sound)
    (:killed org-pomodoro-killed-sound)
    (:short-break org-pomodoro-short-break-sound)
    (:long-break org-pomodoro-long-break-sound)
    (:tick org-pomodoro-ticking-sound)
    (t (error "Unknown org-pomodoro sound: %S" type))))

(defun org-pomodoro-sound-args (type)
  "Return the playback arguments for given TYPE."
  (cl-case type
    (:start org-pomodoro-start-sound-args)
    (:pomodoro org-pomodoro-finished-sound-args)
    (:overtime org-pomodoro-overtime-sound-args)
    (:killed org-pomodoro-killed-sound-args)
    (:short-break org-pomodoro-short-break-sound-args)
    (:long-break org-pomodoro-long-break-sound-args)
    (:tick org-pomodoro-ticking-sound-args)
    (t (error "Unknown org-pomodoro sound: %S" type))))

(defun org-pomodoro-play-sound (type)
  "Play an audio file specified by TYPE (:pomodoro, :short-break, :long-break)."
  (let ((sound (org-pomodoro-sound type))
        (args (org-pomodoro-sound-args type)))
    (cond ((and (fboundp 'sound-wav-play)
		org-pomodoro-play-sounds
		sound)
	   (sound-wav-play sound))
	  ((and org-pomodoro-audio-player
		org-pomodoro-play-sounds
		sound)
	   (start-process-shell-command
	    "org-pomodoro-audio-player" nil
	    (mapconcat 'identity
		       `(,org-pomodoro-audio-player
			 ,@(delq nil (list args (shell-quote-argument (expand-file-name sound)))))
		       " "))))))

(defun org-pomodoro-maybe-play-sound (type)
  "Play an audio file specified by TYPE."
  (when (org-pomodoro-sound-p type)
    (org-pomodoro-play-sound type)))

(defun org-pomodoro-update-mode-line ()
  "Set the modeline accordingly to the current state."
  (let ((parts '()))
    (dolist (instance (org-pomodoro--active-instances))
      (let* ((name (plist-get instance :name))
             (state (plist-get instance :state))
             (time-str (org-pomodoro--instance-format-time instance))
             (display-name (if (> (length name) 10)
                               (concat (substring name 0 10) "…")
                             name))
             (s (cl-case state
                  (:pomodoro
                   (propertize (format "%s~%s" display-name time-str)
                               'face 'org-pomodoro-mode-line))
                  (:overtime
                   (propertize (format "%s+%s" display-name time-str)
                               'face 'org-pomodoro-mode-line-overtime))
                  (:short-break
                   (propertize (format "%s·%s" display-name time-str)
                               'face 'org-pomodoro-mode-line-break))
                  (:long-break
                   (propertize (format "%s··%s" display-name time-str)
                               'face 'org-pomodoro-mode-line-break)))))
        (when s (push s parts))))
    (setq org-pomodoro-mode-line
          (if parts
              (list "[" (mapconcat #'identity (nreverse parts) "|") "] ")
            "")))
  (force-mode-line-update t))

;; Instance tick handler

(defun org-pomodoro--tick-instance (instance)
  "Handle tick for a single INSTANCE."
  (let ((state (plist-get instance :state))
        (name (plist-get instance :name)))
    (when (org-pomodoro--instance-active-p instance)
      (when (< (org-pomodoro--instance-remaining-seconds instance) 0)
        (cl-case state
          (:pomodoro (if org-pomodoro-manual-break
                         (org-pomodoro--overtime-instance instance)
                       (org-pomodoro--finish-instance instance)))
          (:short-break (org-pomodoro--short-break-finished-instance instance))
          (:long-break (org-pomodoro--long-break-finished-instance instance))))
      (run-hook-with-args 'org-pomodoro-tick-hook instance)
      (when (and (member state org-pomodoro-ticking-sound-states)
                 (equal (mod (truncate (org-pomodoro--instance-remaining-seconds instance))
                             org-pomodoro-ticking-frequency)
                        0))
        (org-pomodoro-maybe-play-sound :tick)))))

(defun org-pomodoro--global-tick ()
  "Global tick function that updates all instances."
  ;; Clean up instances with no timer
  (dolist (instance org-pomodoro--instances)
    (org-pomodoro--tick-instance instance))
  (org-pomodoro-update-mode-line))

;; Timer for global tick
(defvar org-pomodoro--global-timer nil
  "Global timer for updating all pomodoro instances.")

(defun org-pomodoro--ensure-global-timer ()
  "Ensure the global tick timer is running."
  (unless (and org-pomodoro--global-timer
               (timerp org-pomodoro--global-timer))
    (setq org-pomodoro--global-timer
          (run-with-timer 1 1 #'org-pomodoro--global-tick))))

(defun org-pomodoro--maybe-stop-global-timer ()
  "Stop global timer if no instances are active."
  (unless (org-pomodoro--any-active-p)
    (when (and org-pomodoro--global-timer
               (timerp org-pomodoro--global-timer))
      (cancel-timer org-pomodoro--global-timer)
      (setq org-pomodoro--global-timer nil))))

;; Instance state management

(defun org-pomodoro--set-instance-state (instance state)
  "Set INSTANCE to STATE and configure end-time."
  (let ((length (plist-get instance :length))
        (short-break (plist-get instance :short-break-length))
        (long-break (plist-get instance :long-break-length))
        (name (plist-get instance :name)))
    (plist-put instance :state state)
    (plist-put instance :end-time
               (cl-case state
                 (:pomodoro (time-add (current-time) (* 60 length)))
                 (:overtime (current-time))
                 (:short-break (time-add (current-time) (* 60 short-break)))
                 (:long-break (time-add (current-time) (* 60 long-break)))
                 (:none nil)))))

(defun org-pomodoro--start-instance (instance &optional state)
  "Start INSTANCE with optional STATE (default :pomodoro)."
  (let* ((name (plist-get instance :name))
         (target-state (or state :pomodoro))
         (length (plist-get instance :length)))
    ;; Ensure mode line is set up
    (unless global-mode-string (setq global-mode-string '("")))
    (unless (memq 'org-pomodoro-mode-line global-mode-string)
      (setq global-mode-string (append global-mode-string
                                       '(org-pomodoro-mode-line))))
    ;; Set state
    (org-pomodoro--set-instance-state instance target-state)
    (plist-put instance :last-clock-in (current-time))
    ;; Ensure global timer is running
    (org-pomodoro--ensure-global-timer)
    ;; Log and notify
    (when (eq target-state :pomodoro)
      (org-pomodoro--log "STARTED '%s' - %d minute pomodoro" name length)
      (org-pomodoro-maybe-play-sound :start)
      (run-hook-with-args 'org-pomodoro-started-hook name))
    (org-pomodoro-update-mode-line)
    (org-agenda-maybe-redo)))

(defun org-pomodoro--reset-instance (instance)
  "Reset INSTANCE to inactive state."
  (let ((name (plist-get instance :name)))
    (plist-put instance :state :none)
    (plist-put instance :end-time nil)
    (org-pomodoro-update-mode-line)
    (org-pomodoro--maybe-stop-global-timer)
    (org-agenda-maybe-redo)))

(defun org-pomodoro--remove-instance (instance)
  "Remove INSTANCE from the instances list."
  (let ((name (plist-get instance :name)))
    (setq org-pomodoro--instances
          (cl-remove-if (lambda (inst)
                          (string= (plist-get inst :name) name))
                        org-pomodoro--instances))
    (org-pomodoro--maybe-stop-global-timer)
    (org-pomodoro-update-mode-line)))

;; Event handlers for instances

(defun org-pomodoro--overtime-instance (instance)
  "Handle INSTANCE entering overtime."
  (let ((name (plist-get instance :name)))
    (org-pomodoro-maybe-play-sound :overtime)
    (org-pomodoro--log "OVERTIME '%s' - pomodoro time is up, waiting for break" name)
    (org-pomodoro-notify (format "Pomodoro '%s' completed!" name)
                         "Now on overtime. Start break by calling org-pomodoro.")
    (org-pomodoro--set-instance-state instance :overtime)
    (run-hook-with-args 'org-pomodoro-overtime-hook name)))

(defun org-pomodoro--finish-instance (instance)
  "Handle INSTANCE finishing successfully."
  (let* ((name (plist-get instance :name))
         (count (1+ (plist-get instance :count)))
         (freq org-pomodoro-long-break-frequency))
    (unless org-pomodoro-clock-break
      (when (org-clocking-p)
        (org-clock-out nil t)))
    (org-pomodoro-maybe-play-sound :pomodoro)
    (plist-put instance :count count)
    (org-pomodoro--log "FINISHED '%s' - pomodoro #%d completed!" name count)
    ;; Start break
    (if (zerop (mod count freq))
        (progn
          (org-pomodoro--log "LONG BREAK '%s' - starting %d minute long break"
                             name (plist-get instance :long-break-length))
          (org-pomodoro--set-instance-state instance :long-break))
      (org-pomodoro--log "SHORT BREAK '%s' - starting %d minute short break"
                         name (plist-get instance :short-break-length))
      (org-pomodoro--set-instance-state instance :short-break))
    (org-pomodoro-notify (format "Pomodoro '%s' completed!" name)
                         "Time for a break.")
    (run-hook-with-args 'org-pomodoro-finished-hook name)
    (org-pomodoro-update-mode-line)
    (org-agenda-maybe-redo)))

(defun org-pomodoro--kill-instance (instance)
  "Kill INSTANCE."
  (let ((name (plist-get instance :name))
        (state (plist-get instance :state)))
    (org-pomodoro--log "KILLED '%s' - %s was terminated"
                       name
                       (cl-case state
                         (:pomodoro "pomodoro")
                         (:short-break "short break")
                         (:long-break "long break")
                         (:overtime "overtime")
                         (t "timer")))
    (org-pomodoro--remove-instance instance)
    (org-pomodoro-notify (format "Pomodoro '%s' killed." name)
                         "One does not simply kill a pomodoro!")
    (org-pomodoro-maybe-play-sound :killed)
    (when (org-clocking-p)
      (if org-pomodoro-keep-killed-pomodoro-time
          (org-clock-out nil t)
        (org-clock-cancel)))
    (run-hook-with-args 'org-pomodoro-killed-hook name)))

(defun org-pomodoro--short-break-finished-instance (instance)
  "Handle short break finished for INSTANCE."
  (let ((name (plist-get instance :name)))
    (when org-pomodoro-clock-break
      (when (org-clocking-p)
        (org-clock-out nil t)))
    (org-pomodoro--log "SHORT BREAK FINISHED '%s' - ready for another pomodoro" name)
    (org-pomodoro--remove-instance instance)
    (org-pomodoro-notify (format "Short break '%s' finished." name)
                         "Ready for another pomodoro?")
    (org-pomodoro-maybe-play-sound :short-break)
    (run-hook-with-args 'org-pomodoro-break-finished-hook name)
    (run-hook-with-args 'org-pomodoro-short-break-finished-hook name)))

(defun org-pomodoro--long-break-finished-instance (instance)
  "Handle long break finished for INSTANCE."
  (let ((name (plist-get instance :name)))
    (when org-pomodoro-clock-break
      (when (org-clocking-p)
        (org-clock-out nil t)))
    (org-pomodoro--log "LONG BREAK FINISHED '%s' - ready for another pomodoro" name)
    (org-pomodoro--remove-instance instance)
    (org-pomodoro-notify (format "Long break '%s' finished." name)
                         "Ready for another pomodoro?")
    (org-pomodoro-maybe-play-sound :long-break)
    (run-hook-with-args 'org-pomodoro-break-finished-hook name)
    (run-hook-with-args 'org-pomodoro-long-break-finished-hook name)))

;; Notification

(defun org-pomodoro-notify (title message)
  "Send a notification with TITLE and MESSAGE using `alert'."
  (alert message :title title :category 'org-pomodoro))

;; Legacy functions for backwards compatibility

(defun org-pomodoro-remaining-seconds ()
  "Return the number of seconds remaining in the current phase as a float.
For multi-pomodoro, returns seconds for the first active instance."
  (let ((active (car (org-pomodoro--active-instances))))
    (if active
        (org-pomodoro--instance-remaining-seconds active)
      0)))

(defun org-pomodoro-format-seconds ()
  "Format the time remaining in the current phase.
For multi-pomodoro, returns time for the first active instance."
  (let ((active (car (org-pomodoro--active-instances))))
    (if active
        (org-pomodoro--instance-format-time active)
      "00:00")))

(defun org-pomodoro-kill ()
  "Kill the current timer, reset the phase and update the modeline.
For multi-pomodoro, prompts to select which instance to kill."
  (interactive)
  (let ((active (org-pomodoro--active-instances)))
    (cond
     ((null active)
      (org-pomodoro--log "No active pomodoros to kill")
      (message "No active pomodoros."))
     ((= (length active) 1)
      (org-pomodoro--kill-instance (car active)))
     (t
      (let* ((names (mapcar (lambda (inst) (plist-get inst :name)) active))
             (selected (completing-read "Kill which pomodoro? " names nil t)))
        (org-pomodoro--kill-instance (org-pomodoro--get-instance selected)))))))

(defun org-pomodoro-reset ()
  "Reset org-pomodoro state (kills all active pomodoros)."
  (interactive)
  (dolist (instance (org-pomodoro--active-instances))
    (org-pomodoro--remove-instance instance))
  (org-pomodoro--log "All pomodoros reset")
  (org-pomodoro-update-mode-line)
  (org-agenda-maybe-redo))

(defun org-pomodoro-extend-last-clock ()
  "Extends last clock to `current-time'."
  (interactive)
  (save-window-excursion
    (org-clock-goto)
    (when (re-search-forward ":LOGBOOK:" (save-excursion (outline-next-heading)) t)
      (org-flag-drawer nil))
    (let ((last-clock (car org-clock-history)))
      (switch-to-buffer (marker-buffer last-clock))
      (goto-char last-clock)
      (let ((item-end (save-excursion (org-end-of-subtree t))))
        (when (re-search-forward "CLOCK: \\(\\[.*?\\]\\)" item-end t)
          (kill-line)
          (org-clock-clock-out
           (cons (copy-marker (match-end 1) t)
                 (org-time-string-to-time (match-string 1)))))))))

(defun org-pomodoro--read-duration (prompt default)
  "Read a duration in minutes from the user.
PROMPT is the prompt string, DEFAULT is shown as the default value."
  (let ((input (read-string (format "%s (default %d): " prompt default)
                            nil nil (number-to-string default))))
    (if (string-empty-p input)
        default
      (let ((num (string-to-number input)))
        (if (> num 0)
            num
          (user-error "Duration must be a positive number"))))))

(defun org-pomodoro--prompt-custom-lengths (arg)
  "Prompt user for custom lengths based on prefix ARG.
C-u (4): prompt for work length only.
C-u C-u (16): prompt for work and break lengths.
Returns a plist of (:length L :short-break S :long-break L) or nil."
  (when arg
    (cond
     ((equal arg '(4))
      ;; C-u: prompt for work length
      (list :length (org-pomodoro--read-duration "Pomodoro length (minutes)"
                                                  org-pomodoro-length)))
     ((equal arg '(16))
      ;; C-u C-u: prompt for work and break lengths
      (list :length (org-pomodoro--read-duration "Pomodoro length (minutes)"
                                                  org-pomodoro-length)
            :short-break (org-pomodoro--read-duration "Short break length (minutes)"
                                                       org-pomodoro-short-break-length)
            :long-break (org-pomodoro--read-duration "Long break length (minutes)"
                                                      org-pomodoro-long-break-length))))))

;;;###autoload
(defun org-pomodoro (&optional arg)
  "Start a new pomodoro or manage existing ones.

When no timer is running for `org-pomodoro` a new pomodoro is started and
the current task is clocked in.  You will be prompted to name the pomodoro.

If pomodoros are already running, you can start additional ones (each with
a unique name) or manage existing ones.

With prefix argument:
  C-u: Prompt for custom pomodoro length.
  C-u C-u: Prompt for custom pomodoro and break lengths."
  (interactive "P")
  ;; Prompt for name
  (let* ((existing-names (mapcar (lambda (inst) (plist-get inst :name))
                                 org-pomodoro--instances))
         (default-name (if (and (eq major-mode 'org-mode)
                                (org-get-heading t t t t))
                           (substring-no-properties (org-get-heading t t t t))
                         "pomodoro"))
         (name (read-string (format "Pomodoro name (default: %s): " default-name)
                            nil nil default-name))
         (existing (org-pomodoro--get-instance name)))
    ;; Check if this name already exists
    (cond
     ;; Existing instance in overtime - finish it
     ((and existing (eq (plist-get existing :state) :overtime))
      (org-pomodoro--finish-instance existing))
     ;; Existing instance active - offer to kill
     ((and existing (org-pomodoro--instance-active-p existing))
      (let ((state-name (cl-case (plist-get existing :state)
                          (:pomodoro "pomodoro")
                          (:short-break "short break")
                          (:long-break "long break")
                          (t "timer"))))
        (if (or (not org-pomodoro-ask-upon-killing)
                (y-or-n-p (format "'%s' is already running (%s, %s remaining). Kill it? "
                                  name state-name
                                  (org-pomodoro--instance-format-time existing))))
            (org-pomodoro--kill-instance existing)
          (message "Timer continues. Use `org-pomodoro' again to manage it."))))
     ;; Start new pomodoro
     (t
      ;; Get custom lengths if prefix arg
      (let* ((custom (org-pomodoro--prompt-custom-lengths arg))
             (length (or (plist-get custom :length) org-pomodoro-length))
             (short-break (or (plist-get custom :short-break) org-pomodoro-short-break-length))
             (long-break (or (plist-get custom :long-break) org-pomodoro-long-break-length))
             (instance (org-pomodoro--make-instance name length short-break long-break)))
        ;; Add to instances list
        (push instance org-pomodoro--instances)
        ;; Log settings if custom
        (when custom
          (org-pomodoro--log "Custom settings for '%s': %d min work, %d min short break, %d min long break"
                             name length short-break long-break))
        ;; Clock in based on context
        (cond
         ((memq major-mode (list 'org-mode 'org-journal-mode))
          (call-interactively 'org-clock-in))
         ((eq major-mode 'org-agenda-mode)
          (org-with-point-at (org-get-at-bol 'org-hd-marker)
            (call-interactively 'org-clock-in)))
         (t (let ((current-prefix-arg '(4)))
              (call-interactively 'org-clock-in))))
        ;; Start the pomodoro
        (org-pomodoro--start-instance instance :pomodoro))))))

;;;###autoload
(defun org-pomodoro-status ()
  "Display the current pomodoro status for all active instances."
  (interactive)
  (let ((active (org-pomodoro--active-instances)))
    (if active
        (progn
          (org-pomodoro--log "STATUS - %d active pomodoro(s):" (length active))
          (dolist (instance active)
            (let* ((name (plist-get instance :name))
                   (state (plist-get instance :state))
                   (state-name (cl-case state
                                 (:pomodoro "Pomodoro")
                                 (:overtime "Overtime")
                                 (:short-break "Short break")
                                 (:long-break "Long break")))
                   (remaining (org-pomodoro--instance-format-time instance))
                   (count (plist-get instance :count)))
              (org-pomodoro--log "  '%s': %s, %s remaining (count: %d)"
                                 name state-name remaining count)))
          (message "See *Messages* for detailed status."))
      (org-pomodoro--log "STATUS - No active pomodoros")
      (message "No active pomodoros."))))

;;;###autoload
(defun org-pomodoro-list ()
  "List all active pomodoros and allow selection to kill."
  (interactive)
  (let ((active (org-pomodoro--active-instances)))
    (if (null active)
        (message "No active pomodoros.")
      (let* ((choices (mapcar (lambda (inst)
                                (let ((name (plist-get inst :name))
                                      (state (plist-get inst :state))
                                      (time (org-pomodoro--instance-format-time inst)))
                                  (cons (format "%s [%s] %s"
                                                name
                                                (cl-case state
                                                  (:pomodoro "WORK")
                                                  (:short-break "SHORT")
                                                  (:long-break "LONG")
                                                  (:overtime "OVERTIME"))
                                                time)
                                        inst)))
                              active))
             (selected (completing-read "Pomodoros (select to kill): "
                                        (mapcar #'car choices) nil t)))
        (when selected
          (let ((instance (cdr (assoc selected choices))))
            (when (y-or-n-p (format "Kill '%s'? " (plist-get instance :name)))
              (org-pomodoro--kill-instance instance))))))))

(provide 'org-pomodoro)

;;; org-pomodoro.el ends here
