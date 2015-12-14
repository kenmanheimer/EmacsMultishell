;;; poptoshell.el --- get to the process buffer and input mark

;; Copyright (C) 1999-2011 Free Software Foundation, Inc. and Ken Manheimer

;; Author: Ken Manheimer <ken dot manheimer at gmail...>
;; Maintainer: Ken Manheimer <ken dot manheimer at gmail...>
;; Created: 1999 -- first public release
;; Keywords: processes
;; Website: https://github.com/kenmanheimer/EmacsUtils
;;
;;; Commentary:
;;
;; I bind to M-<space>, via eg: (global-set-key "\M- " 'pop-to-shell)
;; See the pop-to-shell docstring for details.
;;
;; klm, 02/09/1999.

(defvar non-interactive-process-buffers '("*compilation*" "*grep*"))

(require 'comint)
(require 'shell)

(defcustom pop-to-shell-frame nil
  "*If non-nil, jump to a frame already showing the shell, if any.

Otherwise, open a new window in the current frame."
  :type 'boolean
  :group 'comint)

(defvar pop-to-shell-primary-name "*shell*"
  "Shell name to use for un-modified pop-to-shell buffer target.")

(defun pop-to-shell (&optional arg)

  "Navigate to or within local and remote shell buffers.

Use universal arguments to launch and choose between alternate
shell buffers, select which is default.  With Emacs tramp syntax,
launch or return to a remote shell.

==== Basic operation:

 - If the current buffer is associated with a subprocess (that is
   not among those named on `non-interactive-process-buffers'),
   then focus is moved to the process input point.

   \(You can use a universal argument go to a different shell
   buffer when already in a buffer that has a process - see
   below.)

 - If not in a shell buffer (or with universal argument), go to a
   window that is already showing the (a) shell buffer, if any.

   In this case, the cursor is left in its prior position in the
   shell buffer. (Repeating the command will then go to the
   process input point, by the previous behavior.)

 - Otherwise, start a new shell buffer, using the current
   directory as the working directory..

If the resulting buffer exists and its shell process was
disconnected or otherwise stopped, it's resumed.

===== Universal arg to start and select between named shell buffers:

You can name alternate shell buffers to create or return to using
single or doubled universal arguments:

 - With a single universal argument, prompt for the buffer name
   to use (without the asterisks that shell mode will put around
   the name), defaulting to 'shell'.

   Completion is available.

   This combination makes it easy to start and switch between
   multiple shell buffers.

 - A double universal argument will prompt for the name *and* set
   the default to that name, so the target shell becomes the
   primary.

===== Select starting directory and remote host:

The shell buffer name you give to the prompt for a universal arg
can include a preceding path. That will be used for the startup
directory - and can include tramp remote syntax to specify a
remote shell. If there is an element after a final '/', that's used for the buffer name. Otherwise, the host, domain, or path is used.

For example: '/ssh:myriadicity.net:/' or
'/ssh:myriadicity.net|sudo:root@myriadicity.net:/\#myr', etc.
The stuff between the '/' slashes will be used for
starting the remote shell, and the stuff after the second
slash will be used for the shell name.

===== Persisting your alternate shell buffer names and paths:

You can use emacs builtin SaveHist to preserve your alternate
shell buffer names and paths across emacs sessions. To do so,
customize the `savehist' group, and:

1. Add `pop-to-shell-buffer-name-history' to Savehist Additional Variables.
2. Activate Savehist Mode
3. Save."

  (interactive "P")

  (if (not (boundp 'shell-buffer-name))
      (setq shell-buffer-name "*shell*"))

  (let* ((from-buffer (current-buffer))
         (doublearg (equal arg '(16)))
         (temp (if arg
                   (read-bare-shell-buffer-name
                    (format "Shell buffer name [%s]%s "
                            (substring-no-properties
                             pop-to-shell-primary-name
                             1 (- (length pop-to-shell-primary-name) 1))
                            (if doublearg " <==" ":"))
                    pop-to-shell-primary-name)
                 pop-to-shell-primary-name))
         ;; Make sure it is bracketed with asterisks; silly.
         use-default-dir
         (target-shell-buffer-name
          ;; Derive target name, and default-dir if any, from temp.
          (cond ((string= temp "") pop-to-shell-primary-name)
                ((string-match "^\\*\\(/.*/\\)\\(.*\\)\\*" temp)
                 (setq use-default-dir (match-string 1 temp))
                 (bracket-asterisks 
                  (if (string= (match-string 2 temp) "")
                      (let ((v (tramp-dissect-file-name
                                use-default-dir)))
                        (or (tramp-file-name-host v)
                            (tramp-file-name-domain v)
                            (tramp-file-name-localname v)
                            use-default-dir))
                    (match-string 2 temp))))
                (t (bracket-asterisks temp))))
         (curr-buff-proc (get-buffer-process from-buffer))
         (target-buffer (if (and curr-buff-proc
                        (not (member (buffer-name from-buffer)
                                     non-interactive-process-buffers)))
                   from-buffer
                 (get-buffer target-shell-buffer-name)))
         inwin
         already-there)

    (when doublearg
      (setq pop-to-shell-primary-name target-shell-buffer-name))

    ;; Situate:

    (cond 

     ((and curr-buff-proc
           (not arg)
           (eq from-buffer target-buffer)
           (not (eq target-shell-buffer-name (buffer-name from-buffer))))
      ;; In a shell buffer, but not named - stay in buffer, but go to end.
      (setq already-there t))

     ((string= (buffer-name) target-shell-buffer-name)
      ;; Already in the specified shell buffer:
      (setq already-there t))

     ((or (not target-buffer)
          (not (setq inwin (get-visible-win-for-buffer target-buffer))))
      ;; No preexisting shell buffer, or not in a visible window:
      (pop-to-buffer target-shell-buffer-name pop-up-windows))

       ;; Buffer exists and already has a window - jump to it:
     (t (if (and pop-to-shell-frame
                 inwin
                 (not (equal (window-frame (selected-window))
                             (window-frame inwin))))
            (select-frame-set-input-focus (window-frame inwin)))
        (if (not (string= (buffer-name (current-buffer))
                          target-shell-buffer-name))
            (pop-to-buffer target-shell-buffer-name t))))

    ;; We're in the buffer.

    ;; If we have a use-default-dir, impose it:
    (when use-default-dir
        (cd use-default-dir))

    ;; Activate:

    (if (not (comint-check-proc (current-buffer)))
        (start-shell-in-buffer (buffer-name (current-buffer))))

    ;; If the destination buffer has a stopped process, resume it:
    (let ((process (get-buffer-process (current-buffer))))
      (if (and process (equal 'stop (process-status process)))
          (continue-process process)))
    (if (and (not already-there)
             (not (equal (current-buffer) from-buffer)))
        t
      (goto-char (point-max))
      (and (get-buffer-process from-buffer)
           (goto-char (process-mark (get-buffer-process from-buffer)))))
    )
)

(defun get-visible-win-for-buffer (buffer)
  "Return visible window containing buffer."
  (catch 'got-a-vis
    (walk-windows
     (function (lambda (win)
                 (if (and (eq (window-buffer win) buffer)
                          (equal (frame-parameter
                                  (selected-frame) 'display)
                                 (frame-parameter
                                  (window-frame win) 'display)))
                     (throw 'got-a-vis win))))
     nil 'visible)
    nil))

(defun read-bare-shell-buffer-name (prompt default)
  "PROMPT for shell buffer name, sans asterisks.

Return the supplied name bracketed with the asterisks, or specified DEFAULT
on empty input."
  (let ((got
         (completing-read
          prompt
          ;; COLLECTION:
          (pop-to-shell-buffer-name-candidates)
          ;; PREDICATE:
          nil
          ;; REQUIRE-MATCH:
          'confirm
          ;; INITIAL-INPUT:
          nil
          ;; HIST:
          'pop-to-shell-buffer-name-history
          )))
    (if (not (string= got "")) (bracket-asterisks got) default)))

(defun pop-to-shell-buffer-name-candidates ()
  "Return a list of the shell buffer name candidates.

The list consists of the combination of existing shell buffer
names plus the names in the history (which can include
non-existent buffers, from saved history)."
 (append (remq nil
               (mapcar (lambda (buffer)
                         (let ((name (buffer-name buffer)))
                           (if (with-current-buffer buffer
                                 (eq major-mode 'shell-mode))
                               (if (> (length name) 2)
                                   (substring name 1 (1- (length
                                                          name)))
                                 name))))
                       (buffer-list)))
         pop-to-shell-buffer-name-history)
)

(defun bracket-asterisks (name)
  "Return a copy of name, ensuring it has an asterisk at the beginning and end."
  (if (not (string= (substring name 0 1) "*"))
      (setq name (concat "*" name)))
  (if (not (string= (substring name -1) "*"))
      (setq name (concat name "*")))
  name)
(defun unbracket-asterisks (name)
  "Return a copy of name, removing asterisks, if any, at beginning and end."
  (if (string= (substring name 0 1) "*")
      (setq name (substring name 1)))
  (if (string= (substring name -1) "*")
      (setq name (substring name 0 -1)))
  name)
(defun start-shell-in-buffer (buffer-name)
  ;; Damn comint requires buffer name be bracketed by "*" asterisks.
  (require 'comint)
  (require 'shell)

  (let* ((buffer buffer-name)
         (prog (or explicit-shell-file-name
                   (getenv "ESHELL")
                   (getenv "SHELL")
                   "/bin/sh"))
         (name (file-name-nondirectory prog))
         (startfile (concat "~/.emacs_" name))
         (xargs-name (intern-soft (concat "explicit-" name "-args"))))
    (setq buffer (set-buffer (apply 'make-comint
                                    (unbracket-asterisks buffer-name)
                                    prog
                                    (if (file-exists-p startfile)
                                        startfile)
                                    (if (and xargs-name
                                             (boundp xargs-name))
                                        (symbol-value xargs-name)
                                      '("-i")))))
    (set-buffer buffer-name)
    (shell-mode)))

(provide 'poptoshell)
