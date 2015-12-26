;;; poptoshell.el --- easily manage interaction with multiple shells

;; Copyright (C) 1999-2011 Free Software Foundation, Inc. and Ken Manheimer

;; Author: Ken Manheimer <ken dot manheimer at gmail...>
;; Maintainer: Ken Manheimer <ken dot manheimer at gmail...>
;; Created: 1999 -- first public release
;; Keywords: processes
;; Website: https://github.com/kenmanheimer/EmacsUtils
;;
;;; Commentary:
;;
;; See the pop-to-shell docstring for details.
;; I bind to M-<space>, via eg: (global-set-key "\M- " 'pop-to-shell)
;;
;; TODO:
;; * Change name to multishell.
;;   - Most functions will be prefixed, eg multishell:pop-to-shell
;; * Preservable (savehist) history that associates names with paths
;;   - Using an association list between names and paths
;;   - Searched for search backwards/forwards on isearch-like M-r/M-s bindings
;;     - *Not* searched for regular completion
;;   - Editible
;;     - Using isearch keybinding M-e
;;     - Edits path
;;     - New association overrides previous
;;     - Deleting path removes association and history entry
;; * Customize provision for activating the saves
;;   - Customize entry has warning about activating savehist
;;   - Adds the name/path association list to savehist-additional-variables
;;   - Activates savehist, if inactive
;; * Customize provision for keybinding
;;   - See allout.el allout-command-prefix for dynamic customization example.

(defvar non-interactive-process-buffers '("*compilation*" "*grep*"))

(require 'comint)
(require 'shell)

(defgroup multishell nil
  "Allout extension that highlights outline structure graphically.

Customize `allout-widgets-auto-activation' to activate allout-widgets
with allout-mode."
  :group 'shell)

(defcustom multishell:non-interactive-process-buffers
  '("*compilation*" "*grep*")
  "Buffers with processes but not for interaction."
  :type '(repeat string)
  :group 'multishell)
(defcustom multishell:command-key "\M- "
  "Choose a key to use for "
  :type 'string
  :group 'multishell)
(defun multishell:assert-command-key-or-not (&optional force)
  "Activate multishell command key if customizations dictate.

If optional FORCE is true and customizations dictate, globally
unbind the key.

* `multishell:assert-command-key' - Establish or keep the binding if true
* `multishell:command-key' - Which key to use."
  (if (boundp 'multishell:assert-command-key)
      (cond ((and multishell:assert-command-key multishell:command-key)
             (global-set-key multishell:command-key 'pop-to-shell))
            ((and force (not multishell:assert-command-key))
             (global-unset-key multishell:command-key)))
    )
  )
(defun multishell:do-assert-command-key (option-name option-value)
  "If `multishell:assert-command-key', globally bind pop-to-shell.

Use keybinding identified by `multishell:command-key'."
  (multishell:assert-command-key-or-not (not option-value))
  )
(defcustom multishell:assert-command-key nil
  "Set this to impose the `multishell-command-key binding."
  :type 'boolean
  :set 'multishell:do-assert-command-key
  :group 'multishell)

(defcustom multishell:pop-to-frame nil
  "*If non-nil, jump to a frame already showing the shell, if any.

Otherwise, open a new window in the current frame."
  :type 'boolean
  :group 'shell)

(defcustom pop-to-shell-persist-shell-names nil
  "Remember shell name/path associations across sessions. Note well:
This will activate minibuffer history persistence, in general, if it's not
already active."
  :type 'boolean
  :group 'shell)

(defvar multishell:name-path-assoc nil
  "Assoc list from name to path")

(defvar pop-to-shell-primary-name "*shell*"
  "Shell name to use for un-modified pop-to-shell buffer target.")
(defvar multishell:buffer-name-history nil
  "Distinct pop-to-shell completion history container.")

(defun pop-to-shell (&optional arg)

  "Navigate to or within local and remote shell buffers.

Use universal arguments to launch and choose between alternate
shell buffers and to select which is default.  Prepend a path to
a new shell name to launch a shell in that directory, and use
Emacs tramp syntax to launch a remote shell.

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
2. Activate Savehist Mode, if not already activated.
3. Save."

  (interactive "P")

  (if (not (boundp 'shell-buffer-name))
      (setq shell-buffer-name "*shell*"))

  (let* ((from-buffer (current-buffer))
         (doublearg (equal arg '(16)))
         (temp (if arg
                   (multishell:read-bare-shell-buffer-name
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
     (t (if (and multishell:pop-to-frame
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

(defun multishell:read-bare-shell-buffer-name (prompt default)
  "PROMPT for shell buffer name, sans asterisks.

Return the supplied name bracketed with the asterisks, or specified DEFAULT
on empty input."
  (let* ((candidates (append
                      (remq nil
                            (mapcar (lambda (buffer)
                                      (let ((name (buffer-name buffer)))
                                        (if (with-current-buffer buffer
                                              (eq major-mode 'shell-mode))
                                            ;; Shell mode buffers.
                                            (if (> (length name) 2)
                                                ;; Strip asterisks.
                                                (substring name 1
                                                           (1- (length name)))
                                              name))))
                                    (buffer-list)))))
         (got (completing-read prompt
                               candidates ; COLLECTION
                               nil        ; PREDICATE
                               'confirm   ; REQUIRE-MATCH
                               nil        ; INITIAL-INPUT
                               'multishell:buffer-name-history ; HIST
                               )))
    (if (not (string= got "")) (bracket-asterisks got) default)))

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
    (shell-mode)
    (when (and (file-remote-p default-directory)
             (not (comint-check-proc (current-buffer))))
      (message "(Re)connection failed, doing a cleanup then retry...")
      (sleep-for 0)
      (tramp-cleanup-this-connection)
      (shell-mode))))

(provide 'poptoshell)
