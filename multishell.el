;;; multishell.el --- facilitate multiple local and remote shell buffers

;; Copyright (C) 1999-2016 Free Software Foundation, Inc. and Ken Manheimer

;; Author: Ken Manheimer <ken.manheimer@gmail.com>
;; Version: 1.0.7
;; Created: 1999 -- first public availability
;; Keywords: processes
;; URL: https://github.com/kenmanheimer/EmacsMultishell
;;
;;; Commentary:
;;
;; Easily use and navigate multiple shell buffers, including remote shells.
;; Fundamentally, multishell is the function `multishell-pop-to-shell' -
;; a la `pop-to-buffer' - plus a keybinding. Together, they enable you to:
;;
;; * Get to the input point from wherever you are in a shell buffer,
;;   ... or to any of your shell buffers, from anywhere inside emacs.
;;
;; * Use universal arguments to launch and choose among alternate shell buffers,
;;   ... and change which is the current default.
;;
;; * Easily restart disconnected shells, or shells from prior sessions
;;   ... the latter from Emacs builtin savehist minibuf history persistence
;;
;; * Append a path to a new shell name to launch a shell in that directory,
;;   ... and use a path with Emacs tramp syntax to launch a remote shell -
;;   for example:
;;
;;   * `#root/sudo:root@localhost:/etc` for a buffer named "#root" with a
;;     root shell starting in /etc.
;;
;;   * `/ssh:example.net:/` for a shell buffer in / on example.net.
;;     The buffer will be named "*example.net*".
;;
;;   * `#ex/ssh:example.net|sudo:root@example.net:/etc` for a root shell
;;     starting in /etc on example.net named "*#ex*".
;;
;;   * 'interior/ssh:gateway.corp.com|ssh:interior.corp.com:' to go via
;;     gateway.corp.com to your homedir on interior.corp.com.  The buffer
;;     will be named "*interior*". You could append a sudo hop, and so on.
;;
;; * Thanks to tramp, file visits from the shell will seamlessly be in
;;   the auspices of the target account, and relative to the current
;;   directory, on the host where the shell is running.
;; 
;; See the `multishell-pop-to-shell` docstring for details.
;;
;; Customize-group `multishell' to select and activate a keybinding and set
;; various behaviors. Customize-group `savehist' to preserve buffer
;; names/paths across emacs restarts.
;;
;; Please use
;; [the multishell repository](https://github.com/kenmanheimer/EmacsMultishell)
;; issue tracker to report problems, suggestions, etc.
;;
;; Change Log:
;;
;; * 2016-01-23 1.0.7 Ken Manheimer:
;;   - Remove notes about tramp remote+sudo+homedir problem. Apparently it's
;;     due to something in my local site configuration (happens with -q but
;;     not -Q).
;; * 2016-01-22 1.0.6 Ken Manheimer:
;;   - Add multishell-version function.
;;   - Tweak commentary/comments/docstrings.
;;   - Null old multishell-buffer-name-history var, if present.
;; * 2016-01-16 1.0.5 Ken Manheimer:
;;   - History now includes paths, when designated.
;;   - Actively track current directory in history entries that have a path.
;;     Custom control: multishell-history-entry-tracks-current-directory
;;   - Offer to remove shell's history entry when buffer is killed.
;;     (Currently the only UI mechanism to remove history entries.)
;;   - Fix - prevent duplicate entries for same name but different paths
;;   - Fix - recognize and respect tramp path syntax to start in home dir
;;   - Simplify history var name, migrate existing history if any from old name
;; * 2016-01-04 1.0.4 Ken Manheimer - Released to ELPA
;; * 2016-01-02 Ken Manheimer - working on this in public, but not yet released.
;;
;; TODO:
;;
;; * Find suitable, internally consistent ways to tidy completions, eg:
;;   - first list completions for active shells, then present but inactive,
;;     then historical
;;   - some way for user to toggle between presenting just buffer names vs
;;     full buffer/path
;;     - without cutting user off from easy editing of path
;; * Try minibuffer field boundary at beginning of tramp path, to see whether
;;   the field boundary magically enables tramp path completion.
;; * Assess whether deletion of history entry via kill-buffer is sufficient.

;;; Code:

(require 'comint)
(require 'shell)
(require 'savehist)

(defvar multishell-version "1.0.7")
(defun multishell-version (&optional here)
  "Return string describing the loaded multishell version."
  (interactive "P")
  (let ((msg (concat "Multishell " multishell-version)))
    (if here (insert msg)
      (if (called-interactively-p 'interactive)
          (message "%s" msg)
        msg))))

(defgroup multishell nil
  "Allout extension that highlights outline structure graphically.

Customize `allout-widgets-auto-activation' to activate allout-widgets
with allout-mode."
  :group 'shell)

(defcustom multishell-command-key "\M- "
  "The key to use if `multishell-activate-command-key' is true.

You can instead manually bind `multishell-pop-to-shell` using emacs
lisp, eg: (global-set-key \"\\M- \" 'multishell-pop-to-shell)."
  :type 'key-sequence
  :group 'multishell)

(defvar multishell--responsible-for-command-key nil
  "Coordination for multishell key assignment.")
(defun multishell-activate-command-key-setter (symbol setting)
  "Implement `multishell-activate-command-key' choice."
  (set-default 'multishell-activate-command-key setting)
  (when (or setting multishell--responsible-for-command-key)
    (multishell-implement-command-key-choice (not setting))))
(defun multishell-implement-command-key-choice (&optional unbind)
  "If settings dicate, implement binding of multishell command key.

If optional UNBIND is true, globally unbind the key.

* `multishell-activate-command-key' - Set this to get the binding or not.
* `multishell-command-key' - The key to use for the binding, if appropriate."
  (cond (unbind
         (when (and (boundp 'multishell-command-key) multishell-command-key)
           (global-unset-key multishell-command-key)))
        ((not (and (boundp 'multishell-activate-command-key)
                   (boundp 'multishell-command-key)))
         nil)
        ((and multishell-activate-command-key multishell-command-key)
         (setq multishell--responsible-for-command-key t)
         (global-set-key multishell-command-key 'multishell-pop-to-shell))))

(defcustom multishell-activate-command-key nil
  "Set this to impose the `multishell-command-key' binding.

You can instead manually bind `multishell-pop-to-shell` using emacs
lisp, eg: (global-set-key \"\\M- \" 'multishell-pop-to-shell)."
  :type 'boolean
  :set 'multishell-activate-command-key-setter
  :group 'multishell)

;; Implement the key customization whenever the package is loaded:
(with-eval-after-load "multishell"
  (multishell-implement-command-key-choice))

(defcustom multishell-pop-to-frame nil
  "*If non-nil, jump to a frame already showing the shell, if another one is.

Otherwise, disregard already-open windows on the shell if they're
in another frame, and open a new window on the shell in the
current frame.

\(Use `pop-up-windows' to change multishell other-window vs
current-window behavior.)"
  :type 'boolean
  :group 'multishell)

(defcustom multishell-history-entry-tracks-current-directory t
  "Maintain shell's current directory in its multishell history entry.

When set, the history entry for shells started with explicit
paths will track the shell's current working directory. (Explicit
paths will not be added to local shells started without one,
however.)

If `savehist-save-minibuffer-history' is enabled, the current
working directory of shells \(that were started with an explicit
path) will be conveyed between emacs sessions."
 :type 'boolean
 :group 'multishell)

(defvar multishell-history nil
  "Name/path entries, most recent first.")
;; Migrate the few pre 1.0.5 users to changed history var:
(when (and (not multishell-history)
           (boundp 'multishell-buffer-name-history)
           multishell-buffer-name-history)
  (setq multishell-history multishell-buffer-name-history
        multishell-buffer-name-history nil))

(defvar multishell-primary-name "*shell*"
  "Default shell name for un-modified multishell-pop-to-shell buffer target.

This is adjusted by `multishell-pop-to-shell' when it is
invoked (with doubled universal argument) to set the default.

To preserve changes to this setting across emacs restarts, add it
to `savehist-additional-variables' by customizing the latter.")

;;; Can't just add multishell-primary-name to savehist-additional-variables
;;; - it'll be lost any time the user runs emacs without loading
;;; multishell.  So instead, inform the user that they can customize
;;; savehist-additional-variables.
;;;
;;; I suspect that including savehist-additional-variables *on*
;;; savehist-additional-variables could avoid this problem, as long as it
;;; doesn't conflict with user customizations. However, even if that works,
;;; doing so from multishell would change a behavior (for the better, but)
;;; beyond multishell's scope, making the change hard to track down.

;; (when (not (member 'multishell-primary-name
;;                    savehist-additional-variables))
;;   (setq savehist-additional-variables
;;         (cons 'multishell-primary-name savehist-additional-variables)))

;; Multiple entries happen because completion also adds name to history.
(defun multishell-register-name-to-path (name path)
  "Add or replace entry associating NAME with PATH in `multishell-history'.

If NAME already had a PATH and new PATH is empty, retain the prior one.

Promote added/changed entry to the front of the list."
  ;; Add or promote to the front, tracking path changes in the process.
  (let* ((entries (multishell-history-entries name))
         (path (or path "")))
    (dolist (entry entries)
      (when (string= path "")
        ;; Retain explicit established path.
        (setq path (cadr (multishell-split-entry entry))))
      (setq multishell-history (delete entry multishell-history)))
    (setq multishell-history (push (concat name path)
                                   multishell-history))))

(defun multishell-history-entries (name)
  "Return `multishell-history' entry that starts with NAME, or nil if none."
  (let ((match-expr (concat "^" name "\\\(/.*$\\\)?$"))
        got)
    (dolist (entry multishell-history)
      (when (and (string-match match-expr entry)
                 (not (member entry got)))
        (setq got (cons entry got))))
    got))

(defun multishell-pop-to-shell (&optional arg)
  "Easily navigate to and within multiple shell buffers, local and remote.

Use universal arguments to launch and choose between alternate
shell buffers and to select which is default.  Append a path to
a new shell name to launch a shell in that directory, and use
Emacs tramp syntax to launch a remote shell.

Customize-group `multishell' to set up a key binding and tweak behaviors.

==== Basic operation:

 - If the current buffer is shell-mode (or shell-mode derived)
   buffer then focus is moved to the process input point.

   \(You can use a universal argument go to a different shell
   buffer when already in a buffer that has a process - see
   below.)

 - If not in a shell buffer (or with universal argument), go to a
   window that is already showing the (a) shell buffer, if any.

   In this case, the cursor is left in its prior position in the
   shell buffer. Repeating the command will then go to the
   process input point, per the first item in this list.

   We respect `pop-up-windows', so you can adjust it to set the
   other-buffer/same-buffer behavior.

 - Otherwise, start a new shell buffer, using the current
   directory as the working directory.

If a buffer with the resulting name exists and its shell process
was disconnected or otherwise stopped, it's resumed.

===== Universal arg to start and select between named shell buffers:

You can name alternate shell buffers to create or return to using
single or doubled universal arguments:

 - With a single universal argument, prompt for the buffer name
   to use (without the asterisks that shell mode will put around
   the name), defaulting to 'shell'.

   Completion is available.

   This combination makes it easy to start and switch across
   multiple shell restarts.

 - A double universal argument will prompt for the name *and* set
   the default to that name, so the target shell becomes the
   primary.

   See `multishell-primary-name' for info about preserving the
   setting across emacs restarts.

===== Select starting directory and remote host:

The shell buffer name you give to the prompt for a universal arg
can include an appended path. That will be used for the startup
directory. You can use tramp remote syntax to specify a remote
shell. If there is an element after a final '/', that's used for
the buffer name. Otherwise, the host, domain, or path is used.

For example:

* '#root/sudo:root@localhost:/etc' for a buffer named \"#root\" with a
  root shell starting in /etc.

* '/ssh:example.net:/' for a shell buffer in / on example.net; the buffer
  will be named \"*example.net*\".

* '#ex/ssh:example.net|sudo:root@example.net:/etc' for a root shell
  starting in /etc on example.net named \"*#ex*\".

* 'interior/ssh:gateway.corp.com|ssh:interior.corp.com:' to go
  via gateway.corp.com to your homedir on interior.corp.com.  The
  buffer will be named \"*interior*\". You could append a sudo
  hop to the path, combining the previous example, and so on.

Thanks to tramp, file visits from the shell, and many common
emacs activities, like dired, will seamlessly be in the auspices
of the target account, and relative to the current directory, on
the host where the shell is running.

You can change the startup path for a shell buffer by editing it
at the completion prompt. The new path will be preserved in
history but will not take effect for an already-running shell.

To remove a shell buffer's history entry, kill the buffer and
affirm removal of the entry when prompted.

===== Activate savehist to retain shell buffer names and paths across Emacs restarts:

To have emacs maintain your history of shell buffer names and paths,
customize the savehist group to activate savehist."

  (interactive "P")

  (let* ((from-buffer (current-buffer))
         (from-buffer-is-shell (derived-mode-p 'shell-mode))
         (doublearg (equal arg '(16)))
         (target-name-and-path (multishell-resolve-target-name-and-path
           (if arg
               (multishell-read-bare-shell-buffer-name
                (format "Shell buffer name [%s]%s "
                        (substring-no-properties
                         multishell-primary-name
                         1 (- (length multishell-primary-name) 1))
                        (if doublearg " <==" ":"))
                (multishell-unbracket-asterisks multishell-primary-name))
             (multishell-unbracket-asterisks multishell-primary-name))))
         (use-default-dir (cadr target-name-and-path))
         (target-shell-buffer-name (car target-name-and-path))
         (curr-buff-proc (get-buffer-process from-buffer))
         (target-buffer (if from-buffer-is-shell
                            from-buffer
                          (get-buffer target-shell-buffer-name)))
         inwin
         already-there)

    ;; Register early so the entry is pushed to the front:
    (multishell-register-name-to-path (multishell-unbracket-asterisks
                                       target-shell-buffer-name)
                                      use-default-dir)

    (when doublearg
      (setq multishell-primary-name target-shell-buffer-name))

    ;; Situate:

    (cond 

     ((and (or curr-buff-proc from-buffer-is-shell)
           (not arg)
           (eq from-buffer target-buffer)
           (not (eq target-shell-buffer-name (buffer-name from-buffer))))
      ;; In a shell buffer, but not named - stay in buffer, but go to end.
      (setq already-there t))

     ((string= (buffer-name) target-shell-buffer-name)
      ;; Already in the specified shell buffer:
      (setq already-there t))

     ((or (not target-buffer)
          (not (setq inwin
                     (multishell-get-visible-window-for-buffer target-buffer))))
      ;; No preexisting shell buffer, or not in a visible window:
      (pop-to-buffer target-shell-buffer-name pop-up-windows))

       ;; Buffer exists and already has a window - jump to it:
     (t (if (and multishell-pop-to-frame
                 inwin
                 (not (equal (window-frame (selected-window))
                             (window-frame inwin))))
            (select-frame-set-input-focus (window-frame inwin)))
        (if (not (string= (buffer-name (current-buffer))
                          target-shell-buffer-name))
            (pop-to-buffer target-shell-buffer-name t))))

    ;; We're in the buffer. Activate:

    (if (not (comint-check-proc (current-buffer)))
        (multishell-start-shell-in-buffer (buffer-name (current-buffer))
                                          use-default-dir))

    ;; If the destination buffer has a stopped process, resume it:
    (let ((process (get-buffer-process (current-buffer))))
      (if (and process (equal 'stop (process-status process)))
          (continue-process process)))

    (when (or already-there
             (equal (current-buffer) from-buffer))
      (goto-char (point-max))
      (and (get-buffer-process from-buffer)
           (goto-char (process-mark (get-buffer-process from-buffer)))))))

(defun multishell-kill-buffer-query-function ()
  "Offer to remove multishell-history entry for buffer."
  ;; Removal choice is crucial, so users can, eg, kill a shell with huge
  ;; output backlog, while keeping the history entry to easily restart it.
  ;;
  ;; We use kill-buffer-query-functions instead of kill-buffer-hook because:
  ;;
  ;; 1. It enables the user to remove the history without actually killing a
  ;;    running buffer, by not confirming the subsequent running-proc query.
  ;; 2. kill-buffer-hooks often fails to run when killing shell buffers!
  ;;    It's probably due to failures in other hooks - beyond our control -
  ;;    and anyway, I like the first reason well enough.

  ;; (Use condition-case to avoid inadvertant disruption of kill-buffer
  ;; activity.  kill-buffer happens behind the scenes a whole lot.)
  (condition-case err
        (dolist (entry (and (derived-mode-p 'shell-mode)
                          (multishell-history-entries
                           (multishell-unbracket-asterisks (buffer-name)))))
          (when (and entry
                     (y-or-n-p (format "Remove multishell history entry `%s'? "
                                       entry)))
            (setq multishell-history
                  (delete entry multishell-history))))
    (error
     (message "multishell-kill-buffer-query-function error: %s" err)))
  t)
(add-hook 'kill-buffer-query-functions 'multishell-kill-buffer-query-function)

(defun multishell-get-visible-window-for-buffer (buffer)
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

(defun multishell-read-bare-shell-buffer-name (prompt default)
  "PROMPT for shell buffer name, sans asterisks.

Return the supplied name not bracketed with the asterisks, or specified
DEFAULT on empty input."
  (let* ((candidates
          (append
           ;; Plain shell buffer names appended with names from name/path hist:
           (remq nil
                 (mapcar (lambda (buffer)
                           (let* ((name (multishell-unbracket-asterisks
                                         (buffer-name buffer))))
                             (and (buffer-live-p buffer)
                                  (with-current-buffer buffer
                                    ;; Shell mode buffers.
                                    (derived-mode-p 'shell-mode))
                                  (not (multishell-history-entries name))
                                  name)))
                         (buffer-list)))
           multishell-history))
         (got (completing-read prompt
                               ;; COLLECTION:
                               (reverse candidates)
                               ;; PREDICATE:
                               nil
                               ;; REQUIRE-MATCH:
                               'confirm
                               ;; INITIAL-INPUT
                               nil
                               ;; HIST:
                               'multishell-history)))
    (if (not (string= got ""))
        got
      default)))

(defun multishell-resolve-target-name-and-path (path-ish)
  "Given name/tramp-path PATH-ISH, resolve buffer name and initial directory.

The name is the part of the string up to the first '/' slash, if
any. Missing pieces are filled in from remote path elements, if
any, and multishell history. Given a path and no name, either the
host-name, domain-name, final directory name, or local host name
is used.

Return them as a list (name path), with name asterisk-bracketed
and path nil if none resolved."
  (let* ((splat (multishell-split-entry path-ish))
         (name (car splat))
         (path (cadr splat)))
    (if path
        (if (not name)
            (setq name
                  (if (file-remote-p path)
                      (let ((vec (tramp-dissect-file-name path)))
                        (or (tramp-file-name-host vec)
                            (tramp-file-name-domain vec)
                            (tramp-file-name-localname vec)
                            system-name))
                    multishell-primary-name)))
      ;; No path - get one from history, if present.
      (when (not name)
        (setq name multishell-primary-name))
      (mapcar #'(lambda (entry)
                  (when (or (not path) (string= path ""))
                    (setq path (cadr (multishell-split-entry entry)))))
              (multishell-history-entries
               (multishell-unbracket-asterisks name))))
    (list (multishell-bracket-asterisks name) path)))

(defun multishell-bracket-asterisks (name)
  "Return a copy of name, ensuring it has an asterisk at the beginning and end."
  (if (not (string= (substring name 0 1) "*"))
      (setq name (concat "*" name)))
  (if (not (string= (substring name -1) "*"))
      (setq name (concat name "*")))
  name)
(defun multishell-unbracket-asterisks (name)
  "Return a copy of name, removing asterisks, if any, at beginning and end."
  (if (string= (substring name 0 1) "*")
      (setq name (substring name 1)))
  (if (string= (substring name -1) "*")
      (setq name (substring name 0 -1)))
  name)

(defun multishell-start-shell-in-buffer (buffer-name path)
  "Ensure a shell is started, with name NAME and PATH."
  ;; We work around shell-mode's bracketing of the buffer name, and do
  ;; some tramp-mode hygiene for remote connections.

  (let* ((buffer buffer-name)
         (prog (or explicit-shell-file-name
                   (getenv "ESHELL")
                   (getenv "SHELL")
                   "/bin/sh"))
         (name (file-name-nondirectory prog))
         (startfile (concat "~/.emacs_" name))
         (xargs-name (intern-soft (concat "explicit-" name "-args")))
         is-remote)
    (set-buffer buffer-name)
    (if (and path (not (string= path "")))
        (setq default-directory path))
    (setq is-remote (file-remote-p default-directory))
    (when (and is-remote
               (derived-mode-p 'shell-mode)
               (not (comint-check-proc (current-buffer))))
      ;; We're returning to an already established but disconnected remote
      ;; shell, tidy it:
      (tramp-cleanup-connection
       (tramp-dissect-file-name default-directory 'noexpand)
       'keep-debug 'keep-password))
    ;; (cd default-directory) will connect if remote:
    (when is-remote
      (message "Connecting to %s" default-directory))
    (condition-case err
        (cd default-directory)
      (error
       ;; Aargh. Need to isolate this tramp bug.
       (if (and (stringp (cadr err))
                (string-equal (cadr err)
                              "Selecting deleted buffer"))
           (signal (car err)
                   (list
                    (format "%s, %s (\"%s\")"
                            "Tramp shell can fail on remote|sudo to homedir"
                            "please try again with an explicit path"
                            (cadr err))))
         (signal (car err)(cdr err)))))
    (setq buffer (set-buffer (apply 'make-comint
                                    (multishell-unbracket-asterisks buffer-name)
                                    prog
                                    (if (file-exists-p startfile)
                                        startfile)
                                    (if (and xargs-name
                                             (boundp xargs-name))
                                        (symbol-value xargs-name)
                                      '("-i")))))
    (shell-mode)))

(defun multishell-track-dirchange (name newpath)
  "Change multishell history entry to track current directory."
  (let* ((entries (multishell-history-entries name)))
    (dolist (entry entries)
      (let* ((name-path (multishell-split-entry entry))
             (name (car name-path))
             (path (cadr name-path)))
        (when path
          (let* ((is-remote (file-remote-p path))
                 (vec (and is-remote (tramp-dissect-file-name path nil)))
                 (localname (if is-remote
                                (tramp-file-name-localname vec)
                              path))
                 (newlocalname
                  (replace-regexp-in-string (if (string= localname "")
                                                "$"
                                              (regexp-quote localname))
                                            ;; REP
                                            newpath
                                            ;; STRING
                                            localname
                                            ;; FIXEDCASE
                                            t
                                            ;; LITERAL
                                            t
                                            ))
                 (newpath (if is-remote
                              (tramp-make-tramp-file-name (aref vec 0)
                                                          (aref vec 1)
                                                          (aref vec 2)
                                                          newlocalname
                                                          (aref vec 4))
                            newlocalname))
                 (newentry (concat name newpath))
                 (membership (member entry multishell-history)))
            (when membership
              (setcar membership newentry))))))))
(defvar multishell-was-default-directory ()
  "Provide for tracking directory changes.")
(make-variable-buffer-local 'multishell-was-default-directory)
(defun multishell-post-command-business ()
  "Do multishell bookkeeping."
  ;; Update multishell-history with dir changes.
  (condition-case err
      (when (and multishell-history-entry-tracks-current-directory
                 (derived-mode-p 'shell-mode))
        (let ((curdir (if (file-remote-p default-directory)
                          (tramp-file-name-localname
                           (tramp-dissect-file-name default-directory))
                        default-directory)))
          (when (and multishell-was-default-directory
                     (not (string= curdir multishell-was-default-directory)))
            (multishell-track-dirchange (multishell-unbracket-asterisks
                                         (buffer-name))
                                        curdir))
          (setq multishell-was-default-directory curdir)))
    ;; To avoid disruption as a pervasive hook function, swallow all errors:
    (error
     (message "multishell-post-command-business error: %s" err))))
(add-hook 'post-command-hook 'multishell-post-command-business)

(defun multishell-split-entry (entry)
  "Given multishell name/path ENTRY, return the separated name and path pair.

Returns nil for empty parts, rather than the empty string."
  (string-match "^\\([^/]*\\)\\(/?.*\\)?" entry)
  (let ((name (match-string 1 entry))
        (path (match-string 2 entry)))
    (and (string= name "") (setq name nil))
    (and (string= path "") (setq path nil))
    (list name path)))

(provide 'multishell)

;;; multishell.el ends here
