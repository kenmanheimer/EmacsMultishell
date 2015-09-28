;; xsel.el -- X copy and paste emacs region from emacs tty sessions, using xsel

;; TODO: Check alternative: http://emacs.stackexchange.com/a/819/9668

;; Copyright (C) 2015 Free Software Foundation, Inc. and Ken Manheimer

;; Author: Ken Manheimer <ken dot manheimer at gmail...>
;; Maintainer: Ken Manheimer <ken dot manheimer at gmail...>
;; Created: 1999 -- first public release
;; Keywords: copy, paste, X11
;; Website: https://github.com/kenmanheimer/EmacsUtils

;;; Commentary:
;;
;; If xsel is installed and DISPLAY is working, use `klm:xsel-copy' to copy
;; the region to the X clipboard and `klm:xsel-paste' to paste the contents
;; of the clipboard at point. (The advantage of the latter over regular X
;; mouse paste is `klm:xsel-paste' looks unitary, to emacs, rather than
;; the mouse paste's continuous, parsed/indented/auto-parenned/etc input.)


(defun klm:xsel-check-get-DISPLAY (&optional arg)
  "Ensure X DISPLAY is set, and prompt for it if not.

With universal argument, always prompt to set it, regardless.

Returns the resulting value for DISPLAY."
  (interactive "P")
  (when (or arg (not (getenv "DISPLAY")))
    (setenv "DISPLAY"
            (read-from-minibuffer "DISPLAY: "
                                  (or (getenv "DISPLAY") "localhost:10.0"))))
  (getenv "DISPLAY")
  )

(defun klm:xsel-copy (from to)
  "Place contents of region in X copy/paste buffer, using shell command."
  (interactive "r")
  (when (klm:xsel-check-get-DISPLAY)
    (let ((command (cond ((eq system-type 'darwin) "pbcopy")
                         ((eq system-type 'cygwin) "putclip")
                         ;; Linux &c:
                         (t "xsel --input --clipboard"))))
      (shell-command-on-region from to command)
      (deactivate-mark)
      )))

(defun klm:xsel-paste ()
  "Place contents of region in X copy/paste buffer, using shell command."
  (interactive "")
  (when (klm:xsel-check-get-DISPLAY)
    (let ((command (cond ((eq system-type 'darwin) "pbpaste")
                         ((eq system-type 'cygwin) "getclip")
                         ;; Linux &c:
                         (t "xsel --output --clipboard"))))
      (shell-command command 1)
      )))
