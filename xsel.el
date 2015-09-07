;; xsel.el -- copy and paste from emacs tty sessions, using xsel

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
;; of the clipboard at point. (The advantage of the latter over just usign
;; X paste into the terminal is `klm:xsel-paste' looks unitary, to emacs,
;; rather than being continuous input.)


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
  "Place contents of region in X copy/paste buffer, using `xsel'."
  (interactive "r")
  (when (klm:xsel-check-get-DISPLAY)
    (let ((temp-file-name (make-temp-file "klm:xsel-copy_"))
          (content (buffer-substring from to)))
      (with-temp-file temp-file-name (insert-string content))
      ;; Didn't get call-process to work, and would have to do error handling.
      ;; (call-process "/usr/bin/xsel" temp-file-name nil nil
      ;;               "--input" "--clipboard")
      (shell-command (format "/usr/bin/xsel --input --clipboard < %s"
                             temp-file-name))
      (delete-file temp-file-name nil)
      )))

(defun klm:xsel-paste ()
  "Place contents of region in X copy/paste buffer, using `xsel'."
  (interactive "")
  (when (klm:xsel-check-get-DISPLAY)
    (let (contents)
      (shell-command "/usr/bin/xsel --output --clipboard")
      (save-current-buffer
        (set-buffer "*Shell Command Output*")
        (setq contents (buffer-substring (point-min)(point-max))))
      (insert-string contents)
      )))
