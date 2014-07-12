;;; pdbtrack - Track source file lines as you run python/pdb in an emacs shell.

;;; Standalone Python PDB dynamic file tracking.

(define-minor-mode pdbtrack-minor-mode
  "Show lines in source file when Python PDB debugger steps through them."
  nil ":PDBtrack" :require 'pdbtrack :version "2.1"

  (add-hook 'comint-output-filter-functions
            'pdbtrack-comint-output-filter-function)
  (make-local-variable 'pdbtrack-buffers-to-kill)
  (make-local-variable 'pdbtrack-tracked-buffer)
)

(defcustom pdbtrack-stacktrace-info-regexp
  "> \\([^\"(<]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_<>]+\\)()"
  "Regular Expression matching stacktrace information.
Used to extract the current line and module being inspected."
  :type 'string
  :group 'python
  :safe 'stringp)

(defvar pdbtrack-tracked-buffer nil
  "Variable containing the value of the current tracked buffer.
Never set this variable directly, use
`pdbtrack-set-tracked-buffer' instead.")

(defcustom pdbtrack-remove-new-buffers-after-tracking t
  "Remove buffers visited for the sake of tracking, on pdb session conclusion."
  :type 'boolean
  :group 'python)
(defvar pdbtrack-buffers-to-kill nil
  "List of buffers to be deleted after tracking finishes.")

(defun pdbtrack-set-tracked-buffer (file-name)
  "Set the buffer for FILE-NAME as the tracked buffer.
Internally it uses the `pdbtrack-tracked-buffer' variable.
Returns the tracked buffer."
  (let ((file-buffer (get-file-buffer
                      (concat (file-remote-p default-directory)
                              file-name))))
    (if file-buffer
        (setq pdbtrack-tracked-buffer file-buffer)
      (setq file-buffer (find-file-noselect file-name))
      (when (not (member file-buffer pdbtrack-buffers-to-kill))
        (add-to-list 'pdbtrack-buffers-to-kill file-buffer)))
    file-buffer))

(defun pdbtrack-comint-output-filter-function (output)
  "Move overlay arrow to current pdb line in tracked buffer.
Argument OUTPUT is a string with the output from the comint process."
  (when (and pdbtrack-minor-mode (not (string= output "")))
    (let* ((full-output (ansi-color-filter-apply
                         (buffer-substring comint-last-input-end (point-max))))
           (line-number)
           (file-name
            (with-temp-buffer
              (insert full-output)
              ;; When the debugger encounters a pdb.set_trace()
              ;; command, it prints a single stack frame.  Sometimes
              ;; it prints a bit of extra information about the
              ;; arguments of the present function.  When ipdb
              ;; encounters an exception, it prints the _entire_ stack
              ;; trace.  To handle all of these cases, we want to find
              ;; the _last_ stack frame printed in the most recent
              ;; batch of output, then jump to the corresponding
              ;; file/line number.
              (goto-char (point-max))
              (when (re-search-backward pdbtrack-stacktrace-info-regexp nil t)
                (setq line-number (string-to-number
                                   (match-string-no-properties 2)))
                (match-string-no-properties 1)))))
      (if (and file-name line-number)
          (let* ((tracked-buffer
                  (pdbtrack-set-tracked-buffer file-name))
                 (shell-buffer (current-buffer))
                 (tracked-buffer-window (get-buffer-window tracked-buffer))
                 (tracked-buffer-line-pos))
            (with-current-buffer tracked-buffer
              (set (make-local-variable 'overlay-arrow-string) "=>")
              (set (make-local-variable 'overlay-arrow-position) (make-marker))
              (setq tracked-buffer-line-pos (progn
                                              (goto-char (point-min))
                                              (forward-line (1- line-number))
                                              (point-marker)))
              (when tracked-buffer-window
                (set-window-point
                 tracked-buffer-window tracked-buffer-line-pos))
              (set-marker overlay-arrow-position tracked-buffer-line-pos))
            (pop-to-buffer tracked-buffer)
            (switch-to-buffer-other-window shell-buffer))
        (when pdbtrack-tracked-buffer
          (with-current-buffer pdbtrack-tracked-buffer
            (set-marker overlay-arrow-position nil))
          (when (not pdbtrack-remove-new-buffers-after-tracking)
            (mapc #'(lambda (buffer)
                      (ignore-errors (kill-buffer buffer)))
                  pdbtrack-buffers-to-kill))
          (setq pdbtrack-tracked-buffer nil
                pdbtrack-buffers-to-kill nil)))))
  output)

(provide 'pdbtrack)
