;;; multishell-list.el --- tabulated-list-mode for multishell shell buffers

;; Copyright (C) 2016 Free Software Foundation, Inc. and Ken Manheimer

;; Author: Ken Manheimer <ken.manheimer@gmail.com>
;; Version: 1.0.9
;; Created: 2016 -- first public availability
;; Keywords: processes
;; URL: https://github.com/kenmanheimer/EmacsMultishell

(require 'tabulated-list)

(defun multishell-list-open-pop ()
  "Pop to current entry's shell, and refresh the listing buffer."
  (interactive)
  (multishell-pop-to-shell nil (tabulated-list-get-id)))
(defun multishell-list-open-as-default ()
  "Pop to current entry's shell, and set as the default shell."
  (interactive)
  (message "%s <==" (multishell-name-from-entry (tabulated-list-get-id)))
  (multishell-pop-to-shell '(16) (tabulated-list-get-id)))
(defun multishell-list-open-here ()
  "Switch to current entry's shell buffer."
  (interactive)
  (multishell-pop-to-shell nil (tabulated-list-get-id) 'here))

(defun multishell-list-delete ()
  "Remove current environment variable value."
  (interactive)
  (let* ((where (save-excursion (beginning-of-line) (point)))
         (entry (tabulated-list-get-id))
         (name (multishell-name-from-entry entry))
         (name-bracketed (multishell-bracket name))
         (buffer (get-buffer name-bracketed)))
    (when (multishell-delete-history-name name)
      (and buffer
           ;; If the process is live, let shell-mode get confirmation:
           (or (comint-check-proc (current-buffer))
               (y-or-n-p (format "Kill buffer %s? " name-bracketed)))
           (kill-buffer name-bracketed)))
    (revert-buffer)
    (if (not tabulated-list-sort-key)
        (revert-buffer))
    (goto-char where)))

(defun multishell-list-edit-entry ()
  "Edit the value of current shell entry."
  (interactive)
  (let* ((where (save-excursion (beginning-of-line) (point)))
         (entry (tabulated-list-get-id))
         (name (multishell-name-from-entry entry))
         (revised (multishell-read-unbracketed-entry
                   (format "Edit shell spec for %s: " name)
                   nil
                   entry))
         (revised-path (and revised (cadr (multishell-split-entry revised))))
         (revised-name (multishell-name-from-entry revised))
         buffer)
    (when (not (string= revised entry))
      (multishell-delete-history-name name)
      (when (and (not (string= name revised-name))
                 (setq buffer (get-buffer (multishell-bracket name))))
        (with-current-buffer buffer
          (rename-buffer (multishell-bracket revised-name))))
      (multishell-register-name-to-path revised-name revised-path)
      (revert-buffer)
      (if (not tabulated-list-sort-key)
          (revert-buffer))
      (goto-char where))))

(defun multishell-list-placeholder (value default)
  "Return VALUE if non-empty string, else DEFAULT."
  (if (or (not value) (string= value ""))
      default
    value))
(defun multishell-list-entries ()
  "Generate multishell name/path entries list for tabulated-list."
  (let ((recency 0))
    (mapcar #'(lambda (entry)
                (setq recency (1+ recency))
                (let* ((splat (multishell-split-entry entry))
                       (name (car splat))
                       (buffer (and name
                                    (get-buffer
                                     (multishell-bracket name))))
                       (status (cond ((not buffer) "x")
                                     ((comint-check-proc buffer) "+")
                                     (t ".")))
                       (rest (cadr splat))
                       (dissected (and rest (file-remote-p rest)
                                       (tramp-dissect-file-name rest t)))
                       (path (or (and dissected (aref dissected 3))
                                 rest))
                       (hops (and dissected
                                  (substring
                                   rest 0 (- (length rest) (length path))))))
                  (when (not name)
                    (setq name (multishell-name-from-entry entry)))
                  (list entry
                        (vector (format "%d" recency)
                                status
                                name
                                (multishell-list-placeholder hops "-")
                                (multishell-list-placeholder path ":")))))
            (multishell-all-entries))))

(defun compare-strings-as-numbers (a b)
  (let ((a (aref (cadr a) 0))
        (b (aref (cadr b) 0)))
    (> (string-to-number a) (string-to-number b))))
(define-derived-mode multishell-list-mode
    tabulated-list-mode "Shells"
  "Major mode for listing current and historically registered shells..
\\{multishell-list-mode-map\}"
  (setq tabulated-list-format [("#" 0 compare-strings-as-numbers)
                               ("! " 1 t)
                               ("Name" 15 t)
                               ("Hops" 30 t)
                               ("Path" 30 t)]
        tabulated-list-sort-key nil
        tabulated-list-entries #'multishell-list-entries)
  (tabulated-list-init-header))

(define-key multishell-list-mode-map (kbd "d") 'multishell-list-delete)
(define-key multishell-list-mode-map (kbd "e") 'multishell-list-edit-entry)
(define-key multishell-list-mode-map (kbd "o") 'multishell-list-open-pop)
(define-key multishell-list-mode-map (kbd "O") 'multishell-list-open-as-default)
(define-key multishell-list-mode-map
  (kbd "<return>") 'multishell-list-open-here)

;;;###autoload
(defun multishell-list ()
  "Edit your current and historic list of shell buffers.

Hit ? for a list of commands.

You can get to this shell listing manager by
recursively invoking \\[multishell-pop-to-shell] at either of the
`multishell-pop-to-shell' universal argument prompts."
  (interactive)
  (let ((buffer (get-buffer-create "*Shells*")))
    (pop-to-buffer buffer)
    (multishell-list-mode)
    (tabulated-list-print)))

(provide 'multishell-list)
(require 'multishell)

;;; multishell-list.el ends here
