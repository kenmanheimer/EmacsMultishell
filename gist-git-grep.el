;; From https://gist.github.com/offby1/1240799

;; There's something similar (but fancier) in vc-git.el: vc-git-grep

;; -I means don't search through binary files

;; --no-color, oddly enough, is required to allow emacs to colorize the output

(defcustom git-grep-switches
  "git --no-pager grep --extended-regexp -I -n --ignore-case --no-color"
  "Switches to pass to `git grep'."
  :type 'string)

(defcustom git-grep-default-work-tree (expand-file-name "~/work/adtrack")
  "Top of your favorite git working tree.  \\[git-grep] will search from here if it cannot figure out where else to look."
  :type 'directory
  )

(when (require 'vc-git nil t)

  ;; Uncomment this to try out the built-in-to-Emacs function.
  ;;(defalias 'git-grep 'vc-git-grep)

  (defun git-grep (command-args)
    (interactive
     (let ((root (vc-git-root default-directory)))
       (when (not root)
         (setq root git-grep-default-work-tree)
         (message "git-grep: %s doesn't look like a git working tree; searching from %s instead" default-directory root))
       (list (read-shell-command
              "Run git-grep (like this): "
              (format (concat
                       "cd %s && "
                       "%s -e %s")
                      root
                      git-grep-switches
                      (let ((thing (and
                                    ;; Don't snarf stuff from the buffer if
                                    ;; we're not looking at a file.
                                    ;; Perhaps we should also check to see
                                    ;; if the file is part of a git repo.
                                    buffer-file-name
                                    (thing-at-point
                                     'symbol))))
                        (or (and thing (progn
                                         (set-text-properties 0
                                                              (length thing)
                                                              nil thing)
                                         (shell-quote-argument
                                          (regexp-quote thing))))
                            "")))
                                 'git-grep-history))))
    (let ((grep-use-null-device nil))
      (grep command-args))))

(provide 'gist-git-grep)