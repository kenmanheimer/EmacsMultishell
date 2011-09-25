(defun git-grep (regexp)
  (interactive "sSearch in git repo: ")
  (grep (format "GIT_PAGER='' git grep -nH --no-color -i \"%s\" -- $(git rev-parse --show-toplevel)" regexp)))

(global-set-key (kbd "C-x ?") 'git-grep)
