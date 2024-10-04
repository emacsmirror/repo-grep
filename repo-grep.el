;;; repo-grep.el

;; Author: Bjoern Hendrik Fock

;; Install - add to .emacs or .emacs.d/init.el:
;;   (add-to-list 'load-path "PATH_TO_FOLDER_CONTAINING repo_grep.el")
;;   (autoload 'repo-grep "repo-grep" )
;;   (global-set-key [f12]   'repo-grep)
;;   (global-set-key [C-f12] 'repo-grep-multi)
;;
;; Use:
;;   M-x repo-grep or just hit F12
;;   to search the string under the cursor
;;
;;   M-x repo-grep-multi or hit Ctrl+F12
;;   to search across multiple repositories

(defvar repo-grep_from_folder_above nil
  "If non-nil, grep from one folder level above the top folder.")

(defun repo-grep (&optional predefined-regex)
  "REPO-GREP: Grep code from top of svn/git working copy or current folder"
  (interactive)
  (repo-grep-internal predefined-regex))

(defun repo-grep-multi (&optional predefined-regex)
  "REPO-GREP-MULTI: Grep code from one folder level above the top folder"
  (interactive)
  (let ((repo-grep_from_folder_above t))
    (repo-grep-internal predefined-regex)))

(defun repo-grep-internal (&optional predefined-regex)()
  "Internal function to perform the grep"
  (let* ((default_term (format "\"%s\"" (thing-at-point 'symbol)))
         (search_string (or (read-string (concat "grep for (" default_term "): ")) default_term))
         (search_string (if (equal search_string "") default_term search_string))
         (folder (repo-grep-find-folder))
         (files "*"))
    (grep (format "cd %s && grep -nir %s %s " folder search_string files))))

(defun repo-grep-find-folder ()
  "Find the folder from which to execute the grep command"
  (let ((folder (substring
                 (shell-command-to-string
                  "svn info | grep 'Working Copy Root Path' | awk {'print $5'}") 0 -1)))
    ; SVN - if svn info did not work because you use it in a new not yet added subdirectory
    ; try to do it 1-3 level above
    (if (string-match-p (regexp-quote "svn: warning: W155010") folder)
        (setq folder (substring
                      (shell-command-to-string
                       "svn info .. | grep 'Working Copy Root Path' | awk {'print $5'}") 0 -1)))
    (if (string-match-p (regexp-quote "svn: warning: W155010") folder)
        (setq folder (substring
                      (shell-command-to-string
                       "svn info ../.. | grep 'Working Copy Root Path' | awk {'print $5'}") 0 -1)))
    (if (string-match-p (regexp-quote "svn: warning: W155010") folder)
        (setq folder (substring
                      (shell-command-to-string
                       "svn info ../../.. | grep 'Working Copy Root Path' | awk {'print $5'}") 0 -1)))
    ; PWD - no svn working directory search current directory (and subdirs)
    (if (string-match-p (regexp-quote "svn: E155007") folder)
        (setq folder (substring
                      (shell-command-to-string "pwd") 0 -1)))
    ; GIT - Detect top level from git if in git repository (overwrites svn and pwd)
    (let ((gitfolder (substring
                      (shell-command-to-string
                       "git rev-parse --show-toplevel") 0 -1)))
      (if (not (string-match-p (regexp-quote "fatal: Not a git repository") gitfolder))
          (setq folder gitfolder)))
    (if repo-grep_from_folder_above
        (setq folder (concat folder "/..")))
    folder))

(provide 'repo-grep)

;;; repo-grep.el ends here
