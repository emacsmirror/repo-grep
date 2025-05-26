;;; repo-grep.el --- Fast project-wide search for Emacs 

;; Author:  Bjoern Hendrik Fock
;; Version: 1.0
;; Keywords: search, grep, emacs-tools
;; URL: https://github.com/BHFock/repo-grep

;; Install - add to .emacs or .emacs.d/init.el:
;;   (add-to-list 'load-path "PATH_TO_FOLDER_CONTAINING repo_grep.el")
;;   (autoload 'repo-grep       "repo-grep")
;;   (autoload 'repo-grep-multi "repo-grep")
;;   (global-set-key [f12]   'repo-grep)
;;   (global-set-key [C-f12] 'repo-grep-multi)
;;
;; Advanced configuration - exclude files ending ".log" and "~" from search
;;   (global-set-key [f12] (lambda () (interactive) (repo-grep :exclude-ext '(".log" "~"))))
;;   (global-set-key [C-f12] (lambda () (interactive) (repo-grep-multi :exclude-ext '(".log" "~"))))
;;
;; Advanced configuration - adjust default search term with optional left/right-regex:
;;   ;; find variable assignments
;;   (global-set-key [f11] (lambda () (interactive) (repo-grep :right-regex ".*=")))
;;   (global-set-key [C-f10] (lambda () (interactive) (repo-grep-multi :right-regex ".*=")))
;;   ;; find subroutine calls
;;   (global-set-key [f10] (lambda () (interactive) (repo-grep :left-regex "CALL.*(.*")))
;;
;; Advanced configuration - case-sensitive or case-insensitive search
;;   (setq repo-grep-case-sensitive t)  ;; Case-sensitive search
;;   (setq repo-grep-case-sensitive nil) ;; Case-insensitive search
;;
;; Use:
;;   M-x repo-grep or just hit F12
;;   to search the string under the cursor
;;
;;   M-x repo-grep-multi or hit Ctrl+F12
;;   to search across multiple repositories
;;
;;   Use F11 for your "assignment grep" and F10 for your "call grep".  

(defvar repo-grep-from-folder-above nil
  "If non-nil, grep from one folder level above the top folder.")

(defvar repo-grep-case-sensitive nil
  "If non-nil, grep will be case-sensitive. If nil, grep will be case-insensitive.")

(defun repo-grep (&rest args)
  "REPO-GREP: Grep code from top of svn/git working copy or current folder."
  (interactive)
  (apply 'repo-grep-internal args))

(defun repo-grep-multi (&rest args)
  "REPO-GREP-MULTI: Grep code from one folder level above the top folder."
  (interactive)
  (let ((repo-grep-from-folder-above t))
    (apply 'repo-grep-internal args)))

(defun repo-grep-internal (&rest args)
  "Internal function to perform the grep."
  (let* ((exclude-ext (plist-get args :exclude-ext))
         (left-regex (plist-get args :left-regex))
         (right-regex (plist-get args :right-regex))
         (default-term (format "\"%s\"" (thing-at-point 'symbol)))
         (search-string (or (read-string (concat "grep for ("
                                                 (concat (or left-regex)
                                                         (thing-at-point 'symbol)
                                                         (or right-regex) "): ")
                                                 )) default-term))
         (search-string (if (equal search-string "") default-term search-string))
         (search-string (concat (or left-regex "") search-string (or right-regex "")))
         (folder (repo-grep-find-folder))
         (files (repo-grep-build-file-pattern exclude-ext))
         (case-flag (if repo-grep-case-sensitive "" "-i")))
    (grep (format "cd %s && grep --color -nr %s %s %s" folder case-flag search-string files))))

(defun repo-grep-build-file-pattern (exclude-ext)
  "Build the file pattern for grep based on exclusion extensions."
  (let ((exclude-pattern (if exclude-ext
                             (mapconcat (lambda (ext) (format "--exclude=*%s" ext)) exclude-ext " ")
                           "")))
    (concat "*" " " exclude-pattern)))

(defun repo-grep-find-folder ()
  "Find the folder from which to execute the grep command."
  (let ((folder (substring
                 (shell-command-to-string
                  "svn info | grep 'Working Copy Root Path' | awk {'print $5'}") 0 -1)))
    ;; SVN - if svn info did not work because you use it in a new not yet added subdirectory
    ;; try to do it 1-3 levels above
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
    ;; PWD - no svn working directory, search current directory (and subdirs)
    (if (string-match-p (regexp-quote "svn: E155007") folder)
        (setq folder (substring
                      (shell-command-to-string "pwd") 0 -1)))
    ;; GIT - Detect top level from git if in git repository (overwrites svn and pwd)
    (let ((gitfolder (substring
                      (shell-command-to-string
                       "git rev-parse --show-toplevel") 0 -1)))
      (if (not (string-match-p (regexp-quote "fatal: Not a git repository") gitfolder))
          (setq folder gitfolder)))
    (if repo-grep-from-folder-above
        (setq folder (concat folder "/..")))
    folder))

(provide 'repo-grep)

;;; repo-grep.el ends here
