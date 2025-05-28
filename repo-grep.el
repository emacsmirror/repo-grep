;;; repo-grep.el --- Fast project-wide search for Emacs 

;; Author:  Bjoern Hendrik Fock
;; Version: 1.0
;; License: BSD-3-Clause
;; Keywords: search, grep, emacs-tools
;; URL: https://github.com/BHFock/repo-grep
;;
;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the BSD-3-Clause License.
;;
;; Installation instructions, keybindings, and customisation examples are
;; provided in the README and tutorial.

;;; Commentary:
;; repo-grep provides an interactive, project-wide search for both SVN and Git
;; repositories, as well as standalone directories. See the README and the
;; tutorial for usage examples and advanced configuration options.

;;; Code:

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
         (left-regex  (plist-get args :left-regex))
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
