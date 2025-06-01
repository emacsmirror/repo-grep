;;; repo-grep.el --- Project-wide grep search -*- lexical-binding: t; -*-

;; Author:  Bjoern Hendrik Fock
;; Version: 1.1.0
;; License: BSD-3-Clause
;; Keywords: tools, search, convenience
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/BHFock/repo-grep
;;
;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the BSD-3-Clause License.

;;; Commentary:
;; repo-grep provides an interactive, project-wide search for both SVN and Git
;; repositories, as well as standalone directories. It integrates seamlessly
;; into Emacs and enables recursive grep searches with a single keystroke.
;;
;; The default search term is the symbol under the cursor, which can be
;; interactively edited. Optional keyword arguments allow for regex-based
;; prefix/suffix matching and file extension exclusions.
;;
;; The companion command `repo-grep-multi` enables recursive search across
;; multiple repositories or folders located in the same parent directory.
;;
;; Features include:
;; - Automatic detection of Git or SVN project roots
;; - Regex support for advanced search patterns
;; - Optional case sensitivity and file exclusion
;; - Clickable grep results in a dedicated buffer
;;
;; For installation, configuration, and usage examples, see the README and
;; the tutorial at https://github.com/BHFock/repo-grep.

;;; Code:

(defgroup repo-grep nil
  "Project-wide grep search from Emacs."
  :group 'tools
  :prefix "repo-grep-")

(defcustom repo-grep-from-folder-above nil
  "If non-nil, search from the parent directory of the detected project root."
  :type 'boolean
  :group 'repo-grep)

(defcustom repo-grep-case-sensitive nil
  "If non-nil, perform case-sensitive searches."
  :type 'boolean
  :group 'repo-grep)

;;;###autoload
(defun repo-grep (&rest args)
  "Run a project-wide grep search from the detected repository root.
Accepts keyword arguments for customisation."
  (interactive)
  (apply #'repo-grep--internal args))

;;;###autoload
(defun repo-grep-multi (&rest args)
  "Run a recursive grep across multiple repositories or folders in the same parent directory.
Accepts keyword arguments for customisation."
  (interactive)
  (let ((repo-grep-from-folder-above t))
    (apply #'repo-grep--internal args)))

(defun repo-grep--internal (&rest args)
  "Perform a recursive grep search with optional keyword arguments.
Handles custom exclusions, regex-based matching, and project root detection."
  (let* ((exclude-ext (plist-get args :exclude-ext))
         (left-regex  (plist-get args :left-regex))
         (right-regex (plist-get args :right-regex)))

    ;; Validate arguments
    (when (and left-regex (not (stringp left-regex)))
      (error "LEFT-REGEX must be a string"))
    (when (and right-regex (not (stringp right-regex)))
      (error "RIGHT-REGEX must be a string"))
    (when (and exclude-ext (not (listp exclude-ext)))
      (error "EXCLUDE-EXT must be a list of strings"))

    (let* ((symbol-at-point (thing-at-point 'symbol t))
           (symbol-at-point (or symbol-at-point ""))
           (default-term (format "\"%s\"" symbol-at-point))
           (prompt (concat "grep for ("
                           (or left-regex "")
                           symbol-at-point
                           (or right-regex "")
                           "): "))
           (input (read-string prompt))
           (search-string (if (string= input "") default-term input))
           (search-string (concat (or left-regex "") search-string (or right-regex "")))
           (folder (repo-grep--find-folder))
           (files (repo-grep--build-file-pattern exclude-ext))
           (case-flag (if repo-grep-case-sensitive "" "-i")))

      ;; Ensure a valid folder before executing grep
      (unless (and folder (not (string-empty-p folder)))
        (error "Could not determine project root."))

      (grep (format "cd %s && grep --color -nr %s %s %s"
                    (repo-grep--shell-escape folder)
                    case-flag
                    (repo-grep--shell-escape search-string)
                    files)))))

(defun repo-grep--shell-escape (string)
  "Escape STRING for safe in shell commands"
  (shell-quote-argument string))

(defun repo-grep--build-file-pattern (exclude-ext)
  "Construct a file pattern for grep, excluding extensions listed in EXCLUDE-EXT.
If EXCLUDE-EXT is nil, all files are included."
  (let ((exclude-pattern (if exclude-ext
                             (mapconcat (lambda (ext) (format "--exclude=*%s" ext)) exclude-ext " ")
                           "")))
    (concat "*" " " exclude-pattern)))

(defun repo-grep--find-folder ()
  "Determine the appropriate folder to run grep in.
Tries SVN first, falls back to the current directory, and then uses Git if found.
Returns the folder as a string, trimmed of extra whitespace."
  (let ((folder (string-trim
                 (shell-command-to-string
                  "svn info | grep 'Working Copy Root Path' | awk {'print $5'}"))))
    ;; SVN - if svn info did not work because you use it in a new, not yet added subdirectory,
    ;; try to do it 1-3 levels above.
    (if (string-match-p (regexp-quote "svn: warning: W155010") folder)
        (setq folder (string-trim
                      (shell-command-to-string
                       "svn info .. | grep 'Working Copy Root Path' | awk {'print $5'}"))))
    (if (string-match-p (regexp-quote "svn: warning: W155010") folder)
        (setq folder (string-trim
                      (shell-command-to-string
                       "svn info ../.. | grep 'Working Copy Root Path' | awk {'print $5'}"))))
    (if (string-match-p (regexp-quote "svn: warning: W155010") folder)
        (setq folder (string-trim
                      (shell-command-to-string
                       "svn info ../../.. | grep 'Working Copy Root Path' | awk {'print $5'}"))))
    ;; PWD - if no SVN working directory is found, try the current directory.
    (if (string-match-p (regexp-quote "svn: E155007") folder)
        (setq folder (string-trim
                      (shell-command-to-string "pwd"))))

    ;; Prefer Git repo root if available
    (let ((gitfolder (string-trim
                      (shell-command-to-string
                       "git rev-parse --show-toplevel"))))
      (if (not (string-match-p (regexp-quote "fatal: Not a git repository") gitfolder))
          (setq folder gitfolder)))

    ;; If requested, adjust to one folder level above
    (if repo-grep-from-folder-above
        (setq folder (concat folder "/..")))

    ;; Validate repository detection before returning
    (unless (and folder (not (string-empty-p folder)))
      (error "Could not determine root folder."))

    folder))

(provide 'repo-grep)

;;; repo-grep.el ends here
