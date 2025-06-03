;;; repo-grep.el --- Project-wide grep search -*- lexical-binding: t; -*-

;; Author:  Bjoern Hendrik Fock
;; Version: 1.2.1
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
         (left-regex  (repo-grep--sanitise-regex (plist-get args :left-regex)))
         (right-regex (repo-grep--sanitise-regex (plist-get args :right-regex))))

    ;; Validate arguments
    (when (and exclude-ext (not (listp exclude-ext)))
      (error "EXCLUDE-EXT must be a list of strings"))

    ;; Extract symbol under cursor or use fallback
    (let* ((symbol-at-point (thing-at-point 'symbol t))
           (symbol-at-point (or symbol-at-point ""))
           (default-term symbol-at-point) ;; DO NOT quote yet
           (prompt (concat "grep for ("
                           (or left-regex "")
                           symbol-at-point
                           (or right-regex "")
                           "): "))
           (input (read-string prompt nil nil symbol-at-point))
           (sanitised-input (repo-grep--sanitise-input input))
           (search-term (if (string-empty-p sanitised-input) default-term sanitised-input))
           (search-pattern (concat (or left-regex "") search-term (or right-regex "")))
           (folder (repo-grep--find-folder))
           (files (split-string (repo-grep--build-file-pattern exclude-ext)))
           (case-flag (if repo-grep-case-sensitive "" "-i")))

      ;; Ensure a valid folder before executing grep
      (unless (and folder (not (string-empty-p folder)))
        (error "Could not determine project root."))

      (let ((default-directory folder))
        (compilation-start
         ;; quote only the search pattern (not the file globs)
         (mapconcat #'identity
                    (append (list "grep" "--color" "-nr" case-flag (shell-quote-argument search-pattern))
                            files)
                    " ")
         'grep-mode)))))

(defun repo-grep--build-file-pattern (exclude-ext)
  "Construct a file pattern for grep, excluding extensions listed in EXCLUDE-EXT.
If EXCLUDE-EXT is nil, all files are included."
  (let ((exclude-pattern (if exclude-ext
                             (mapconcat (lambda (ext)
                                          (format "--exclude=*%s"
                                                  (repo-grep--sanitise-ext ext)))
                                        exclude-ext " ")
                           "")))
    (concat "*" " " exclude-pattern)))

(defun repo-grep--find-folder ()
  "Determine the appropriate folder to run grep in.
Uses Emacs' built-in VCS detection and falls back to `default-directory`."
  (let ((folder (or (vc-root-dir)
                    default-directory)))
    (when repo-grep-from-folder-above
      (setq folder (expand-file-name ".." folder)))
    (unless (and folder (file-directory-p folder))
      (error "Could not determine a valid project root folder."))
    folder))

(defun repo-grep--sanitise-input (input)
  "Sanitise INPUT by removing potentially dangerous shell characters."
  (replace-regexp-in-string "[`$&|;<>]" "" input))

(defun repo-grep--sanitise-regex (regex)
  "Validate REGEX contains only safe characters for shell execution."
  (when (and regex (not (stringp regex)))
    (error "REGEX must be a string or nil"))
  (when (and regex (string-match-p "[`$&;|<>\"'\\\\]" regex))
    (error "Regex contains potentially dangerous characters: %s" regex))
  regex)

(defun repo-grep--sanitise-ext (ext)
  "Ensure EXT only contains safe characters for shell globbing."
  (if (string-match-p "[^A-Za-z0-9._~-]" ext)
      (error "Unsafe character in file extension: %s" ext)
    ext))

(provide 'repo-grep)

;;; repo-grep.el ends here
