;;; repo-grep.el --- Project-wide grep search -*- lexical-binding: t; -*-

;; Author:  Bjoern Hendrik Fock
;; Version: 1.3.0
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
;; - Optional restriction to a specific subfolder within the project root
;; - Interactive subfolder selection via prompt or Dired
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

(defcustom repo-grep-subfolder nil
  "Optional subfolder under the project root to start the search from.
Ignored when using `repo-grep-multi`."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Subfolder name"))
  :group 'repo-grep)

;;;###autoload
(defun repo-grep-set-subfolder ()
  "Interactively set `repo-grep-subfolder to start search from."
  (interactive)
  (let* ((root (or (vc-root-dir) default-directory))
         (selected-dir (read-directory-name "Select subfolder: " root nil t)))
    (setq repo-grep-subfolder (file-relative-name selected-dir root))
    (message "Search restricted to: %s" repo-grep-subfolder)))

;;;###autoload
(defun repo-grep-set-subfolder-from-dired ()
  "Set `repo-grep-subfolder` from the current directory in Dired."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (error "This command must be run from a Dired buffer"))
  (let* ((root (or (vc-root-dir) default-directory))
         (dir (dired-get-file-for-visit)))
    (unless (file-directory-p dir)
      (error "Selected item is not a directory"))
    (setq repo-grep-subfolder (file-relative-name dir root))
    (message "Search restricted to: %s" repo-grep-subfolder)))

(defcustom repo-grep-case-sensitive nil
  "If non-nil, perform case-sensitive searches."
  :type 'boolean
  :group 'repo-grep)

;;;###autoload
(defun repo-grep-set-case-sensitivity ()
  "Interactively set `repo-grep-case-sensitive` to ON or OFF."
  (interactive)
  (let* ((options '(("ON" . t) ("OFF" . nil)))
         (current (if repo-grep-case-sensitive "ON" "OFF"))
         (choice (completing-read
                  (format "Case-sensitive search is currently %s. Choose new value: " current)
                  (mapcar #'car options)
                  nil t)))
    (setq repo-grep-case-sensitive (cdr (assoc choice options)))
    (message "Case-sensitive search is now %s"
             (if repo-grep-case-sensitive "ENABLED" "DISABLED"))))

;;;###autoload
(defun repo-grep (&rest args)
  "Run a project-wide grep search from the detected repository root.

This command performs a recursive grep search starting from the
project root (Git, SVN, or current directory). The default search
term is the symbol under the cursor, which can be edited
interactively.

Optional keyword arguments in ARGS:
  :exclude-ext   List of file extensions to exclude (e.g., '(\".log\" \".tmp\")).
  :left-regex    Regex pattern to prepend to the search term.
  :right-regex   Regex pattern to append to the search term.

Search respects `repo-grep-case-sensitive` and can be scoped to
a subfolder via `repo-grep-subfolder`.

Results are displayed in a dedicated grep buffer with clickable links."
  (interactive)
  (apply #'repo-grep--internal args))

;;;###autoload
(defun repo-grep-multi (&rest args)
  "Run a recursive grep across multiple repositories or folders in the same parent directory.

This command performs a recursive grep search across all sibling
directories under the parent of the current project root. It is
useful for searching across multiple related repositories or
projects at once.

Optional keyword arguments in ARGS:
  :exclude-ext   List of file extensions to exclude (e.g., '(\".log\" \".tmp\")).
  :left-regex    Regex pattern to prepend to the search term.
  :right-regex   Regex pattern to append to the search term.

Search respects `repo-grep-case-sensitive` and ignores
`repo-grep-subfolder` since the search spans multiple roots.

Results are displayed in a dedicated grep buffer with clickable links."
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
Uses Emacs' built-in VCS detection and falls back to `default-directory`.
If `repo-grep-subfolder` is set and valid, append it to the root."
  (let ((folder (or (vc-root-dir)
                    default-directory)))
    (when repo-grep-from-folder-above
      (setq folder (expand-file-name ".." folder)))
    (when (and repo-grep-subfolder (not repo-grep-from-folder-above))
      (let ((sub (expand-file-name repo-grep-subfolder folder)))
        (if (file-directory-p sub)
            (setq folder sub)
          (error "Subfolder '%s' does not exist under project root" repo-grep-subfolder))))
    (unless (and folder (file-directory-p folder))
      (error "Could not determine a valid project root folder."))
    folder))

(defun repo-grep--sanitise-input (input)
  "Validate INPUT for shell safety while allowing common programming characters."
  (when (string-match-p "[`&;|<>\"'\\]" input)
    (error "Search input contains potentially dangerous characters: %s" input))
  input)

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
