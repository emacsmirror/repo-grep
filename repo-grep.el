;;; repo-grep.el --- Project-wide grep search -*- lexical-binding: t; -*-

;; Author:  Bjoern Hendrik Fock
;; Version: 1.5.0
;; License: BSD-3-Clause
;; Keywords: tools, search, convenience

;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/BHFock/repo-grep
;;
;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the BSD-3-Clause License.

;;; Commentary:
;; repo-grep provides interactive, recursive grep-based search for Git and SVN
;; repositories, as well as arbitrary directories. It integrates into Emacs and
;; allows project-wide text searches with minimal setup.
;;
;; The default search term is the symbol under the cursor, which can be interactively
;; edited before execution. Optional keyword arguments support regex-based
;; prefix/suffix matching and file extension filtering.
;;
;; The companion command `repo-grep-multi` performs recursive searches across
;; multiple repositories or directories located under the same parent folder.
;;
;; Features include:
;; - VCS-aware project root detection (Git or SVN)
;; - Optional restriction to a subdirectory within the project
;; - Case sensitivity and binary file handling options
;; - Customisable include/exclude file patterns
;; - Clickable results in a standard `*grep*` buffer
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
  "Interactively set `repo-grep-subfolder` to a subdirectory under the project root."
  (interactive)
  (let* ((root (or (vc-root-dir) default-directory))
         (selected-dir (read-directory-name "Select subfolder: " root nil t)))
    (setq repo-grep-subfolder (file-relative-name selected-dir root))
    (message "Search restricted to: %s" repo-grep-subfolder)))

;;;###autoload
(defun repo-grep-set-subfolder-from-dired ()
  "Set `repo-grep-subfolder` based on the current directory in a Dired buffer."
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
  "Interactively toggle `repo-grep-case-sensitive` between ON and OFF."
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

(defcustom repo-grep-ignore-binary t
  "If non-nil, grep will ignore binary files using '--binary-files=without-match'."
  :type 'boolean
  :group 'repo-grep)

;;;###autoload
(defun repo-grep-set-ignore-binary ()
  "Interactively toggle `repo-grep-ignore-binary` between ON and OFF."
  (interactive)
  (let* ((options '(("ON" . t) ("OFF" . nil)))
         (current (if repo-grep-ignore-binary "ON" "OFF"))
         (choice (completing-read
                  (format "Ignore binary files is currently %s. Choose new value: " current)
                  (mapcar #'car options)
                  nil t)))
    (setq repo-grep-ignore-binary (cdr (assoc choice options)))
    (message "Ignore binary files is now %s"
             (if repo-grep-ignore-binary "ENABLED" "DISABLED"))))

;;;###autoload
(defun repo-grep (&rest args)
  "Run a project-wide grep search from the detected repository root.

This command performs a recursive grep search starting from the
project root (Git, SVN, or current directory).  The default search
term is the symbol under the cursor, which can be edited
interactively.

Optional keyword arguments in ARGS:
  :exclude-ext   List of file extensions to exclude (e.g., '(\".log\" \".tmp\")).
  :include-ext   List of file extensions to include (e.g., '(\".el\" \".py\")).
  :left-regex    Regex pattern to prepend to the search term.
  :right-regex   Regex pattern to append to the search term.

Search respects `repo-grep-case-sensitive` and can be scoped to
a subfolder via `repo-grep-subfolder`.

Results are displayed in a dedicated grep buffer with clickable links."
  (interactive)
  (apply #'repo-grep--internal args))

;;;###autoload
(defun repo-grep-multi (&rest args)
  "Recursively grep across sibling repositories or directories under a common parent folder.

This command performs a recursive grep search across all sibling
directories under the parent of the current project root.  It is
useful for searching across multiple related repositories or
projects at once.

Optional keyword arguments in ARGS:
  :exclude-ext   List of file extensions to exclude (e.g., '(\".log\" \".tmp\")).
  :include-ext   List of file extensions to include (e.g., '(\".el\" \".py\")).
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
Handles custom exclusions, regex-based matching, and project root detection.

Optional keyword arguments in ARGS:
  :exclude-ext   List of file extensions to exclude.
  :include-ext   List of file extensions to include.
  :left-regex    Regex pattern to prepend to the search term.
  :right-regex   Regex pattern to append to the search term."
  (let* ((exclude-ext (plist-get args :exclude-ext))
         (include-ext (plist-get args :include-ext))
         (left-regex  (repo-grep--sanitise-regex (plist-get args :left-regex)))
         (right-regex (repo-grep--sanitise-regex (plist-get args :right-regex))))

    ;; Validate arguments
    (when (and exclude-ext (not (listp exclude-ext)))
      (error "EXCLUDE-EXT must be a list of strings"))
    (when (and include-ext (not (listp include-ext)))
      (error "INCLUDE-EXT must be a list of strings"))

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
           (files (split-string (repo-grep--build-file-pattern include-ext exclude-ext)))
           (case-flag (if repo-grep-case-sensitive "" "-i"))
           (binary-flag (if repo-grep-ignore-binary "--binary-files=without-match" "")))

      ;; Ensure a valid folder before executing grep
      (unless (and folder (not (string-empty-p folder)))
        (error "Could not determine project root"))

      (let ((default-directory folder))
        (compilation-start
         ;; quote only the search pattern (not the file globs)
         (mapconcat #'identity
                    (append (list "grep" "--color" "-nr"
                                  case-flag
                                  binary-flag
                                  (shell-quote-argument search-pattern))
                            files)
                    " ")
         'grep-mode)))))

(defun repo-grep--build-file-pattern (include-ext exclude-ext)
  "Construct a file pattern for grep, including and excluding extensions."
  (let ((parts '()))
    ;; Add include patterns
    (when include-ext
      (dolist (ext include-ext)
        (push (format "--include=*%s" (repo-grep--sanitise-ext ext)) parts)))
    ;; Add exclude patterns
    (when exclude-ext
      (dolist (ext exclude-ext)
        (push (format "--exclude=*%s" (repo-grep--sanitise-ext ext)) parts)))
    ;; Add wildcard only if no includes specified
    (unless include-ext
      (push "*" parts))
    ;; Join with spaces
    (mapconcat #'identity (nreverse parts) " ")))

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
      (error "Could not determine a valid project root folder"))
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
