;;; repo-grep.el

;; Author: Bjoern Hendrik Fock

;; Install: 
;; Add to .emacs or .emacs.d/init.el
;; (add-to-list 'load-path "PATH_TO_FOLDER_CONTAINING repo_grep.el")
;; (autoload 'repo-grep "repo-grep" )
;;
;; Add some key bindings
;;
;; (global-set-key [f12] 'repo-grep) ; f12
;;
;; Use:
;; M-x repo-grep or just hit F12
;; to search the string under the cursor

(defun repo-grep ()
  "REPO-GREP: Grep code from top of svn/git working copy or current folder"
  (interactive)
                                        ; search string
  (setq default_term (format "\"%s\"" (thing-at-point 'symbol) ))
  (setq search_string (or (read-string (concat "grep for (" default_term "): ")) default_term))
  (setq search_string (if (equal search_string "") default_term search_string))

  ; folder from which the grep is executed

  ; SVN - detect Subversion Working Copy Root Path
  (setq folder (substring
		(shell-command-to-string
		 "svn info | grep 'Working Copy Root Path' | awk {'print $5'}"
		 ) 0 -1))
  ; if svn info did not work because you use it in a new not yet added subdirectory
  ; try to do it 1-3 level above
  (if (string-match-p (regexp-quote "svn: warning: W155010") folder)
      (setq folder (substring
		(shell-command-to-string
		 "svn info .. | grep 'Working Copy Root Path' | awk {'print $5'}"
		 ) 0 -1)))
  (if (string-match-p (regexp-quote "svn: warning: W155010") folder)
      (setq folder (substring
		(shell-command-to-string
		 "svn info ../.. | grep 'Working Copy Root Path' | awk {'print $5'}"
		 ) 0 -1)))
  (if (string-match-p (regexp-quote "svn: warning: W155010") folder)
      (setq folder (substring
		(shell-command-to-string
		 "svn info ../../.. | grep 'Working Copy Root Path' | awk {'print $5'}"
		 ) 0 -1)))

  ; PWD - no svn working directory search current directory (and subdirs)
  (if (string-match-p (regexp-quote "svn: E155007") folder) 
      (setq folder (substring
		    (shell-command-to-string "pwd")
		    0 -1)))

  ; GIT - Detect top level from git if in git repository (overwrites svn and pwd)
  (setq gitfolder (substring
		  (shell-command-to-string
		   "git rev-parse --show-toplevel"
		   ) 0 -1))
  (if (not (string-match-p (regexp-quote "fatal: Not a git repository") gitfolder))
      (setq folder gitfolder))
  
   ; files which are greped
  (setq files "*")

(grep (format "cd %s && grep -nir %s %s " folder search_string files))
  
)
 (provide 'repo-grep)

;;; repo-grep.el ends here
