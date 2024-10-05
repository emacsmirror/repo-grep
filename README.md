# repo-grep

**repo-grep** offers a recursive grep through the folder structure of your cloned git repository or your svn working copy in Emacs. The default search term is the string under the current cursor position. Interactive modification of the search term is possible, and the search term can include regular expression. Regular expressions can also be configured as pre/suffix for the default search term.

**repo-grep-multi** offers a recursive grep across multiple repositories/folders contained in the same directory as the repository in which the search is initiated.

## Install

Download the code

```
mkdir YOUR_REPO_GREP_FOLDER
git clone https://github.com/BHFock/repo-grep.git YOUR_REPO_GREP_FOLDER
```

Adjust your Emacs configuration file `~/.emacs` or `~/.emacs.d/init.el` to include 

```
(add-to-list 'load-path "YOUR_REPO_GREP_FOLDER")
(autoload 'repo-grep "repo-grep")
(autoload 'repo-grep-multi "repo-grep")
(global-set-key [f12] 'repo-grep)
(global-set-key [C-f12] 'repo-grep-multi)
```

## Customisation 

Modify your default search term (string under cursor) with suffixes to find variable assignments

```
(global-set-key [f11] (lambda () (interactive) (repo-grep "" ".*=")))
```

or prefixes, e.g. to search for FORTRAN function calls 

```
(global-set-key [f10] (lambda () (interactive) (repo-grep "CALL.*(.*" "")))"
```

## Use

Once you completed above installation you can open your cloned (or checked out) code, position the cursor over a term of interest in Emacs, press the F12 key, and confirm the default search term by pressing enter. Press "ctrl F12" to search across multiple repositories in the same directory. To modify the search term, type in your new search term before pressing enter.

Enjoy!
