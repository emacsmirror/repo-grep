# repo-grep

**repo-grep** offers a recursive grep through the folder structure of your cloned Git repository or your SVN working copy in Emacs. It uses the string under the current cursor as the default search term, which you can modify interactively. The search term can include a regular expression, and you can configure regex patterns as a prefix or suffix to further refine the search.

**repo-grep-multi** provides a recursive grep across multiple repositories or folders that reside in the same directory as the repository where the search is initiated.

For a more detailed guide on repo-grep’s features, see the [repo-grep tutorial](docs/repo-grep-tutorial.md).

## Install

Download the code

```
git clone https://github.com/BHFock/repo-grep.git ~/repo-grep
```

Adjust your Emacs configuration file `~/.emacs` or `~/.emacs.d/init.el` to include 

```
(add-to-list 'load-path "~/repo-grep")
(autoload 'repo-grep "repo-grep")
(autoload 'repo-grep-multi "repo-grep")
(global-set-key [f12] 'repo-grep)
(global-set-key [C-f12] 'repo-grep-multi)
```

You can change `~/repo-grep` to any folder you prefer, just update the load-path accordingly.

## Customisation 

Change to case-sensitive search

```
(setq repo-grep-case-sensitive t) 
```

Exclude files ending ```.log``` and ```~``` from the search

```
(global-set-key [f12] (lambda () (interactive) (repo-grep :exclude-ext '(".log" "~"))))
```

Modify your default search term (string under cursor) with suffixes to find variable assignments

```
(global-set-key [f11] (lambda () (interactive) (repo-grep :right-regex ".*="))) ;; assignment grep
```

or prefixes, e.g. to search for subroutine calls 

```
(global-set-key [f10] (lambda () (interactive) (repo-grep :left-regex "CALL.*(.*"))) ;; call grep
```

## Use
Once you have completed the installation above you can open your cloned (or checked out) code, position the cursor over a term of interest in Emacs, press the F12 key, and confirm the default search term by pressing enter. Press "ctrl F12" to search across multiple repositories in the same directory. To modify the search term, type in your new search term before pressing enter or customise the default search term as demonstrated above.

Enjoy!
