# repo-grep

**repo-grep** offers a recursive grep through the folder structure of your cloned git repository or your svn working copy in Emacs. The default search term is the string under the current cursor position. Interactive modification of the search term is possible, and the search term can include regular expression.

## Install

Download the code

```
mkdir YOUR_REPO_GREP_FOLDER
git clone https://github.com/BHFock/repo-grep.git YOUR_REPO_GREP_FOLDER
```

Adjust your Emacs configuration file ~/.emacs or ~/.emacs.d/init.el to include 

```
(add-to-list 'load-path "YOUR_REPO_GREP_FOLDER")
(autoload 'repo-grep "repo-grep")
(global-set-key [f12] 'repo-grep)
```

## Use

Once you completed above installation you can open your cloned (or checked out) code, position the cursor over a term of interest in Emacs, press the F12 key, and confirm the default search term by pressing enter. To modify the search term, type in your new search term before pressing enter.

Enjoy!
