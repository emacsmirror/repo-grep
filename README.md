# repo-grep

**repo-grep** offers a recursive grep through the folder structure of your cloned Git repository or your SVN working copy in Emacs. It uses the string under the current cursor as the default search term, which you can modify interactively. The search term can include a regular expression, and you can configure regex patterns as a prefix or suffix to further refine the search.

**repo-grep-multi** provides a recursive grep across multiple repositories or folders that reside in the same directory as the repository where the search is initiated.

For a more detailed guide on repo-grep’s features, see the [repo-grep tutorial](docs/repo-grep-tutorial.md).

## Install

Clone the repository:

```
git clone https://github.com/BHFock/repo-grep.git ~/repo-grep
```

Add this to your Emacs configuration (`~/.emacs` or `~/.emacs.d/init.el`):

```elisp
(add-to-list 'load-path "~/repo-grep")
(autoload 'repo-grep "repo-grep")
(autoload 'repo-grep-multi "repo-grep")
(global-set-key [f12] 'repo-grep)
(global-set-key [C-f12] 'repo-grep-multi)
```

## Features & Customisation

Customise `repo-grep` to fit your workflow:

### Case sensitivity
  
Toggle with `M-x repo-grep-set-case-sensitivity` or set directly: `(setq repo-grep-case-sensitive t)`

### Restrict to subfolder
  
Interactively with `M-x repo-grep-set-subfolder` or directly: `(setq repo-grep-subfolder "src")`

### File type filters
  
Exclude extensions: `:exclude-ext '(".log" "~")`
Include only specific types: `:include-ext '(".f90" ".F90")`

### Binary file search
  
Skip by default; toggle via `M-x repo-grep-set-ignore-binary` or set: `(setq repo-grep-ignore-binary nil)`

### Context-aware search using regex
  
Right-hand side matches: `:right-regex ".*="` or subroutine calls (Fortran): `:left-regex "CALL.*(.*"`

Define custom keybindings for specialised searches as needed.

## Usage

1. Open a file in your project.
2. Place the cursor over a symbol.
3. Press `F12` to search the current repo.
Or `Ctrl + F12` to search across all sibling repos.
Edit the suggested term or press `Enter` to accept it. Results appear in a clickable `*grep*` buffer.

Enjoy!
