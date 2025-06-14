# repo-grep

**repo-grep** offers a recursive grep through the folder structure of your cloned Git repository or your SVN working copy in Emacs. It uses the symbol under the cursor as the default search term, which you can edit interactively. The search term can include a regular expression, and you can configure regex patterns as a prefix or suffix to further refine the search.

**repo-grep-multi** provides a recursive grep across multiple repositories or folders located in the same parent directory.

## Why use repo-grep?

- Recursive search from the project root — Git, SVN, or any directory
- One-keystroke grep from the symbol under cursor
- Optional regex context and file-type filters
- Multi-repo search from a shared parent folder
- No setup or project configuration required

## Dependencies

- Emacs ≥ 25.1  
- `grep` (available on Unix-like systems)  
- Optional: Git or SVN for root detection

## Quickstart

### 1. Clone the repository

```
git clone https://github.com/BHFock/repo-grep.git ~/repo-grep
```

### 2. Add this to your Emacs configuration (`~/.emacs` or `~/.emacs.d/init.el`)

```elisp
(add-to-list 'load-path "~/repo-grep")
(autoload 'repo-grep "repo-grep")
(autoload 'repo-grep-multi "repo-grep")
(global-set-key [f12] 'repo-grep)
(global-set-key [C-f12] 'repo-grep-multi)
```

### 3. Open any file and start searching

- Place the cursor over a symbol (e.g., variable or function name)
- Press `F12` to search the current repository
- Press `Ctrl + F12` to search across sibling repositories
- Edit the suggested term or press `Enter` to accept
- Browse results in the clickable `*grep*` buffer

You can edit the search term using regex patterns for more precise results. For a more detailed guide on repo-grep’s features, see the [repo-grep tutorial](docs/repo-grep-tutorial.md).

## Advanced Usage & Customisation

Customise `repo-grep` to fit your workflow:

### Case sensitivity
  
Toggle with `M-x repo-grep-set-case-sensitivity` or set directly: 
```elisp
(setq repo-grep-case-sensitive t)
```

### Restrict to subfolder
  
Interactively with `M-x repo-grep-set-subfolder` or set directly: 

```elisp
(setq repo-grep-subfolder "src")
```

### File type filters
  
Exclude extensions: `:exclude-ext '(".log" "~")`
Include only specific types: `:include-ext '(".f90" ".F90")`

### Binary file search
  
Skip by default; toggle via `M-x repo-grep-set-ignore-binary` or set:

```elisp
(setq repo-grep-ignore-binary nil)
```

### Context-aware search using regex

Narrow results by matching context around the search term:

```elisp
(setq repo-grep-left-regex "CALL.*(")    ;; Fortran subroutine calls
(setq repo-grep-right-regex ".*=")       ;; assignment expressions
```

Define custom keybindings for specialised searches as needed.
