# repo-grep

[![Tutorial](https://img.shields.io/badge/Tutorial-View-blue)](https://github.com/BHFock/repo-grep/blob/main/docs/repo-grep-tutorial.md)

## Recursive code search in Emacs

When working across large projects or multiple repositories, staying inside Emacs for navigation and discovery keeps your workflow efficient and uninterrupted.

**repo-grep** runs a recursive grep through the folder structure of your Git repository, SVN working copy, or plain folder. It uses the symbol under the cursor as the default search term, which you can edit interactively. The search term can include a regular expression, and you can configure regex patterns as a prefix or suffix to further refine the search.

**repo-grep-multi** extends this to a recursive grep across multiple repositories or folders located in the same parent directory.

## Why use repo-grep?

- Recursive search from the project root — Git, SVN, or any directory
- One-keystroke search using the symbol under the cursor
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

```
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

You can refine the search term with regular expressions for more precise results. For a more detailed guide on repo-grep’s features, see the [repo-grep tutorial](docs/repo-grep-tutorial.md).

## Advanced Usage & Customisation

Customise `repo-grep` to fit your workflow:

### Case sensitivity
  
Toggle with `M-x repo-grep-set-case-sensitivity` or set directly: 
```
(setq repo-grep-case-sensitive t)
```

### Restrict to subfolder
  
Interactively with `M-x repo-grep-set-subfolder` or set directly: 

```
(setq repo-grep-subfolder "src")
```

### File type filters

Filter which files are searched by specifying extensions to include or exclude:

Use `:exclude-ext` to ignore certain file types (e.g., logs, backups).
Use `:include-ext` to restrict search to specific file types (e.g., .f90).

Example usage (see the [tutorial](docs/repo-grep-tutorial.md) for more details):

```
(repo-grep :exclude-ext '(".log" "~"))
(repo-grep :include-ext '(".f90" ".F90"))
```

### Binary file search

Binary files are skipped by default. You can change this via `M-x repo-grep-set-ignore-binary` or set:

```
(setq repo-grep-ignore-binary nil)
```

### Context-aware search using regex

Narrow results by matching context around the search term:

```
(setq repo-grep-left-regex "CALL.*(")    ;; Fortran subroutine calls
(setq repo-grep-right-regex ".*=")       ;; assignment expressions
```

You can define custom keybindings to frequently used patterns or filters.

## Development status

`repo-grep` is regarded as feature-complete. Further enhancements or pull requests are not planned at present, but the code is available to use, fork or adapt under the licence terms.

## Security

All operations are local; the package initiates no network connections and executes no remote code. Arguments passed to external commands are sanitised to mitigate shell-injection risks. Users are encouraged to review the source and report any security concerns via the issue tracker.
