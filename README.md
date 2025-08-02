# repo-grep

[![Tutorial](https://img.shields.io/badge/Tutorial-View-blue)](https://github.com/BHFock/repo-grep/blob/main/docs/repo-grep-tutorial.md)

## Recursive project-wide code search in Emacs

When working across large projects or multiple repositories, staying inside Emacs for navigation and code discovery keeps your workflow efficient and uninterrupted.

**repo-grep** runs a recursive grep through the folder structure of your Git repository, SVN working copy, or plain folder. It uses the symbol under the cursor as the default search term, which you can edit interactively. The search term can include a regular expression, and you can configure regex patterns as a prefix or suffix to further refine the search.

**repo-grep-multi** searches across all sibling folders under the parent directory of the current project. 

## Why use repo-grep?

- Recursive search from the project root — Git, SVN, or any directory
- Instant search using the symbol under the cursor — editable before execution
- Multi-repo search from a shared parent folder
- Optional regex context and file-type filters
- Search results appear in a persistent `*grep*` buffer for convenient review and navigation
- Input is sanitised to prevent unsafe shell execution
- No setup or project configuration required

## Dependencies

- Emacs ≥ 25.1  
- `grep` (available on Unix-like systems)

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

Customise repo-grep to fit your workflow:

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
Use `:include-ext` to restrict search to specific file types.

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

Use regex fragments to match symbols in specific code contexts.

#### Example: Subroutine calls

```
(repo-grep :left-regex "CALL.*(")
```

Matches lines like CALL my_subroutine(...).

#### Example: Assignments

```
(repo-grep :right-regex ".*=")
```

Matches lines where the symbol appears on the left-hand side of an assignment.

You can define custom keybindings to frequently used patterns or filters.

## Project status

repo-grep is considered feature-complete. It remains available for use, fork, or adaptation under the license terms. Further changes are not planned.

## Security

repo-grep performs local, read-only searches using standard grep and core Emacs functionality. It does not initiate network connections, execute remote code, or require elevated permissions. Inputs to shell commands are sanitised to reduce shell-injection risk.

The package is available via [MELPA](https://melpa.org/#/repo-grep), which performs an initial manual review but builds updates automatically from the main branch. Users concerned about supply-chain integrity are encouraged to review the source and consider installing from a tagged release or known commit.

## License

BSD 3-Clause License — see [LICENSE](./LICENSE) for details.
