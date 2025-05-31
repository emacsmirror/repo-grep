# repo-grep tutorial

## 1. Introduction to repo-grep

Navigating large codebases doesn’t have to be slow or fragmented. *Repo-grep* brings fast, project-wide search directly into Emacs — eliminating the need to switch to a terminal or external tools.

### Key features:

- **One-keystroke search:** Instantly grep for the symbol under your cursor — with minimal prompts and no setup.
- **Smart project root detection:** Automatically locates your Git or SVN root; falls back to the current directory if needed.
- **Multi-repo search:** Scan all sibling repos with `repo-grep-multi`.
- **Searches all files:** All files under the root are searched, including those not tracked by version control.
- **Regex support:** Add prefix/suffix patterns to fine-tune matches.
- **Custom file exclusions and case sensitivity:** Tailor results to your workflow.

*Repo-grep* keeps you focused inside Emacs — no context switching, no distractions.

## 2. Installation & setup

### Prerequisites

Before installing *repo-grep*, make sure the following tools are available:

- **Emacs**: Version 24.4 or newer recommended.
- **Git or SVN** (optional): Enables automatic project root detection.
- **awk**: Used internally for text parsing; usually preinstalled on Unix-based systems (Linux, macOS).

No additional dependencies like `ripgrep` or `projectile` are required. This makes setup simple.

### Installing repo-grep

#### Clone the repository

Run the following command in your terminal:

```
git clone https://github.com/BHFock/repo-grep.git ~/repo-grep
```

You can place it in any directory of your choice, but ensure you update your Emacs configuration accordingly.

#### Load repo-grep in Emacs

Edit your Emacs configuration file (`~/.emacs` or `~/.emacs.d/init.el`) and add:

```elisp
;; Add repo-grep to your Emacs load path
(add-to-list 'load-path "~/repo-grep")

;; Autoload repo-grep functions for efficient project-wide search
(autoload 'repo-grep "repo-grep")
(autoload 'repo-grep-multi "repo-grep")
```

Make sure `~/repo-grep` matches the actual path where you cloned the repository.

#### Define keybindings

To enable instant searching, bind the functions to convenient keys:

```elisp
(global-set-key [f12] 'repo-grep)         ;; Single-repository search
(global-set-key [C-f12] 'repo-grep-multi) ;; Multi-repository search
```

Once you've saved these changes, reload your Emacs configuration or restart Emacs to apply them. You're now ready to start searching with *repo-grep* using just a keystroke. Next, we’ll look at basic usage patterns and how to refine your searches.

## 3. Basic usage

### Triggering the search

To start a search, place your cursor (point) over a symbol — such as a variable, function name, or keyword — and press `F12` (or run `M-x repo-grep`). *Repo-grep* will automatically detect the symbol under the cursor and use it as the default search term.

### Interactive query

You'll be prompted in the minibuffer with the detected symbol pre-filled. You can press `Enter` to search as-is, or edit the term before confirming. This prompt supports regular expressions, so you can write more flexible patterns — for example `variable.*=` to match lines where a variable is assigned.

### Executing the search

Once confirmed, *repo-grep* locates the appropriate search root:

* If you're in a Git repository, it uses `git rev-parse` to find the root.
* If you're in an SVN checkout, it uses the SVN structure.
* Otherwise, it defaults to the current directory.

Regardless of how the root is detected, *repo-grep* searches all files within that root — not just those tracked by version control. This makes it ideal for scanning generated files, uncommitted changes, or legacy code alongside source files.

### Reviewing results

Search results appear in a dedicated `*grep*` buffer. Each result is a clickable link — click (or press `RET`) to jump directly to the matching line in its file. This makes it easy to quickly inspect multiple matches across your project.

With just one keystroke, *repo-grep* turns symbol lookup into a fast, interactive process — no need to leave Emacs or set up project metadata.

## 4. Advanced features

### Multi-repository search

If your projects are structured as multiple repositories under a common folder, *repo-grep-multi* can search them all in one go. If you have multiple repositories in the same parent directory (e.g., `~/projects/repo1`, `~/projects/repo2`), *repo-grep-multi* will search across all repositories under `~/projects`. Internally, this is done by setting a flag that tells *repo-grep* to move up one folder level from the detected Git or SVN root. This makes multi-repo searches automatic and context-aware.

#### Example: Bind to `Ctrl + F12`

```elisp
(global-set-key [C-f12] 'repo-grep-multi)
```

Now pressing `Ctrl + F12` will search through all folders under the parent directory — no need to manually navigate or run multiple searches.

### Regex prefixes and suffixes

You can customise searches to match specific code patterns using `:left-regex` or `:right-regex`. These let you “wrap” the search term in regex — useful for targeting things like assignments, function calls, or declarations.

#### Example 1: Match variable assignments (symbol on the left-hand side)

To find where a variable is being assigned a value — and not just used in a calculation — you can append an equals sign using `:right-regex`.

```elisp
(global-set-key [f11]
  (lambda () (interactive)
    (repo-grep :right-regex ".*=")))
```

This searches for cases where the symbol appears to the left of an equals sign. For example, if your cursor is on `gravity_at_sea_level`, this pattern would match:

```
gravity_at_sea_level = 9.81
```

But it would not match if the variable is only used on the right-hand side:

```
weight = mass * gravity_at_sea_level
```

#### Example 2: Build an interactive call tree (subroutine calls)

If you're exploring a large Fortran or procedural codebase, you might want to trace which routines call a given subroutine — essentially building a lightweight, interactive call tree.

You can do this using a prefix regex that matches subroutine call sites, e.g., lines starting with `CALL`. This allows you to quickly jump to all points where a subroutine is invoked.

```elisp
(global-set-key [f10]
  (lambda () (interactive)
    (repo-grep :left-regex "CALL.*")))
```

With this setup, place your cursor over the name of a subroutine, press F10, and Emacs will list every line where it is called. Since results are clickable in the `*grep*` buffer, you can walk through each call site interactively — making it easy to understand control flow and dependencies, without any plugins or static analysis tools.

### Exclude unwanted file types

To keep your results clean, you can tell *repo-grep* to ignore specific file extensions — such as logs, compiled outputs, or Emacs backups.

#### Example: Exclude `.log` and `~` files

```elisp
(global-set-key [f9]
  (lambda () (interactive)
    (repo-grep :exclude-ext '(".log" "~"))))
```

This ensures that temporary or irrelevant files don’t clutter your search output.

### Toggle case sensitivity

By default, *repo-grep* performs case-insensitive searches — which is often useful in general-purpose code scanning. If you want to enforce case-sensitive matching:

```elisp
(setq repo-grep-case-sensitive t)
```

To restore the default (case-insensitive):

```elisp
(setq repo-grep-case-sensitive nil)
```

Each of these features can be customised in your Emacs config to fit your workflow — from targeted regex searches to ignoring unwanted files.

## 5. Summary

*Repo-grep* is a versatile Emacs tool that enhances how you:

* Explore unfamiliar codebases
* Debug across multiple files
* Refactor consistently
* Navigate large projects efficiently — all without leaving Emacs.

By combining one-keystroke convenience, version control awareness, and regex power *repo-grep* brings intuitive, project-wide search to your fingertips.
