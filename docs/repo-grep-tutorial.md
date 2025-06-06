# repo-grep tutorial

## Table of Contents
1. [Introduction to repo-grep](#1-introduction-to-repo-grep)
2. [Installation & setup](#2-installation--setup)
3. [Basic usage](#3-basic-usage)
4. [Advanced features](#4-advanced-features)
5. [Summary](#5-summary)

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

Before installing *repo-grep*, make sure the following is available:

- **Emacs**: Version 25.1 or newer is required.
- **Grep**: *Repo-grep* executes shell-based grep commands to perform searches.
- **Git or SVN** (optional): Enables automatic project root detection if available.

No additional tools or packages are required. This makes setup simple.

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

### Starting a search from the cursor position

To start a search, place your cursor over a symbol — such as a variable, function name, or keyword — and press `F12` (or run `M-x repo-grep`). *Repo-grep* will automatically detect the symbol under the cursor and use it as the default search term. This detection is powered by Emacs’ built-in `thing-at-point`. This works best when your cursor is on a meaningful name in the code, like a variable or subroutine.

### Interactive query

You'll be prompted in the minibuffer with the detected symbol pre-filled. You can press `Enter` to search as-is, or edit the term before confirming. This prompt supports regular expressions, so you can write more flexible patterns — for example `variable.*=` to match lines where a variable is assigned.

### Executing the search

Once confirmed, *repo-grep* locates the appropriate folder to search from:

* If you're in a Git repository or SVN working copy, it uses Emacs' built-in VCS root detection.
* If no VCS root is found, it defaults to the current directory.
* If repo-grep-subfolder is set, the search is restricted to that subfolder under the root.
* If repo-grep-from-folder-above is non-nil (as in *repo-grep-multi*), the search starts from the parent directory of the detected root.

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


### Restrict search to a specific subfolder

Sometimes you don’t want to search your entire project — just a focused part of it. That’s where `repo-grep-subfolder` comes in.

You can use this setting to limit your search to a specific subdirectory under the project root. This is especially useful when your project has a typical structure like:

```
project-root/
├── src/
├── test/
└── build/
```

Let’s say you only want to search inside `src/` and ignore everything in `test/` and `build/`. You can do that by setting:

```elisp
(setq repo-grep-subfolder "src")
```

Now, all searches will be scoped to `project-root/src/` — and nothing outside it. This also works for nested subdirectories. For example, if you want to search only within `src/physics/` and ignore `src/io/`, just set:

```elisp
(setq repo-grep-subfolder "src/physics")
```

You can set this manually in your config, or interactively using:

```elisp
M-x repo-grep-set-subfolder
```

This will prompt you to select a subfolder from your project root.

Alternatively, if you're already browsing in Dired, you can set the subfolder directly from there:

```elisp
M-x repo-grep-set-subfolder-from-dired
```

This makes it easy to narrow your search to exactly the part of the codebase you care about — without touching the rest.

Note: `repo-grep-subfolder` is ignored when using *repo-grep-multi*, since multi-repo search always starts from the parent directory of the detected root.

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

By default, *repo-grep* performs case-insensitive searches, which is often useful for general-purpose code scanning. If you want to enforce case-sensitive matching, you have two options:

#### Option 1: Set it in your configuration

```elisp
(setq repo-grep-case-sensitive t)
```

To restore the default (case-insensitive):

```elisp
(setq repo-grep-case-sensitive nil)
```

#### Option 2: Toggle it interactively

You can also toggle case sensitivity at any time using the built-in interactive command:

```elisp
M-x repo-grep-set-case-sensitivity
```

This will prompt you to choose between ON and OFF, and update the setting accordingly — no need to edit your config or restart Emacs.

### Customise match context with regex prefixes and suffixes

You can customise searches to match specific code patterns using `:left-regex` or `:right-regex`. These options allow you to match a symbol only when it appears in a specific context — such as the left-hand side of an assignment or in a subroutine call. You can do this by prepending or appending regular expression fragments to your search term.

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

## 5. Summary

*Repo-grep* is a versatile Emacs tool that enhances how you:

* Explore unfamiliar codebases
* Debug across multiple files
* Refactor consistently
* Navigate large projects efficiently — all without leaving Emacs.

By combining one-keystroke convenience, version control awareness, and regex power *repo-grep* brings intuitive, project-wide search and code navigation to your fingertips.
