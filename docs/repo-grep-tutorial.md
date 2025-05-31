# repo-grep tutorial

## 1. Introduction to repo-grep

Navigating large codebases can be difficult — especially when working across disconnected tools. *Repo-grep* brings project-wide search directly into Emacs, eliminating the need to jump into the terminal or external tools. It’s fast, flexible, and designed to fit naturally into your Emacs workflow.

### Why repo-grep?

*Repo-grep* is designed to enhance developer productivity by providing instant, seamless search with minimal setup—helping engineers work more efficiently, navigate large codebases with ease, and stay focused on writing high-quality code. Core features include:

- **Instant search with one keystroke:**   
  Press a key (like `F12`) and instantly search for the symbol under your cursor — no prompts, no friction.

- **Automatic project root detection:**  
  *Repo-grep* automatically locates your project root using Git or SVN. For larger setups with multiple repositories, use *repo-grep-multi* to search all of them from a shared parent directory.

- **Comprehensive file search:**  
  All files under the root — including those not tracked by version control — are scanned recursively.

- **Powerful regex support:**  
  Use regular expressions for advanced, precise search patterns.

- **Customisable patterns, file exclusion, and case sensitivity:**  
  Easily refine your search with regex prefixes and suffixes, exclude specific file types (like `.log` files or backups), and toggle between case-sensitive and case-insensitive searches.

### Automatic folder selection

One of *repo-grep*'s key features is its ability to intelligently determine the search scope based on the repository type:

* **SVN:** If you're in an SVN working copy, *repo-grep* searches the entire SVN directory structure by default.
* **Git:** For Git repositories, it uses `git rev-parse --show-toplevel` to find the root and searches all files from there.
* **No VCS:** If no Git or SVN metadata is found, it simply searches from the current directory down — useful for standalone folders.

### How repo-grep works

Under the hood, *repo-grep* uses standard grep, but wraps it in an Emacs-friendly workflow. With a single command, you can:

* Search for the word under your cursor or type a custom pattern.
* Perform multi-repo searches with *repo-grep-multi*.
* Use regular expressions to refine your search.
* Automatically locate the right folder, even in complex setups.

This tutorial will walk you through installation, configuration, and key use cases so you can make *repo-grep* a seamless part of your Emacs setup

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

### Reviewing results

Search results appear in a dedicated `*grep*` buffer. Each result is a clickable link — click (or press `RET`) to jump directly to the matching line in its file. This makes it easy to quickly inspect multiple matches across your project.

With just one keystroke, *repo-grep* turns symbol lookup into a fast, interactive process — no need to leave Emacs or set up project metadata.

## 4. Advanced features


### Multi-repository search

If your projects are structured as multiple repositories under a common folder, *repo-grep-multi* can search them all in one go. It works by shifting the search root one directory above the Git/SVN root, allowing you to include sibling repositories.

#### Example: Bind to `Ctrl + F12`

```elisp
(global-set-key [C-f12] 'repo-grep-multi)
```

Now pressing `Ctrl + F12` triggers a recursive search across all sibling directories — no manual navigation needed.

### Regex prefixes and suffixes

You can customise searches to match specific code patterns using `:left-regex` or `:right-regex`. These let you “wrap” the search term in regex — useful for targeting things like assignments, function calls, or declarations.

#### Example 1: Match variable assignments

```elisp
(global-set-key [f11]
  (lambda () (interactive)
    (repo-grep :right-regex ".*=")))
```

This searches for lines where the symbol is followed by an equals sign — useful for finding assignments like:

```
gravity_at_sea_level = 9.81
```

#### Example 2: Match subroutine calls

```elisp
(global-set-key [f10]
  (lambda () (interactive)
    (repo-grep :left-regex "CALL.*")))
```

This matches lines where the symbol is preceded by CALL, e.g. to understand the call tree of a Fortran programme.

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
