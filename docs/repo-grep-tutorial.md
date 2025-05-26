# Repo-Grep Tutorial

## 1. Introduction to Repo-Grep

Navigating large codebases is hard — especially when working across disconnected tools. repo-grep brings project-wide search directly into Emacs, eliminating the need to jump into the terminal or external tools. It’s fast, flexible, and designed to fit naturally into your Emacs workflow.

### Why Repo-Grep?

- **Automatic Project Root Detection:**  
  Repo-grep automatically locates your project root using Git or SVN. For larger setups with multiple repositories, use `repo-grep-multi` to search all of them from a shared parent directory.

- **Comprehensive File Search:**
  All files under the root — including those not tracked by version control — are scanned recursively.

- **Powerful Regex Support:**  
  Use regular expressions for advanced, precise search patterns.

- **Instant Search with One Keystroke:**
  Hit a key (like `F12`) and instantly search for the symbol under your cursor — no prompts, no friction.

- **Customisable Patterns, File Exclusion, and Case Sensitivity:**  
  Easily refine your search with regex prefixes and suffixes, exclude specific file types (like `.log` files or backups), and toggle between case-sensitive and case-insensitive searches.

### Automatic Folder Selection

One of repo-grep's key features is its ability to intelligently determine the search scope based on the repository type:

* **SVN:** If you're in an SVN working copy, repo-grep searches the entire SVN directory structure by default.
* **Git:** For Git repositories, it uses `git rev-parse --show-toplevel` to find the root and searches all files from there.
* **No VCS:** If no Git or SVN metadata is found, it simply searches from the current directory down — useful for standalone folders.

### How Repo-Grep Works

Under the hood, repo-grep uses standard search tools like grep, but wraps them in Emacs-friendly workflows.
With a single command, you can:

* Search for the word under your cursor or type a custom pattern.
* Perform multi-repo searches with repo-grep-multi.
* Use regular expressions to refine your search.
* Automatically locate the right folder, even in complex setups.

This tutorial will walk you through installation, configuration, and key use cases so you can make repo-grep a seamless part of your Emacs setup

## 2. Installation & Setup

### Prerequisites

Before installing `repo-grep`, make sure the following tools are available:

- **Emacs**: Version 26 or newer recommended.
- **Git or SVN** (optional): Enables automatic project root detection.
- **awk**: Used internally for text parsing; usually preinstalled on Unix-based systems (Linux, macOS).

No external dependencies like `ripgrep` are required.

### Installing repo-grep

#### Clone the Repository

Open a terminal and run:

```sh
git clone https://github.com/BHFock/repo-grep.git
```

This clones the latest version of `repo-grep` to your system. You can place it anywhere, but a common location is `~/.emacs.d/lisp/`


#### Load repo-grep in Emacs

Add the following line to your Emacs configuration file (init.el or .emacs):

```elsip
(load "/path/to/repo-grep/repo-grep.el")
```

Replace `/path/to/repo-grep/` with the actual path where you cloned the repository.

#### Defining Keybindings

One of *repo-grep*'s core features is the ability to search the string under the cursor with a **single keystroke**, making it extremely fast and efficient.

##### Primary Keybinding: One-Keystroke Search

To bind `repo-grep` to the `F12` key for instant search of the symbol at point:

```elisp
(global-set-key [f12] 'repo-grep)
```

##### Multi-Repository Search Keybinding

To bind multi-repository search to `Ctrl + F12`:

```elisp
(global-set-key [C-f12] 'repo-grep-multi)
```

This allows you to search across multiple repositories under a shared parent folder.

##### Reloading Keybindings

After saving these changes, reload your Emacs config or restart Emacs for the keybindings to take effect. You're now ready to start searching with repo-grep using just a keystroke. Next, we’ll look at basic usage patterns and how to refine your searches.

## 3. Basic Usage

### Triggering the Search

To start a search, place your cursor (point) over a symbol — such as a variable, function name, or keyword — and press `F12` (or run `M-x repo-grep`). `repo-grep` will automatically detect the symbol under the cursor and use it as the default search term.

### Interactive Query 

A prompt displays the default search term at the bottom. Modify it if needed, then press `Enter` to confirm. For example, use regular expressions like `variable.*=` as your modified search term to find variable assignments.

### Interactive Query

You'll be prompted in the minibuffer with the detected symbol pre-filled. You can press `Enter` to search as-is, or edit the term before confirming. This prompt supports regular expressions, so you can write more flexible patterns — for example `variable.*=` to match lines where a variable is assigned.

### Executing the Search

Once confirmed, `repo-grep` locates the appropriate search root:

* If you're in a Git repository, it uses `git rev-parse` to find the root.
* If you're in an SVN checkout, it uses the SVN structure.
* Otherwise, it defaults to the current directory.

### Reviewing Results

Search results appear in a dedicated `*grep*` buffer. Each result is a clickable link — click (or press `RET`) to jump directly to the matching line in its file. This makes it easy to quickly inspect multiple matches across your project.

With just one keystroke, `repo-grep` turns symbol lookup into a fast, interactive process — no need to leave Emacs or set up project metadata.

## 4. Advanced Features


### Multi-Repository Search

If your projects are structured as multiple repositories under a common folder, `repo-grep-multi` can search them all in one go. It works by shifting the search root **one directory above** the Git/SVN root, allowing you to include sibling repositories.

#### Example: Bind to `Ctrl + F12`

```elisp
(global-set-key [C-f12] 'repo-grep-multi)
```

Now pressing `Ctrl + F12` triggers a recursive search across all sibling directories — no manual navigation needed.

### Regex Prefixes and Suffixes

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


### Exclude Noisy File Types

To keep your results clean, you can tell `repo-grep` to ignore specific file extensions — such as logs, compiled outputs, or Emacs backups.

#### Example: Exclude `.log` and `~` files

```elisp
(global-set-key [f9]
  (lambda () (interactive)
    (repo-grep :exclude-ext '(".log" "~"))))
```

This ensures that temporary or irrelevant files don’t clutter your search output.


### Toggle Case Sensitivity

By default, `repo-grep` performs **case-insensitive** searches — which is often useful in general-purpose code scanning. If you want to enforce case-sensitive matching:


```elisp
(setq repo-grep-case-sensitive t)
```

To restore the default (case-insensitive):


```elisp
(setq repo-grep-case-sensitive nil)
```

Each of these features can be customised in your Emacs config to fit your workflow — from targeted regex searches to ignoring unwanted files.


## 5. Practical Examples

* **Searching for Function Definitions:** Place your cursor on a function name and trigger a search to see all occurrences in your codebase.
* **Finding Variable Assignments:** Use regex customization (e.g., adding :right-regex ".*=") to locate lines where variables are assigned values.
* **Debugging Log Searches:** Optionally exclude log files from your search results or include them selectively to track down issues.

Repo-grep's versatility makes it an invaluable tool for debugging, refactoring, and exploring large codebases—all without leaving the Emacs environment.
