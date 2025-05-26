# Repo-Grep Tutorial

## 1. Introduction to Repo-Grep

Navigating large codebases can be challenging. Repo-grep offers a seamless, Emacs-integrated search experience that keeps you focused on your code by eliminating the need to switch between tools.

### Why Repo-Grep?

- **Automatic Folder Detection:**  
  It automatically finds your repository root using Git or SVN, ensuring you search in the correct directory. If needed, `repo-grep-multi` can shift the search context one level up to cover multiple repositories in a common parent directory.
  
- **Recursive File Search:**  
  Repo-grep scans every file in the folder structure, including those not tracked by version control.

- **Powerful Regex Support:**  
  Use regular expressions for advanced, precise search patterns.

- **One-Keystroke Search:**  
  A single keystroke (e.g., `F12`) runs an interactive search using the symbol under your cursor.

- **Customisable Patterns, File Exclusion, and Case Sensitivity:**  
  Easily refine your search with regex prefixes and suffixes, exclude specific file types (like `.log` files or backups), and toggle between case-sensitive and case-insensitive searches.

### Automatic Folder Selection

One of repo-grep's key features is its ability to intelligently determine the search scope based on the repository type:

* If the current directory is part of an SVN working copy, repo-grep searches all folders within the SVN structure.
* If the directory is part of a Git repository, Git takes precedence. Repo-grep uses git rev-parse --show-toplevel to locate the repository root, then recursively searches all files within that directory.
* If neither SVN nor Git is detected, repo-grep defaults to searching all subdirectories from the current folder, making it useful even for non-version-controlled projects.

### How Repo-Grep Works

At its core, repo-grep leverages standard search utilities but optimizes them for Emacs users. With a simple command, users can:
Search for the word under the cursor or specify a custom query.

* Perform multi-repository searches using repo-grep-multi.
* Use regular expressions for advanced filtering.
* This tutorial will guide you through installation, basic usage, and advanced features to help you maximize repo-grep's potential in your development workflow.

## 2. Installation & Setup

### Prerequisites

Before installing repo-grep, ensure you have:

* Emacs installed.
* Git or SVN (optional, but enhances functionality).
* AWK (pre-installed on most Unix-based systems).

### Installing repo-grep

#### Clone the Repository

Open a terminal and run:

```sh
git clone https://github.com/BHFock/repo-grep.git
```
This will download the latest version of repo-grep to your local machine.

#### Load repo-grep in Emacs

Add the following line to your Emacs configuration file (init.el or .emacs):

```elsip
(load "/path/to/repo-grep/repo-grep.el")
```

Replace `/path/to/repo-grep/` with the actual path where you cloned the repository.

#### Defining Keybindings

One of *repo-grep*'s core features is the ability to search the string under the cursor with a **single keystroke**, making it extremely fast and efficient.

##### Primary Keybinding: One-Keystroke Search
Assign `F12` to immediately trigger *repo-grep* and search for the string under the cursor:

```elisp
(global-set-key [f12] 'repo-grep)
```

##### Multi-Repository Search Keybinding

For broader searches across multiple repositories, use `Ctrl + F12`

```elisp
(global-set-key [C-f12] 'repo-grep-multi)
```

This allows rapid multi-repository searches without manual directory selection.

##### Reloading Keybindings

After adding these lines to your configuration file, reload Emacs to apply the changes. With these keybindings, repo-grep enables instant searching with minimal effort, making code navigation seamless and efficient.

## 3. Basic Usage

### Triggering the Search  
Place your cursor over a symbol like a variable or function name you want to search for and press `F12` (or run `M-x repo-grep`). The command automatically captures that symbol as the default search term.

### Interactive Query 
A prompt displays the default search term at the bottom. Modify it if needed, then press `Enter` to confirm. For example, use regular expressions like `variable.*=` as your modified search term to find variable assignments.

### Executing the Search 
Repo-grep determines the repository root (via SVN or Git) and recursively searches all files in that folder structure.

### Reviewing Results  
The grep results appear in a dedicated buffer containing clickable links. Clicking a link takes you directly to the corresponding match in your code.

This concise workflow lets you quickly navigate your code with interactive searches and direct result navigation.

## 4. Advanced Features

### Multi-Repository Searches with repo-grep-multi

For broader searches across multiple repositories or folders, use `repo-grep-multi`. This command shifts the search directory one level up from the repository root. To activate it, add the following to your Emacs configuration file (init.el or .emacs):

```elisp
(global-set-key [C-f12] 'repo-grep-multi)
```

This binding lets you perform multi-repository searches with a single keystroke.

### Customizing Searches with Regex Prefixes and Suffixes

Tailor your search queries by adding regular expression prefixes or suffixes. For example, if you want to search for variable assignments or subroutine calls, modify the search term by configuring your keybindings as follows:
 
```elisp
;; For searching variable assignments
(global-set-key [f11] (lambda () (interactive) (repo-grep :right-regex ".*=")))

;; For searching subroutine calls
(global-set-key [f10] (lambda () (interactive) (repo-grep :left-regex "CALL.*(.*")))
```

These adjustments allow you to predefine search patterns directly in your Emacs configuration.

### Excluding Certain File Types

Filter out irrelevant results by excluding files with specific extensions, such as log files or backups. To implement this, add the following to your Emacs config:

```elisp
(global-set-key [f12] 
  (lambda () (interactive) (repo-grep :exclude-ext '(".log" "~"))))
```

This setting ensures that unwanted file types do not clutter your search results.

### Making Searches Case-Sensitive

By default repo-grep is case-insensitive. Control the sensitivity of your searches by setting the repo-grep-case-sensitive variable. To perform case-sensitive searches, include this in your configuration:

```elisp
(setq repo-grep-case-sensitive t)
```

Each of these advanced features is configurable through your Emacs config file, giving you the flexibility to fine-tune repo-grep to match your development needs.
