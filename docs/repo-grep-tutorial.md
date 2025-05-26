# Repo-Grep Tutorial

## 1. Introduction to Repo-Grep

Navigating large codebases can be tedious, especially when searching for specific functions, variables, or patterns across multiple files. While tools like grep provide powerful search capabilities, repo-grep offers a streamlined approach tailored for Emacs users working within version-controlled repositories

### Why Repo-Grep?

Repo-grep is designed to integrate seamlessly with Emacs, allowing users to quickly search through a repository without leaving their editor. Whether you're debugging, refactoring, or exploring unfamiliar code, repo-grep simplifies the process by:

* Automatically detecting the repository root.
* Searching efficiently across all files in your folder structure.
* Providing an interactive interface for refining search queries.

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
 
- **Triggering the Search:**  
  Place your cursor over a symbol like a variable or function name you want to search for and press `F12` (or run `M-x repo-grep`). The command automatically captures that symbol as the default search term.

- **Interactive Query:**  
  A prompt displays the default search term at the bottom. Modify it if needed, then press `Enter` to confirm. For example, use regular expressions like `variable.*=` as your modified search term to find variable assignments.

- **Executing the Search:**  
  Repo-grep determines the repository root (via SVN or Git) and recursively searches all files in that folder structure.

- **Reviewing Results:**  
  The grep results appear in a dedicated buffer containing clickable links. Clicking a link takes you directly to the corresponding match in your code.

This concise workflow lets you quickly navigate your code with interactive searches and direct result navigation.


## 4. Advanced Features

- Using repo-grep-multi to search across multiple repositories.
- Customizing searches with regex prefixes and suffixes.
- Excluding certain file types (e.g., .log or backup files).
- Making searches case-sensitive or case-insensitive.

## 5. Practical Examples
- Walk through real-world examples of searching for function definitions, variable assignments, or debugging logs.
- Show how repo-grep can be integrated into workflows for faster code navigation.
