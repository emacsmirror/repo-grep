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
* If the directory is part of a Git repository, Git takes precedence, and the search is performed across all tracked files.
* If neither SVN nor Git is detected, repo-grep defaults to searching all subdirectories from the current folder, making it useful even for non-version-controlled projects.

### How Repo-Grep Works

At its core, repo-grep leverages standard search utilities but optimizes them for Emacs users. With a simple command, users can:
Search for the word under the cursor or specify a custom query.

* Perform multi-repository searches using repo-grep-multi.
* Use regular expressions for advanced filtering.
* This tutorial will guide you through installation, basic usage, and advanced features to help you maximize repo-grep's potential in your development workflow.

## 2. Installation & Setup

- Step-by-step guide on installing and configuring repo-grep in Emacs.
- Explain how to modify .emacs or init.el to integrate it properly.
- Show how to bind keys for quick access.

## 3. Basic Usage
 
- Demonstrate how to search using the default behavior (string under cursor).
- Explain interactive modification of search terms.
- Show how to use regular expressions for more refined searches.

## 4. Advanced Features

- Using repo-grep-multi to search across multiple repositories.
- Customizing searches with regex prefixes and suffixes.
- Excluding certain file types (e.g., .log or backup files).
- Making searches case-sensitive or case-insensitive.

## 5. Practical Examples
- Walk through real-world examples of searching for function definitions, variable assignments, or debugging logs.
- Show how repo-grep can be integrated into workflows for faster code navigation.
