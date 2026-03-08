# Security Policy for repo-grep 

## Overview 

[repo-grep](https://github.com/BHFock/repo-grep) is a tool for performing local, read-only, recursive grep searches within Emacs. It operates entirely on local files and directories, using standard system utilities and Emacs core functionality. 

## Security Considerations 
- `repo-grep` does not initiate any network connections.
- It does not execute remote code or require elevated system permissions.
- Inputs passed to shell commands are sanitised to minimise the risk of shell injection.
- The tool relies on standard system `grep` utilities and Emacs features.

## Trust Model

`repo-grep` assumes a trusted local development environment. It does not attempt to sandbox or verify the integrity of the underlying `grep` binary or Emacs installation. Users are responsible for ensuring their system tools are secure and up to date. 

## Reporting Security Issues 

If you notice something that might be a security issue, please report it privately using GitHub’s [security advisory form](../../security/advisories/new).

## Updates and Releases 

Users are encouraged to rely on tagged releases or specific commits to ensure supply-chain integrity. Reviewing the [source code](https://github.com/BHFock/repo-grep/blob/main/repo-grep.el) is recommended for users with security concerns.

The package is also available via [MELPA](https://melpa.org/#/repo-grep), which performs an initial manual review but builds updates automatically from the main branch. Users concerned about supply-chain integrity may prefer to install from a tagged release or known commit.
