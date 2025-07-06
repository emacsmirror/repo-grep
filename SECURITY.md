# Security Policy for repo-grep

## Overview

[repo-grep](https://github.com/BHFock/repo-grep) is a tool for performing local, read-only recursive grep searches within Emacs. It operates entirely on local files and directories, using standard system utilities and Emacs core functionality.

## Security Considerations

- repo-grep does not initiate any network connections.
- It does not execute any remote code or require elevated system permissions.
- Inputs passed to shell commands are sanitised to minimise risks of shell injection.
- The tool relies on standard system `grep` utilities and Emacs features.

## Reporting Security Issues

If you discover a security vulnerability or have concerns related to repo-grep, please report it by opening an issue on this GitHub repository.

## Updates and Releases

Users are encouraged to use tagged releases or specific commits to ensure supply-chain integrity. Reviewing the source code is recommended for those with security concerns.
