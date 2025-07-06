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

If you discover a security vulnerability or have concerns related to `repo-grep`, please report it by opening an issue on this GitHub repository. For sensitive disclosures, you may also contact the maintainer directly via GitHub. 

## Updates and Releases 

Users are encouraged to rely on tagged releases or specific commits to ensure supply-chain integrity. Reviewing the source code is recommended for users with security concerns.
