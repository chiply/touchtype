# Contributing to touchtype

Thank you for considering a contribution!

## Development Setup

1. Install [Eask](https://emacs-eask.github.io/)
2. Clone the repo and run `eask install-deps`
3. Byte-compile with `eask compile`
4. Run tests with `eask test ert test/touchtype-test.el`
5. Lint with `eask lint package && eask lint checkdoc`

## Pull Requests

* Keep PRs focused on a single change
* Add or update tests for new behaviour
* Run the full lint suite before opening a PR
* Follow the existing code style (lexical binding, cl-lib, checkdoc-clean docstrings)
