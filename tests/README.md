# Testing emacs-writing

This directory contains tests for the emacs-writing package.

## Running Tests

### All Tests

From Emacs:

```elisp
(load-file "tests/writing-test.el")
(writing-run-tests)
```

Or from command line:

```sh
emacs -batch -l tests/writing-test.el -f ert-run-tests-batch-and-exit
```

### Word Count Tests

Requires org-context-extended to be installed.

```elisp
(load-file "tests/test-wordcount.el")
(writing-wordcount-run-tests)
```

## Test Structure

- `writing-test.el` - Core functionality tests
- `test-wordcount.el` - Word counting functionality tests
- Future: `test-modes.el` - Writing modes tests
- Future: `test-search.el` - Search functionality tests

## Test Coverage

Current test coverage focuses on:
- Core utility functions
- Configuration variables
- Project detection
- File name sanitization

Future test additions will cover:
- Word counting accuracy
- Mode mutual exclusivity
- Search functionality
- Capture system
- Export filters

## Test Data

Test data should be placed in `tests/data/` directory (to be created).

Example test files:
- Sample novel.org with properties
- Sample notes structure
- Sample character database

## Continuous Integration

TODO: Set up CI with Codeberg workflows
