# emacs-writing Test Suite

Comprehensive test suite for the emacs-writing package using Emacs Lisp Regression Testing (ERT).

## Test Coverage

The test suite covers all major modules:

- **Core Utilities** (`writing-test.el`) - Project detection, utilities, feature detection
- **Project Creation** (`test-project.el`) - Template processing, validation, project structure
- **Capture System** (`test-capture.el`) - Capture templates, file creation, project-aware routing
- **Search Functions** (`test-search.el`, `test-search-links.el`) - org-ql searches, link extraction helpers
- **Character Linking** (`test-character-links.el`) - ID-based character links, timeline generation
- **Location Linking** (`test-location-links.el`) - ID-based location links
- **Plot Thread Linking** (`test-plot-links.el`) - Plot thread tracking, statistics, timeline
- **Column View** (`test-column-view.el`) - Link stripping in column view
- **Export Filters** (`test-export.el`) - Scene break replacement for various backends
- **Word Counting** (`test-wordcount.el`) - Accurate word counting (requires org-context-extended)

## Running Tests

### Interactive Mode

Run all tests:
```elisp
M-x emacs-writing-run-all-tests
```

Run specific test suites:
```elisp
M-x emacs-writing-run-core-tests
M-x emacs-writing-run-project-tests
M-x emacs-writing-run-capture-tests
M-x emacs-writing-run-search-tests
M-x emacs-writing-run-linking-tests
M-x emacs-writing-run-export-tests
M-x emacs-writing-run-wordcount-tests
```

View test statistics:
```elisp
M-x emacs-writing-test-statistics
```

### Batch Mode

Run all tests from command line:
```sh
cd /path/to/emacs-writing
emacs -batch -l tests/run-all-tests.el -f emacs-writing-run-tests-batch
```

Or simply:
```sh
emacs -batch -l tests/run-all-tests.el
```
(runs automatically when loaded in batch mode)

### Individual Test Files

Run individual test files:
```sh
# Core tests
emacs -batch -l tests/writing-test.el -f ert-run-tests-batch-and-exit

# Project creation tests
emacs -batch -l tests/test-project.el -f test-project-run-tests

# Capture system tests
emacs -batch -l tests/test-capture.el -f test-capture-run-tests

# And so on...
```

## Test Organization

### Test Files

- `writing-test.el` - Core utilities and configuration
- `test-wordcount.el` - Word counting (optional dependency)
- `test-project.el` - Project creation and templates
- `test-capture.el` - Capture system
- `test-search.el` - Search functions
- `test-search-links.el` - Link extraction helpers
- `test-character-links.el` - Character linking
- `test-location-links.el` - Location linking
- `test-plot-links.el` - Plot thread linking
- `test-column-view.el` - Column view enhancement
- `test-export.el` - Export filters
- `run-all-tests.el` - Master test runner

### Test Naming Convention

All test functions follow the pattern:
- `test-MODULE-DESCRIPTION` for new test files
- `writing-test-DESCRIPTION` for core tests
- `test-FEATURE-ASPECT` for specific features

Examples:
- `test-project-module-loads`
- `writing-test-sanitize-filename`
- `test-capture-character-file-detection`
- `test-export-scene-break-replacement-html`

## Writing New Tests

### Basic Test Structure

```elisp
(ert-deftest test-module-feature-description ()
  "Test that feature works correctly."
  (should (equal expected-value (function-call))))
```

### Common Patterns

**Testing function existence:**
```elisp
(should (fboundp 'function-name))
```

**Testing variable existence:**
```elisp
(should (boundp 'variable-name))
```

**Testing with temporary files:**
```elisp
(let ((temp-file (make-temp-file "test-" nil ".org")))
  (unwind-protect
      (progn
        ;; Test code here
        )
    ;; Cleanup
    (when (file-exists-p temp-file)
      (delete-file temp-file))))
```

**Testing with temporary buffers:**
```elisp
(with-temp-buffer
  (org-mode)
  (insert "* Test content")
  ;; Test code here
  )
```

### Test Dependencies

Some tests require optional dependencies:
- `test-wordcount.el` requires `org-context-extended`
- `test-search.el` requires `org-ql`
- `test-*-links.el` require `org-id`

Tests automatically check for dependencies and skip if not available.

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: purcell/setup-emacs@master
        with:
          version: 29.1
      - name: Install dependencies
        run: |
          emacs -batch --eval "(package-initialize)" \
                --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
                --eval "(package-refresh-contents)" \
                --eval "(package-install 'org-ql)" \
                --eval "(package-install 'org-context-extended)"
      - name: Run tests
        run: emacs -batch -l tests/run-all-tests.el
```

### Makefile Example

```makefile
.PHONY: test test-core test-project test-all

test-all:
	emacs -batch -l tests/run-all-tests.el

test-core:
	emacs -batch -l tests/writing-test.el -f writing-run-tests

test-project:
	emacs -batch -l tests/test-project.el -f writing-project-run-tests

test-interactive:
	emacs -Q -l tests/run-all-tests.el --eval "(emacs-writing-run-all-tests)"
```

## Test Statistics

As of the latest version:
- **Test Files:** 11
- **Total Tests:** 150+
- **Coverage:** All major modules and features

Run `M-x emacs-writing-test-statistics` for current statistics.

## Troubleshooting

### Tests fail to load

Ensure all paths are correctly set:
```elisp
(let ((default-directory "/path/to/emacs-writing/"))
  (add-to-list 'load-path default-directory)
  (load-file "tests/run-all-tests.el"))
```

### Missing dependencies

Install required packages:
```elisp
(package-install 'org-ql)
(package-install 'org-context-extended)
(package-install 'writeroom-mode)
```

### Batch mode errors

Use `-Q` flag to avoid loading user configuration:
```sh
emacs -Q -batch -l tests/run-all-tests.el
```

## Contributing

When adding new features to emacs-writing:

1. Create corresponding test file in `tests/`
2. Follow naming conventions
3. Test both success and failure cases
4. Add tests to `emacs-writing-test-files` in `run-all-tests.el`
5. Update this README if adding new test categories
6. Run full test suite before committing

## License

Copyright (C) 2025 Javier Castilla

Tests are part of the emacs-writing package and share the same license.
