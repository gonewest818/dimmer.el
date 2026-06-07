## Why

The dimmer.el project currently uses CircleCI with a custom Makefile and manual dependency bootstrap (`.emacs/init.el` + `.emacs/dependencies.el`). CircleCI is deprecated for this project, and the toolchain is fragile — it requires manual GPG key setup, custom Emacs initialization, and doesn't integrate cleanly with modern GitHub Actions workflows. Migrating to Eask + GitHub Actions provides a maintained, declarative build tool with first-class CI support, sandboxed dependencies, and built-in linting commands that replace the ad-hoc `elisp-lint` invocation.

## What Changes

- **Add** `Eask` file declaring package metadata and development dependencies (replaces `.emacs/dependencies.el`)
- **Add** `.github/workflows/ci.yml` for GitHub Actions CI (replaces `.circleci/config.yml`)
- **Simplify** `Makefile` to delegate to `eask` commands (optional local convenience)
- **Remove** `.circleci/config.yml`, `.emacs/init.el`, `.emacs/dependencies.el`, `.emacs/.emacs-custom.el*`
- **No changes** to `dimmer.el` source — `Package-Requires: ((emacs "25.1"))` stays for this PR; legacy focus-handling code remains

## Capabilities

### New Capabilities

- `eask-build`: Declarative project configuration via `Eask` file with development dependencies (elisp-lint, ert)
- `github-actions-ci`: Multi-Emacs version CI pipeline (25.3, 26.3, 27.2, 28.2, 29.4, 30.1, snapshot) with lint, compile, and test stages
- `eask-lint-suite`: Granular linting commands (`checkdoc`, `package`, `declare`, `indent`, `elisp-lint`) replacing monolithic `elisp-lint-files-batch`

### Modified Capabilities

- `build-toolchain`: Migration from Makefile + CircleCI to Eask + GitHub Actions; same capabilities (lint, compile, test), new implementation

## Impact

- **Files added**: `Eask`, `.github/workflows/ci.yml`
- **Files modified**: `Makefile` (simplified)
- **Files removed**: `.circleci/config.yml`, `.emacs/init.el`, `.emacs/dependencies.el`, `.emacs/.emacs-custom.el*`
- **No runtime changes**: `dimmer.el` unchanged; Emacs 25.1 support retained
- **MELPA release**: Unaffected; future PR can add Eask maintainer plugin for automation