## 1. Eask Configuration

- [x] 1.1 Create `Eask` file at repository root with package-file declaration for `dimmer.el`
- [x] 1.2 Add `elisp-lint` to development dependencies in `Eask` file
- [x] 1.3 Verify `eask analyze` passes on the new `Eask` file
- [x] 1.4 Verify `eask install-deps --dev` installs elisp-lint in `.eask/`
- [x] 1.5 Verify `eask files` lists `dimmer.el` correctly

## 2. GitHub Actions Workflow

- [x] 2.1 Create `.github/workflows/ci.yml` with matrix strategy for Emacs 25.3, 26.3, 27.2, 28.2, 29.4, 30.1, snapshot
- [x] 2.2 Configure `purcell/setup-emacs` action for each matrix version
- [x] 2.3 Add Eask installation step via webinstall script
- [x] 2.4 Add `eask install-deps --dev` step
- [x] 2.5 Add lint stages: `eask lint checkdoc`, `eask lint package`, `eask lint declare`, `eask lint indent`, `eask lint elisp-lint`
- [x] 2.6 Add compile stage: `eask compile`
- [x] 2.7 Configure caching for `.eask` directory
- [x] 2.8 Handle package-lint on Emacs 25/26 (skip or allow warnings)

## 3. Makefile Simplification

- [x] 3.1 Update `Makefile` to delegate `lint`, `compile`, `test`, `install-deps`, `clean` to `eask` commands
- [x] 3.2 Keep `EMACS ?= emacs` and `EASK` variable detection
- [x] 3.3 Verify `make lint`, `make compile`, `make install-deps` work locally

## 4. Cleanup Legacy Files

- [x] 4.1 Remove `.circleci/config.yml`
- [x] 4.2 Remove `.emacs/init.el`
- [x] 4.3 Remove `.emacs/dependencies.el`
- [x] 4.4 Remove `.emacs/.emacs-custom.el` and `.emacs/.emacs-custom.el~`
- [x] 4.5 Remove `.emacs/` directory if empty

## 5. Verification

- [x] 5.1 Push changes and verify GitHub Actions CI passes on all Emacs versions
- [x] 5.2 Verify `make lint` and `make compile` work locally
- [x] 5.3 Verify `eask lint checkdoc`, `eask lint package`, `eask lint declare`, `eask lint indent`, `eask lint elisp-lint` all pass locally
- [x] 5.4 Verify `eask compile` generates `dimmer.elc` without errors
- [x] 5.5 Confirm `dimmer.el` still loads and works on Emacs 25.1+ (no runtime changes)