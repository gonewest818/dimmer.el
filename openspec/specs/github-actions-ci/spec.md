## Requirements

### Requirement: GitHub Actions CI workflow
The project SHALL have a GitHub Actions workflow at `.github/workflows/ci.yml` that runs on push and pull request.

#### Scenario: Workflow triggers on push and PR
- **WHEN** code is pushed to any branch or a pull request is opened
- **THEN** CI workflow runs automatically

### Requirement: Multi-Emacs version test matrix
The CI workflow SHALL test against Emacs versions 25.3, 26.3, 27.2, 28.2, 29.4, 30.1, and snapshot.

#### Scenario: All Emacs versions tested
- **WHEN** CI workflow runs
- **THEN** jobs execute for each version in the matrix

### Requirement: Eask installation in CI
The CI workflow SHALL install Eask via the official webinstall script before running commands.

#### Scenario: Eask available in CI environment
- **WHEN** `eask --version` runs in CI
- **THEN** command succeeds and shows version

### Requirement: Dependency installation in CI
The CI workflow SHALL run `eask install-deps --dev` to install development dependencies.

#### Scenario: Dependencies installed in sandbox
- **WHEN** `eask install-deps --dev` completes
- **THEN** `elisp-lint` is available in `.eask/`

### Requirement: Lint stages in CI
The CI workflow SHALL run the following lint checks:
- `eask lint checkdoc`
- `eask lint package`
- `eask lint declare`
- `eask lint indent`
- `eask lint elisp-lint`

#### Scenario: All lint checks pass on supported Emacs versions
- **WHEN** lint commands run on Emacs 27+
- **THEN** all commands exit with code 0

#### Scenario: Package-lint behavior on Emacs 25/26
- **WHEN** `eask lint package` runs on Emacs 25.3/26.3
- **THEN** command may warn about `frame-focus-state` but does not fail the build (or is skipped for these versions)

### Requirement: Compile stage in CI
The CI workflow SHALL run `eask compile` to byte-compile the package.

#### Scenario: Byte-compilation succeeds
- **WHEN** `eask compile` runs
- **THEN** `dimmer.elc` is generated and no errors occur

### Requirement: Cache Eask package directory
The CI workflow SHALL cache the `.eask` directory to speed up subsequent runs.

#### Scenario: Cache hit on subsequent runs
- **WHEN** CI runs again with same dependencies
- **THEN** `.eask` directory is restored from cache
