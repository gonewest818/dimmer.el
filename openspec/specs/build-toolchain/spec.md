## Requirements

### Requirement: Eask as primary build tool
The build toolchain SHALL use Eask as the primary build tool, replacing the custom Makefile + CircleCI bootstrap.

#### Scenario: Eask commands work locally
- **WHEN** developer runs `eask lint checkdoc`, `eask compile`, etc.
- **THEN** commands execute successfully without manual Emacs bootstrap

### Requirement: GitHub Actions as CI platform
The CI platform SHALL be GitHub Actions, replacing CircleCI.

#### Scenario: CI runs on GitHub Actions
- **WHEN** code is pushed to GitHub
- **THEN** GitHub Actions workflow executes the build toolchain

### Requirement: Sandboxed dependency management
Dependency management SHALL be handled by Eask's sandbox (`.eask/`), replacing manual `.emacs/init.el` bootstrap.

#### Scenario: Dependencies isolated per project
- **WHEN** `eask install-deps --dev` runs
- **THEN** dependencies installed in `.eask/` without affecting user's Emacs config

### Requirement: Build toolchain executes lint, compile, and test
The project SHALL provide a build toolchain that executes linting, byte-compilation, and testing.

#### Scenario: Lint execution
- **WHEN** user runs the lint command
- **THEN** all configured lint checks execute and report results

#### Scenario: Compile execution
- **WHEN** user runs the compile command
- **THEN** `dimmer.el` is byte-compiled to `dimmer.elc`

#### Scenario: Test execution (placeholder)
- **WHEN** user runs the test command
- **THEN** command succeeds (no tests yet; placeholder for future)

### Requirement: CI runs on multiple Emacs versions
The CI system SHALL execute the build toolchain across multiple Emacs versions.

#### Scenario: CI matrix covers Emacs 25 through snapshot
- **WHEN** CI pipeline runs
- **THEN** jobs execute for Emacs 25.3, 26.3, 27.2, 28.2, 29.4, 30.1, snapshot
