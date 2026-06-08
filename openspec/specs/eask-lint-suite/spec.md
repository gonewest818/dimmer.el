## Requirements

### Requirement: Granular lint commands available
The project SHALL support individual lint commands via `eask lint` subcommands.

#### Scenario: checkdoc lint runs
- **WHEN** `eask lint checkdoc` is executed
- **THEN** checkdoc validation runs on `dimmer.el` and exits with code 0 on success

#### Scenario: package-lint runs
- **WHEN** `eask lint package` is executed
- **THEN** package-lint validates `dimmer.el` metadata and structure

#### Scenario: check-declare runs
- **WHEN** `eask lint declare` is executed
- **THEN** check-declare validates all `defvar`, `defcustom`, `defun` declarations

#### Scenario: indent lint runs
- **WHEN** `eask lint indent` is executed
- **THEN** indentation and whitespace validated per `.dir-locals.el` (80 column, no tabs)

#### Scenario: elisp-lint runs with project config
- **WHEN** `eask lint elisp-lint` is executed
- **THEN** full elisp-lint suite runs respecting `.dir-locals.el` settings

### Requirement: Lint commands replace Makefile lint target
The `eask lint` subcommands SHALL collectively replace the `make lint` target functionality.

#### Scenario: Local developer runs lint via eask
- **WHEN** developer runs `eask lint checkdoc && eask lint package && eask lint declare && eask lint indent && eask lint elisp-lint`
- **THEN** all checks pass (equivalent to previous `make lint`)
