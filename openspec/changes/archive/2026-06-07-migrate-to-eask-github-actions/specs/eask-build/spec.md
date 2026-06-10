## ADDED Requirements

### Requirement: Eask project configuration file
The project SHALL have an `Eask` file at the repository root declaring package metadata and development dependencies.

#### Scenario: Eask file exists and is valid
- **WHEN** `eask analyze` is run on the `Eask` file
- **THEN** command exits with code 0 and no errors

#### Scenario: Package file declaration is correct
- **WHEN** `eask files` is run
- **THEN** output includes `dimmer.el` as the package file

### Requirement: Development dependencies declared
The `Eask` file SHALL declare `elisp-lint` as a development dependency.

#### Scenario: Development dependencies install correctly
- **WHEN** `eask install-deps --dev` is run
- **THEN** `elisp-lint` is installed in the project sandbox (`.eask/`)

### Requirement: Package metadata in Eask file
The `Eask` file SHALL declare package name, version, and summary consistent with `dimmer.el` header.

#### Scenario: Metadata matches dimmer.el
- **WHEN** `eask info` is run
- **THEN** package name is "dimmer", version matches `dimmer.el` Version header