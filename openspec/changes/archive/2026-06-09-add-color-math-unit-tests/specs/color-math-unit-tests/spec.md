## Requirements

### Requirement: Color math helpers are covered by unit tests
The project SHALL provide unit tests for the primary color math helpers used by dimming logic.

#### Scenario: Interpolation helpers are tested
- **WHEN** the test suite runs
- **THEN** `dimmer-lerp` and the `dimmer-lerp-in-*` helpers are exercised with representative inputs

#### Scenario: RGB computation is tested
- **WHEN** the test suite runs
- **THEN** `dimmer-compute-rgb` is verified for representative interpolation cases

#### Scenario: Face color handling is tested
- **WHEN** the test suite runs
- **THEN** `dimmer-face-color` is verified for normal face colors and non-string attribute values such as `reset`
