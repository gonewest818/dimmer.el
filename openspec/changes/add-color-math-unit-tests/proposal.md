## Why

`dimmer.el` currently lacks focused unit coverage around its color interpolation and face-color handling helpers. Recent regressions around face attributes like `reset` show that these paths are subtle and worth locking down with tests before further changes.

## What Changes

- Add unit tests for `dimmer-lerp` and the `dimmer-lerp-in-*` helpers.
- Add unit tests for `dimmer-compute-rgb` to verify interpolation behavior and edge cases.
- Add unit tests for `dimmer-face-color`, including handling of non-string face attribute values such as `reset`.
- Add tests that exercise color math behavior across the supported adjustment modes where relevant.
- Create a new ERT test file at `test/dimmer-test.el` and wire it into the project’s Eask/Makefile/CI test path.

## Capabilities

### New Capabilities
- `color-math-unit-tests`: test coverage for interpolation, RGB computation, and face color normalization helpers.

### Modified Capabilities

## Impact

- Affects test coverage only.
- Adds the initial test harness for the project’s ERT coverage.
- May require test helpers or fixtures for face/color setup.
- Uses the Eask/ERT test runner path (`eask test`) via the Makefile and GitHub Actions.
- No runtime API changes are intended.
