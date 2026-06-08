## Context

`dimmer.el` contains helper functions for interpolating colors and deriving dimmed face attributes. These helpers are pure or nearly pure enough to be covered by unit tests, which makes them a good target for regression protection.

The project already uses Eask for test execution, and the intended runner for this coverage is the existing ERT-based path invoked by `eask test` against `test/dimmer-test.el`.

This change also establishes the first dedicated test file for the repository, updates the Makefile test target so it actually runs the ERT suite, and adds the CI step so GitHub Actions executes the same coverage path.

The recent `reset`-attribute issue showed that face color handling can break in subtle ways when Emacs returns non-string attribute values. Tests should capture both normal interpolation behavior and these edge cases.

## Goals / Non-Goals

**Goals:**
- Verify the numeric behavior of the color interpolation helpers.
- Verify that `dimmer-compute-rgb` produces stable, expected outputs for representative inputs.
- Verify that `dimmer-face-color` handles face attributes that are not simple strings.
- Lock in regressions around `reset`-like values so future changes do not reintroduce the crash.

**Non-Goals:**
- Do not change runtime color algorithms as part of this change.
- Do not add new user-facing configuration.
- Do not redesign the dimming pipeline or buffer remapping logic.

## Decisions

1. **Add focused unit tests near the existing helper functions**
   - Rationale: The affected behavior is localized to helper functions, so direct unit tests provide the clearest regression coverage.
   - Implementation shape: Create `test/dimmer-test.el` with ERT forms that exercise the helper functions directly.
   - Alternatives considered: Broad integration tests via buffer dimming. Rejected because they are harder to isolate and more brittle.

2. **Prefer deterministic assertions over broad visual checks**
   - Rationale: Color math should be validated by explicit expected values or stable invariants, not by manual inspection.
   - Alternatives considered: Snapshot-style visual tests. Rejected because they are expensive and less precise for helper-level behavior.

3. **Run coverage through the existing Eask test command**
   - Rationale: The repository’s test entrypoint is already `eask test`, so the new coverage should fit the existing CI/local workflow instead of adding a separate harness.
   - Alternatives considered: A standalone script or one-off Emacs batch invocation. Rejected because it would duplicate the test path.

4. **Keep Makefile and CI aligned with the Eask test entrypoint**
   - Rationale: Developers and CI should invoke the same test command to avoid drift.
   - Implementation shape: Update the `test` target in the Makefile to call the ERT suite, and add the corresponding GitHub Actions step.
   - Alternatives considered: Running tests only in CI or only locally. Rejected because it creates inconsistent verification.

5. **Test `reset` normalization as a value-level edge case**
   - Rationale: The bug is triggered by a specific face attribute value, so the test should verify that the helper behaves correctly when that value is encountered.
   - Alternatives considered: Version-gated tests tied to a specific Emacs release. Rejected because the behavior is better described by the returned value than by release number.

## Risks / Trade-offs

- More unit tests may require test helpers or temporary face definitions. → Keep helpers minimal and colocated with the test file.
- Exact color outputs can differ if color-space calculations are sensitive to Emacs internals. → Assert on stable expectations and use representative cases only.
- Tests that touch face attributes can depend on runtime environment. → Use narrow, controlled test setup and avoid global state where possible.
