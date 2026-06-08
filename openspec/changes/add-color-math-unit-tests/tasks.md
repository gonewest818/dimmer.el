## 1. Test harness setup

- [ ] 1.1 Create `test/dimmer-test.el` as the new ERT test file for the project
- [ ] 1.2 Update the Makefile `test` target so it runs the ERT suite through Eask instead of only depending on compile
- [ ] 1.3 Add the GitHub Actions test step so CI runs the same Eask/ERT coverage path

## 2. Test coverage for interpolation helpers

- [ ] 2.1 Add ERT tests for `dimmer-lerp` covering endpoints and midpoints
- [ ] 2.2 Add ERT tests for `dimmer-lerp-in-*` helpers covering representative easing/interpolation cases

## 3. Test coverage for RGB computation

- [ ] 3.1 Add ERT tests for `dimmer-compute-rgb` with stable color inputs
- [ ] 3.2 Add assertions for output stability across expected color-space paths under `eask test`

## 4. Test coverage for face color handling

- [ ] 4.1 Add ERT tests for `dimmer-face-color` with standard string face colors
- [ ] 4.2 Add regression coverage for `dimmer-face-color` when face attributes resolve to `reset`
- [ ] 4.3 Add test coverage for the normalized `unspecified` path used after `reset` handling
