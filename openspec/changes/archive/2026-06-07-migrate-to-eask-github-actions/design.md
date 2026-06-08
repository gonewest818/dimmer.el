## Context

The dimmer.el project currently uses:
- **CircleCI** (`.circleci/config.yml`) with Docker images `silex/emacs:25-ci` through `silex/emacs:master-ci`
- **Custom Makefile** that bootstraps an isolated Emacs environment via `.emacs/init.el` → `.emacs/dependencies.el`
- **Manual GPG key setup** for ELPA package signing
- **elisp-lint** invoked via `make lint` with `--no-package-lint` flag

This toolchain is fragile, requires manual maintenance, and CircleCI is no longer preferred. The project needs a modern, declarative build tool with native GitHub Actions support.

Eask provides:
- Declarative `Eask` file for project metadata and dependencies
- Built-in sandboxing (replaces custom `.emacs/init.el`)
- `eask lint` subcommands for granular linting (checkdoc, package, declare, indent, elisp-lint)
- `eask compile` for byte-compilation
- Native GitHub Actions workflow generation
- Active maintenance and community (190 stars, recent releases)

## Goals / Non-Goals

**Goals:**
- Replace CircleCI + custom Makefile + manual bootstrap with Eask + GitHub Actions
- Keep Emacs 25.1 runtime support (Package-Requires unchanged)
- Provide granular linting (checkdoc, package-lint, check-declare, indent, elisp-lint)
- Test across Emacs 25.3–30.1 + snapshot
- Simplify local development via `eask` commands (optional Makefile wrapper)

**Non-Goals:**
- Modify `dimmer.el` source code (legacy focus-handling code stays)
- Update `Package-Requires` to 27+ (separate PR)
- Add test suite (separate PR — visual testing is hard)
- Automate MELPA release (separate PR — Eask maintainer plugin)
- Remove `.dir-locals.el` (project-specific indentation stays)

## Decisions

### 1. Use Eask over Eldev or Cask
**Choice**: Eask
**Rationale**: 
- Active maintenance (Feb 2026 release, 2,354 commits)
- GitHub Actions native with `purcell/setup-emacs` + webinstall
- Declarative `Eask` file (simpler than Eldev's programmable config)
- Built-in `eask lint elisp-lint` respects `.dir-locals.el`
- Single binary install via webinstall (no Python/Node runtime needed for CI)
- Eldev is also viable (pure Elisp, 256 stars) but Eask's CLI + GA integration is more turnkey

### 2. Keep Emacs 25.1 in CI matrix
**Choice**: Test 25.3, 26.3, 27.2, 28.2, 29.4, 30.1, snapshot
**Rationale**: 
- `Package-Requires: ((emacs "25.1"))` declares 25.1 support
- `frame-focus-state` (Emacs 27+) is guarded by `boundp` check; old `focus-in-hook`/`focus-out-hook` path works on 25/26
- `package-lint` fails on `frame-focus-state` for 25.1 — current Makefile disables it (`--no-package-lint`); Eask's `eask lint package` will also fail on 25.1
- **Mitigation**: Run `eask lint package` only on Emacs 27+ in CI, or accept that package-lint warnings on 25/26 are expected

### 3. Eask file structure
**Choice**: Minimal `Eask` with `package-file` + `development` scope for elisp-lint
**Rationale**: 
- Single-file package (`dimmer.el`) — no multi-file complexity
- Development deps only needed for CI; users don't install them
- `ert` is built-in, no need to declare

### 4. GitHub Actions workflow
**Choice**: Matrix strategy with `purcell/setup-emacs` + Eask webinstall
**Rationale**: 
- Standard Emacs CI pattern in community
- Caches `.eask` package directory for speed
- Runs `eask lint checkdoc`, `eask lint package`, `eask lint declare`, `eask lint indent`, `eask lint elisp-lint`, `eask compile`

### 5. Makefile simplification
**Choice**: Keep minimal Makefile delegating to `eask`
**Rationale**: 
- Local muscle memory (`make lint`, `make compile`)
- Optional — users can use `eask` directly
- Easy to drop later

## Risks / Trade-offs

| Risk | Mitigation |
|------|------------|
| `eask lint package` fails on Emacs 25/26 due to `frame-focus-state` | Run package-lint only on 27+ in CI matrix; or accept warnings on older versions |
| Eask webinstall script changes breaking CI | Pin Eask version in workflow or vendor the install script |
| No test suite — CI only lint/compiles | Add unit tests for color math in follow-up PR |
| MELPA recipe still generated manually | Add Eask maintainer plugin (`eask release`) in follow-up |
| Developers unfamiliar with Eask | Document common commands in README; Makefile provides familiar interface |

## Migration Plan

1. **Add** `Eask` file at repo root
2. **Add** `.github/workflows/ci.yml`
3. **Update** `Makefile` to delegate to `eask`
4. **Remove** `.circleci/config.yml`, `.emacs/`, `.emacs-custom.el*`
5. **Verify** CI passes on all Emacs versions
6. **Update** README with Eask commands (optional)

**Rollback**: Revert commit; CircleCI config preserved in git history.

## Open Questions

1. **Package-lint on Emacs 25/26**: Should CI skip `eask lint package` for 25.3/26.3, or run it and allow failures? Current Makefile uses `--no-package-lint` globally.
2. **Eask version pinning**: Install latest via webinstall, or pin to specific version (e.g., `0.12.9`)?
3. **README update**: Add Eask usage section in this PR or separate?