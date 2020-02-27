# dimmer.el changelog

Contributor note:

Going forward, please make sure pull requests include a changelog
entry in this file. Include a concise description of the feature and
refer to the issue number that was resolved.

## Snapshot (on melpa.org)

- Features
  - Added `dimmer-buffer-exclusion-predicates` as another mechanism to
    specify buffers that can't be dimmed, resolving [#25].
  - Renamed `dimmer-exclusion-regexp-list` to
    `dimmer-buffer-exclusion-regexps` to clarify what this setting
    does.
  - Added `dimmer-configure-org` [#32]
  - Expanded the part of the documentation that describes what the
    "dimming" calculation really does. [#31]
- Bugfixes
  - Fixed a bug where the `*Org Agenda*` buffer got "stuck" in a
    dimmed state for users who install `org-plus-contrib`. [#33]
  - Fixed typo in the detection of ` *LV*` buffers used in `hydra` and
    other packages. [#35]
- Development and CI improvements
  - Added more debugging print statements with controllable verbosity.
  - Upgraded to latest `elisp-lint` release.
  - Added Emacs 27 to the CI test matrix.
  - Update build steps to download ELPA's new gnupg signing key explicitly.

## 0.4.1 (stable.melpa.org)

- Features
  - Added the ability to change the background color, the foreground
    color (default), or both [#20]
- Bugfixes
  - Fixed the dimming math when working in HSL colorspace [#21]

## 0.4.0

- Features
  - Replaced `dimmer-exclusion-regexp` with simpler
    `dimmer-exclusion-regexp-list` [#16]
  - Added `dimmer-prevent-dimming-predicates` to inhibit the dimming /
    un-dimming of buffers when the specified condition is true. Useful for
    packages that display pop-ups temporarily. [#16]
  - Added `dimmer-configure-helm`, `dimmer-configure-which-key`, and
    `dimmer-configure-hydra` to simplify configuration for those packages.
    [#10, #15, #17 & #19]
  - Added `dimmer-watch-frame-focus-events` to control whether dimmer will
    dim all buffers in all frames when Emacs no longer has focus in the
    windowing system.
  - By default, do not dim the Minibuffer [#10]
- Bugfixes
  - Properly handle focus in/out events on Emacs 27 [#24]
- Development and CI environment updates
  - Replace [Cask](https://github.com/cask/cask) dependency with custom
    .emacs/init.el
  - Migrate tests to CircleCI and drop Travis CI [#11]
  - Adopt `elisp-lint` package as an additional code quality test
  - Make `.dir-locals.el` settings apply to `emacs-lisp-mode` only [#13]

## 0.3.0

- Features
  - Improved dimming calculation:
    - "dimming" now mixes the foreground color with the default
      background color, such that setting `dimmer-fraction` to 0.20
      means the resulting color contains 0.20 of the background, and
      0.80 of the foreground color.
    - Dimming calculations are performed in a colorspace that makes
      all faces get approximately the same effect. You shouldn't see
      certain colors "pop" more than others.
    - Colorspace is user-configurable with via `dimmer-use-colorspace`.

## 0.2.2

- Features
  - Dim buffers when switching between Emacs frames.
  - Dim all buffers when focus is lost completely.
  - Rename `dimmer-percent` to `dimmer-fraction` for clarity.
- Bugfixes
  - Cache dimmed colors based on face color, not face name.
  - Gracefuly handle bad colors (colors where `color-defined-p` is nil). [#6]

## 0.2.1

- Features
  - Documentation improvements to clarify expected dimming behavior.
- Bugfixes
  - none

## 0.2.0

- First stable release
