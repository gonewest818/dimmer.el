# dimmer.el changelog

Contributor note:

Going forward, please make sure pull requests include a changelog
entry in this file. Include a concise description of the feature and
refer to the issue number that was resolved.

## Snapshot (on melpa.org)

- Dev and CI environment updates
  - Replace Cask with custom .emacs/init.el
  - Adopt `elisp-lint` package as an additional code quality test
  - Migrate tests to CircleCI and drop Travis CI [#11]
  - Make `.dir-locals.el` settings apply to `emacs-lisp-mode` only [#13]


## 0.3.0 (stable.melpa.org)

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
