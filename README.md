# dimmer.el

Visually highlight the selected buffer.

[![MELPA Stable](https://stable.melpa.org/packages/dimmer-badge.svg)](https://stable.melpa.org/#/dimmer)
[![MELPA](https://melpa.org/packages/dimmer-badge.svg)](https://melpa.org/#/dimmer)
[![CircleCI](https://img.shields.io/circleci/project/github/gonewest818/dimmer.el.svg)](https://circleci.com/gh/gonewest818/dimmer.el)

## Description

This package provides a minor mode that indicates which buffer is
currently active by dimming the faces in the other buffers.  It does
this nondestructively, and computes the dimmed faces dynamically such
that your overall color scheme is shown in a muted form without
requiring you to define what is a "dim" version of every face.

`dimmer.el` can be configured to adjust foreground colors (default),
background colors, or both.

## Demo

![Demo Video](https://github.com/gonewest818/dimmer.el/raw/master/doc/dimmer-demo.gif)

## Usage Example

     (require 'dimmer)
     (dimmer-configure-which-key)
     (dimmer-configure-helm)
     (dimmer-mode t)

## Configuration

By default dimmer excludes the minibuffer and echo areas from
consideration, so that most packages that use the minibuffer for
interaction will behave as users expect.

* `dimmer-configure-helm` is a convenience function for helm users that
further modifies the customizations so helm buffers are not dimmed.

* `dimmer-configure-hydra` is a convenience function for hydra users
that modifies the customizations so "*LV*" buffers are not dimmed.

* `dimmer-configure-which-key` is a convenience function for which-key
users that modifies the customizations so which-key popups are not dimmed.

Please submit pull requests with configurations for other packages!

## Customization

* `dimmer-adjustment-mode` controls what aspect of the color scheme is
adjusted when dimming.  Choices are `:foreground` (default),
`:background`, or `:both`. 

* `dimmer-fraction` controls the degree to which buffers are dimmed.
Range is 0.0 - 1.0, and default is 0.20.  Increase value if you
like the other buffers to be more dim.

* `dimmer-exclusion-regexp-list` can be used to specify buffers that
should never be dimmed.  If the buffer name matches any regexp in this
list then `dimmer.el` will not dim that buffer.  Note, this variable
replaces `dimmer-exclusion-regexp` which is obsolete starting with
release 0.4.0 of this package.)

* `dimmer-prevent-dimming-predicates` can be used to prevent dimmer from
altering the dimmed buffer list.  This can be used to detect cases
where a package pops up a window temporarily, and we don't want the
dimming to change.  If any function in this list returns a non-nil
value, dimming state will not be changed. (Note, this variable replaces
`dimmer-exclusion-predicates` which was introduced in snapshots prior
to release 0.4.0 of this package.)

* `dimmer-watch-frame-focus-events` controls whether dimmer will dim all
buffers when Emacs no longer has focus in the windowing system. This
is enabled by default. Some users may prefer to set this to nil, and
have the dimmed / not dimmed buffers stay as-is even when Emacs
doesn't have focus.

* `dimmer-use-colorspace` allows you to specify what color space the
dimming calculation is performed in. In the majority of cases you
won't need to touch this setting. See Troubleshooting below ("dimmed
colors look wrong") for an example where you might need to set this.

## How Colors Are Adjusted

This package chooses a "dimmed" version of the foreground (and
optionaly the background color) of each of your defined faces.  It
does this by shifting everything by a fraction toward the `default`
face.

If your `default` foreground is "white" then the background colors
of each of your faces will be shifted toward "white" when dimmed.

Similarly, if your `default` background is a dark blue, then the
foreground colors of your faces will be shifted "darker" and "more
blue" when buffers are dimmed.

## Troubleshooting

### Windows aren't dimming when I switch from one to another.

Do you have different buffers in those windows? It's very common to
test this package by installing it, launching emacs, and splitting
`*scratch*` into two windows and then toggling between the two
windows. Users may be surprised to find nothing happens.

Why not? This module makes use of the `face-remap-*` APIs in Emacs,
and these APIs work on *buffers* rather than windows. If you have
multiple windows displaying the same buffer they will dim or undim
together. Which means, until and unless you switch the other window to
a new buffer, you won't see the dimming effect.

If/when Emacs offers the controls I need to do window-specific face
remaps, I'll gladly put this feature in. In the meantime, my
configuration combines this package with `global-hl-line-mode` so I
still have a visual cue which window is active.

### It doesn't dim enough. I can barely see it.

Users of light themes may need to increase `dimmer-fraction` in order
to see the effect. I may someday increase the default.

### Dimmed colors sometimes look "wrong". They're getting shifted to the wrong hue.

Try customizing `dimmer-use-colorspace` and choose RGB
instead. Customize this variable and read the documentation in the
customization screens for more background on this issue.

## License

Copyright (C) 2017-2019 Neil Okamoto

Distributed under the GNU General Public License, Version 3

