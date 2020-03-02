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

[![Demo Video](https://github.com/gonewest818/dimmer.el/raw/master/doc/dimmer-demo.gif)](https://github.com/gonewest818/dimmer.el/raw/master/doc/dimmer-demo.webm)

(click thumbnail for higher quality)

## Usage Example

     (require 'dimmer)
     (dimmer-configure-which-key)
     (dimmer-configure-helm)
     (dimmer-mode t)

## Configuration

By default dimmer excludes the minibuffer and echo areas from
consideration, so that most packages that use the minibuffer for
interaction will behave as users expect.

* `dimmer-configure-company-box` is a convenience function for users
of company-box.  It prevents dimming the buffer you are editing when a
company-box popup is displayed.

* `dimmer-configure-helm` is a convenience function for helm users to
ensure helm buffers are not dimmed.

* `dimmer-configure-hydra` is a convenience function for hydra users
to ensure ` *LV*` buffers are not dimmed.

* `dimmer-configure-org` is a convenience function for org users to
ensure org-mode buffers are not dimmed.

* `dimmer-configure-posframe` is a convenience function for posframe
users to ensure posframe buffers are not dimmed.

* `dimmer-configure-magitis a convenience function for magit users to
ensure transients are not dimmed.

Please submit pull requests with configurations for other packages!

## Customization

* `dimmer-adjustment-mode` controls what aspect of the color scheme is
adjusted when dimming.  Choices are `:foreground` (default),
`:background`, or `:both`. 

* `dimmer-fraction` controls the degree to which buffers are dimmed.
Typical range is 0.0 - 1.0, and default is 0.20.  Increase value if
you like the other buffers to be more dim.  Some users have pointed
out to me that this value can actually be negative(!) with interesting
effects.

* `dimmer-buffer-exclusion-regexps` can be used to specify buffers
that should never be dimmed.  If the buffer name matches any regexp in
this list then `dimmer.el` will not dim that buffer.  Note, this
variable replaces `dimmer-exclusion-regexp` and
`dimmer-exclusion-regexp-list`, which are now obsolete.

* `dimmer-buffer-exclusion-predicates` can be used to specify buffers
that should never be dimmed.  If any predicate function in this list
returns true for the buffer then `dimmer.el` will not dim that buffer.

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

## How colors are adjusted

This package chooses a "dimmed" version of the foreground (and/or the
background color) of each of your defined faces.  It does this by
shifting those face some fraction of the way toward the colors defined
in the `default` face.

In spite of the name I chose for this package, the newly adjusted
color isn't always "dimmer" that the original. It's more accurate to
say we're reducing the contrast of your color theme by bringing the
foreground and background colors closer together.

Here are a few examples of that.

* Let's say you have a theme with "black" text on a "white"
  background. When dimmer acts on a buffer like that, the text becomes
  a "dark gray" (when in `:foreground` mode), or the background
  becomes a "light gray" (when in `:background` mode). The result is
  that the overall contrast between the foreground and background is
  reduced because the dark and light grays are closer in value than
  then original black and white.
  
* Conversely, if your theme is "white" text on "black" background,
  then the dimmed version of that is "light gray" text on a "dark
  gray" background.

* Finally, it's not just the lightness but also the hue that is
  adjusted.  So if your theme is "light yellow" text on a "dark blue"
  background then imagine a color gradient between those two
  colors. The "dimmed" text will be darker, with a bit of blue
  added. The background will be lighter with a little bit of yellow
  added.

Perhaps this all sounds complicated but rest assured that in practice,
the dimmed colors will look natural and (most importantly) will be
consistent with your selected theme.  It'll just seem like the selected
buffer "pops" visually compared to the rest.

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

Copyright (C) 2017-2020 Neil Okamoto

Distributed under the GNU General Public License, Version 3

