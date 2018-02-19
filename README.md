# dimmer.el

Visually highlight the selected buffer.

[![MELPA Stable](https://stable.melpa.org/packages/dimmer-badge.svg)](https://stable.melpa.org/#/dimmer)
[![MELPA](https://melpa.org/packages/dimmer-badge.svg)](https://melpa.org/#/dimmer)
[![CircleCI](https://img.shields.io/circleci/project/github/gonewest818/dimmer.el.svg)](https://circleci.com/gh/gonewest818/dimmer.el)

## Description

This module provides a minor mode that indicates which buffer is
currently active by dimming the faces in the other buffers.  It does
this nondestructively, and computes the dimmed faces dynamically such
that your overall color scheme is shown in a muted form without
requiring you to define what is a "dim" version of every face.

The `default` background color is the target for all dimming
calculations. If your default background is "white" then faces will be
made brighter when "dimmed". If your default background is a dark
blue, then faces will be shifted "darker" and "more blue" when buffers
are dimmed.

## Demo

![Demo Video](https://github.com/gonewest818/dimmer.el/raw/master/doc/dimmer-demo.gif)

## Usage

     (require 'dimmer) ; unless installed as a package
     (dimmer-mode)

## Customization

`dimmer-fraction` controls the degree to which buffers are dimmed.
Range is 0.0 - 1.0, and default is 0.20.  Increase value if you like
the other buffers to be more dim.

`dimmer-exclusion-regexp` can be used to specify buffers that should
never be dimmed. If the buffer name matches this regexp then
`dimmer.el` will not dim that buffer.

`dimmer-use-colorspace` allows you to specify what color space the
dimming calculation is performed in. In the majority of cases you
won't need to touch this setting. See Troubleshooting below ("dimmed
colors look wrong") for an example where you might need to set this.

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

Copyright (C) 2017-2018 Neil Okamoto

Distributed under the GNU General Public License, Version 3

