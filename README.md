# dimmer.el

Visually highlight the selected buffer.

[![MELPA](https://melpa.org/packages/dimmer-badge.svg)](https://melpa.org/#/dimmer)
[![Build Status](https://travis-ci.org/gonewest818/dimmer.el.svg?branch=master)](https://travis-ci.org/gonewest818/dimmer.el)

## Description

This module provides a subtle visual indication which window is
currently active by dimming the faces on the others.  It does this
nondestructively, and computes the dimmed faces dynamically such
that your overall color scheme is shown in a muted form without
requiring you to define the "dim" versions of every face.

The percentage of dimming is user configurable.  In addition, for
users of "light" themes there is a dimmer-invert flag that adjusts
the faces brighter (toward white, rather than toward black).

Unlike the 'hiwin' module which has a similar goal, this module
does *not* change the color of the background in any way.  It only
adjusts foregrounds.  In the underlying implementation we do not
use overlays, and therefore we avoid some of the visual problems
the hiwin module exhibits when highlighting interactive shells
and/or repls.

## Caveats

This module makes use of the `face-remap-*` APIs in Emacs and these
APIs work on buffers rather than windows. This means anytime you have
multiple windows displaying the same buffer they will dim or undim
together. In my configuration I combine this package with
`global-hl-line-mode` so that it's also clear which window is active.

## Demo

![Demo Video](https://github.com/gonewest818/dimmer.el/raw/master/doc/dimmer-demo.gif)

## Usage

     (require 'dimmer)
     (dimmer-activate)

## Customization

`dimmer-percent` controls the degree to which unselected buffers are dimmed.
Range is 0.0 - 1.0, and default is 0.20.  Increase value if you like the other
buffers to be more dim.

Set `dimmer-invert` to `t` if you have a dark-on-light theme such that your
faces need to be *lighter* in order to appear dimmer onscreen.

Use `dimmer-exclusion-regexp` to describe patterns for buffer names that
should never be dimmed, for example, you could match buffers created by helm.
