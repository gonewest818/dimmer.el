# dimmer.el gallery

This gallery shows how `dimmer.el` changes inactive windows across a small set
of representative themes.  Each strip shows increasing `dimmer-fraction` values
from left to right; `orig` is the undimmed reference slice.

The examples use these themes:

* Catppuccin Latte: light pastel theme with a distinct warm background hue.
* Doom One: saturated dark theme with cool accents.
* Doom Molokai: saturated high-contrast theme with vivid warm accents.
* Doom Gruvbox Light: light earth-toned theme with warm backgrounds.
* Zenburn: muted neutral gray theme.

## Foreground

Foreground mode dims text and other foreground colors toward the theme's
default background.  This is the default behavior and usually gives the most
subtle inactive-window effect.

![Catppuccin Latte foreground](example-catppuccin-latte-foreground-comp.png)

![Doom One foreground](example-doom-one-foreground-comp.png)

![Doom Molokai foreground](example-doom-molokai-foreground-comp.png)

![Doom Gruvbox Light foreground](example-doom-gruvbox-light-foreground-comp.png)

![Zenburn foreground](example-zenburn-foreground-comp.png)

## Background

Background mode dims the background colors toward the theme's default
foreground.  This can create a stronger pane-level distinction than foreground
mode, especially in themes with expressive background colors.

![Catppuccin Latte background](example-catppuccin-latte-background-comp.png)

![Doom One background](example-doom-one-background-comp.png)

![Doom Molokai background](example-doom-molokai-background-comp.png)

![Doom Gruvbox Light background](example-doom-gruvbox-light-background-comp.png)

![Zenburn background](example-zenburn-background-comp.png)

## Both

Both mode adjusts foreground and background together.  The configured fraction
is split between the two directions so the combined effect remains controlled.

![Catppuccin Latte both](example-catppuccin-latte-both-comp.png)

![Doom One both](example-doom-one-both-comp.png)

![Doom Molokai both](example-doom-molokai-both-comp.png)

![Doom Gruvbox Light both](example-doom-gruvbox-light-both-comp.png)

![Zenburn both](example-zenburn-both-comp.png)

## Desaturate

Desaturate mode reduces color saturation while preserving each color's
lightness.  This keeps the visual weight of the inactive window but makes it
less chromatic.

![Catppuccin Latte desaturate](example-catppuccin-latte-desaturate-comp.png)

![Doom One desaturate](example-doom-one-desaturate-comp.png)

![Doom Molokai desaturate](example-doom-molokai-desaturate-comp.png)

![Doom Gruvbox Light desaturate](example-doom-gruvbox-light-desaturate-comp.png)

![Zenburn desaturate](example-zenburn-desaturate-comp.png)

## Hueshift

Hueshift mode shifts color-bearing face attributes toward a configured target
hue while preserving each color's saturation and lightness.  The gallery uses
the default target hue from the theme background.

![Catppuccin Latte hueshift](example-catppuccin-latte-hueshift-comp.png)

![Doom One hueshift](example-doom-one-hueshift-comp.png)

![Doom Molokai hueshift](example-doom-molokai-hueshift-comp.png)

![Doom Gruvbox Light hueshift](example-doom-gruvbox-light-hueshift-comp.png)

![Zenburn hueshift](example-zenburn-hueshift-comp.png)
