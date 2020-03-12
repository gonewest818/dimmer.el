# Recipe for converting video to animated gif using ffmpeg
# Reference: https://engineering.giphy.com/how-to-make-gifs-with-ffmpeg/

VIDEOS = dimmer-demo.webm

GIFS = $(VIDEOS:.webm=.gif)

default: $(GIFS)

clean:
	rm -f $(GIFS)

%.gif : %.webm
	ffmpeg -i $< -filter_complex "[0:v] fps=15,scale=w=640:h=-1,split [a][b];[a] palettegen [p];[b][p] paletteuse" $@


## Script to generate screenshot gallery

EMACS ?= emacs

GALLERY_TMPL = "(go :MODE FRAC 'THEME)"
GALLERY_THEMES = zenburn \
                 solarized-light \
                 solarized-dark \
                 spacemacs-light \
                 spacemacs-dark \
                 tomorrow-day \
                 tomorrow-night \
                 tomorrow-blue \
                 tomorrow-bright \
                 tomorrow-eighties
GALLERY_MODES = foreground background both
GALLERY_FRACS = -0.2 -0.1 0.1 0.2 0.4
GALLERY_CASES = $(foreach theme,$(GALLERY_THEMES), \
                  $(foreach mode,$(GALLERY_MODES), \
                    $(subst MODE,$(mode), \
                      $(subst THEME,$(theme),example-THEME-MODE))))
GALLERY = $(foreach theme,$(GALLERY_THEMES), \
            $(foreach mode,$(GALLERY_MODES), \
              $(foreach frac,$(GALLERY_FRACS), \
                $(subst FRAC,$(frac), \
                  $(subst MODE,$(mode), \
                    $(subst THEME,$(theme),$(GALLERY_TMPL)))))))

gallery:
	for i in ${GALLERY} ; do \
	    ${EMACS} -q -l gallery.el --eval "$$i" ;\
	done
	for i in ${GALLERY_CASES} ; do \
	    python comp.py $$i ${GALLERY_FRACS} ;\
	done
