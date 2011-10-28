#!/usr/bin/make -f

XELATEX = xelatex
STYLE = borland
PYGMENTIZE = pygmentize
PYGMENTIZE_OPTS = -O linenos

HSFILES = \
  Matrix.hs.tex\
  Gauss.hs.tex\
  Generation.hs.tex\
  Main.hs.tex

.PHONY: clean

all: Lösung.pdf

%.pdf: %.tex definitions.tex $(HSFILES)
	$(XELATEX) -halt-on-error $< >/dev/null
	$(XELATEX) -halt-on-error $< >/dev/null

clean:
	$(RM) -f *.log *.pdf *.aux *.hs.tex

definitions.tex:
	$(PYGMENTIZE) -S $(STYLE) -f tex > $@

%.hs.tex: ../src/%.hs
	$(PYGMENTIZE) $(PYGMENTIZE_OPTS) -o $@ $<
