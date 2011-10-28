#!/usr/bin/make -f

XELATEX = xelatex

.PHONY: clean

all: Lösung.pdf

%.pdf: %.tex
	$(XELATEX) -halt-on-error $< >/dev/null
	$(XELATEX) -halt-on-error $< >/dev/null

clean:
	$(RM) -f *.log *.pdf *.aux

