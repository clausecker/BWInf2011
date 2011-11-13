#!/usr/bin/make -f

XELATEX = xelatex
STYLE = borland
PYGMENTIZE = pygmentize
PYGMENTIZE_OPTS = -O linenos

HSFILES = \
  def.h.tex\
  main.c.tex\
  process.h.tex\
  process.c.tex\
  input.h.tex\
  input.c.tex\
  output.h.tex\
  output.h.tex

.PHONY: clean

all: doc.pdf

%.pdf: %.tex definitions.tex $(HSFILES)
	$(XELATEX) -halt-on-error $< >/dev/null
	$(XELATEX) -halt-on-error $< >/dev/null

clean:
	$(RM) -f *.log *.pdf *.aux *.c.tex *.h.tex definitions.tex

definitions.tex:
	$(PYGMENTIZE) -S $(STYLE) -f tex > $@

%.c.tex: %.c
	$(PYGMENTIZE) $(PYGMENTIZE_OPTS) -o $@ $<

%.h.tex: %.h
	$(PYGMENTIZE) $(PYGMENTIZE_OPTS) -o $@ $<

