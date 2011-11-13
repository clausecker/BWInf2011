#!/usr/bin/make -f

XELATEX = xelatex
STYLE = borland
PYGMENTIZE = pygmentize
PYGMENTIZE_OPTS = -O linenos
CFLAGS += -O2

CFILES = \
  def.h.tex\
  main.c.tex\
  process.h.tex\
  process.c.tex\
  input.h.tex\
  input.c.tex

OBJECTS = input.o\
          process.o\
          main.o

.PHONY: clean

all: doc.pdf Aufgabe5

Aufgabe5: $(OBJECTS)
	$(CC) $(CFLAGS) -o Aufgabe5 $(OBJECTS)

%.pdf: %.tex definitions.tex $(CFILES)
	$(XELATEX) -halt-on-error $< >/dev/null
	$(XELATEX) -halt-on-error $< >/dev/null


clean:
	$(RM) -f *.log *.pdf *.aux *.c.tex *.h.tex *.o Aufgabe5 definitions.tex

definitions.tex:
	$(PYGMENTIZE) -S $(STYLE) -f tex > $@

%.c.tex: %.c
	$(PYGMENTIZE) $(PYGMENTIZE_OPTS) -o $@ $<

%.h.tex: %.h
	$(PYGMENTIZE) $(PYGMENTIZE_OPTS) -o $@ $<

