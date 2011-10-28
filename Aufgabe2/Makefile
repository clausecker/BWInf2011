#!/usr/bin/make -f

GHC = ghc
XELATEX = xelatex

.PHONY: clean doc-clean doc build-clean Aufgabe2

all: doc build

clean: doc-clean build-clean

doc-clean:
	$(MAKE) -C doc clean

doc:
	$(MAKE) -C doc all

build: Aufgabe2

Aufgabe2:
	$(MAKE) -C src all

build-clean:
	$(MAKE) -C src clean
