#!/usr/bin/make -f

GHC = ghc -outputdir ../dist -package random -package mtl
GHCFLAGS = -O3 -Wall -rtsopts

.PHONY: clean ../Aufgabe2

all: ../Aufgabe2

../Aufgabe2:
	$(GHC) $(GHCFLAGS) -o ../Aufgabe2 --make Main.hs

clean:
	$(RM) -rf ../dist ../Aufgabe2
