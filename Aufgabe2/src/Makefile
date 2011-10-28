#!/usr/bin/make -f

GHC = ghc -outputdir ../dist
GHCFLAGS = -O3 -Wall

.PHONY: clean

all: ../Aufgabe2

../Aufgabe2:
	$(GHC) $(GHCFLAGS) -o ../Aufgabe2 --make Main.hs

clean:
	$(RM) -rf ../dist ../Aufgabe2
