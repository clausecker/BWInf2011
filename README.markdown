Lösungen der 1. Runde des 30. BWInf
===================================

Dies sind die Lösungen zur ersten Runde des 30. Bundeswettbewerbes Informatik
(2011) von

 * Alexandra Piloth
 * Robert Clausecker

Hinweise zur Compilierung
-------------------------

Zur Compilierung der Lösungsprogramme und Dokumentation sind zumindest folgende
Werkzeuge notwendig. Es kann sein, dass ich Dinge übersehen habe:

 * XeLaTeX mit Linux Libertine, getestet wurde TeXlive 2009
 * GHC Version 7 oder höher
 * Make
 * Pygmentize

Die Aufgabe 2 enthält ein Makefile zur Vereinfachung der Compilierung. Ein
Aufruf `make` im Verzeichnis Aufgabe2 baut das Programm sowie die Dokumentation.
Es sind zudem noch die Ziele `doc`, `build` und `clean` verfügbar.

Für die Aufgabe 1 ist kein Makefile enthalten, ein zweimaliger Aufruf von
`xelatex Lösung.pdf` sollte zum Bauen der Lösung ausreichen.
