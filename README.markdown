Lösungen der 1. Runde des 30. BWInf
===================================

Dies sind die Lösungen zur ersten Runde des 30. Bundeswettbewerbes Informatik
(2011) von

 * Alexandra Piloth
 * Robert Clausecker
 * Tobias Bucher

Diese Lösungen wurden hier platziert, um anderen zu zeigen, wie man diese
Aufgaben potenziell lösen kann. Wir übernehmen keine Garantie für die
Funktionsfähigkeit der Lösungen. Jeder kann diese Lösungen unter den
Bedingungen der WTFPL, Version 2 verwenden.

Hinweise zur Compilierung
-------------------------

Zur Compilierung der Lösungsprogramme und Dokumentation sind zumindest folgende
Werkzeuge notwendig. Es kann sein, dass ich Dinge übersehen habe:

 * XeLaTeX mit Linux Libertine, getestet wurde TeXlive 2009
 * GHC Version 7 oder höher
 * Ein C-Compiler
 * Make
 * Pygmentize

Die Aufgaben 2 und 5 enthalten Makefiles zur Vereinfachung der Kompilierung. Ein
Aufruf `make` im Verzeichnis Aufgabe2 baut das Programm sowie die Dokumentation.
Es sind zudem noch die Ziele `doc`, `build` und `clean` verfügbar. Im Odner
Aufgabe5 stehen hingegen nur die Ziele `all` und `clean` zur Verfügung. In
beiden Fällen wird am Ende eine Ausführbare Datei mit dem Namen Aufgabe2 bzw.
Aufgab5 aufzufinden sein.

Für die Aufgabe 1 ist kein Makefile enthalten, ein zweimaliger Aufruf von
`xelatex Lösung.pdf` sollte zum Bauen der Lösung ausreichen.
