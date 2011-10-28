module Main (main) where

import Matrix
import Gauss
import Generation

import System.Environment

main :: IO ()
main = do args <- getArgs
          case args of
            _ -> putStr helpString

helpString :: String
helpString =
  "Lösung der zweiten Aufgabe des 30. Bundeswettbewerbs Informatik\n\
  \von Alexandra Piloth, Robert Clausecker\n\n\
  \Benutzung:\n\n\
  \  Aufgabe2 <Funktion> <Argumente>\n\n\
  \Funktionen:\n\n\
  \  help       Diese Hilfe ausgeben\n\
  \  instance # Löst eine Schaltung mit gegebener Lampenkombination\n\
  \      \"    all Gibt alle Lösungen aus\n\
  \      \"    any Gibt genau eine Lösung aus\n\
  \  solve      Löst eine Schaltung für beliebige Lampenkombinationen\n\
  \  generate n Erzeugt eine zufällige Schaltung mit n Lampen\n\
  \  usable     Prüft eine Schaltung auf Brauchbarkeit\n\n\
  \Für Details zum Ein- und Ausgabeformat sei auf die ausführliche Dokumen-\n\
  \tation verwiesen. Eingabe erfolgt immer über die Standardeingabe, Ausgabe\n\
  \immer über die Standardausgabe.\n"
