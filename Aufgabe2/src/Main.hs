module Main (main) where

import Matrix
import Gauss
import Generation

import Control.Monad
import Data.Maybe
import System.Environment

main :: IO ()
main = do args <- getArgs
          c    <- getContents
          let matrixVoid = read c :: Matrix ()
              matrixBool = read c :: Matrix Bool
              solutionScheme = completeGJ matrixVoid
              solutions :: (Functor m, MonadPlus m) => m [Bool]
              solutions = gaussJordan $ gaussianElimination matrixBool
              mkOutput :: (Functor m, MonadPlus m) => m [Bool] -> m String
              mkOutput  = fmap (unwords . map (show . fromEnum))
          case args of
            ["solve"] ->
              when (isJust solutionScheme) $ print (fromJust solutionScheme)

            ["generate",n] -> stateToIO (solvableRandom $ read n) >>= print
            ["usable"]     -> print $ hasUniqueSolution $ matrixVoid

            ["instance","all"] -> putStr $ unlines (mkOutput solutions)

            ["instance","any"] ->  putStrLn $ maybe "" id (mkOutput solutions)

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
