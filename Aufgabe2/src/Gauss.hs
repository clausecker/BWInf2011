{-# LANGUAGE BangPatterns #-}
module Gauss where

import Control.Applicative
import Control.Monad
import Data.List

import Matrix

{- Einfaches Gaussverfahren. Eventuelle Leerzeilen bleiben am Ende zurück.
Hinweise zur Implementation:

* Der Code arbeitet die Matrix als Liste ab und spuckt dabei nacheinander
  Listenelemente aus.

* Die Funktion go ist eine rekursive Anwendung des Gaußverfahrens auf die noch
  verbleibenden Zeilen. Der Parameter n beschreibt die erste zu beachtende
  Spalte.

* In jedem Schritt wird die Restmatrix mit partition danach aufgeteilt, ob der
  Koeffizient in Spalte n gesetzt ist. Die aufgeteilten Listen sind m1 und m0.

* Wenn ein solcher existiert (pivot), so wird er zurückgegeben und alle anderen
  Zeilen, die diesen gesetzt haben (m1') werden mit ihm addiert, sodass alle
  verbleibenden Zeilen an Position n eine 0 haben. Der Algorithmus wird rekursiv
  mit (n+1) und den verbliebenen Zeilen ausgeführt. Die entstehende Umsortierung
  der Zeilen ist zur Berechnung der Determinante aber egal, sodass es keinen
  Unterschied macht, ob man 

* Wenn nicht, so wird geprüft ob alle Zeilen nur aus Nullen bestehen, in diesem
  Fall ist der fertig, da keine weitere Reduzierung durchgeführt werden muss.
  Ansonsten wird n erhöht und der Algorithmus nochmal durchgeführt.

-}
gaussianElemination :: RHS rhs => Matrix rhs -> Matrix rhs
gaussianElemination (Matrix m) = Matrix $ go 0 m where
  go n m = case m1 of
      [] | all rowEmpty m0 -> m
         | otherwise       -> go (n+1) m
      (pivot:m1')          -> pivot : go (n+1) (map (pivot +=+) m1' ++ m0)
    where (m1,m0) = partition (columnSet n) m

{-
Vorbereitung für das Gauß-Jordan-Verfahren. Dieser Algorithmus verschiebt die
Leerzeilen derartig, dass sich diese immer in den Zeilen befinden, in denen
Spalten übersprungen wurden. Wenn die Matrix nicht lösbar ist, wird Nothing
zurückgegeben. Die Funktion erwartet, dass die Matrix eine Ausgabe von
gaussianEliminiation ist.

Der Tupel, der foldr übergeben wird, enthält folgendes:

  * Eine Liste der bereits abgearbeiteten Listenelemente (rs)
  * Eine Liste der Leerzeilen (e) 
  * Die aktuelle Position innerhalb der Stufenform (pos)

Es gibt folgende vier Unterfälle bei der Abarbeitung der Matrix:

  Die Zeile ist leer und die rechte Seite ist 0:
    Füge die Zeile zur Liste der Leerzeilen hinzu.
  Die rechte Seite ist nicht Null:
    Breche ab. Das Gauss-Jordan-Verfahren ist nicht anwendbar
  Der erste Eintrag der Zeile liegt auf der Diagonale:
    Füge die Zeile zur Ausgabeliste hinzu.
  Der erste Eintrag der Zeile liegt weiter links:
    Nimm einen Eintrag aus e und lege ihn auf die Ausgabe. Wende das Verfahren
    erneut mit der selben Zeile an.
  
-}
prepareGaussJordan :: RHS rhs => Matrix rhs -> Maybe (Matrix rhs)
prepareGaussJordan (Matrix m) = finish <$> foldr ((=<<) . go) init m where
  finish (a,_,_) = Matrix a
  init = Just ([],[],length m-1)
  go r (rs,e@ ~(e':es),pos)
    | rowEmpty r       = (rs,r:e,pos) <$ (guard . isZero . getRHS) r
    | isFirstCol pos r = Just (r:rs,e,pos-1)
    | otherwise        = go r (e':rs,es,pos-1)

-- TEST TEST TEST

m7  :: Matrix Bool
-- m7: Erste Beispielmatrix
m7 = read
  "1 1 0 0 0 0 1 | 0\n\
  \1 1 1 0 0 0 0 | 0\n\
  \0 1 1 1 0 0 0 | 1\n\
  \0 0 1 1 1 0 0 | 0\n\
  \0 0 0 1 1 1 0 | 1\n\
  \0 0 0 0 1 1 1 | 0\n\
  \1 0 0 0 0 1 1 | 1"

-- m9: Zweite Beispielmatrix
m9 :: Matrix ()
m9 = read
  "1 1 0 1 0 0 0 0 0\n\
  \1 1 1 0 1 0 0 0 0\n\
  \0 1 1 0 0 1 0 0 0\n\
  \1 0 0 1 1 0 1 0 0\n\
  \1 0 1 0 1 0 1 0 1\n\
  \0 0 1 0 1 1 0 0 1\n\
  \0 0 0 1 0 0 1 1 0\n\
  \0 0 0 0 1 0 1 1 1\n\
  \0 0 0 0 0 1 0 1 1"

-- mx: Matrix mit zwei Leerzeilen
mx :: Matrix Bool
mx = read
  "1 0 0 0 1 1 0 0 0 | 0\n\
  \1 1 1 0 1 0 0 0 0 | 0\n\
  \0 1 1 0 0 1 0 0 0 | 0\n\
  \1 0 1 0 1 0 1 0 1 | 0\n\
  \1 0 1 0 1 0 1 0 1 | 0\n\
  \0 0 1 0 1 1 0 0 1 | 0\n\
  \0 0 0 1 0 0 1 1 0 | 0\n\
  \0 0 0 0 1 0 1 1 1 | 0\n\
  \0 0 0 0 0 1 0 1 1 | 0"
