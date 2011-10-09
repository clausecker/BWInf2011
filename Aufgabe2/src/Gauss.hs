{-# LANGUAGE BangPatterns #-}
module Gauss where

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
