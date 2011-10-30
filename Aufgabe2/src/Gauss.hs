{-# LANGUAGE BangPatterns #-}
module Gauss (
  gaussianElimination,
  gaussJordan,
  hasUniqueSolution,
  completeGJ
) where

import Control.Applicative
import Control.Monad

import Data.Foldable (foldrM)
import Data.Function
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
gaussianElimination :: RHS rhs => Matrix rhs -> Matrix rhs
gaussianElimination = go 0 where
  go n m = case m1 of
      [] | all rowEmpty m0 -> m
         | otherwise       -> go (n+1) m
      (pivot:m1')          -> pivot : go (n+1) (map (pivot +=+) m1' ++ m0)
    where (m1,m0) = partition (columnSet n) m

{- Das Gauss-Jordan-Verfahren

Dieser Algorithmus erhält als Parameter eine durch gaussianElimination
vorbehandelte Matrix. Diese wird dann mit Hilfe des Gauß-Jordan-Verfahrens
in eine Einheitsmatrix überführt. Da diese immer gleich und daher redundant ist,
ist die Ausgabe der Vektor A' dargestellt als Liste. Im Falle einer singulären
Eingabematrix kann es keine oder mehrere Lösungen geben. Daher ist das Ergebnis
in einen Container der Typklasse MonadPlus verpackt, der dies erlaubt. Man kann
für den Container wahlweise Maybe (ein oder kein Ergebnis) oder [] (alle
Ergebnisse) wählen.

Implementation:

Wir gehen die Matrix von unten nach oben durch und führen dabei die aktuelle
Spalte in ihr sowie eine Liste der bereits bekannten Lösungen mit. Angenommen
ist, dass die Matrix vorher mit der Funktion gaussianElimination in Stufenform
überführt wurde. In jeder Iteration gibt es die folgenden drei Fälle:

Zeile ist leer:
  Prüfe die rechte Seite der Gleichung. Ist sie Null, dann wird einfach mit der
  nächsten Zeile fortgefahren,ohne die aktuelle Spalte zu verändert. Ist sie
  nicht Null, dann gibt es keine Lösungen.

Aktuelle Spalte ist die erste gesetzte Spalte in der Zeile:
  Setzt nacheinander die Lösungen der vorhergehenden Zeilen ein und errechne
  dadurch die Lösung der aktuellen Spalte. Lege diese auf die Liste der Lösungen
  und gehe eine Spalte nach links.

ansonsten:
  Es muss nun eine Spalte links von der aktuellen geben, die gesetzt ist. Da es
  nun eine nicht eindeutig lösbare Spalte gibt, kann deren Wert zufällig gesetzt
  werden. Dies wird über eine kreative Verwendung von MonadPlus bewerkstelligt.
  Nachdem die Lösung auf die Liste gelegt wurde, wird die selbe Zeile, aber mit
  aktueller Spalte eins weiter links, in die Iterationsfunktion gefüttert.

Am Ende liegen die Lösungen in der richtigen Reihenfolge auf der Lösungsliste.
Diese wird extrahiert und ausgegeben.
-}
gaussJordan :: (Functor m, MonadPlus m, RHS rhs) => Matrix rhs -> m [rhs]
gaussJordan m = fst <$> foldrM go ([],length m-1) m where
  go r (rs,pos) | rowEmpty r       = (rs,pos) <$ (guard . isZero . getRHS) r
                | isFirstCol pos r = return (r':rs,pos-1)
                | otherwise        = forkVar >>= go r where
    (r0,r1) = bothRHS
    forkVar = (mplus `on` return) (r0:rs,pos-1) (r1:rs,pos-1)
    r'      = getRHS . foldr (uncurry insertVar) r $ zip [pos+1..] rs

-- Prüft ob eine Matrix eine Determinante von 1 hat.
hasUniqueSolution :: RHS rhs => Matrix rhs -> Bool
hasUniqueSolution = and . zipWith columnSet [0..] . gaussianElimination

-- Wendet das komplette Gauss-Jordan-Verfahren auf eine Matrix an und gibt das
-- Resultat aus. Wenn keine Lösung verfügbar, dann Nothing.
completeGJ :: Matrix a -> Maybe (Matrix ())
completeGJ = fmap fromList . gaussJordan . gaussianElimination . augmentRHS
