{-# LANGUAGE BangPatterns #-}
module Matrix (
    Matrix (..),
    MatrixRow (..),
    (+=+),
    insertVar,
    augmentRHS,
    RHS ()
  ) where

import Data.Bits

data MatrixRow rhs = MatrixRow !Integer !rhs

-- Eine Darstellung als Liste ist für unsere Zwecke vollkommen ausreichend.
newtype Matrix rhs = Matrix [MatrixRow rhs]

-- Addiert zwei Reihen einer Matrix
(+=+) :: RHS rhs => MatrixRow rhs -> MatrixRow rhs -> MatrixRow rhs
(MatrixRow al ar) +=+ (MatrixRow bl br) = MatrixRow (al `xor` bl) (ar =+ br)

-- Setzt für Variable n ein (Wichtig beim Gauß-Jordan-Verfahren). Zählung
-- beginnt bei 0 (wie immer)
insertVar :: RHS rhs => Int -> rhs -> MatrixRow rhs -> MatrixRow rhs
insertVar n ins row@(MatrixRow lhs rhs)
  | testBit lhs n = MatrixRow (clearBit lhs n) (ins =+ rhs)
  | otherwise     = row

-- Fügt eine Einheitsmatrix auf der rechten Seite hinzu
augmentRHS :: Matrix () -> Matrix Integer
augmentRHS (Matrix m) = Matrix $ go m 0 where
  go [] !_ = []
  go (MatrixRow lhs _ : xs) n = (MatrixRow lhs $ bit n) : go xs (n + 1)

--------
-- Hilfsfunktionen
--------

instance RHS rhs => Show (Matrix rhs) where
  show (Matrix m) = unlines (map showRow m) where
    size                    = length m
    showRow (MatrixRow l r) = tail $ showIntegerLen size l ++ showRHS size r

-- Lesen nach dem Prinzip GIGO, d.h. "garbage in, garbage out"
instance RHS rhs => Read (Matrix rhs) where
  readsPrec _ str = [(Matrix $ map step2 step1,"")] where
    -- In Wörter zerlegen, in Matrix und Ergebnis aufteilen.
    step1           = map (break (== "|") . words) $ lines str
    step2 (lhs,rhs) = MatrixRow lhs' rhs' where
      rhs' = readRHS rhs
      lhs' = foldr1 (\a b -> a + 2*b) $ map read lhs

-- | Darstellung der rechten Seite der Gleichung; dies ist im Wesentlichen eine
-- Zeile von A. Um Duplizierungen von Quelltext zu vermeiden, verwenden wir
-- diese Typklasse. (RHS: Right Hand Side)
class RHS a where
  -- | Addiert zwei Zeilen. Auch für Einsetzung verwendet. Symbol: Addition auf
  -- der rechten Seite vom Gleichheitszeichen, also =+
  (=+) :: a -> a -> a
  -- | Falls die Determinante 0 ist, ist die rechte Seite hier 0? Wenn nicht
  -- eindeutig definiert, immer False.
  isNull :: a -> Bool
  -- | Rechte Seite anzeigen (für show usw.). Argument ist Anzahl der Zeilen.
  showRHS :: Int -> a -> String
  -- | Rechte Seite einlesen. Exception wenn Fehlschlag. Eingabe ist eine Liste
  -- von Dingen, die ursprünglich mit einem Leerzeichen getrennt waren.
  readRHS :: [String] -> a

-- keine rechte Seite; es wird nur die Koeffizientenmatrix betrachtet
instance RHS () where
  _ =+ _      = ()
  isNull _    = False
  showRHS _ _ = ""
  readRHS _   = ()

-- Ein spezieller Fall wird betrachtet
instance RHS Bool where
  (=+)        = (/=)
  isNull      = not
  showRHS _ x | x         = " | 1"
              | otherwise = " | 0"
  readRHS ["|","1"] = True
  readRHS ["|","0"] = False
  readRHS r         = error $
    "Kann rechte Seite der Matrix nicht parsen: " ++ show (unwords r)

-- Verallgemeinerung der Aufgabenstellung. Das Bit mit der geringsten Wertigkeit
-- ist ganz links; dies ist für den Anwendungszweck besonders praktisch, da mit
-- der Zählung der Lampen von links begonnen wird.
instance RHS Integer where
  (=+)         = xor
  isNull       = (== 0)
  showRHS n x  = " |" ++ showIntegerLen n x
  readRHS ("|":xs) = foldr1 (\a b -> a + 2*b) $ map read xs
  readRHS r        = error $
    "Kann rechte Seite der Matrix nicht parsen: " ++ show (unwords r)

-- Stellt einen Integer bitweise dar. Verwendet soviele Stellen wie angegeben.
-- Am wenigsten signifikante Stelle zuerst.
showIntegerLen :: Int -> Integer -> String
showIntegerLen 0 !_ = ""
showIntegerLen n  x | even x = " 0" ++ showIntegerLen (n-1) (x `div` 2)
                    | odd  x = " 1" ++ showIntegerLen (n-1) (x `div` 2)
