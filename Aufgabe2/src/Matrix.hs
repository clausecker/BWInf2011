{-# LANGUAGE BangPatterns #-}
module Matrix (
    Matrix,
    MatrixRow (),
    (+=+),
    augmentRHS,
    columnSet,
    fromList,
    getRHS,
    insertVar,
    isFirstCol,
    makeRow,
    rowEmpty,
    RHS (..)
  ) where

import Data.Bits

data MatrixRow rhs = MatrixRow !Integer !rhs

-- Eine Darstellung als Liste ist für unsere Zwecke vollkommen ausreichend.
type Matrix rhs = [MatrixRow rhs]

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
augmentRHS :: Matrix rhs -> Matrix Integer
augmentRHS = go 0 where
  go !_ [] = []
  go  n (MatrixRow lhs _ : xs) = (MatrixRow lhs $ bit n) : go (n + 1) xs

-- Ist Spalte n gesetzt?
columnSet :: Int -> MatrixRow rhs -> Bool
columnSet n (MatrixRow lhs _) = testBit lhs n

-- Ist Spalte n die erste, die gesetzt ist?
isFirstCol :: Int -> MatrixRow rhs -> Bool
isFirstCol n (MatrixRow lhs _) = (-1 `shiftL` n) .&. lhs == lhs

-- Sind alle Einträge einer Zeile 0?
rowEmpty :: MatrixRow rhs -> Bool
rowEmpty (MatrixRow lhs _) = lhs == 0

-- liefert rechte Seite der Matrix
getRHS :: MatrixRow rhs -> rhs
getRHS (MatrixRow _ rhs) = rhs

-- liefert eine Zeile, in der Spalte n gesetzt ist und mit gegebener RHS.
makeRow :: Int -> rhs -> MatrixRow rhs
makeRow n rhs = MatrixRow (bit n) rhs

-- Erstellt eine Matrix aus einer Liste von Integern
fromList :: [Integer] -> Matrix ()
fromList = map (flip MatrixRow ())

--------
-- Hilfsfunktionen
--------

instance RHS rhs => Show (MatrixRow rhs) where
  showList m rst = unlines (map showRow m) ++ rst where
    size                    = length m
    showRow (MatrixRow l r) = tail $ showIntegerLen size l ++ showRHS size r

  show (MatrixRow l r) = lhs ++ showRHS len r where
    go n 0 acc = (init acc,n)
    go n k acc | even k    = go (n+1) (k `div` 2) (acc ++ "0 ")
               | otherwise = go (n+1) (k `div` 2) (acc ++ "1 ")
    (lhs,len) = go 0 l " "


-- Lesen nach dem Prinzip GIGO, d.h. "garbage in, garbage out"
instance RHS rhs => Read (MatrixRow rhs) where
  readList str = [(map read $ lines str,"")]

  readsPrec _ str = [(MatrixRow lhs' rhs',"")] where
    (lhs,rhs) = break (== "|") $ words str
    rhs'      = readRHS rhs
    lhs'      = foldr1 (\a b -> a + 2*b) $ map read lhs

-- | Darstellung der rechten Seite der Gleichung; dies ist im Wesentlichen eine
-- Zeile von A. Um Duplizierungen von Quelltext zu vermeiden, verwenden wir
-- diese Typklasse. (RHS: Right Hand Side)
class RHS a where
  -- | Addiert zwei Zeilen. Auch für Einsetzung verwendet. Symbol: Addition auf
  -- der rechten Seite vom Gleichheitszeichen, also =+
  (=+) :: a -> a -> a
  -- | Rechte Seite anzeigen (für show usw.). Argument ist Anzahl der Zeilen.
  showRHS :: Int -> a -> String
  -- | Rechte Seite einlesen. Exception wenn Fehlschlag. Eingabe ist eine Liste
  -- von Dingen, die ursprünglich mit einem Leerzeichen getrennt waren.
  readRHS :: [String] -> a
  -- | Falls die Determinante 0 ist, ist die rechte Seite hier 0? Wenn nicht
  -- eindeutig definiert, immer False.
  isZero :: a -> Bool
  isZero = const False
  -- | Bestimmt beide RHS-Werte. Dies ist nur für den Fall Bool bedeutsam, in
  -- allen anderen, wo isZero eh immer False liefert, ist dieser Wert belanglos.
  bothRHS :: (a,a)
  bothRHS = undefined

-- keine rechte Seite; es wird nur die Koeffizientenmatrix betrachtet
instance RHS () where
  _ =+ _      = ()
  showRHS _ _ = ""
  readRHS _   = ()

-- Ein spezieller Fall wird betrachtet
instance RHS Bool where
  (=+)        = (/=)
  isZero      = not
  bothRHS     = (False,True)
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
  showRHS n x  = " |" ++ showIntegerLen n x
  readRHS ("|":xs) = foldr1 (\a b -> a + 2*b) $ map read xs
  readRHS r        = error $
    "Kann rechte Seite der Matrix nicht parsen: " ++ show (unwords r)

-- Stellt einen Integer bitweise dar. Verwendet soviele Stellen wie angegeben.
-- Am wenigsten signifikante Stelle zuerst.
showIntegerLen :: Int -> Integer -> String
showIntegerLen 0 !_ = ""
showIntegerLen n  x | even x    = " 0" ++ showIntegerLen (n-1) (x `div` 2)
                    | otherwise = " 1" ++ showIntegerLen (n-1) (x `div` 2)
