module Generation (
  randomMatrix,
  solvableRandom,
  stateToIO,
  detState
) where

import Matrix
import Gauss

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import System.Random

-- Erzeugt eine zufällige Matrixzeile der Länge l
randomRow :: RandomGen g => Int -> State g (MatrixRow ())
randomRow l = flip MatrixRow () <$> state (randomR (0,2^l-1))

-- Erzeugt eine zufällige Matrix der Größe l x l
randomMatrix :: RandomGen g => Int -> State g (Matrix ())
randomMatrix = replicateM <*> randomRow

-- Erzeugt eine zufällige, lösbare Matrix.
solvableRandom :: RandomGen g => Int -> State g (Matrix ())
solvableRandom i = do m <- randomMatrix i
                      if hasUniqueSolution m
                         then return m
                         else solvableRandom i

-- Macht aus einem solchen State-Transformer eine IO-Funktion
stateToIO :: State StdGen a -> IO a
stateToIO action = evalState action <$> newStdGen

-- Verwendung eines deterministischen Generators
detState :: State StdGen a -> Int -> a
detState action seed = evalState action $ mkStdGen seed
