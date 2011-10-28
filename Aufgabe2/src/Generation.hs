module Generation where

import Matrix
import Gauss

import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Bits
import System.Random

-- Erzeugt eine zufällige Matrixzeile der Länge l
randomRowS :: RandomGen g => Int -> State g (MatrixRow ())
randomRowS l = flip MatrixRow () <$> state (randomR (0,2^l-1))

-- Erzeugt eine zufällige Matrix der Größe l x l
randomMatrixS :: RandomGen g => Int -> State g (Matrix ())
randomMatrixS = replicateM <*> randomRowS

-- Das gleiche mit IO
randomMatrixIO :: Int -> IO (Matrix ())
randomMatrixIO l = evalState (randomMatrixS l) <$> newStdGen
