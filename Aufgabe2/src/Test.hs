module Test (
  module Gauss,
  module Matrix,
  m7, m9, mx
)where

import Gauss
import Matrix

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
