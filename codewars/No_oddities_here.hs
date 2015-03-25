module Codewars.Oddities where

import Data.List

noOdds :: Integral n => [n] -> [n]
noOdds = filter even