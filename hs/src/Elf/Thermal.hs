module Elf.Thermal where

import Elf.Util
import Prelude hiding (lex)

type Vec2 = (Int, Int)

type Line = (Vec2, Vec2)

type Lines = [Line]

type DangerPoints2 = [Vec2]

readInput :: String -> Lines
readInput =
  lines
    >>> chunksOf 3
    >>> (<&> parseLine)

parseLine :: [String] -> Line
parseLine [a, _, b] = (readPoint a, readPoint b)
parseLine _ = error ""

readPoint :: String -> Vec2
readPoint =
  splitOneOf ","
    >>> fmap read
    >>> (\[a, b] -> (a, b))
