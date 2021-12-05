-- vim: et ts=2 sw=2
module Elf.Util
  ( (>>>),
    (&),
    (<&>),
    transpose,
    Elf.Util.lex,
    readBinary,
  )
where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (transpose)
import Data.List.Split (splitOneOf)

-- Split the input into whitespace separated tokens.
--
-- Filters out empty.
lex :: String -> [String]
lex =
  lines
    >>> (>>= splitTokens)
    >>> (>>= filterEmpty)

-- Split the string at whitespace
splitTokens :: String -> [String]
splitTokens = splitOneOf " "

-- Used to filter out empty lexemes
filterEmpty :: [Char] -> [String]
filterEmpty [] = []
filterEmpty th = [th]

readBinary :: [Int] -> Int
readBinary =
  snd . foldl next (1, 0)
  where
    next (p, s) b -> (p * 2, b + s * 2)
