-- vim: et ts=2 sw=2
module Elf.Util
  ( (>>>),
    (&),
    (<&>),
    (<=<),
    (>=>),
    (>>^),
    transpose,
    Elf.Util.lex,
    splitOneOf,
    chunksOf,
    readBinary,
    indexOf,
    toIndeces,
    unfoldr,
    Kleisli,
    partition,
    join,
  )
where

import Control.Arrow (Kleisli, (>>>), (>>^))
import Control.Monad (join, (<=<), (>=>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (partition, transpose, unfoldr)
import Data.List.Split (chunksOf, splitOneOf)

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
    next (p, s) b = (p * 2, b + s * 2)

indexOf :: (Eq a) => a -> [a] -> Maybe Int
indexOf x xs =
  let idxs = iterate (+ 1) 0
      items = zip xs idxs
   in lookup x items

toIndeces :: [a] -> [Int]
toIndeces = reverse . snd . foldl f z
  where
    z = (0, [])
    f = \(i, l) _ -> (i + 1, i : l)
