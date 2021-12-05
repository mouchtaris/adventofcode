-- vim: et ts=4 sw=4
module Elf.Sonar where

import Control.Arrow ((>>>)) -- forward composition
import Data.Functor ((<&>)) -- reverse fmap

-- Anser1 : Increases in input list
depthIncreases :: Depths -> Int
depthIncreases = count . detect2 lt

-- Anser2 : Increases in windows of 3
depthIncreases3 :: Depths -> Int
depthIncreases3 = depthIncreases . (fmap sum) . (slide 3)

-- The official depths readings type description.
type Depths = [Int]

-- Parse text input to official Depths.
parseInput :: String -> Depths
parseInput = lines >>> (<&> read)

-- Make a list of n "sliding windows" of elements of a given list.
slide :: Int -> [a] -> [[a]]
slide n ~a =
  let head = take n a
      rest = drop 1 a
      rec = if length rest < n then [] else slide n rest
   in head : rec

-- A property that holds (or not) between two elements.
type Prop2 a = a -> a -> Bool

lt :: Ord a => Prop2 a
lt a b = a < b

gt :: Ord a => Prop2 a
gt a b = a > b

-- Reduce a list to a list of detections (bool) of a property
-- between two (2) consecutive elements.
detect2 :: Prop2 a -> [a] -> [Bool]
detect2 prop (a : tail@(b : _)) = prop a b : detect2 prop tail
detect2 _ _ = []

-- Count the occurencies of True.
count :: [Bool] -> Int
count (a : t) = (if a then 1 else 0) + count t
count _ = 0
