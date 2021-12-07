-- vim: et ts=2 sw=2
module Elf.Bingo2 where

import Elf.Util
  ( chunksOf,
    indexOf,
    lex,
    partition,
    splitOneOf,
    toIndeces,
    transpose,
    unfoldr,
    (&),
    (<&>),
    (>=>),
    (>>>),
  )
import Prelude hiding (Read, lex)

data Game = Game
  { pool :: [Int],
    plane :: [Int],
    boardlen :: Int,
    deadpool :: [(Int, [Int])]
  }
  deriving (Show)

type Board = [Int]

type Row = [Int]

winids :: Game -> [Int]
winids = deadpool >=> snd

battlefield :: Game -> Game
battlefield = last . playGame

playGame :: Game -> [Game]
playGame = rounds >>> takeWhile (not . gameEnd)

gameEnd :: Game -> Bool
gameEnd (Game [] _ _ _) = True
gameEnd _ = False

rounds :: Game -> [Game]
rounds = iterate playRound

playRound :: Game -> Game
playRound g@(Game [] _ _ _) = g
playRound (Game (bingo : pool') plane n deadpool) =
  -- Add "pull-ed" number and new winners to deadpool.
  -- Keep non-winners in plane.
  Game pool' plane' n deadpool'
  where
    -- "Area" of a board
    area = n * n
    boards :: [Board]
    boards = chunksOf area plane
    wins :: [Board]
    rest :: [Board]
    (wins, rest) = partition winning boards
    plane' :: [Int]
    plane' = concat rest
    deadpool' :: [(Int, [Int])]
    deadpool' = (bingo, winids) : deadpool
    haveBeenBingo :: Int -> Bool
    haveBeenBingo n = n `elem` (deadpool <&> fst) || n == bingo
    winning :: Board -> Bool
    winning board = any winning rows || any winning columns
      where
        rows :: [Row]
        rows = chunksOf n board
        columns :: [Row]
        columns = transpose rows
        winning :: Row -> Bool
        winning = all haveBeenBingo
    winids :: [Int]
    winids = fmap winid wins
      where
        winid board = bingo * foldl antibingoval 0 board
        antibingoval s n = s + if haveBeenBingo n then 0 else n

parseInput :: String -> Game
parseInput =
  lex
    >>> ( \(pool' : plane') ->
            let pool = read <$> splitOneOf "," pool'
                plane = read <$> plane'
                game = Game pool plane 5 []
             in game
        )
