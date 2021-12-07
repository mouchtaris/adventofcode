-- vim: et ts=2 sw=2
module Elf.Bingo where

import Elf.Util
  ( chunksOf,
    indexOf,
    join,
    lex,
    splitOneOf,
    toIndeces,
    transpose,
    unfoldr,
    (&),
    (<&>),
    (>>>),
  )
import Prelude hiding (lex)

data Game = Game
  { pool :: [Int],
    plane :: [Int],
    bingomask :: [Bool],
    graveyard :: [Int],
    board_length :: Int,
    pool_restriction :: Int
  }
  deriving (Eq)

setPlane :: [Int] -> Game -> Game
setPlane w (Game p _ m v _bl _pr) = Game p w m v _bl _pr

setBingomask :: [Bool] -> Game -> Game
setBingomask m (Game p w _ v _bl _pr) = Game p w m v _bl _pr

parseInput :: String -> Game
parseInput =
  lex
    >>> ( \(pool' : plane') ->
            let pool = read <$> (splitOneOf "," pool')
                plane = read <$> plane'
                bingomask = replicate (length plane) False
                pool_restriction = 10
                board_length = 5
                graveyard = []
                game = Game pool plane bingomask graveyard board_length pool_restriction
             in game
        )

playRound :: Game -> Game
playRound (Game (pull : pool) plane bmask grv _blen _prst) =
  let bmask' = (zip bmask plane) <&> \(bmsk, tckn) -> bmsk || (pull == tckn)
   in Game pool plane bmask' (pull : grv) _blen _prst

playAll :: Game -> [Game]
playAll g = unfoldr f g
  where
    f g =
      if (length (pool g)) == 0
        then Nothing
        else Just (g', g')
      where
        g' = playRound g

endConditions :: Game -> (Bool, Bool)
endConditions g =
  (anyWinner, allWon)
  where
    anyWinner = any winning boards
    allWon = all winning boards
    winning board = any winning rows || any winning collumns
      where
        winning = all bingo
        rows = chunksOf n board
        collumns = transpose rows
        bingo = id
    boards = chunksOf area bmask
    area = n * n
    n = board_length g
    bmask = bingomask g

playToFirstWinner :: Game -> Game
playToFirstWinner g =
  g
    & (playAll >>> dropWhile noWinner >>> head)
  where
    noWinner = not . fst . endConditions

playToAllWinners :: Game -> Game
playToAllWinners g =
  g
    & (playAll >>> dropWhile notAllWinners >>> head)
  where
    notAllWinners = not . snd . endConditions

type Result = ([Int], [Bool])

unplay :: (Result, Game) -> Game
unplay = reset . snd

play :: Game -> (Result, Game)
play game =
  if any winning boards
    then (result, game)
    else (play . playRound) $ game
  where
    Game pool plane bmask grv n _ = game
    result = winner
    winner = (head . filter winning) boards
    boards = zip (chunksOf area plane) (chunksOf area bmask)
    area = n * n
    winning board =
      any winning rows
        || any winning collumns
      where
        rows = chunksOf n (snd board)
        collumns = transpose rows
        winning = all bingo
        bingo = id

reset :: Game -> Game
reset (Game pool plane bmask grave n r) =
  Game
    (reverse grave ++ pool)
    plane
    (fmap (const False) bmask)
    []
    n
    r

replay :: Game -> (Result, Game)
replay = play . reset

regame :: Game -> Game
regame = snd . replay

result :: Game -> Result
result = fst . play

winRound :: Game -> Int
winRound = length . graveyard . regame

numberwang :: Game -> Int
numberwang = head . graveyard . regame

numberwangs :: Game -> [Int]
numberwangs g' =
  select <$> numbers
  where
    ((t, b), _) = play g'
    select (n, w) = if w then n else 0
    numbers = zip t b

nonwangs :: Game -> [Int]
nonwangs g' =
  select <$> numbers
  where
    (t, b) = result g'
    select (n, w) = if w then 0 else n
    numbers = zip t b

winid :: Game -> Int
winid g' =
  let g = regame g'
      a = (sum . nonwangs) g
      b = numberwang g
   in a * b

inverseBoards :: Game -> Game
inverseBoards g = g'
  where
    g' = (setBingomask rbmask . setPlane rboards) $ g
    rboards = join (reverse boards)
    rbmask = join (reverse bmasks)
    boards = chunksOf area (plane g)
    bmasks = chunksOf area (bingomask g)
    area = n * n
    n = board_length g

showGame :: Game -> String
showGame game =
  let (Game pl w m v n _) = game
      area = n * n
      boards = chunksOf area w
      rows = chunksOf n
      poolStr = "  Pool (" ++ (show . length $ pl) ++ "): " ++ show pl
      graveStr = "  Grave (" ++ (show . length $ v) ++ "): " ++ show v
      boardsStr = "  Board Finger: " ++ (boards & (fmap (show . head) >>> unwords))
   in poolStr
        ++ "\n"
        ++ graveStr
        ++ "\n"
        ++ boardsStr
        ++ "\n"

instance Show Game where
  show g = "\nGaming\n" ++ showGame g
