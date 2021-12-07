-- vim: et ts=2 sw=2
module Elf.Navigator
  ( parseInput,
    Instructions,
    powerpoint_1,
    powerpoint_2,
  )
where

import Elf.Util
  ( chunksOf,
    splitOneOf,
    (<&>),
    (>>>),
  )

data Command
  = Up {units :: Int}
  | Down {units :: Int}
  | Forward {units :: Int}
  deriving (Show)

parseCommand :: [String] -> Command
parseCommand (name : unitsText : _)
  | name == "up" = Up unit
  | name == "down" = Down unit
  | name == "forward" = Forward unit
  where
    unit = read unitsText

-- The official Navigator State Type
type Instructions = [Command]

parseInput :: String -> Instructions
parseInput =
  lines
    >>> (>>= splitOneOf "\n ")
    >>> (chunksOf 2)
    >>> (<&> parseCommand)

data Vec2 = Vec2 {x, y :: Int}
  deriving (Show)

data Position = Position
  { aim :: Int,
    point :: Vec2
  }
  deriving (Show)

data Strategy = Direct | Aiming

getStrategy :: Int -> Strategy
getStrategy n
  | n == 1 = Direct
  | n == 2 = Aiming

execute :: Strategy -> Position -> Command -> Position
execute Direct (Position aim (Vec2 x y)) (Up d) = Position aim (Vec2 x (y - d))
execute Direct (Position aim (Vec2 x y)) (Down d) = Position aim (Vec2 x (y + d))
execute Direct (Position aim (Vec2 x y)) (Forward d) = Position aim (Vec2 (x + d) y)
execute Aiming (Position aim p) (Up d) = Position (aim - d) p
execute Aiming (Position aim p) (Down d) = Position (aim + d) p
execute Aiming (Position aim (Vec2 x y)) (Forward d) = Position aim (Vec2 (x + d) (y + d * aim))

point0 = Vec2 0 0

position0 = Position 0 point0

navigate :: Int -> Instructions -> Int
navigate n =
  foldl (execute (getStrategy n)) position0
    >>> (\(Position _ (Vec2 a b)) -> a * b)

powerpoint_1 = navigate 1

powerpoint_2 = navigate 2 -- 1781819478
