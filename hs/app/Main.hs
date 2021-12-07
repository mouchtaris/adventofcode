-- vim: et ts=2 sw=2
module Main where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Elf.Bingo
import qualified Elf.Bingo
import qualified Elf.Bingo2
import qualified Elf.Navigator
import qualified Elf.Radiation
import qualified Elf.Sonar
import qualified Elf.Thermal
import qualified Elf.Util as U
import qualified Tf.Hello

put :: Show a => String -> a -> IO ()
put name = putStrLn . (\s -> name ++ ": " ++ s) . show

main :: IO ()
main =
  do
    putStrLn "Good day."
    readSonarInputSelector >>= readSonarInput >>= reportSonar
    0 & readNavigationInput >>= reportNavigation
    0 & readRadiationInput >>= reportRadiation
    0 & readBingoInput >>= reportBingo
    0 & readBingo2Input >>= reportBingo2
    0 & readLinesInput >>= reportThermal

reportThermal :: Elf.Thermal.Lines -> IO ()
reportThermal dngpnt2 =
  do
    put "[Thermal:  Wotsy                     ]" "Hi"

reportBingo2 :: Elf.Bingo2.Game -> IO ()
reportBingo2 game =
  do
    put "[Bingo:  Wotsy                       ]" (length winids)
    put "[Bingo:  Wotsy                       ]" (head winids, last winids)
  where
    winids = ((Elf.Bingo2.winids . Elf.Bingo2.battlefield) game)

reportBingo :: Elf.Bingo.Game -> IO ()
reportBingo game =
  do
    put "[Bingo:       Backport testing       ]" ((unplay . play) game == game)
    put "[Bingo:       Telefrag quality       ]" (play game == replay game)
    put "[Bingo:       ---   ----   ---       ]" "--"
    put "[Bingo:  Fresh game                  ]" game
    put "[Bingo:  Play forwards game          ]" (regame game)
  where
    invgame = inverseBoards game

reportRadiation :: Elf.Radiation.Readings -> IO ()
reportRadiation readings =
  do
    put "[Radiation:   # Readings             ]" (length readings)
    put "[Radiation:   Gamma binary           ]" (Elf.Radiation.gammaBin readings)
    put "[Radiation:   Epsilon binary         ]" (Elf.Radiation.epsilonBin readings)
    put "[Radiation:   Oxygen binary          ]" (Elf.Radiation.oxygenBin readings)
    put "[Radiation:   CO2 binary             ]" (Elf.Radiation.co2Bin readings)
    put "[Radiation:   Gamma indication       ]" (Elf.Radiation.gamma readings)
    put "[Radiation:   Epsilon indication     ]" (Elf.Radiation.epsilon readings)
    put "[Radiation:   Power consumption      ]" (Elf.Radiation.powerConsumption readings)
    put "[Radiation:   Oxygen indication      ]" (Elf.Radiation.oxygen readings)
    put "[Radiation:   CO2 indication         ]" (Elf.Radiation.co2 readings)
    put "[Radiation:   Life support indicatio ]" (Elf.Radiation.lifeSupport readings)

reportNavigation :: Elf.Navigator.Instructions -> IO ()
reportNavigation instrs =
  do
    put "[Navigation:  Direct Powerpoint      ]" (Elf.Navigator.powerpoint_1 instrs)
    put "[Navigation:  Aiming Powerpoint      ]" (Elf.Navigator.powerpoint_2 instrs)

reportSonar :: Elf.Sonar.Depths -> IO ()
reportSonar sonarInput =
  do
    put "[Sonar:       Navigation Readings    ]" (length sonarInput)
    put "[Sonar:       Depth Increases        ]" (Elf.Sonar.depthIncreases sonarInput)
    put "[Sonar:       Depth-3 Increases      ]" (Elf.Sonar.depthIncreases3 sonarInput)

type PathId = Int

linesInputPath _ = "../elf_04_lines_input.txt"
readLinesInput :: PathId -> IO Elf.Thermal.Lines
readLinesInput =
  linesInputPath
    >>> readFile
    >>> (<&> Elf.Thermal.readInput)

bingoInputPath _ = "../elf_03_bingo_input.txt"

readBingo2Input :: PathId -> IO Elf.Bingo2.Game
readBingo2Input =
  bingoInputPath
    >>> readFile
    >>> (<&> Elf.Bingo2.parseInput)

readBingoInput :: PathId -> IO Elf.Bingo.Game
readBingoInput =
  bingoInputPath
    >>> readFile
    >>> (<&> Elf.Bingo.parseInput)

radiationInputPath _ = "../elf_02_radiation_input.txt"

readRadiationInput :: PathId -> IO Elf.Radiation.Readings
readRadiationInput =
  radiationInputPath
    >>> readFile
    >>> (<&> Elf.Radiation.parseInput)

navigationInputPath _ = "../elf_01_navigation_input.txt"

readNavigationInput :: PathId -> IO Elf.Navigator.Instructions
readNavigationInput =
  navigationInputPath
    >>> readFile
    >>> (<&> Elf.Navigator.parseInput)

readSonarInput :: PathId -> IO Elf.Sonar.Depths
readSonarInput =
  sonarInputPath
    >>> readFile
    >>> (<&> Elf.Sonar.parseInput)

readSonarInputSelector :: IO Int
readSonarInputSelector = readFile sonarInputPathSelectorPath <&> read

sonarInputPathSelectorPath :: String
sonarInputPathSelectorPath = "../elf_00_sonar_input_selector.txt"

sonarInputPath :: PathId -> String
sonarInputPath 0 = "../elf_00_sonar_input.txt"
sonarInputPath 1 = "../elf_00_sonar_input_small.txt"
sonarInputPath _ = "/dev/null"
