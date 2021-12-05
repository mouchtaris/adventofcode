-- vim: et ts=2 sw=2
module Main where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Elf.Navigator
import qualified Elf.Radiation
import qualified Elf.Sonar
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

reportRadiation :: Elf.Radiation.Readings -> IO ()
reportRadiation readings =
  do
    put "[Radiation:   # Readings             ]" (take 2 readings)
    put "[Radiation:   Gamma indication       ]" (Elf.Radiation.gamma readings)
    put "[Radiation:   Epsilon indication     ]" (Elf.Radiation.epsilon readings)
    put "[Radiation:   Power consumption      ]" (Elf.Radiation.powerConsumption readings)

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
