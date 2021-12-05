-- vim: et ts=2 sw=2
module Elf.Radiation
  ( parseInput,
    Readings,
    gammaBin,
    epsilonBin,
    gamma,
    epsilon,
    powerConsumption,
  )
where

import Elf.Util (readBinary, transpose, (&), (<&>), (>>>))
import qualified Elf.Util as U (lex)
import Prelude hiding (Read)

type Readings = [String]

parseInput :: String -> Readings
parseInput = U.lex

type Read a = Readings -> a

type Indicator = Read Int

type Binary = [Int]

powerConsumption :: Indicator
powerConsumption = \r -> (gamma r) * (epsilon r)

gamma :: Indicator
gamma = readBinary . gammaBin

epsilon :: Indicator
epsilon = readBinary . epsilonBin

gammaBin :: Read Binary
gammaBin = leader propMax

epsilonBin :: Read Binary
epsilonBin = leader propMin

propMax :: Property
propMax (a, b) = if a > b then 0 else 1

propMin :: Property
propMin (a, b) = if a < b then 0 else 1

-- Select a bit based on measurements.
-- Returns the bit as Int.
type Property = (Int, Int) -> Int

leader :: Property -> Readings -> Binary
leader property =
  transpose
    >>> (<&> property . sumScore . (<&> zeroOne))
  where
    zeroOne '0' = (1, 0)
    zeroOne '1' = (0, 1)
    sumScore = foldl sumv2 (0, 0)
      where
        sumv2 (a, x) (b, y) = (a + b, x + y)
