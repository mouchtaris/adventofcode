-- vim: et ts=2 sw=2
module Elf.Radiation
  ( parseInput,
    Readings,
    powerConsumption,
    gamma,
    epsilon,
    oxygen,
    co2,
    gammaBin,
    epsilonBin,
    oxygenBin,
    co2Bin,
    lifeSupport,
  )
where

import Elf.Util (readBinary, transpose, (&), (<&>), (>>>))
import qualified Elf.Util as U (lex)
import Prelude hiding (Read)

type Binary = [Int]

type Readings = [Binary]

parseInput :: String -> Readings
parseInput =
  U.lex >>> (<&> (<&> zeroOne))
  where
    zeroOne '0' = 0
    zeroOne '1' = 1

type Read a = Readings -> a

type Indicator = Read Int

powerConsumption :: Indicator
powerConsumption = \r -> (gamma r) * (epsilon r)

lifeSupport :: Indicator
lifeSupport = \r -> (oxygen r) * (co2 r)

gamma :: Indicator
gamma = readBinary . gammaBin

epsilon :: Indicator
epsilon = readBinary . epsilonBin

oxygen :: Indicator
oxygen = readBinary . oxygenBin

co2 :: Indicator
co2 = readBinary . co2Bin

gammaBin :: Read Binary
gammaBin = leader propMax

epsilonBin :: Read Binary
epsilonBin = leader propMin

oxygenBin :: Read Binary
oxygenBin = compete propMax 0

co2Bin :: Read Binary
co2Bin = compete propMin 0

propMax :: Property
propMax (a, b) = if a > b then 0 else 1

propMin :: Property
propMin (a, b) = if a <= b then 0 else 1

-- Select a bit based on measurements.
-- Returns the bit as Int.
type Property = (Int, Int) -> Int

-- Pick the "leader" word from a set of readings.
--
-- The leader is the selected bit from each row of the
-- transposition.
--
-- The prefered bit is selected based on a `property`,
-- which selects '1' or '0' based on the counts
-- of bits in each transposed horizontal word.
leader :: Property -> Read Binary
leader property =
  transpose
    >>> (<&> (property . sumScore . (<&> zeroOne)))
  where
    zeroOne 0 = (1, 0)
    zeroOne 1 = (0, 1)
    sumScore = foldl sumv2 (0, 0)
      where
        sumv2 (a, x) (b, y) = (a + b, x + y)

-- Select only those readings among their set, which have
-- the same bit on the position of each `round` as the bit
-- of the leader word based on `property`, on the same position.
--
-- Repeated until only one reading is left, which is the evaluation result.
compete :: Property -> Int -> Read Binary
compete property round fighters =
  case filter accept fighters of
    [winner] -> winner
    fighters' -> compete property (round + 1) fighters'
  where
    accept :: Binary -> Bool
    accept reading = reading !! round == lead !! round

    lead :: Binary
    lead = leader property fighters
