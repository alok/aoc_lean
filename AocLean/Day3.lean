import Lake
import Lean
import AocLean.Basic


def findMatching (anchorChar: Nat × Nat) :=
  let (r,c) := anchorChar
  -- let symbol := input[r].get! c

/-

The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)

Here is an example engine schematic:

467..114.. --> find numbers: (467: 0:0, 0:1, 0:2), (114: 0:6, 0:7, 0:8)
...*...... --> find chars: (* = 0:2, 0:3, 0:4, 1:2, 1:3, 1:4, 2:3, 2:4, 2:5)
..35..633. --> for each char index ci (for all numbers index ni ( if ci == ni add number linked to ni (+ total)))
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

Of course, the actual engine schematic is much larger. What is the sum of all of the part numbers in the engine schematic?

To begin, get your puzzle input.

Answer:



-/
-- Symbol is any non-digit that's not a period
-- Number is many1 digit

-- 467..114..
-- ...*......
-- ..35..633.
-- ......#...
-- 617*......
-- .....+.58.
-- ..592.....
-- ......755.
-- ...$.*....
-- .664.598..


/- Top left going left to right, top to bottom.

Remember that because indexes are Nats, they never go negative. -/
def neighbors (pos: Nat × Nat) :=  [
  (r-1,c-1),
  (r-1,c),
  (r-1,c+1),
  (r,c-1),
  (r,c+1),
  (r+1,c-1),
  (r+1,c),
  (r+1,c+1),
].filter (· ≠ pos) |> Lean.HashSet.fromIter
  where
    r := pos.1
    c := pos.2
#eval neighbors (0, 0)
#eval  "12*3--4.a5.a" Lean.Parsec.validSymbol
#eval "467..114.."
#eval "...*......"


def main := do
  let input ← IO.FS.lines "/Users/alokbeniwal/aoc_lean/data/day3.txt"
  let input := parseInput input[0]!
  println! "{input}"
#eval main
