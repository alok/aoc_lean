-- This module serves as the root of the `AocLean` library.
-- Import modules here that should be built as part of the library.
import «AocLean».Basic
import AocLean.Day1

def main := do
  IO.println (day1 (←IO.FS.lines "/Users/alokbeniwal/aoc_lean/data/day1.txt"))
#eval main
