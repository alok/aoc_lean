import Lake
import Lean
import AocLean.Day1


/-
For example, the record of a few games might look like this:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

Game (id: Number) intercalate ";" (Draws : List Draw)
Draw: 0-3 cubes separated by commas
cube: num color
parse: (line: String) -> (id: Nat, draws: List Draw)

In game 1, 3 sets of cubes are revealed from the bag (and then put back again). The first set is 3 blue cubes and 4 red cubes; the second set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.

Which games would have been possible if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

In the example above, games 1, 2, and 5 would have been possible if the bag had been loaded with that configuration. However, game 3 would have been impossible because at one point the Elf showed you 20 red cubes at once; similarly, game 4 would also have been impossible because the Elf showed you 15 blue cubes at once. If you add up the IDs of the games that would have been possible, you get 8.

Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs of those games?

-------------------------------------

Concepts:

- parsing (there's no zeros)
- Draw is 3-tuple RGB
- Game is list draw
- Valid draw
- Game valid if all draws are

Given games, get id +1 for 1-indexing |> sum

Leans random will be fun. Time to learn from Jax.

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue

format: semicolon delimited games, comma delimited draws, space delimited cubes
game: sepBy ; (sepBy , (sepBy space cube))
-/
-- TODO is sepBy defined?
inductive Color where
| r
| g
| b
deriving BEq, Repr,Inhabited

structure Cube where
  count : Nat := 0
  color : Color
deriving BEq, Repr,Inhabited

structure Draw where
  red: Cube := { color := Color.r}
  green: Cube:= { color := Color.g}
  blue: Cube:= { color := Color.b}
deriving BEq, Repr,Inhabited

instance : LE Draw where
  le a b := a.red.count <= b.red.count && a.green.count <= b.green.count && a.blue.count <= b.blue.count

abbrev Game := Array Draw

def LINE := "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green; Game 123: 1 red"

-- impl notation so Float64[a,b,c] will coerce to Float64
syntax "Draw[" term "]" : termn
#eval Draw[1]
namespace Parser

def Lean.Parsec.oneOf (xs: Array (Lean.Parsec a)):= xs.foldl (· <|> ·) (Lean.Parsec.fail "empty")

structure Matrix (rows: Nat) (cols: Nat) (α: Type) where
  data : Array (Array α)
deriving BEq, Repr

structure NDArray (shape: List Nat) (α: Type) where
  data : Array α
deriving BEq, Repr

--TODO use LE instance
def satisfy(sample: Draw)  (model: Draw) : Bool := sample.red.count <= model.red.count && sample.green.count <= model.green.count && sample.blue.count <= model.blue.count

def number := Lean.Parsec.many1 (Lean.Parsec.satisfy  (·.isDigit))
-- TODO turn Array char into Nat
def game := Lean.Parsec.pstring "Game " *> number <* (Lean.Parsec.pstring ": "  )
def color := Lean.Parsec.oneOf  (#["red","blue","green"].map Lean.Parsec.pstring)
#eval ["red","blue","green"].map (·.iter |> color)
def singleDraw := do
  game *>
  number
  -- color comma
#eval singleDraw LINE.iter
-- def draw
-- TODO how to parse more?
#eval game LINE.iter
end Parser
def tryParse (line: String) : Option Draw := do
  let mut draws := line.splitOn "; "
  -- Game NUM:
  -- gameId = Lean.Parsec.pstring "Game :" <* number *> (spaces? >> colon >> spaces?)
  --
  -- Game 1: NUM color, 4 red; 1 red, 2 green, 6 blue; 2 green
  -- draw := sepBy ',' (NUM color) -- need both num and color
  -- game := sepBy ';' draw
  .none

#eval tryParse "a"
def parseLine (line: String) : Draw :=
  let draws := line.splitOn "; "
  let draws := draws.map (fun (draw: String) => draw.splitOn ", ")
  let draws := draws.map (fun (draw: List String) =>
    let cubes := draw.map (fun (cube: String) =>
      let count := (cube.splitOn " ").get! 0|>.toNat!
      let color := (cube.splitOn " ").get! 1|>.toNat!
      { count := count, color := color }
    )
    let red := cubes.filter (fun (cube: Cube) => cube.color == Color.r)
    let green := cubes.filter (fun (cube: Cube) => cube.color == Color.g)
    let blue := cubes.filter (fun (cube: Cube) => cube.color == Color.b)
    { red := red.head!, green := green.head!, blue := blue.head! }
  )
  { red := draws.head!, green := draws.head!, blue := draws.head! }

def sepBy1 (p : Lean.Parsec String) (sep : Lean.Parsec String) : Lean.Parsec (Array String) :=
  do
    let x ← p
    let xs ← Lean.Parsec.many (sep >> p)
    return (x :: xs)

instance : HAndThen (Lean.Parsec a) (Lean.Parsec b) (Lean.Parsec b) where
  hAndThen p q := do
    let _ ← p
    q ()

def sepBy {α β : Type} (p : Lean.Parsec α) (sep : Lean.Parsec β) : Lean.Parsec (Array α) :=
  sepBy1 p sep <|> Lean.Parsec.pure []
