import Lake
import Lean

def Lean.Parsec.oneOf (xs: Array (Lean.Parsec a)):= xs.foldl (· <|> ·) (Lean.Parsec.fail "empty input")
def String.empty := ""


def charListToString (cs: List Char):= cs.foldl (init := "") .push
#eval charListToString ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p']

def Lean.Parsec.number := Lean.Parsec.many1 Lean.Parsec.digit
#eval Lean.Parsec.number "1234a5a".iter
def Lean.Parsec.arrayCharToString : Lean.Parsec (Array Char) → Lean.Parsec String
| p => p >>= fun cs => Lean.Parsec.pure (charListToString cs.toList)
/-- Insert many elements into a HashSet. -/
def Lean.HashSet.fromIter [BEq α] [Hashable α] [ForIn Id seq α] (elements : seq) : HashSet α := Id.run do
  let mut s: HashSet α := HashSet.empty
  for a in elements do
    s := s.insert a
  return s
#eval (Lean.HashSet.fromIter [0,1,2,3,4,5,6,7,8,9]).contains 5

instance [Repr a] [Hashable a] [BEq a]:  Repr (Lean.HashSet a) where
  reprPrec hashset n := Id.run do
    (<- Std.Format.bracket "{" (<- hashset.toList.repr n) "}")

def Lean.Parsec.digitStr:=Lean.Parsec.oneOf (#["0","1","2","3","4","5","6","7","8","9"].map Lean.Parsec.pstring)
-- TODO how to cast char parser to str parser
/- Brute force search for answer.-/
partial def tryParseOne (s:String) (parser: Lean.Parsec String)  :=
  if s.isEmpty then none
  else match Lean.Parsec.attempt parser s.iter with
    | .success pos res => some (res, pos)
    | .error pos err => tryParseOne s.iter.next.remainingToString parser
#eval tryParseOne "1234a5a" Lean.Parsec.number.arrayCharToString
#eval Lean.HashSet.fromIter [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

/- brute -/
partial def tryParseAll (s:String) (parser: Lean.Parsec String) : List (Option String) :=
  if s.isEmpty then []
  else match Lean.Parsec.attempt parser s.iter with
    | .success pos res => [some (res,pos)] ++ tryParseAll (s.iter.forward res.length).remainingToString parser
    | .error pos err => tryParseAll s.iter.next.remainingToString parser -- only advance 1, eg case 'a123'
#eval tryParseAll "1234a5a" Lean.Parsec.number.arrayCharToString

def Lean.Parsec.validSymbol : Lean.Parsec String :=
  Lean.Parsec.oneOf (#["+", "-", "*", "/", "^"].map Lean.Parsec.pstring)
#eval tryParseAll "124213*3--4.a5.a" Lean.Parsec.validSymbol

def parseSymbolsInLine (s: String) := tryParseAll s Lean.Parsec.validSymbol
#eval parseSymbolsInLine "12*3--4.a5.a"

-- find any char, find pos on 1d matrix
-- 1 4
-- forall first digits
-- for all second
