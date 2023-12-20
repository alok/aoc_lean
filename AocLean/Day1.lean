import Lake
import Lean
import AocLean.Basic
namespace NumeralParser
  def zero := Lean.Parsec.pstring "zero" <|> Lean.Parsec.pstring "0"
  def one := Lean.Parsec.pstring "one" <|> Lean.Parsec.pstring "1"
  def two := Lean.Parsec.pstring "two" <|> Lean.Parsec.pstring "2"
  def three := Lean.Parsec.pstring "three" <|> Lean.Parsec.pstring "3"
  def four := Lean.Parsec.pstring "four" <|> Lean.Parsec.pstring "4"
  def five := Lean.Parsec.pstring "five" <|> Lean.Parsec.pstring "5"
  def six := Lean.Parsec.pstring "six" <|> Lean.Parsec.pstring "6"
  def seven := Lean.Parsec.pstring "seven" <|> Lean.Parsec.pstring "7"
  def eight := Lean.Parsec.pstring "eight" <|> Lean.Parsec.pstring "8"
  def nine := Lean.Parsec.pstring "nine" <|> Lean.Parsec.pstring "9"
  /- Reversed strings-/
  def zero' := Lean.Parsec.pstring "orez" <|> Lean.Parsec.pstring "0"
  def one' := Lean.Parsec.pstring "eno" <|> Lean.Parsec.pstring "1"
  def two' := Lean.Parsec.pstring "owt" <|> Lean.Parsec.pstring "2"
  def three' := Lean.Parsec.pstring "eerht" <|> Lean.Parsec.pstring "3"
  def four' := Lean.Parsec.pstring "ruof" <|> Lean.Parsec.pstring "4"
  def five' := Lean.Parsec.pstring "evif" <|> Lean.Parsec.pstring "5"
  def six' := Lean.Parsec.pstring "xis" <|> Lean.Parsec.pstring "6"
  def seven' := Lean.Parsec.pstring "neves" <|> Lean.Parsec.pstring "7"
  def eight' := Lean.Parsec.pstring "thgie" <|> Lean.Parsec.pstring "8"
  def nine' := Lean.Parsec.pstring "enin" <|> Lean.Parsec.pstring "9"
  #eval (NumeralParser.zero)  "0".iter
  #eval (Lean.Parsec.pstring "0" <|> Lean.Parsec.pstring "zero") "0".iter
  #eval Lean.Parsec.pstring "ba" "ba".iter



-- TODO generalize to any sequence

def nine'' := Lean.Parsec.oneOf #[Lean.Parsec.pstring "nine", Lean.Parsec.pstring "enin", Lean.Parsec.pstring "9"]
#eval nine'' "nine".iter
#eval nine "nine".iter
end NumeralParser

def String.reverse (s : String) : String := s.toList.reverse.map Char.toString |> .join
#eval "a132".reverse = "231a"

def Char.toDigit
  | '0' => 0
  | '1' => 1
  | '2' => 2
  | '3' => 3
  | '4' => 4
  | '5' => 5
  | '6' => 6
  | '7' => 7
  | '8' => 8
  | '9' => 9
  | _ => panic! "Invalid digit"

private def String.toDigit
| "0"| "zero"| "orez" => 0
| "1" | "one" | "eno" => 1
| "2" | "two" | "owt" => 2
| "3" | "three" | "eerht" => 3
| "4" | "four" | "ruof" => 4
| "5" | "five" | "evif" => 5
| "6" | "six" | "xis" => 6
| "7" | "seven" | "neves" => 7
| "8" | "eight" | "thgie" => 8
| "9" | "nine" | "enin" => 9
|_ => panic! "bad string"
#eval ("nine".toDigit, "0".toDigit)

def Array.reduce (f : α → β → α) (init : α) (as : Array β) : α :=
  as.foldl f init
def Array.sum (xs : Array Nat) := xs.reduce (· + ·) 0

open NumeralParser in
def numeral := zero <|> one <|> two <|> three <|> four <|> five <|> six <|> seven <|> eight <|> nine

open NumeralParser in
/-reversed parsers-/
def numeral' := zero' <|> one' <|> two' <|> three' <|> four' <|> five' <|> six' <|> seven' <|> eight' <|> nine'

def tryParseAll (s:String) (parser: Lean.Parsec String) :=
  match Lean.Parsec.attempt parser s.iter with
  | .success _ res => res
  -- only advance by 1 in case another match can happen right after
  | .error ..   => tryParseAll s.iter.next.remainingToString parser


partial def String.firstNumeral := tryParseAll (parser:=numeral)


partial def String.lastNumeral (s:String) (parser:=numeral'):= tryParseAll s.reverse.firstNumeral parser


#eval "five".firstNumeral.toDigit == 5
#eval "afive".firstNumeral.toDigit == 5
#eval "fivea".lastNumeral
#eval "fivea".reverse.firstNumeral numeral'
#eval "one".lastNumeral
#eval "0nine onea".lastNumeral
#eval "abc".iter.next.remainingToString
#eval ("abc".iter.next.next.next).remainingToString
#eval "aaafour5fourthreeonezblqnsfk1aaa".firstNumeral
#eval "aaaa5fourthreeonezblqnsfk1twoaa".lastNumeral
#eval "oneone".lastNumeral == "eno"
#eval "1318".lastNumeral == "8"
#eval "1rdtwofjvdllht5eightsixfourbl".lastNumeral

private def String.numFromLine (line : String) : Nat := 10 * line.firstNumeral.toDigit + line.lastNumeral.toDigit
#eval "2318".numFromLine == 28
#eval "1rdtwofjvdllht5eightsixfourbl".numFromLine
def day1 (xs : Array String) : Nat :=
  xs.map String.numFromLine |>.sum
