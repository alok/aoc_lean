# %%
from collections import defaultdict
from dataclasses import dataclass
from enum import Enum
from functools import partial
from pathlib import Path
from typing import Self


INPUT = Path("/Users/alokbeniwal/aoc_lean/data/day2.txt")


"""
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
"""


class Color(Enum):
    r = 0
    g = 1
    b = 2

    @staticmethod
    def from_str(s: str):
        s = s.strip().lower()
        if s == "red":
            return Color.r
        elif s == "green":
            return Color.g
        elif s == "blue":
            return Color.b
        else:
            raise ValueError


@dataclass
class Cube:
    color: Color
    count: int = 0

    def __le__(self, other: Self) -> bool:
        assert self.color == other.color
        return self.count <= other.count


@dataclass
class Draw:
    red: Cube
    green: Cube
    blue: Cube

    def __le__(self, other: Self) -> bool:
        return (
            self.red <= other.red
            and self.green <= other.green
            and self.blue <= other.blue
        )


def parse_cube(s: str) -> Cube:
    ct, color = s.split(" ")
    ct, color = int(ct), Color.from_str(color)
    return Cube(color, ct)


Game = list[Draw]

LINE = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"


def parse_line(s: str = LINE):
    """this has a running example to make tracking state easier."""

    def sep_by(s: str, sep: str, strip: bool = True):
        return [draw.strip() for draw in s.split(sep)]

    # strip off Game
    # "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green; Game 123: 1 red"
    s = s[len("GAME ") :]
    # "1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green; Game 123: 1 red"
    id_num, s = s.split(":")
    id_num = int(id_num)
    # "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green; Game 123: 1 red"
    draws = sep_by(s, sep=";")
    out = []
    for draw in draws:
        # 3 blue, 4 red
        game = sep_by(draw, ",")
        cubes = [parse_cube(g) for g in game]
        # add missing colors for uniform comparison
        available_colors = {Color.r, Color.g, Color.b}
        for c in cubes:
            available_colors = available_colors - {c.color}
        for c in available_colors:
            cubes.append(Cube(color=c))
            # sort to make constructing cube easier by RGB order
        cubes.sort(key=lambda cube: cube.color.value)
        draw = Draw(*cubes)
        out.append(draw)

    return id_num, out


# "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green; Game 123: 1 red"


# parse_line(LINE)
# parse_line("Game 123: 1 red")


def filter_line(games: list[Draw], model_draw: Draw) -> bool:
    """Returns true if the game is possible with this draw"""
    return all([ game <= model_draw for game in games])


def main():
    model_draw = Draw(Cube(Color.r, 12), Cube(Color.g, 13), Cube(Color.b, 14))
    valid_ids=[]
    for line in INPUT.open():
        id_num, draws = parse_line(line)


        if filter_line(draws, model_draw):
            valid_ids.append(id_num)
        # print(parse_line(line))
    print(sum(valid_ids))
    # filter ids


# , 2, and 5 would have been possible if the bag had been loaded with that configuration. However, game 3 would have been impossible because at one point the Elf showed you 20 red cubes at once; similarly, game 4 would also have been impossible because the Elf showed you 15 blue cubes at once. If you add up the IDs of the games that would have been possible, you get 8.

# Determine which games would have been possible if the bag had been loaded with only . What is the sum of the IDs of those games?


main()
