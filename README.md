# Advent of Code 2023

TODO inline the problem statements 

## Day 1

A linewise problem, so we can focus attention to just 1 line.

I use a parser combinator to find the first matching numeral. Handy that it's just built in to Lean, but I read the code and it's so simple, just like implementing one from scratch.

Their bottom-up nature appeals to my problem solving style.

To get the last matching numeral, I reverse the input string and then reverse the strings the parser is seeking.

Lean has a truly amazing amount of syntactic sugar. Unprecedented really. I thought I had become jaded about syntax but wow.

I really like the local "mutability". It let me write the code in a way that just ran without being perfect the first time. Then I cleaned it up.

I learned from Rust that local mutability is fine, glad to see this embrace.

Locally adding functions to types (like `String.toDigit`) allows the very useful extended dot notation.

Named and default arguments and partial application play together nicely.