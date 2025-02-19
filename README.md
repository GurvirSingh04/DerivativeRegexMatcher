# Brzozowski Regular Expression Pattern Matcher

Discover the Scala Derivative Regular Expression Matcher, a robust tool designed for advanced pattern matching using Brzozowski's derivatives of regular expressions. This powerful matcher empowers you to efficiently search, edit, and analyse text patterns by incorporating the principles of regular expressions.

## Purpose

Regular expressions play a pivotal role in text pattern matching, offering flexibility in applications like data analysis and security breach detection in network traffic. This repository introduces an innovative approach by harnessing derivatives of regular expressions to elevate the matching capabilities.

## How It Operates

The fundamental concept involves taking the derivative of a regular expression concerning a character, resulting in a new expression that matches strings with that character removed.

## Usage

Explore the matcher through examples like:

```scala
size(ders(("a" * 50).toList, EVIL)) == 7
simp(Iterator.iterate(ONE:Rexp)(r => SEQ(r, ONE | ONE)).drop(50).next) == ONE
simp(Iterator.iterate(ONE:Rexp)(r => ALT(r, r)).drop(20).next) == ONE
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))
matcher(EVIL, "a" * 1000000) == false
```

These examples showcase the efficiency and versatility of the Scala Derivative Regular Expression Matcher in diverse scenarios.

Feel free to experiment and seamlessly integrate this tool into your projects to elevate your text pattern matching capabilities.

