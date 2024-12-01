#!/usr/bin/env raku

my @words = words;
my @left-list = @words[0,2...*];
my @right-list = @words[1,3...*];

say "Part1: " ~ (zip @left-list.sort, @right-list.sort, with => { abs($^x - $^y) }).sum;
say "Part2: " ~ @left-list.map({$^x * @right-list.grep: $^x}).sum;
