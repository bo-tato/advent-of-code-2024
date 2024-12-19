my @patterns = lines.first.split: ", ";
my @designs = lines[1..*];

# cute solution that should work but doesn't, see: https://github.com/rakudo/rakudo/issues/5742
say "Part1: ", +@designs.race.grep: /^ @patterns+ $/;
