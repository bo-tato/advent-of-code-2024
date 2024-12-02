multi is-safe(@report where @report.head > @report.tail) { is-safe(@report.reverse) }

multi is-safe(@report) {
    @report.rotor(2 => -1).map(-> ($a, $b) { 1 <= ($b - $a) <= 3 }).all
}

sub remove-at(@list is copy, $index) {
    @list.splice($index, 1);
    @list
}

sub part2-is-safe(@report) {
  @report.keys.map({is-safe(remove-at(@report, $^index))}).any
}

say +lines».words.grep: &part2-is-safe;
