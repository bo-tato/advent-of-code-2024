sub follows-rules (@update, @rules) {
    none do for @rules -> ($x, $y) {
        with @update.first($x, :k) -> $x-pos {
            with @update.first($y, :k) -> $y-pos {
                $x-pos > $y-pos
            }
        }
    }
}

sub middle-page (@update) {
    @update[@update div 2]
}

sub order-update (@update is copy, @rules) {
    my @prev-update;
    repeat {
        @prev-update = @update.clone;
        for @rules -> ($x, $y) {
            with @update.first($x, :k) -> $x-pos {
                with @update.first($y, :k) -> $y-pos {
                    when $x-pos > $y-pos {
                        @update[$x-pos, $y-pos] = @update[$y-pos, $x-pos];
                    }
                }
            }
        }
    } until @prev-update eqv @update;
    @update
}

my ($rules, $updates) = slurp.split("\n\n");

my @rules = $rules.lines».split("|")».Array;
my @updates = $updates.lines».split(",");
say "Part1: " ~ @updates.grep({follows-rules($_, @rules)}).map(&middle-page).sum;
say "Part2: " ~ @updates.grep({!follows-rules($_, @rules)}).map({order-update($_, @rules)}).map(&middle-page).sum;
