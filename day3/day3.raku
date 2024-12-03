given slurp.subst: /"don't()" .*? "do()"/, :g {
    say m:g/'mul(' (\d+) ',' (\d+) ')'/.map({
            my ($x, $y) = .list;
            $x * $y
        }).sum
}
