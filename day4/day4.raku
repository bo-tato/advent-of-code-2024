my @input = lines».comb».Array;

sub diagonals (@matrix) {
    my $size = @matrix.elems;
    my @result;
    for ^$size -> $diag {
        @result.push: do for $diag ..^ $size Z @matrix -> ($col, @row) {
            @row[$col]
        }
        @result.push: do for @matrix[$diag..*] Z 0..* -> (@row, $col) {
            @row[$col]
        } when $diag > 0
    }
    @result
}

sub count-xmas (@matrix) {
    my $sum = 0;
    for @matrix -> @line {
        $sum += @line.join.indices("XMAS");
        $sum += @line.join.indices("SAMX");
    }
    $sum
}

say (@input, ([Z] @input), diagonals(@input), diagonals(@input».reverse)).map(&count-xmas).sum;

sub is-xmas($c1, $c2) {
    $c1 eq 'M' && $c2 eq 'S' ||
    $c1 eq 'S' && $c2 eq 'M'
}

my $count = 0;
for 1..138 X 1..138 -> ($row, $col) {
    $count++ when @input[$row;$col] eq 'A' &&
                  is-xmas(@input[$row-1;$col-1], @input[$row+1;$col+1]) &&
                  is-xmas(@input[$row-1;$col+1], @input[$row+1;$col-1])
}
say $count;
