sub find-common($s) {
    ($s.substr(0..$s.chars/2-1), $s.substr($s.chars/2..$s.chars)).map({ $_.comb.Set }).reduce(-> $a, $b { $a (&) $b })
}

#say find-common('vJrwpWtwJgWrhcsFMMfFFhFp');
#say find-common('jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL');

sub encode-letter($c) {
    if 'Z'.ord >= $c.ord >= 'A'.ord {
        ($c.ord - 'A'.ord) + 27
    } else {
        ($c.ord - 'a'.ord) + 1
    }
}

#say encode-letter('p');
#say encode-letter('L');

sub part1($lines) {
    $lines.split(/\n/).map({ encode-letter find-common($_).keys.first  }).sum
}

sub part2($lines) {
    my @lines = $lines.split(/\n/);
    @lines.rotor(3).map({ $_.map({ $_.comb.Set  }).reduce(-> $acm, $x { $acm (&) $x }).keys.first }).flat.map({ encode-letter $_  }).sum
}

my $input = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";

sub MAIN($file) {
   my $content = $file.IO.slurp.chomp;
   say part1($content);
   say part2($content);
}

