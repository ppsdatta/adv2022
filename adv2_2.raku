my %scores = 'A' => 1, 'B' => 2, 'C' => 3;
my %win-map = 'A' => 'C', 'B' => 'A', 'C' => 'B';
my %lose-map = 'C' => 'A', 'A' => 'B', 'B' => 'C';

sub MAIN($file-name) {
    my $file = open $file-name;
    my $total-sum = 0;

    for $file.lines -> $fline {
        my $line = $fline.chomp;
        my ($m1, $m2) = $line.split(/\s+/);

        my $sum-score = 0;

        if $m2 eq 'Y' {
            $sum-score += 3; # Make draw
            $sum-score += %scores{$m1};
        } elsif $m2 eq 'X' {
            $sum-score += 0; # Lose
            $sum-score += %scores{%win-map{$m1}};
        } else {
            $sum-score += 6; # Win
            $sum-score += %scores{%lose-map{$m1}};
        }

        $total-sum += $sum-score;
    }

    say $total-sum;

    close $file;
}