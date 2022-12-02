my %scores = 'X' => 1, 'Y' => 2, 'Z' => 3;
my %win-map = 'A' => 'Z', 'B' => 'X', 'C' => 'Y', 'X' => 'C', 'Y' => 'A', 'Z' => 'B';

sub MAIN($file-name) {
    my $file = open $file-name;
    my $total-sum = 0;

    for $file.lines -> $fline {
        my $line = $fline.chomp;
        my ($m1, $m2) = $line.split(/\s+/);

        my $sum-score = %scores{$m2};

        if %win-map{$m1} eq $m2 {
            $sum-score += 0; # Lost
        } elsif %win-map{$m2} eq $m1 {
            $sum-score += 6; # Won
        } else {
            $sum-score += 3;
        }

        $total-sum += $sum-score;
    }

    say $total-sum;

    close $file;
}