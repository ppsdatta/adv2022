sub MAIN($file-name) {
    my $file = open $file-name;
    my $sum = 0;
    my @sums;

    for $file.lines -> $fline {
        my $line = $fline.chomp;

        if !$line {
            @sums.push($sum);
            $sum = 0;
        } else {
            $sum += $line.Int;
        }
    }

    @sums.push($sum);

    say @sums.sort.tail(3).sum;
}
