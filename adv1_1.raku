sub MAIN($file-name) {
    my $file = open $file-name;
    my $sum = 0;
    my $max = 0;

    for $file.lines -> $fline {
        my $line = $fline.chomp;

        if !$line {
            $max = max $max, $sum;
            $sum = 0;
        } else {
            $sum += $line.Int;
        }
    }

    $max = max $max, $sum;
    $max.say;
}
