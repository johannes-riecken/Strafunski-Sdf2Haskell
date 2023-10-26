#!/usr/bin/perl -w
use v5.30;
use Data::Dumper;

$^I = '';

while (<>) {
    if ($_ eq "import Data.Generics.Strafunski.StrategyLib.Models.Deriving.TermRep\n") {
        $_ .= "import Prelude hiding (Left, Right)\n";
    }
    while (/Sdf_(\w++)/g) {
        my $subject = $&;
        my $replacement = join "", map { "\u$_" } split "_", $1;
        my $padding = ' ' x (length($subject) - length($replacement));
        if ('"' eq substr $', 0, 1) {
            $_ = "$`$replacement\"$padding" . substr $', 1;
        } else {
            $_ = "$`$replacement$padding$'";
        }
    }
    print;
}
