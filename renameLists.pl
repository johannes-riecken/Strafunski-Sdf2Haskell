#!/usr/bin/perl -w
use v5.30;
use Data::Dumper;

$^I = '';
$/ = '';

while (<>) {
    s/^data (\S++) = List\d*+( ++\[[^ \]]*+\]\n\s*+deriving \([^\(\)]++\)\s*+)$/data $1 = $1$2/;
    print;
}
