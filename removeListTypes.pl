#!/usr/bin/perl -w
# I'm not using this for now, as it still seems necessary to distinguish between
# these different types of lists (as evidenced by their different serialization)
use v5.30;
use Data::Dumper;

$^I = '';
$/ = '';

while (<>) {
    s/^data (\S++) = List\d*+ ++(\[[^ \]]*+\])\n\s*+deriving \([^\(\)]++\)\s*+$/type $1 = $2\n\n/;
    print;
}
