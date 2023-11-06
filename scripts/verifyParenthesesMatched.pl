#!/usr/bin/perl -w
use v5.30;
use Data::Dumper;

my %h_paren = (
    '(' => 1,
    ')' => -1,
);
my %h_bracket = (
    '[' => 1,
    ']' => -1,
);

my $paren_count = 0;
my $bracket_count = 0;
my $pos = 0;

while (<>) {
    my @chars = split //, $_;
    for (@chars) {
        if ($_ eq '(' or $_ eq ')') {
            $paren_count += $h_paren{$_};
        } elsif ($_ eq '[' or $_ eq ']') {
            $bracket_count += $h_bracket{$_};
        }
        if ($paren_count < 0) {
            die "invalid paren $_ at $pos";
        }
        if ($bracket_count < 0) {
            die "invalid bracket $_ at $pos";
        }
        $pos++;
    }
}
say "parens: $paren_count";
say "brackets: $bracket_count";
