#!/usr/bin/perl -w
use v5.30;
use Data::Dumper;

# $^I = '';
$/ = '';

while (<>) {
    s/\bConc(\((?:[^()]++|(?1))*+\))/my $x = $&; $x =~ s,\bRange(\((?:[^()]++|(?1))*+\)),CharRange($&),gr/ge;
    s/\bRange(\((?:[^()]++|(?1))*+\))/my $x = $&; $x =~ s,\bShort\b,SHORT,gr/ge;
    s/\b\Qamb([Layout(), Sort("LAYOUT")])\E/Sort("LAYOUT")/g;
    s/\bConc(\((?:[^()]++|(?1))*+\))/my $x = $&; $x =~ s,\bShort(\((?:[^()]++|(?1))*+\)),CharRange(Character($&)),gr/ge;
    s/\bRange(\((?:[^()]++|(?1))*+\))/my $x = $&; $x =~ s,\bSHORT,Short,gr/ge;
    s/\bPresent\(\s*+Conc(\((?:[^()]++|(?1))*+\s*+\))\s*+\)/Present1(Conc$1)/g;
    s/\bProd(\((?:[^()]++|(?1))*+\))/my $x = $&; $x =~ s,\bCharClass(\((?:[^()]++|(?1))*+\)),CharClass1$1,gr/ge;
    s/\bAttrs(\((?:[^()]++|(?1))*+\))/my $x = $&; $x =~ s,\bTerm\(\s*+Default(\((?:[^()]++|(?1))*+\))\s*+\),Cons1$1,gr/ge;
    s/\bAttrs(\((?:[^()]++|(?1))*+\))/my $x = $&; $x =~ s,Unquoted(\((?:[^()]++|(?1))*+\)),Literal(Uqlit$1),gr/ge;
    s/\bAttrs(\((?:[^()]++|(?1))*+\))/my $x = $&; $x =~ s,Quoted(\((?:[^()]++|(?1))*+\)),Literal(Quoted$1),gr/ge;
    s/\bLit(\("\\"(?:[^"\\]++|\\.)*+"\))/Lit(Quoted$1)/g;
    # s/\bamb\(\s*+\[\s*+(\w++(\((?:[^()]++|(?2))*+\)))\s*+,\s*+\w++(\((?:[^()]++|(?3))*+\))\s*+\]\s*+\)/$1/g;
    s/\bAssoc\b/Atr/g;
    s/\bUnquoted\b/Uqlit/g;
    print;
}
