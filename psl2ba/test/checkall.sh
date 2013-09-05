#!/bin/bash

# Formulas from "Linear Encoding of Bounded LTL Model Checking". 
# Biere, Heljanko, Junttila, Latvala, Schuppan
./check.sh bhjls

# Formulas from Dax/Klaedtke
./check.sh dk

# Formulas from "Optimizing Büchi Automata".
# Etessami, Holzmann
./check.sh eh

# Formulas from "Efficient Büchi Automata from LTL Formulae".
# Somenzi, Bloem
./check.sh sb

# Formulas form patterns of commonly used specification formulas
# http://patterns.projects.cis.ksu.edu/documentation/patterns/ltl.shtml
./check.sh patterns


# Link the results
out="<html><body>"
out="$out<a href=\"bhjls/result.htm\">bhjls/result.htm</a><br>"
out="$out<a href=\"dk/result.htm\">dk/result.htm</a><br>"
out="$out<a href=\"eh/result.htm\">eh/result.htm</a><br>"
out="$out<a href=\"sb/result.htm\">sb/result.htm</a><br>"
out="$out<a href=\"patterns/result.htm\">patterns/result.htm</a><br>"
out="$out</body></html>"

echo $out > result.htm

echo "Results can be found in result.htm"