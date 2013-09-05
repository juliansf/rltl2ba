#!/bin/bash

# Paths to rtl2ba and to NuSMV
rtl2ba="../bin/rtl2ba"
nusmv="./NuSMV"
test="./test_eq"


if [ ! -e "$rtl2ba" ]
then
  echo "$rtl2ba is needed but does not exists"
  exit 1
fi
if [ ! -e "$nusmv" ]
then
  echo "$nusmv is needed but does not exists"
  exit 1
fi



if [ "$1" = "" ]; then
  echo "usage: $0 <directory> [line]"
  exit 1
fi

cd $1
sed -e 's/--.*//' formulas.csv > nocomments.csv
if [ "$2" = "" ]; then
  "../$test" -htm nocomments.csv "../$rtl2ba" "../$nusmv" > result.htm
  echo -e "\nSee the results in $1/result.htm"
else
  sed -n -e "$2p" nocomments.csv > foo.csv
  "../$test" -htm foo.csv "../$rtl2ba" "../$nusmv" > foo.htm
  echo -e "\nSee the results in $1/foo.htm"
fi
cd ..
