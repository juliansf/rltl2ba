#!/bin/bash

# Paths to rtl2ba and to NuSMV
rtl2ba="../bin/rtl2ba"
nusmv="./NuSMV"


if [ ! -e "$rtl2ba" ]
then
  echo "$rtl2ba is needed but does not exists"
	echo "example: ./eq.sh atva/response/ 6"
  exit 1
fi
if [ ! -e "$nusmv" ]
then
  echo "$nusmv is needed but does not exists"
	echo "example: ./eq.sh atva/response/ 6"
  exit 1
fi



if [ "$1" = "" ]; then
  echo "usage: $0 <directory> [line]"
  exit 1
fi
if [ "$2" = "" ]; then
  echo "usage: $0 <directory> [line]"
  exit 1
fi


sed -e 's/--.*//' $1/formulas.csv > nocomments.csv
nocopyright="sed -e 's/^\*\*\*.*//'"
check1="$rtl2ba -prefix 'dk_' -subeq '"`sed -n -e "$2p" nocomments.csv | sed -e "s/,/' '/"`"' | $nusmv | $nocopyright"
check2="$rtl2ba -prefix 'dk_' -supeq '"`sed -n -e "$2p" nocomments.csv | sed -e "s/,/' '/"`"' | $nusmv | $nocopyright"
echo '#!/bin/bash' > tmp.sh
echo $check1 >> tmp.sh
echo "echo" >> tmp.sh
echo "echo" >> tmp.sh
echo "echo" >> tmp.sh
echo $check2 >> tmp.sh
echo "echo" >> tmp.sh
echo "echo" >> tmp.sh
echo "echo" >> tmp.sh
chmod.exe u+x tmp.sh
echo
echo
echo
./tmp.sh
