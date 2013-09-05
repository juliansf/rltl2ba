#! /bin/sh

CREATE="ln -f -s"

HEADERS="cudd/cudd.h cudd/cuddInt.h dddmp/dddmp.h mtr/mtr.h st/st.h \
    util/util.h epd/epd.h obj/cuddObj.hh mnemosyne/mnemosyne.h"

LIBS="cudd/libcudd.a mtr/libmtr.a st/libst.a util/libutil.a epd/libepd.a"

printf "Setting up cudd...  "
if [ ! -d include ]; then
    mkdir include
fi
cd include
for file in $HEADERS; do
    if [ ! -h `basename $file` ] && [ -f ../$file ]; then 
	$CREATE ../$file .
    fi
done 
cd ..



