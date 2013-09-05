#! /bin/sh

CREATE="ln -f -s"

LIBS="cudd/libcudd.a mtr/libmtr.a st/libst.a util/libutil.a epd/libepd.a"

printf "Finishing up cudd... "

if [ ! -d lib ]; then 
    mkdir lib
fi

cd lib
for file in $LIBS; do
    if [ ! -h `basename $file` ] && [ -f ../$file ]; then 
	$CREATE ../$file .
    fi
done 
if [ -f libutil.a ]; then
    mv libutil.a libcudd_util.a
fi
cd ..

printf "DONE\n"



