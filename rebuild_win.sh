#!/bin/bash
./update_src.sh

cd src/filter_tags
make -f Makefile_xmingw clean
make -f Makefile_xmingw
/bin/cp -f filter_tags.exe ../../inst/bin
cd ../..

cd src/find_tags
make -f Makefile_xmingw clean
make -f Makefile_xmingw
/bin/cp -f find_tags.exe find_tags_unifile.exe ../../inst/bin
cd ../..
