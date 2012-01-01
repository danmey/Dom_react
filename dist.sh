#!/bin/sh

dist_files="Makefile    \
            configure   \
            setup.ml    \
            INSTALL.txt \
            README.txt  \
            AUTHORS.txt"

base_files=`git ls-files | grep -v .gitignore`
name=`sed -n "s/Name:[ \t]*//p" _oasis`
version=`sed -n "s/Version:[ \t]*//p" _oasis`
canonical_name="$name-$version"

rm -fr $canonical_name
rm -f $canonical_name.tar.gz
mkdir $canonical_name

for f in $dist_files $base_files; do
    mkdir -p $canonical_name/`dirname $f`
    cp -R $f $canonical_name/$f
done

tar czf "$canonical_name.tar.gz" $canonical_name
rm -fr $canonical_name
