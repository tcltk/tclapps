#!/bin/sh
# Arguments
# 1 - Package directory
# 2 - Destination directory for documentation (fixed: html)
# 3 - Label (optional, basename #1 else)

src=$1 ; shift
dst=$1 ; shift
lbl=$1

if [ x$src = x -o x$dst = x ] ; then
    echo 1>&2 usage: $0 source destination
    exit 1
fi

if [ x$lbl = x ] ; then
    lbl=`basename $src`
fi

# Steps
# - Generate the destination
# - Generate the file map, the navbars, the toc, index, and the pages
#   in this order
# - Delete the transient files.

rm -rf   $dst
mkdir -p $dst

echo Find and map sources ...
    dtp map -ext html -out $dst -trail 2 `find $src -type f -name '*.man' | sort` > $$.map
    echo _index_ $dst/index.html >> $$.map
    echo _toc_   $dst/toc.html   >> $$.map


echo Fixed nagivation bars ...
    dtp navbar $$.map _toc_   _toc_ 'Table Of Contents' /off   _index_ 'Index' /on   > $$.nb_toc
    dtp navbar $$.map _index_ _toc_ 'Table Of Contents' /on    _index_ 'Index' /off  > $$.nb_idx
    dtp navbar $$.map _index_ _toc_ 'Table Of Contents' /pass  _index_ 'Index' /pass > $$.nb_page
    # In the last command _index_ is a dummy, but has to be a valid symbolic filename.


echo Meta information ...
    dtp meta $$.map > $$.meta


echo Table Of Contents ...
    dtp toc \
	-title "$lbl -- Table of Contents" \
	-desc Modules \
	$$.meta \
	> $$.toc

    dtp gen-toc \
	-varfile header $$.nb_toc \
	html $$.map $$.toc > $dst/toc.html


echo Index ...
    dtp idx \
	-title $lbl \
	-desc "Keyword index" \
	$$.meta \
	> $$.idx
    dtp gen-idx \
	-varfile header $$.nb_idx \
	html $$.map $$.idx > $dst/index.html


echo Pages ...
    dtp gen-doc \
	-varfile header $$.nb_page \
	-subst   header _toc_   $dst/toc.html \
	-subst   header _index_ $dst/index.html \
	html $$.map $$.meta

rm $$.*
exit
