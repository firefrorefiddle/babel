#!/bin/sh

INPUT=$1
OUTPUT=`basename -s .html $INPUT`.txt

egrep '^.+[0-9]*\s+[A-Z][a-z][a-z]\s+[0-9]+\s\s[0-9][0-9]:[0-9][0-9]' $1 > $OUTPUT
