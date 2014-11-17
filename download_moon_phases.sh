#!/bin/sh

BASEURL='http://eclipse.gsfc.nasa.gov/phase/phases'
EXT='.html'
START_DATES_BCE=`seq -1999 100 -99`
START_DATES_CE=`seq 1 100 3901`

DIR=moon_phases_html

if [ -f $DIR ]
then
   printf "error: not a directory. $DIR"
   exit 1
fi

if [ ! -e $DIR ] 
then
   mkdir $DIR
fi

for d in $START_DATES_BCE
do 
   wget `printf "$BASEURL%05d$EXT\n" $d` $DIR
done

for d in $START_DATES_CE
do 
   wget `printf "$BASEURL%04d$EXT\n" $d` $DIR
done
