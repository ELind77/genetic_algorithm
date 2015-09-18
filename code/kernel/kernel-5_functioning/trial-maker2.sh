#!/bin/bash

# TRIAL-MAKER.SH
#=========================================================
# Generates trail.txt from data.txt by choosing NUM-CASES 
# lines where each line is randomly chosen from the range 
# START - END.
#
#  INPUTS:   START, int
#		Must be greater than 31
#	     END, int
#		Must be <= 12928
#  	     NUM-CASES, int
#  OUTPUT:   NONE
#  SIDE-FX:  Creates data.txt and clobbers the old file.
#            So be careful!


# NUMS: the randomly selected lines
NUMS=`shuf -i $1-$2 -n $3`


for var in $NUMS
do 
	echo "(`sed -n "$var p" data.txt`)"
done > trial.txt

