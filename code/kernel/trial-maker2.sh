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


# START and END are incremented by 2 because the data
# isn't all filled in until the 29th day of trading
# but data.txt is kept unedited so that all of the
# prior days are avaliable to the user.

# START
START=`expr $1 + 2`
# END
END=`expr $2 + 2`

# NUMS: the randomly selected lines
NUMS=`shuf -i $START-$END -n $3 | sort -n`

#echo $NUMS

for var in $NUMS
do 
	echo "(`sed -n "$var p" data.txt`)"
done > trial.txt

