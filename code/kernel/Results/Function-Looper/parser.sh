#!/bin/bash

#  Simple script to parse data frim function looper test data

# PARSER.SH
#===========================================
#   INPUT:  FILE-NAME
#   Parsed text data


# INFILE
INFILE="$1"

grep "Average standardized-fitness" $INFILE | awk '{print $2 " " $6}' | sed 's/://g' | sed 's/\.$//g'


