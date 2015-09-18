#!/bin/bash

for var in $*
do 
	echo "(`sed -n "$var p" data.txt`)))))))))))"
done > trial.txt

