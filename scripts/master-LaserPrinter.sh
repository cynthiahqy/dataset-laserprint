#!/bin/bash

# Set locations
## paths
LP=~/Dropbox/RA-LaserPrinter
scripts="$LP/scripts"
logs="$LP/logs"

## file paths
progress="$logs/progressLog.txt"
clean="$logs/cleaningLog.txt"
master="$scripts/master-LaserPrinter.sh"

pleaseyn="Please answer yes or no"

# Begin session
while true; 
do
    read -p "Date stamp session? " yn
    case $yn in
        [Yy]* ) echo >> $progress;
                echo "START $(date)" >> $progress; 
		break;;
        [Nn]* ) break;;
        * ) echo $pleaseyn;;
    esac
done

# Extraction functions

function mvFolder {
	local targetFol=$1
	local sourceFile=$2
	mv -v $2 $1 >> $clean
}


