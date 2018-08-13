#!/bin/bash 
if [ -f RB_Pams.csv ]
then
	IFS=","
	i=1
	function retrieveData () {
		while read f1
		do
			dataArray[$i]=$f1
			let i=$i+1
		done < RB_Pams.csv
	}
else
	echo -n "File does not exist"
fi
