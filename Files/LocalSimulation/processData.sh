#!/bin/bash

#-----Import variables-----#
source setVariables.sh

function analyseParameters () {

	#-----Check all parameters-----#
	for ((i= $initialParameterNumber; i <= $finalParameterNumber; i = i + $stepParameterNumber))
	#for i in 0 5 15 25 50
	do
		#-----Check if parameter directory exists-----#
		if [ -d Results/$expName/$i/ ]
			then
				( analyseIterations ) &
				# analyseIterations
				# echo  "$i"
			else
				echo -e "Directory parameter '$i' does not exist\n "
		fi
	done
	wait
	paste -d, Results/$expName/*.csv > Results/$expName.csv
	rm -f Results/$expName/*.csv
}

function analyseIterations () {
	#-----Check all iterations-----#
	for ((j= $initialIterationNumber; j <= $finalIterationNumber; j = j + $stepIterationNumber))
	do
		#-----Check if iteration exists-----#
		if [ -d Results/$expName/$i/$j/ ] 
			then
				analyseFiles
				# echo  -n  "."
			# else
				# echo -e "Directory parameter '$i' iteration '$j' does not exist\n "
				# echo -n  "x"	
		fi
	done	
	#-----To see PROGRESS values-----#
	#paste -d, Results/$expName/$i/*.csv > Results/$expName/x$i$expName.csv 
	cat Results/$expName/$i/*.csv > Results/$expName/output$i.csv	
	#-----To see FINAL----#
	cut -d, -f14 Results/$expName/output$i.csv > Results/$expName/x$i$expName.csv
	rm -f Results/$expName/output$i.csv
	#-----Delete files----#
	rm -f Results/$expName/$i/*.csv
}

function analyseFiles () {

	#-----Copy file-----#
	cp -R Results/$expName/$i/$j/global.csv Results/$expName/$i/$j/foraging_one_row.csv
	#-----Import variables-----#
	# 4186 - 14, 3887 - 13, 3588 - 12, 3289 - 11, 2990 - 10, 2691 - 9, 2392 - 8, 2093 - 7, 1794 - 6, 1495 - 5, 1196 - 4, 897 - 3, 598 - 2
	for k in {1..59} #119
	do
		#-----Remove ineccessary rows-----#
		sed -i -e '2d' Results/$expName/$i/$j/foraging_one_row.csv
	done
	for k in {1..3}
	do
		#-----Remove ineccessary rows-----#
		sed -i -e '16d' Results/$expName/$i/$j/foraging_one_row.csv
	done
	#-----Copy and remove columns-----#
	cut -d, -f2 Results/$expName/$i/$j/foraging_one_row.csv > Results/$expName/$i/$j/foraging_one_column.csv	#Remove all columnes    #REMOVE COLUMN
	cut -d, -f2,3-15 Results/$expName/$i/$j//foraging_one_row.csv > Results/$expName/$i/$j/columns.csv	
	#-----To see FINAL values-----#
	sed 1d Results/$expName/$i/$j/foraging_one_row.csv > Results/$expName/$i/foraging_no_header_$j.csv	
 									#Remove title   #REMOVE ROWS
	#-----To see PROGRESS----#
	#sed 1d Results/$expName/$i/$j/foraging_one_column.csv > Results/$expName/$i/foraging_one_column$j.csv 

}

#-----Check if type directory exists-----#
clear
echo "Processing data!"
if [ -d Results/$expName/ ]
	then 	
		echo "Start!"
		analyseParameters
		echo "End!"
        	else 
        		echo -e "Directory type '$expName' does not exist\n "
fi
