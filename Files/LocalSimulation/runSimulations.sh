#!/bin/bash


# Parameter
function analyseParameters {
	#for i in 4 6 8 10 15 20 30
	for ((i= $initialParameterNumber; i <= $finalParameterNumber; i = i + $stepParameterNumber))
	do
		# Directory
		if [ -d Results/$expName/$i ]
			then echo -e "\nDirectory parameter $i exists\n" >> Results/Logs/$FILE.txt
			else mkdir Results/$expName/$i
		fi
		analyseIterations
		echo -n "."
	done
}

# Iteration
function analyseIterations {
	for ((j= $initialIterationNumber; j <= $finalIterationNumber; j = j + $stepIterationNumber))
	do
		filename=$i$expName$j

		cp -R experiments/psiswarm_dps.argos experiments/$filename.argos

		modifyFiles

		# Directory
		if [ -d Results/$expName/$i/$j ]
			then echo -e "Directory iteration $j exists \n" >> Results/Logs/$FILE.txt
			else mkdir Results/$expName/$i/$j
		fi

		#------------Log----------#
		echo -e "PARAMETER: \t\t$i \tITERATION: \t$j" >> Results/Logs/$FILE.txt

		#------------Copy simulation----------#
		cp -R experiments/$i$expName$j.argos Results/$expName/$i/$j
		#------------Run simulation----------#
		argos3 -c experiments/$i$expName$j.argos
		#------------Remove executable----------#
		rm -r experiments/$filename.argos
	done
}

function modifyFiles {

	# Simulation parameters
	perl -i -pe 's|(experiment length=)".*?"|$1"'"$length"'"|g' experiments/$filename.argos # Change experiment lenght
	perl -i -pe 's|(random_seed=)".*?"|$1"'"$j"'"|g' experiments/$filename.argos # Change seed
	# Robots' Parameters
	perl -i -pe 's|(partition_type=)".*?"|$1"'"$partType"'"|g' experiments/$filename.argos # Select partition type 
	perl -i -pe 's|(initial_partition_length=)".*?"|$1"'"$travellingDistance"'"|g' experiments/$filename.argos # Select partition type 

	perl -i -pe 's|(gain=)".*?"|$1"'"$gain"'"|g' experiments/$filename.argos # Select partition type 
	perl -i -pe 's|(alpha=)".*?"|$1"'"$i"'"|g' experiments/$filename.argos # Select partition type 

	# File path
	perl -i -pe 's|(output="Results)/.*?/|$1/'"$expName"'/|g' experiments/$filename.argos # Change file path to experiment type
	perl -i -pe 's|(output="Results/'"$expName"')/.*?/|$1/'"$i"'/|g' experiments/$filename.argos #-Change file path to parameter in specific----------# 
	perl -i -pe 's|(output="Results/'"$expName"'/'"$i"')/.*?/|$1/'"$j"'/|g' experiments/$filename.argos # Change file path to specific experiment number


}


# Variables
source setVariables.sh

FILE=$expName

VAR="Output\n"
ANS='r'

if [ -f Results/Logs/$FILE.txt ]
then
        echo -n "File '$FILE' exists; should I replace it, append to it, or cancel operation (r/a/c)? "
        read ANS
fi

case "$ANS" in
        'r') echo -e $VAR  > Results/Logs/$FILE.txt;;
        'a') echo -e $VAR >> Results/Logs/$FILE.txt;;
          *) exit 0;;
esac

# Log
echo -e "\nEXPERIMENT NAME: \t$expName \tPARTITION TYPE: $partType"  >> Results/Logs/$FILE.txt
echo -e "TRAVELLINGDISTANCE: \t$travellingDistance \tERROR: \t\t$accError" >> Results/Logs/$FILE.txt
echo -e "Parameter type \t$pamVar\n" >> Results/Logs/$FILE.txt

# Directory
if [ -d Results/$expName ]
	then echo -e "\nDirectory type $expName exists\n" >> Results/Logs/$FILE.txt
	else mkdir Results/$expName 
fi
clear
echo -n "Start"
analyseParameters
echo "End"

