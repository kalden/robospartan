#!/bin/bash

source progressBar.sh .
# Initilalize parameters
let distribution=1
let sampleSize=1
let iterationNumber=1
let maxSampleSize=200
let n=$maxSampleSize
# Create directories and copy files
function copyFiles () {
	if [ ! -d Results/ConsistencyAnalysis ]
		then mkdir Results/ConsistencyAnalysis 
	fi
	if [ ! -d Results/ConsistencyAnalysis/$sampleSize ]
		then mkdir Results/ConsistencyAnalysis/$sampleSize 
	fi
	if [ ! -d Results/ConsistencyAnalysis/$sampleSize/$distribution ]
		then mkdir Results/ConsistencyAnalysis/$sampleSize/$distribution 
	fi
	if [ ! -d  Results/ConsistencyAnalysis/$sampleSize/$distribution/$iterationNumber ]
		then mkdir  Results/ConsistencyAnalysis/$sampleSize/$distribution/$iterationNumber 
	fi
	if [ -d Results/Samples/samples/$i ]
		then cp -R Results/Samples/samples/$i/*.csv Results/ConsistencyAnalysis/$sampleSize/$distribution/$iterationNumber 
	fi
	
}

# Allocate each sample properly
function recursive () {
	if [ $iterationNumber -le $n ]
		then
		let sampleSize=n
		copyFiles
		let n=$n-2
		if [ $n -ge 1 ] 
			then
				recursive
		fi
	fi
	let n=$maxSampleSize
}

#Main loop
function mainLoop () {
	echo "Consistency analysis file formating!"
	echo "Start!"
	for ((i=1; i <= 4000; i = i + 1))
	do
		recursive
		# Reset parameters if criteria is met
		if [ $iterationNumber -eq $maxSampleSize ] 
			then 
				let iterationNumber=1
				let distribution=$distribution+1
				# echo -n "."
				# Progress bar
				let j=$i/40
				print_progress_bar ${BAR_WIDTH} ${j}
    				echo -en "\r"
			else let iterationNumber=$iterationNumber+1
		fi
	done
	echo 
	echo "Done!"
}

# Verify directory exists
clear
if [ -d Results/Samples ]
	then 
		mainLoop
		Rscript Spartan_SamplesSize.R
	else  echo "Directory 'Samples' does not exist"
fi
exit 
