#!/bin/bash 
IFS="," 
i=1
function retrieveData () {
	while read f1 f2 f3 f4
	do 
		dataArray[$i]=$f1
		let i=$i+1 
		dataArray[$i]=$f2
                let i=$i+1
		dataArray[$i]=$f3
                let i=$i+1
		dataArray[$i]=$f4
		let i=$i+1
	#done < LHC_Pams_DPS_2.csv
	done < LHC_Pams_CPS_2.csv
	#done < LHC_Pams_NPS_2.csv
	#done < LHC_Pams_SPS_2.csv
}	 
