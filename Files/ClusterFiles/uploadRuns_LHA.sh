#!/bin/bash

source retrieveData_LHA.sh .

retrieveData

samplesNumber=1000
j=2001

for ((i= 501; i <= $samplesNumber; i = i + 1))
do
	# KA: expName can be set in the cluster script generator
	perl -i -pe 's|(let expName)=.*? #|$1='"$i"' #|g' Samples_LHA.sh # Change seed

	# Swarm size modifies entity parameter in distribute tags
	perl -i -pe 's|(swarmSize)=.*? #|$1='"${dataArray[$j]}"' #|g' Samples_LHA.sh # Change seed
        let j=$j+1

	# Home source distance is in loop functions and impacts arena size calculations
	perl -i -pe 's|(homeSourceDistance)=.*? #|$1='"${dataArray[$j]}"' #|g' Samples_LHA.sh # Change seed
        let j=$j+1

	#perl -i -pe 's|(let alpha)=.*? #|$1='"${dataArray[$j]}"' #|g' Samples_LHA.sh

	# Epsilon in params, and modifies ok
   perl -i -pe 's|(let epsilon)=.*? #|$1='"${dataArray[$j]}"' #|g' Samples_LHA.sh
	#perl -i -pe 's|(let initialP)=.*? #|$1='"${dataArray[$j]}"' #|g' Samples_LHA.sh
	let j=$j+1
	#perl -i -pe 's|(let gain)=.*? #|$1='"${dataArray[$j]}"' #|g' Samples_LHA.sh

	# memFac in params, and modifies ok
	perl -i -pe 's|(let memFac)=.*? #|$1='"${dataArray[$j]}"' #|g' Samples_LHA.sh
	let j=$j+1

	qsub Samples_LHA.sh
done
