#!/bin/bash

source retrieveData_RB.sh .
retrieveData

FILE=Samples_RB


if [ -f $FILE.sh ]
then
  samplesNumber=10
  j=1
  for ((i= 0; i <= $samplesNumber; i = i + 1))
  do
    perl -i -pe 's|(let expName)=.*? #|$1='"${dataArray[$j]}"' #|g' $FILE.sh # Change seed
    perl -i -pe 's|(let alpha)=.*? #|$1='"${dataArray[$j]}"' #|g' $FILE.sh # Change seed
    let j=$j+1
    qsub $FILE.sh
  done
else
  echo -n "$FILE does not exist"
fi
