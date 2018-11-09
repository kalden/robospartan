Steps

Create sample in app 1:
  -input parameters, measures, settings
  -click create sample
  -Download settings and data (ensure file names end in .csv)
  -Modify argos file
  -This will also produce empty logs and results directories in your main directory
  
Go into clusterRunSimulations.sh
  -change parameter samples on 3rd line
  
Go into setVariablesNEW.sh 
  -set length, final run number (amount of samples), final seed iteration number(amount of replicas)
  -Out file name, combined file name and processed file name can be whatever you want
  -Data File name MUST be set to whatever you named the data file in app 1
  
Zip up directory and place it on the cluster, before unzipping the file 

run on the cluster using clusterRunSimulations.sh

Take the directory back from the cluster once results are completed

Once on local machine, run processDataNEW.sh and then run combineResultsCORRECT.sh (make sure you select 'yes' for combining the file)

Open app2:
  -insert settings file from app 1
  -insert file path that the combined file is located
  -insert the combined file(called that in setVariablesNEW.sh)
  -follow app instructions to make the graphs and then show the graphs and select which you want to see
  
