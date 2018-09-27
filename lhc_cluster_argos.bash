#$-S /bin/bash 
#$-cwd 
#$-t 1-65 
#$-l h_vmen=8G 
#$-l h_rt=23:59:59 
 
parameterFilePath = '/home/fgch500/robospartan/argosFiles' 
 
for i in {1..400} 
do 
	 if [ ! -d $parameterOutputPath/$SGE_TASKID/ ]; then 
	 	 mkdir $parametersOutputPath/$SGE_TASK_ID 
	 fi 
 
	 if [ ! -d $parameterOutputPath/$SGE_TASKID/$i ]; then 
	 	 mkdir $parametersOutputPath/$SGE_TASK_ID/$i 
	 fi 
 
	 argos3 -c $parameterFilePath/argos_experiment_set_$SGE_TASK_ID.argos 
done