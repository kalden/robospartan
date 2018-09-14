#$ -cwd
#$ -V
#$ -l h_rt=01:00:00
# -m be
# -M ebb505@york.ac.uk
#$ -l h_vmem=2G
#$ -pe smp 2
#$ -t 1-180
#$ -o /scratch/ebb505/Logs
#$ -e /scratch/ebb505/Logs
#!/bin/bash
float_scale=2
function float_eval()
{
    local stat=0
    local result=0.0
    if [[ $# -gt 0 ]]; then
        result=$(echo "scale=$float_scale; $*" | bc -q 2>/dev/null)
        stat=$?
        if [[ $stat -eq 0  &&  -z "$result" ]]; then stat=1; fi
    fi
    echo $result
    return $stat
}

fileFolder=CPS_SA_Hom #
let expName=1 #

let simulationLength=54000 #
#let randomSeed=${SGE_TASK_ID}
let randomSeed=1

let partType=4  # 1 - NPS, 2 - SPS, 3 - DPS, 4 - CPS
let errorType=0 # 0 - Homogeneous, 1 - Heterogeneous

# DPS
let initialP=67 # Initial partition length
let gain=80 # Gain 
let alpha=50 # Alpha
# CPS
let epsilon=2 # Epsilon
let memFac=3 # Memory factor
# Loop function 
let transferType=1 # 0 - Direct transfer, 1 - Indirect transfer
homeSourceDistance=1 #
homeSourceDistance=$(float_eval "$homeSourceDistance+0.3")
# Arena size
width=$(float_eval "$homeSourceDistance+1.0")
heigth=$(float_eval "$homeSourceDistance+1.0")
#wallLengthWidth=$(float_eval "$homeSourceDistance+0.9")
wallLengthWidth="2.2"
wallLengthHeigth=$(float_eval "$homeSourceDistance+0.3")
wallPosWidth=$(float_eval "($homeSourceDistance+0.3)/2")
#wallPosHeigth=$(float_eval "($homeSourceDistance+0.9)/2")
wallPosHeigth="1.1"
# Robots
swarmSize= #
centerPos=$(float_eval "($homeSourceDistance-0.3)/2+0.05")
#centerPos="0.55"
distancePos=$(float_eval "$centerPos*2.0")


#if [ -d /scratch/ebb505/$fileFolder ]
#	then echo "Directory exists"
#	else mkdir /scratch/ebb505/$fileFolder
#fi
#
#if [ -d /scratch/ebb505/$fileFolder/$expName ]
#	then echo "Directory exists"
#	else mkdir /scratch/ebb505/$fileFolder/$expName 
#fi

fileName="robospartan_test"
#fileName=$fileFolder$expName$randomSeed

echo -e "<?xml version=\"1.0\" ?>"  >> experiments/$fileName.argos
echo -e "<argos-configuration>"  >> experiments/$fileName.argos
echo -e " " >> experiments/$fileName.argos
echo -e " <framework>"  >> experiments/$fileName.argos
echo -e "   <system threads=\"0\"/>"  >> experiments/$fileName.argos
echo -e "   <experiment length=\"$simulationLength\""  >> experiments/$fileName.argos
echo -e "   ticks_per_second=\"10\""  >> experiments/$fileName.argos
echo -e "   random_seed=\"$randomSeed\"/>"  >> experiments/$fileName.argos
echo -e " </framework>"  >> experiments/$fileName.argos
echo -e " " >> experiments/$fileName.argos
echo -e " <controllers>"  >> experiments/$fileName.argos
echo -e "   <psiswarm_dps id=\"psiswarm_dps\""  >> experiments/$fileName.argos
echo -e "     library=\"build/controllers/psiswarm_dps/libpsiswarm_dps\">"  >> experiments/$fileName.argos
echo -e "     <actuators>"  >> experiments/$fileName.argos                               
echo -e "       <psiswarm_wheels implementation=\"default\"/>"  >> experiments/$fileName.argos
echo -e "     </actuators>"  >> experiments/$fileName.argos
echo -e "     <sensors>"  >> experiments/$fileName.argos
echo -e "      <psiswarm_proximity implementation=\"default\" show_rays=\"true\"/>"  >> experiments/$fileName.argos
echo -e "     </sensors>"  >> experiments/$fileName.argos
echo -e "     <params>"  >> experiments/$fileName.argos
echo -e "       <wheel_turning hard_turn_angle_threshold=\"70\""  >> experiments/$fileName.argos
echo -e "         soft_turn_angle_threshold=\"25\""  >> experiments/$fileName.argos
echo -e "        no_turn_angle_threshold=\"12.5\""  >> experiments/$fileName.argos
echo -e "         max_speed=\"4.0\" />"  >> experiments/$fileName.argos
echo -e "       <partitioning partition_type=\"$partType\""  >> experiments/$fileName.argos
echo -e "         initial_partition_length=\"$initialP\""  >> experiments/$fileName.argos
echo -e "         gain=\"$gain\""  >> experiments/$fileName.argos
echo -e "         alpha=\"$alpha\" "  >> experiments/$fileName.argos
echo -e "         epsilon=\"$epsilon\""  >> experiments/$fileName.argos
echo -e "         memory_factor=\"$memFac\""  >> experiments/$fileName.argos
echo -e "         error_type=\"$errorType\" />"  >> experiments/$fileName.argos
echo -e "     </params>"  >> experiments/$fileName.argos
echo -e "   </psiswarm_dps>"  >> experiments/$fileName.argos
echo -e " </controllers>"  >> experiments/$fileName.argos
echo -e " " >> experiments/$fileName.argos
echo -e " <loop_functions library=\"build/loop_functions/psiswarm_dps_loop_functions/libpsiswarm_dps_loop_functions\""  >> experiments/$fileName.argos
echo -e "   label=\"psiswarm_dps_loop_functions\" >"  >> experiments/$fileName.argos
echo -e "   <dps individual=\"/scratch/ebb505/$fileFolder/$expName/$randomSeed/individual.csv\""  >> experiments/$fileName.argos
echo -e "   	global=\"/scratch/ebb505/$fileFolder/$expName/$randomSeed/global.csv\""  >> experiments/$fileName.argos
echo -e "     transfer_type=\"$transferType\""  >> experiments/$fileName.argos
echo -e "     home_source_distance=\"$homeSourceDistance\"/>"  >> experiments/$fileName.argos
echo -e " </loop_functions>"  >> experiments/$fileName.argos
echo -e " " >> experiments/$fileName.argos
echo -e " <arena size=\"$heigth,$width,2\" center=\"0,0,1\">"  >> experiments/$fileName.argos
echo -e "   <floor id=\"floor\""  >> experiments/$fileName.argos
echo -e "     source=\"loop_functions\""  >> experiments/$fileName.argos
echo -e "     pixels_per_meter=\"50\" />"  >> experiments/$fileName.argos
echo -e "   <box id=\"wall_north\" size=\"$wallLengthHeigth,0.01,0.25\" movable=\"false\">"  >> experiments/$fileName.argos
echo -e "     <body position=\"0,$wallPosHeigth,0\" orientation=\"0,0,0\"/>"  >> experiments/$fileName.argos
echo -e "   </box>"  >> experiments/$fileName.argos
echo -e "   <box id=\"wall_south\" size=\"$wallLengthHeigth,0.01,0.25\" movable=\"false\">"  >> experiments/$fileName.argos
echo -e "     <body position=\"0,-$wallPosHeigth,0\" orientation=\"0,0,0\"/>"  >> experiments/$fileName.argos
echo -e "   </box>"  >> experiments/$fileName.argos
echo -e "   <box id=\"wall_east\" size=\"0.01,$wallLengthWidth,0.25\" movable=\"false\">"  >> experiments/$fileName.argos
echo -e "     <body position=\"$wallPosWidth,0,0\" orientation=\"0,0,0\"/>"  >> experiments/$fileName.argos
echo -e "   </box>"  >> experiments/$fileName.argos
echo -e "   <box id=\"wall_west\" size=\"0.01,$wallLengthWidth,0.25\" movable=\"false\">"  >> experiments/$fileName.argos
echo -e "     <body position=\"-$wallPosWidth,0,0\" orientation=\"0,0,0\"/>"  >> experiments/$fileName.argos
echo -e "   </box>"  >> experiments/$fileName.argos
echo -e "   <distribute>"  >> experiments/$fileName.argos
echo -e "     <position method=\"grid\" center=\"$centerPos,0,0\""  >> experiments/$fileName.argos
echo -e "       distances=\"$distancePos,0.15,0\""  >> experiments/$fileName.argos
echo -e "       layout= \"1,$swarmSize,1\" />"  >> experiments/$fileName.argos
echo -e "     <orientation method=\"constant\" values=\"180,0,0\" />"  >> experiments/$fileName.argos
echo -e "     <entity quantity= \"$swarmSize\" max_trials=\"1\">"  >> experiments/$fileName.argos
echo -e "       <psi-swarm id=\"psiswarm\">"  >> experiments/$fileName.argos
echo -e "         <controller config=\"psiswarm_dps\" />"  >> experiments/$fileName.argos
echo -e "       </psi-swarm>"  >> experiments/$fileName.argos
echo -e "     </entity>"  >> experiments/$fileName.argos
echo -e "   </distribute>"  >> experiments/$fileName.argos
echo -e " </arena>"  >> experiments/$fileName.argos
echo -e " " >> experiments/$fileName.argos
echo -e " <physics_engines>"  >> experiments/$fileName.argos
echo -e "   <dynamics2d id=\"dyn2d\"/>"  >> experiments/$fileName.argos
echo -e " </physics_engines>"  >> experiments/$fileName.argos
echo -e " <media/>"  >> experiments/$fileName.argos
# echo -e " " >> experiments/$fileName.argos
# echo -e " <visualization>"  >> experiments/$fileName.argos
# echo -e "   <qt-opengl>"  >> experiments/$fileName.argos
# echo -e "     <camera>"  >> experiments/$fileName.argos
# echo -e "     <placement idx=\"0\""  >> experiments/$fileName.argos
# echo -e "       position=\"0,0,1.25\""  >> experiments/$fileName.argos
# echo -e "       look_at=\"0,0,0\""  >> experiments/$fileName.argos
# echo -e "       lens_focal_length=\"20\" />"  >> experiments/$fileName.argos
# echo -e "     </camera>"  >> experiments/$fileName.argos
# echo -e "     <user_functions label=\"psiswarm_dps_qt_user_functions\" />"  >> experiments/$fileName.argos
# echo -e "   </qt-opengl>"  >> experiments/$fileName.argos
# echo -e " </visualization>"  >> experiments/$fileName.argos#
echo -e "</argos-configuration>"  >> experiments/$fileName.argos

#mkdir /scratch/ebb505/$fileFolder/$expName/$randomSeed
#cp experiments/$fileName.argos /scratch/ebb505/$fileFolder/$expName/$randomSeed

#export LD_LIBRARY_PATH=/scratch/ebb505/code/ArgosProgram_38/lib/argos3/:$LD_LIBRARY_PATH
#export PKG_CONFIG_PATH=/scratch/ebb505/code/ArgosProgram_38/lib/pkgconfig/:$PKG_CONFIG_PATH
#export PATH=/scratch/ebb505/code/ArgosProgram_38/bin:$PATH

#argos3 -c experiments/$fileName.argos
#rm -r experiments/$fileName.argos
