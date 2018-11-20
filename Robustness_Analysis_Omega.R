library(spartan)
# Folder containing the example simulation results. Make sure the folder is unzipped
FILEPATH <- "/home/kja505/Downloads/SwarmTaxis/Results/Robustness_Analysis"
# Array of the parameters to be analysed.
# Note only two of the six here for download size reasons
PARAMETERS <- c("quantity")
# Similar to the sampling function discussed above, there are two ways to specify
# parameter value information in the analysis. Ensure you are using the appropriate
# method, setting these to NULL if using the alternative (see comments in sampling
# function description).
# Method 1:
PMIN <- c(2)
PMAX <- c(28)
PINC <- c(1)
BASELINE <- c(25)
MEASURES <- c("distanceToBeacon","robotDensity")
# What each measure represents. Used in graphing results
MEASURE_SCALE <- c("Distance", NULL)
CSV_FILE_NAME <- "Robustness_Summary.csv"
# The results of the A-Test comparisons of each parameter value against that of the
# parameters baseline value are output as a file. This sets the name of this file.
# Current versions of spartan output this to a CSV file
ATESTRESULTSFILENAME <- "Omega_ATests.csv"
# A-Test result value either side of 0.5 at which the difference between two sets of
# results is significant
ATESTSIGLEVEL <- 0.23

all_results<-NULL

current_value<-PMIN
for(param_set in 1:27)
{
  set_results<-NULL
  for(replicate in 1:100)
  {
    result<-read.csv(file.path(FILEPATH,param_set,replicate,"global.csv"))
    # Take the final row
    set_results<-rbind(set_results,cbind(current_value, result[10000,]))
  }
  colnames(set_results)<-c("quantity",MEASURES)
  current_value<-current_value+PINC[1]
  all_results<-rbind(all_results,set_results)
  
}
colnames(all_results)<-c("quantity",MEASURES)
write.csv(all_results,file=file.path(FILEPATH,"Robustness_Summary.csv"),row.names=F)


oat_csv_result_file_analysis(FILEPATH, CSV_FILE_NAME, PARAMETERS, BASELINE, MEASURES, ATESTRESULTSFILENAME, PMIN, PMAX, PINC, PARAMVALS=NULL)
oat_graphATestsForSampleSize(FILEPATH, PARAMETERS, MEASURES, ATESTSIGLEVEL, ATESTRESULTSFILENAME, BASELINE, PMIN, PMAX, PINC, PARAMVALS=NULL, output_types = c("png"))
oat_plotResultDistribution(FILEPATH, PARAMETERS, MEASURES, MEASURE_SCALE, CSV_FILE_NAME, BASELINE, PMIN, PMAX, PINC, PARAMVALS=NULL, output_types = c("png"))
