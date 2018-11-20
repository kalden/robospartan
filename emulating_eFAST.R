# Number of curves for eFAST
numCurves <- 3
# Number of samples to take from each curve
numSamples <- 65
# Where samples are stored
filepath <- getwd()
# Recall we need to add a dummy parameter for statistical comparison
parameters <- c("quantity","omega",
                "cool_off_period",
                "shadowed_avoidance_radius","illuminated_avoidance_radius",
                "dummy")
measures<-c("distanceToBeacon","efficiency")
PMIN <- cbind(2,15,0,0,0,0.15,0)
# Min of all parameters. Include a min value for dummy (0 here)
PMAX <-cbind(28,35,10,0.15,0.3,1)

load("/home/kja505/Documents/roboSpartan/Machine_learning_emulator_app/built_ensemble.Rda")

#### Step 1: Use Spartan Technique 4 to generate samples
efast_generate_sample(filepath, numCurves,numSamples,parameters, PMIN, PMAX)
# As this method produces several CSV files (one per parameter/curve), and it is these files which are 
# read in to make the predictions, these have not been included as an object that can be loaded in. 
# However an example set is available on the project website. Extract that into the working folder and then run the below.

# Start the clock!
ptm <- proc.time()

# Make predictions and plot the results
emulate_efast_sampled_parameters(filepath, built_ensemble, parameters, measures, numCurves, normalise = TRUE)

# Analyse:
efast_run_Analysis(filepath,measures,parameters,numCurves, numSamples,1:2,0.95,TRUE,"eFAST_Analysis.csv")

proc.time() - ptm