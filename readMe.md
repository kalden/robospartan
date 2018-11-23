## RoboSpartan

Transferring our expertise in analyzing biological simulations, we are developing extensive new infrastructure, called RoboSpartan, that will permit an increased understanding and optimization of the behaviours of robotic systems, and aid targeted experimentation in hardware. RoboSpartan will possess the capability to: automate simulation parameter value sampling and result analysis for conducting uncertainty and sensitivity analyses; apply machine learning approaches to develop a surrogate model for use in place of a simulator where analyses become less tractable; and utilize evolutionary and Bayesian computation techniques to identify parameter regions giving rise to desired behaviours.

In the below, we examine the sampling and analysis of five parameters that influence behaviour of the Omega algorithm used in swarm robotics. Currently we are offering this platform as a download to execute locally. In the coming months, we will look to host this online to make the next version easier to use.

---

### 1. Running the Initial Version

We would suggest that the initial version of this platform is run in RStudio. You should be able to download or fork this repository, then open the project in RStudio. In addition, you will require the following R packages:
* spartan (version 3.0.2)
* packages spartan is dependent upon: lhs, gplots, XML, plotrix, mlegp, ggplot2, neuralnet, psych, mco, randomForest, e1071
* shiny
* shinyjs
* shinycssloaders
* xml2
* DT
* readr

However, we are working towards running the tool online. The first app, the Parameter Sampling App, is available to run via the Shiny Apps site, at https://robospartan.shinyapps.io/sampling/

---

### 2. Case Study

Here, and in supporting papers for this tool, we show how RoboSpartan can be used to understand 5 parameters that influence the Omega algorithm used in swarm robotics. We examine five parameters, and two simulation responses.

We are going to generate parameter value sets that change the values of 5 parameters:
* quantity (number of robots), range between 2 and 28, with a calibrated value of 20
* omega (ticks), range between 15 and 35, with a calibrated value of 25
* shadowed_avoidance_radius (m), range between 0.05 and 0.15, with a calibrated value of 0.1
* illuminated_avoidance_radius (m), range between 0.15 and 0.3, with a calibrated value of 0.15
* cool_off_period (ticks), range between 0 and 10, with a calibrated value of 5
We are interested in seeing the effects the parameter values have on two outputs:
* distanceToBeacon: distance the swarm is from the beacon at the end of the simulation
* efficiency: swarm efficiency in reaching the beacon

In this repository, the file epuck_omega_algorithm.argos is the simulation that we will modify using RoboSpartan, collating the executed results prior to analysis using the second RoboSpartan app.

---

### 3. Parameter Sampling App

RoboSpartan can generate parameter value sets for three sensitivity analysis techniques, one local (that changes the value of one parameter at a time), and two global (that change all five of the above sequentially). We have not duplicated the detail of each technique here, instead we refer the reader to the vignettes for the spartan package: (https://cran.r-project.org/web/packages/spartan/vignettes/sensitivity_analysis.html)

If running locally, in the main roboSpartan folder, open app.R in RStudio. On the toolbar above the file editor should be a button labelled "Run App". Click the down arrow beside "Run App", and choose "Run External". Then press "Run App", and the parameter sampling app will open in a web browser. Alternatively you can run this app online at https://robospartan.shinyapps.io/sampling/. In this app you can:
* Select the analysis technique, for which you are generating samples, from the drop down box
* Declare your parameter names and ranges. If one of your parameters is a whole number, RoboSpartan can note this and round the sampled value accordingly, by pressing the checkbox.
* State the output responses from the simulation, for which you are interested in understanding the impact occurred by a change in parameter value
* State your sampling settings. For a latin-hypercube, you will need to state the number of samples to generate, and the sampling algorithm (normal or optimal - note optimal can take a long time). For eFAST, you need to state the number of samples to generate from each curve, and the number of resample curves. For robustness analysis there are no additional settings. In all cases, you will need to state the number of replicate executions you want to do for each parameter value set (if your sim is stochastic), as this will be included on generated cluster scripts.

If you now press the "Create Sample" button, a sample is created and shown in the panel on the right hand side of the application. Below the samples you have two buttons: one to download the sample ("Download Data"), and one to download the settings used in generating these samples ("Download settings"). The settings is handy to download, as you can enter this file into the next app that analyses these samples, to save having to input the same parameter and measure information again.

Finally, bottom of the left hand panel is a section to upload and generate ARGoS simulation files. This takes an ARGoS simulation file and alters the values of each parameter in the file, generating one file to match each sample shown on the right hand side. You can try this by entering the parameters and measures for the omega algorithm, described above, and specify the ARGoS file to modify as the "epuck_omega_algorithm.argos" file included in the git repository. With the file uploaded, pressing "Download Modified ARGoS Files" will download a zip file of all simulation configuration files for the generated sample. To generate scripts to run this experiment on a sun grid engine, press the "Generate SGE Cluster Script" button. This downloads two files: one to run the parameter sets on an SGE, and the second to post-process the results into a format that can be input into the second app below.

---

### 4. Parameter Analysis App

Once you have your parameter value sets and performed the executions, you can use the second RoboSpartan app to analyse the data. To aid demonstration of this process, in this repository there is a folder, "Test_Settings_and_Results", that contains RoboSpartan settings files and simulation execution results for all three sensitivity analyses. From RStudio, open the app.R file that is contained in the "analysis_platform" folder, again running externally as detailed for the sampling app. With the app open:
* You will need to upload a settings file created when the parameter sample was generated in the previous app. This saves you having to enter all the simulation parameter and response information again. If using the examples in the "Test_Settings_and_Results" folder: for robustness analysis upload "omega_Robustness_settingFile.csv"; for latin-hypercube upload "omega_LHC_settingFile.csv"; for eFAST upload "omega_eFAST_settingFile.csv"
* RoboSpartan will then populate the screen with your simulation information, and show on the left hand side that it is aware which type of analysis is being performed.
* You will then have to upload a summary of all executions performed for each parameter set in sampling. If using the example data: for robustness analysis upload "omegaAlgorithmRobustnesscombinedParamsAndResults.csv"; for latin-hypercube upload "/omegaAlgorithmLHCcombinedParamsAndResults.csv"; for eFAST upload "eFAST_Sample_Outputs.zip". Note the latter is a zip file comprised of several CSV files, one for each curve-parameter-measure pair. See the spartan package vignettes for more information on this result file.
* Should you wish, you can provide a scale in which the simulation responses are measured, that is used when plotting the simulation results (by amending the labels on the appropriate axis).
* You can then change some default analysis settings, dependent on the analysis. For robustness analysis, you can change the value for which an A-Test score is deemed scientifically significant should you wish. For efast analysis, you can change the confidence interval used when calculating statistical significance using the t-test (again see the description of the technique in the spartan R package). There are no additional arguments for a latin-hypercube PRCC analysis.
* With all the above successfully stated, you can press the button at the bottom of the app to generate the results. These will be shown in the main panel. You will have the option to look at each result plot in the browser. Alternatively, you can download all the produced statistics for the analysis (graphs as well as CSV files, the settings file, and executions file) as a zip file. All results are deleted when you leave the app.

Again to save duplication we do not detail how one should interpret each graph here, instead we refer you to the spartan package vignette that contains all the analyses used by RoboSpartan (https://cran.r-project.org/web/packages/spartan/vignettes/sensitivity_analysis.html)







