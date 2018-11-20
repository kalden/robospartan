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

### 3. Parameter Sampling App

We are going to generate parameter value sets that change the values of 5 parameters:
* quantity (number of robots), range between 2 and 28, with a calibrated value of 20
* omega (ticks), range between 15 and 35, with a calibrated value of 25
* shadowed_avoidance_radius (m), range between 0.05 and 0.15, with a calibrated value of 0.1
* illuminated_avoidance_radius (m), range between 0.15 and 0.3, with a calibrated value of 0.15
* cool_off_period (ticks), range between 0 and 10, with a calibrated value of 5
We are interested in seeing the effects the parameter values have on two outputs:
* distanceToBeacon: distance the swarm is from the beacon at the end of the simulation
* efficiency: swarm efficiency in reaching the beacon

RoboSpartan can generate parameter value sets for three sensitivity analysis techniques, one local (that changes the value of one parameter at a time), and two global (that change all five of the above sequentially). We have not duplicated the detail of each technique here, instead we refer the reader to the vignettes for the spartan package: (https://cran.r-project.org/web/packages/spartan/vignettes/sensitivity_analysis.html)

In the main roboSpartan folder, open app.R in RStudio. On the toolbar above the file editor should be a button labelled "Run App". Click the down arrow beside "Run App", and choose "Run External". Then press "Run App", and the parameter sampling app will open in a web browser. In this app you can:
* Select the analysis technique, for which you are generating samples, from the drop down box
* Declare your parameter names and ranges. If one of your parameters is a whole number, RoboSpartan can note this and round the sampled value accordingly, by pressing the checkbox.
* State the output responses from the simulation, for which you are interested in understanding the impact occurred by a change in parameter value
* State your sampling settings. For a latin-hypercube, you will need to state the number of samples to generate, and the sampling algorithm (normal or optimal - note optimal can take a long time). For eFAST, you need to state the number of samples to generate from each curve, and the number of resample curves. For robustness analysis there are no additional settings. In all cases, you will need to state the number of replicate executions you want to do for each parameter value set (if your sim is stochastic), as this will be included on generated cluster scripts.

If you now press the "Create Sample" button, a sample is created and shown in the panel on the right hand side of the application. Below the samples you have two buttons: one to download the sample ("Download Data"), and one to download the settings used in generating these samples ("Download settings"). The settings is handy to download, as you can enter this file into the next app that analyses these samples, to save having to input the same parameter and measure information again.

Finally, bottom of the left hand panel is a section to upload and generate ARGoS simulation files. This takes an ARGoS simulation file and alters the values of each parameter in the file, generating one file to match each sample shown on the right hand side. You can try this by entering the parameters and measures for the omega algorithm, described above, and specify the ARGoS file to modify as the "epuck_omega_algorithm.argos" file included in the git repository. With the file uploaded, pressing "Download Modified ARGoS Files" will download a zip file of all simulation configuration files for the generated sample. To generate scripts to run this experiment on a sun grid engine, press the "Generate SGE Cluster Script" button. This downloads two files: one to run the parameter sets on an SGE, and the second to post-process the results into a format that can be input into the second app below.







