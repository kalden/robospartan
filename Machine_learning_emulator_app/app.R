library(shiny)
library(spartan)
library(shinyjs)
library(ggplot2)
library(shinyBS)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("RoboSpartan: Machine Learning Emulator Generation App"),
  useShinyjs(),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Settings:"),
        
        fileInput(inputId = "settingsFile",
                  label = "Import Settings File")
        
      ),
      
      wellPanel(id = "dataInput",
                h4("Data Input:"),
                fileInput(inputId = "simData", label = "Choose Input Data")),
     
      wellPanel(id = "datasetValues",
                h4("Partitioned Dataset Variables:"),
                h6(tags$em("These values must add up to 100%...")),
                br(),
                numericInput(inputId = "percentTrain", label = "Percent Train", value = 75, min = 0, max = 100),
                numericInput(inputId = "percentTest", label = "Percent Test", value = 15, min = 0, max = 100),
                numericInput(inputId = "percentValidation", label = "Percent Validation", value = 10, min = 0, max = 100)
      ),
      
      wellPanel(id = "models",
                h4("Emulator Models Selection:"),
                
                bsButton(inputId = "SVM", label="SVM", width='30%'),
                #actionButton(inputId = "SVM", label = "SVM", width = '30%'),
                bsButton(inputId = "RF", label = "RF", width = '30%'),
                bsButton(inputId = "GLM", label = "GLM", width = '30%'),
                bsButton(inputId = "NNET", label = "NNET", width = '30%'),
                bsButton(inputId = "GP", label = "GP", width = '30%'),
                bsButton(inputId = "Ensemble", label = "Ensemble", width = '30%'),
                tags$style("#clearModels {border: 2px solid #dd4b39; text-align:center;;}"),
                bsButton(inputId = "clearModels", label = "Clear", width = '90%')
      ),
      
      wellPanel(id = "netStructWell",
                h4("Network Structure:"),
                h6(tags$em("Multiple values to same network structure should be seperated by a comma and no space.")),
                textInput(inputId= "netStructText", label = "Input Network Structure"),
                p(),
                tags$style("#addNetStruct {border: 3px solid #008000;}"),
                actionButton(inputId = "addNetStruct", label = "Add to Network Structure"),
                p(),
                tags$style("#clearNetStruct {border: 3px solid #dd4b39;}"),
                actionButton(inputId = "clearNetStruct", label = "Clear Network Structure")),
                #p(),
                #tags$style("#netStructComp {border: 3px solid #0000FF; font-weight: bold;}"),
                #actionButton(inputId = "netStructComp", label = "Network Structure Completed")),
      
      #wellPanel(id = "partitionDataset",
      #          h4("Partition the Dataset:"),
      #          actionButton(inputId = "partitionDataset", label = "Partition")),
      
      #wellPanel(id = "algorithmSettings",
      #          h4("Make the Algorithm Settings:"),
      #          actionButton(inputId = "algSet", label = "Algorithm Settings")),
      
      wellPanel(id = "makeEnsemble",
                h4("Generate the Emulators and Ensembles:"),
                actionButton(inputId = "ensembles", label = "Generate Predictive Models")),
      width = 4),
    
    # Show a plot of the generated distribution
    mainPanel(
      div(tableOutput("parameter_table"),style="font-size:90%"),
      div(tableOutput("measures_table"),style = "font-size:90%"),
      selectInput(inputId = "whichData",
                  label = NULL,
                  choices = c("Algorithm Settings - Network Structures", "Generated Ensembles and Emulators")),
      #div(tableOutput( uiOutput("choiceTable")),style="font-size:90%"),
      # tabsetPanel(id = "PartitionedData_tabs", type = "tabs",
      #             tabPanel(title = "Training"),
      #             tabPanel(title = "Testing"),
      #             tabPanel(title = "Validation"),
      #             tabPanel(title = "Pre Normed Values")
      #             ),
      # tabsetPanel(id = "AlgorithmSettings_tabs", type = "tabs",
      #             tabPanel(title = "Settings"),
      #             tabPanel(title= "Network Structures")),
      wellPanel(id = "graphSettingsWell",
                selectInput(inputId = "graphSelect",
                            label = "Select Graph",
                            choices = ""),
                selectInput(inputId = "testOrTrain",
                            label = "Testing or Training Set Data",
                            choices = c("TrainingSet", "TestSet")),
                selectInput(inputId = "measureSelect",
                            label = "Measure Select",
                            choices = ""),
                actionButton(inputId = "showGraph", label = "Show Graph"),
                downloadButton(outputId = "zip_analysis", label = "Download Emulators & Outputs in Zip File")
      ),
      #div(tableOutput("PartitionedData_table"),style="font-size:90%"),
      div(tableOutput("NetStructs_table"), style = "font-size:90%"),
      div(tableOutput("AlgorithmSettings_table"),style="font-size:90%"),
      div(tableOutput("Emulations_table"),style="font-size:90%"),
      imageOutput(outputId = "Graph"),
      width = 8)
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # KA: Moved Finlay's attributes into server so no longer global
  emulation_attrs <- reactiveValues()
  emulation_attrs$modelList <- c()
  
  # Output measures
  #emulation_attrs$measures <-c()
  #measures<-c("Velocity","Displacement","PatchArea")
  #emulation_attrs$columnNames <<- c("Measures")
  emulation_attrs$sampleMaxes <- c()
  emulation_attrs$sampleMins <-c()
  emulation_attrs$measures<-c()
  # Set the algorithm settings. Let's say in this case we're generating a neural 
  # network, so we need to feed in some potential structures to examine
  #emulation_attrs$networkStructures<-list(c(4),c(3),c(4,3),c(4,4,3),c(4,4),c(4,3,3),c(4,4,4,3),c(4,3,2))
  emulation_attrs$networkStructures<-list()
  emulation_attrs$PartData <- FALSE
  emulation_attrs$algsDone <- FALSE
  emulation_attrs$ensembleDone <- FALSE
  emulation_attrs$netStruct <- c()
  emulation_attrs$userNetStruct <- c()
  emulation_attrs$structDone <- FALSE
  emulation_attrs$count <- 1
  emulation_attrs$partitionedData <- NULL
  #emulation_attrs$partition_data<- NULL
  
  measureValues <- reactiveValues()
  myValues <- reactiveValues() 
  PartitionedData_table <-reactiveValues()
  AlgorithmSettings_table <-reactiveValues()
  options(shiny.maxRequestSize=100*1024^2)
  shinyjs::disable("ensembles")
  #shinyjs::disable("algSet")
  shinyjs::hide("whichData")
  shinyjs::hide("PartitionedData_table")
  shinyjs::hide("AlgorithmSettings_table")
  shinyjs::hide("NetStructs_table")
  shinyjs::hide("AlgorithmSettings_tabs")
  shinyjs::hide("PartitionedData_tabs")
  shinyjs::hideElement("graphSettingsWell")
  #shinyjs::disable("partitionDataset")
  shinyjs::hide("netStructWell")
  shinyjs::hide("models")
  shinyjs::hide("dataInput")
  shinyjs::hide("datasetValues")
  #shinyjs::hide("partitionDataset")
  #shinyjs::hide("algorithmSettings")
  shinyjs::hide("makeEnsemble")
  #shinyjs::hide("zip_analysis")
  # Hide the ensemble button until two algorithms have been selected
  shinyjs::disable("Ensemble")
  
  # We're going to have a specific directory named with the date and time, incase of multiple users
  emulation_attrs$user_dir <- file.path(getwd(),paste0("rs_analysis_",gsub(" ","_",gsub(":","_",toString(Sys.time())))))
  # Also stored as non-reactive so can delete at the end of the session
  user_dir<-file.path(getwd(),paste0("rs_analysis_",gsub(" ","_",gsub(":","_",toString(Sys.time())))))
  
  output$zip_analysis <- downloadHandler(
    filename = function() {
      paste0("rs_analysis_",gsub(" ","_",gsub(":","_",toString(Sys.time()))),".zip")
    },
    content = function(file) { 
      
      showModal(modalDialog(
        title = "Analysis Complete",
        HTML("Analysis Results Downloaded in ZIP file <br> Results will be automatically deleted from the server at the end of this session")))
      
      zip(zipfile = file, dir(user_dir, full.names = TRUE), flags="-qjr")
      
      
    })
  
  observeEvent(input$clearModels,
               {
                 updateButton(session, "SVM", style="default")
                 updateButton(session, "RF", style="default")
                 updateButton(session, "GLM", style="default")
                 updateButton(session, "NNET", style="default")
                 updateButton(session, "GP", style="default")
                 updateButton(session, "Ensemble", style="default")
                 shinyjs::enable("SVM")
                 shinyjs::enable("RF")
                 shinyjs::enable("GLM")
                 shinyjs::enable("NNET")
                 shinyjs::enable("GP")
                 shinyjs::disable("Ensemble")
                 
                 # Disable the neural network structures well, will have been shown if NN was selected
                 shinyjs::hide("netStructWell")
                 
                 emulation_attrs$modelList <<- c()
                 #print(emulation_attrs$modelList)
                 
                 # Disable the make ensemble button
                 shinyjs::disable("ensembles")
               }
  )  
  observeEvent(input$SVM,
               {
                 emulation_attrs$modelList <<- c(emulation_attrs$modelList, "SVM")
                 
                 # Change colour of button to show selected
                 updateButton(session, "SVM", style="success")
                 
                 #print(emulation_attrs$modelList)
                 shinyjs::disable("SVM")
                 
                 # Enable ensemble if more than one algorithm have now been selected
                 if(length(emulation_attrs$modelList)>1 && !"Ensemble" %in% emulation_attrs$modelList)
                 {
                   shinyjs::enable("Ensemble")
                 }
                 
                 # Enable the button to start the analysis
                 shinyjs::enable("ensembles")
               }
  )
  
  observeEvent(input$RF,
               {
                 emulation_attrs$modelList <<- c(emulation_attrs$modelList, "RF")
                 
                 # Change colour of button to show selected
                 updateButton(session, "RF", style="success")
                 
                 #print(emulation_attrs$modelList)
                 shinyjs::disable("RF")
                 
                 # Enable ensemble if more than one algorithm have now been selected
                 if(length(emulation_attrs$modelList)>1 && !"Ensemble" %in% emulation_attrs$modelList)
                 {
                   shinyjs::enable("Ensemble")
                 }
                 
                 # Enable the button to start the analysis
                 shinyjs::enable("ensembles")
               }
  )
  
  observeEvent(input$NNET,
               {
                 emulation_attrs$modelList <<- c(emulation_attrs$modelList, "NNET")
                
                 # Change colour of button to show selected
                 updateButton(session, "NNET", style="success")
                 
                 # print(emulation_attrs$modelList)
                 shinyjs::disable("NNET")
                 
                 # Show the neural network structures panel here - doesn't need to be shown if not selected
                 shinyjs::show("netStructWell")
                 
                 # Enable ensemble if more than one algorithm have now been selected
                 if(length(emulation_attrs$modelList)>1 && !"Ensemble" %in% emulation_attrs$modelList)
                 {
                   shinyjs::enable("Ensemble")
                 }
                 
                 # Enable the button to start the analysis
                 shinyjs::enable("ensembles")
                 
               }
  )
  
  observeEvent(input$GLM,
               {
                 emulation_attrs$modelList <<- c(emulation_attrs$modelList, "GLM")
                 
                 # Change colour of button to show selected
                 updateButton(session, "GLM", style="success")
                 
                 #print(emulation_attrs$modelList)
                 shinyjs::disable("GLM")
                 
                 # Enable ensemble if more than one algorithm have now been selected
                 if(length(emulation_attrs$modelList)>1 && !"Ensemble" %in% emulation_attrs$modelList)
                 {
                   shinyjs::enable("Ensemble")
                 }
                 
                 # Enable the button to start the analysis
                 shinyjs::enable("ensembles")
               }
  )
  
  observeEvent(input$GP, 
               {
                 emulation_attrs$modelList <<- c(emulation_attrs$modelList, "GP")
                 
                 # Change colour of button to show selected
                 updateButton(session, "GP", style="success")
                 
                 #print(emulation_attrs$modelList)
                 shinyjs::disable("GP")
                 
                 # Enable ensemble if more than one algorithm have now been selected
                 if(length(emulation_attrs$modelList)>1 && !"Ensemble" %in% emulation_attrs$modelList)
                 {
                   shinyjs::enable("Ensemble")
                 }
                 
                 # Enable the button to start the analysis
                 shinyjs::enable("ensembles")
               }
  )
  
  observeEvent(input$Ensemble, 
               {
                 emulation_attrs$modelList <<- c(emulation_attrs$modelList, "Ensemble")
                 
                 # Change colour of button to show selected
                 updateButton(session, "Ensemble", style="success")
                 
                 #print(emulation_attrs$modelList)
                 shinyjs::disable("Ensemble")
                 
                 # Enable the button to start the analysis
                 shinyjs::enable("ensembles")
               }
  )
  
  #observeEvent(input$netStructComp,
  #             {
  #               shinyjs::disable("netStructreComp")
  #               shinyjs::disable("addNetStruct")
  #               shinyjs::disable("netStructText")
  #               emulation_attrs$structDone <-TRUE
  #             }
  #             )
  
  observeEvent(input$addNetStruct,
               {
                 if (input$netStructText != "")
                 {
                   stringTing <- toString(input$netStructText)
                   if (grepl(" ", stringTing, fixed = TRUE) == F) #Checking the user hasnt used a space
                   {
                     shinyjs::show("NetStructs_table")
                     emulation_attrs$networkStructures[[emulation_attrs$count]]<-as.numeric(unlist(strsplit(input$netStructText,",")))
                     #print(emulation_attrs$networkStructures)
                     updateTextInput(session, inputId = "netStructText", value = "")
                     #emulation_attrs$userNetStruct <-cbind(emulation_attrs$userNetStruct, emulation_attrs$networkStructures)
                     emulation_attrs$userNetStruct <-rbind(emulation_attrs$userNetStruct,cbind(emulation_attrs$count, input$netStructText))
                     colnames(emulation_attrs$userNetStruct)<-c("Structure Number","Structure Examined")
                     output$NetStructs_table <- renderTable(emulation_attrs$userNetStruct, striped = TRUE, bordered = TRUE)
                     emulation_attrs$count <- emulation_attrs$count+1
                   }
                   else 
                   {
                     showModal(modalDialog(
                       title = "No Spaces",
                       paste0("No spaces allowed when defining network structures")))
                   }
                 }
                 
               })
  
  observeEvent(input$clearNetStruct,
               {
                 emulation_attrs$networkStructures<-list()
                 emulation_attrs$count <<- 1
               })
  
  observeEvent(input$simData, 
               {
                 shinyjs::show("datasetValues")
                 shinyjs::show("models")
                 shinyjs::show("makeEnsemble")
               })
  
  observeEvent(input$partitionDataset,
               {
                 shinyjs::hide("NetStructs_table")
                 showModal(modalDialog(
                   title = "Partitioning Data",
                   paste0("Data is currently being partitioned...")))
                 #newData <- input$simData$datapath
                 simDataset <-read.csv(input$simData$datapath, header = TRUE)
                 lister <<- c()
                 dataRemoved <<- FALSE
                 #print(simDataset)
                 for (i in 1:(length(simDataset[1, ])))
                 {
                   if(min(simDataset[i]) == max(simDataset[i]))
                   {
                     lister <- c(lister, colnames(simDataset[i]))
                     dataRemoved <<- TRUE
                   }
                 }
                 listerString <- toString(lister)
                 #print(emulation_attrs$measures)
                 #print(parameterList)
                 #print(emulation_attrs$sampleMins)
                 #print(emulation_attrs$sampleMaxes)
                 #partitionedData <<- partition_dataset(sim_data_for_emulation, parameterList, percent_train = input$percentTrain, percent_test = input$percentTest, percent_validation = input$percentValidation, normalise = TRUE, sample_mins = sampleMins, sample_maxes = sampleMaxes)
                 emulation_attrs$partitionedData <<- partition_dataset(simDataset, parameterList, emulation_attrs$measures, percent_train = input$percentTrain, percent_test = input$percentTest, percent_validation = input$percentValidation, normalise = TRUE, sample_mins = rbind(emulation_attrs$sampleMins), sample_maxes = rbind(emulation_attrs$sampleMaxes))
                 {}  
                 if (dataRemoved == TRUE)
                 {
                   showModal(modalDialog(
                     title = "Complete",
                     paste0("Data has been successfully partitioned. The measure(s) ", listerString,", have not been partitioned. This is due to their values all being identical.")))
                   emulation_attrs$measures <- emulation_attrs$measures[!emulation_attrs$measures %in% lister] #Remove the non partitioned measures as these will not be included in the graph options
                 }
                 else 
                 {
                   showModal(modalDialog(
                     title = "Complete",
                     paste0("All data has been successfully partitioned.")))
                 }
                 updateSelectInput(session, inputId = "measureSelect", choices = emulation_attrs$measures)
                 emulation_attrs$PartData <<- TRUE
                 #partition_data <<- get(load("/home/fgch500/robospartan/Machine_learning_emulator_app/partitioned_data.Rda"))
                 preNorms <<- cbind(emulation_attrs$partitionedData$pre_normed_mins, emulation_attrs$partitionedData$pre_normed_maxes)
                 if(!is.null(emulation_attrs$partitionedData)) 
                 {
                   colnames(preNorms) <<- c("Pre Normed Mins", "Pre Normed Maxs")
                 }
                 
                 #shinyjs::enable("algSet")
                 #print(partitionedData)
               }
               
  )
  
  observeEvent(input$testOrTrain, shinyjs::hide("Graph"))
  
  observeEvent(input$measureSelect, shinyjs::hide("Graph"))
  
  observeEvent(input$showGraph,
               {
                 if (input$graphSelect == "Ensemble")
                 {
                   testOrTrainVal = "Testing"
                 }
                 
                 else
                 {
                   testOrTrainVal = input$testOrTrain 
                 }
                 
                 #print(paste0(input$graphSelect, "_", testOrTrainVal, "_", input$measureSelect, ".png"))
                 
                 output$Graph <-  renderImage(list(src = file.path(emulation_attrs$user_dir,paste0(input$graphSelect, "_", testOrTrainVal, "_", input$measureSelect, ".png")) , width = '100%', alt = paste("Image not found")), deleteFile = FALSE)
                 shinyjs::show("Graph")
               })
  

  
  
  observeEvent(input$ensembles,
               {
                 current_wd<-getwd()
                 # Change into the users directory
                 setwd(emulation_attrs$user_dir)
                 
                 # Code from previous partition dataset button
                 shinyjs::hide("NetStructs_table")
                 showModal(modalDialog(
                   title = "Partitioning Data",
                   paste0("Data is currently being partitioned...")))
                 #newData <- input$simData$datapath
                 simDataset <-read.csv(input$simData$datapath, header = TRUE)
                 lister <<- c()
                 dataRemoved <<- FALSE
                # print(simDataset)
                 for (i in 1:(length(simDataset[1, ])))
                 {
                   if(min(simDataset[i]) == max(simDataset[i]))
                   {
                     lister <- c(lister, colnames(simDataset[i]))
                     dataRemoved <<- TRUE
                   }
                 }
                 listerString <- toString(lister)
                 #print(emulation_attrs$measures)
                 #print(parameterList)
                 #print(emulation_attrs$sampleMins)
                 #print(emulation_attrs$sampleMaxes)
                 #partitionedData <<- partition_dataset(sim_data_for_emulation, parameterList, percent_train = input$percentTrain, percent_test = input$percentTest, percent_validation = input$percentValidation, normalise = TRUE, sample_mins = sampleMins, sample_maxes = sampleMaxes)
                 emulation_attrs$partitionedData <- partition_dataset(simDataset, parameterList, emulation_attrs$measures, percent_train = input$percentTrain, 
                                                                      percent_test = input$percentTest, percent_validation = input$percentValidation, normalise = TRUE, 
                                                                      sample_mins = rbind(emulation_attrs$sampleMins), sample_maxes = rbind(emulation_attrs$sampleMaxes))
                 {}  
                 if (dataRemoved == TRUE)
                 {
                   showModal(modalDialog(
                     title = "Complete",
                     paste0("Data has been successfully partitioned. The measure(s) ", listerString,", have not been partitioned. This is due to their values all being identical.")))
                   emulation_attrs$measures <- emulation_attrs$measures[!emulation_attrs$measures %in% lister] #Remove the non partitioned measures as these will not be included in the graph options
                 }
                 else 
                 {
                   showModal(modalDialog(
                     title = "Complete",
                     paste0("All data has been successfully partitioned.")))
                 }
                 updateSelectInput(session, inputId = "measureSelect", choices = emulation_attrs$measures)
                 emulation_attrs$PartData <<- TRUE
                 #partition_data <<- get(load("/home/fgch500/robospartan/Machine_learning_emulator_app/partitioned_data.Rda"))
                 preNorms <<- cbind(emulation_attrs$partitionedData$pre_normed_mins, emulation_attrs$partitionedData$pre_normed_maxes)
                 if(!is.null(emulation_attrs$partitionedData)) 
                 {
                   colnames(preNorms) <<- c("Pre Normed Mins", "Pre Normed Maxs")
                 }
                 
                 #shinyjs::enable("algSet")
                 
                 # Code from previous algorithm settings button
                 
                 if(! "NNET" %in% emulation_attrs$modelList)
                 {
                   # Don't have to worry about network structures, can proceed without
                   algorithmSettings<-emulation_algorithm_settings(network_structures=emulation_attrs$networkStructures)
                   emulation_attrs$algsDone<-TRUE
                 }
                 else
                 {
                   print(emulation_attrs$networkStructures)
                   
                   if(length(emulation_attrs$networkStructures)>0)
                   {
                     # Make up the algorithm settings
                     print(paste0("Network Structures: ",emulation_attrs$networkStructures))
                     algorithmSettings<-emulation_algorithm_settings(network_structures=emulation_attrs$networkStructures)
                     emulation_attrs$algsDone<-TRUE
                   }
                   else
                   {
                     emulation_attrs$algsDone<-FALSE
                   }
                   
                 }
                 
                 if (emulation_attrs$algsDone == TRUE)
                 {
                   shinyjs::hide("PartitionedData_table")
                   #print(str(networkStructures))
                   settings <- cbind(algorithmSettings$num_trees, algorithmSettings$num_of_generations, algorithmSettings$num_of_folds, algorithmSettings$save_emulators, algorithmSettings$save_ensemble, algorithmSettings$plot_test_accuracy)
                   colnames(settings)<-c("Number of Trees", "Number of Generations", "Number of Folds", "Save Emulators (1-True, 0-False)","Save Ensemble (1-True, 0-False)", "Plot Test Accuracy (1-True, 0-False)")
                   if (emulation_attrs$structDone == TRUE)
                   {
                     for (networkNumber in 1:length(emulation_attrs$networkStructures))
                     {
                       indivStructure <- paste(unlist(emulation_attrs$networkStructures[[networkNumber]]), collapse = ', ') #Convert the list element into a string with comma seperation between the values
                       emulation_attrs$netStruct <-rbind(netStruct, cbind(networkNumber, indivStructure))
                     }
                     colnames(emulation_attrs$netStruct) <- c("Network", "Structure")
                   }
                   output$AlgorithmSettings_table <- renderTable(emulation_attrs$netStruct, striped = TRUE, bordered = TRUE)
                   # switch(input$AlgorithmSettings_tabs, "Network Structures" =  output$AlgorithmSettings_table <- renderTable(netStruct, striped = TRUE, bordered = TRUE),
                   #        "Settings" = output$AlgorithmSettings_table <- renderTable(settings, striped = TRUE, bordered = TRUE))
                   updateSelectInput(session, inputId = "whichData", selected = "Algorithm Settings - Network Structures")
                   #shinyjs::show("AlgorithmSettings_table")
                 
                 
                 # Old ensemble button code:
                 
                 #print(length(emulation_attrs$modelList))
                 if (length(emulation_attrs$modelList) > 0){
                   showModal(modalDialog(
                     title = "Generating Emulators and Ensembles",
                     "Emulators and Ensembles are being generated..."))
                   #print(emulation_attrs$measures)
                   #print(emulation_attrs$partitionedData)
                   
                   
                   # KA: Now doing one per emulator, as an ensemble may not have been requested:
                   if(!"Ensemble" %in% emulation_attrs$modelList)
                   {
                     # Build each emulator independently
                     for(model_type in emulation_attrs$modelList)
                    {
                      generate_requested_emulations(c(model_type), emulation_attrs$partitionedData, parameterList, emulation_attrs$measures, algorithmSettings, normalised=TRUE, output_formats=c("png"))
                     }
                   }
                   else
                   {
                     # Need to take ensemble out of the modelList
                     # Use the method that builds the ensemble and an emulator
                     generated_ensemble<-generate_emulators_and_ensemble(emulation_attrs$modelList[!emulation_attrs$modelList %in% c("Ensemble")], parameterList, emulation_attrs$measures, 
                                                                         emulation_attrs$partitionedData, algorithm_settings = algorithmSettings, normalised=TRUE, output_formats = c("png"))
                   }
                   
                   #print(generated_ensemble)
                   updateSelectInput(session, inputId = "graphSelect", choices = emulation_attrs$modelList)
                   updateSelectInput(session, inputId = "whichData", selected ="Generated Ensembles and Emulators")
                   #load(file = "built_ensemble.Rda")
                   emulation_attrs$ensembleDone <- TRUE
                   showModal(modalDialog(
                     title = "Complete",
                   "Emulators and Ensembles have been generated")) 
                 }
                 }
                 else
                 {
                   showModal(modalDialog(
                     title = "First Complete Network Structure",
                     "Network Structure needs completing before algorithm settings can be produced"))
                 }
                 #else
                 #{
                  # showModal(modalDialog(
                  #   title = "Add More Models",
                  #   "To generate an ensemble you need at least two types of emulator models"))
                 #}
                 
                 setwd(current_wd)
               })
  
  output$parameter_table<-renderTable({
    if(length(myValues$table)>1)
    {
      colnames(myValues$table) <- c("Parameter","Min","Max")
      myValues$table
    } 
    
  })
  
  observeEvent(input$graphSelect,
               {
                 shinyjs::hide("Graph")
                 if (input$graphSelect == "Ensemble")
                 {
                   shinyjs::disable("testOrTrain")
                 }
                 else
                 {
                   shinyjs::enable("testOrTrain")
                 }
               }
               
  )

  observeEvent(input$whichData,
               {
                 #print(input$whichData)
                 if(input$whichData == "Algorithm Settings - Network Structures")
                 {
                   #print(emulation_attrs$algsDone)
                   if (emulation_attrs$algsDone == TRUE)
                   {
                     shinyjs::show("AlgorithmSettings_table")
                     shinyjs::hide("PartitionedData_table")
                     shinyjs::show("AlgorithmSettings_tabs")
                     shinyjs::hide("PartitionedData_tabs")
                     shinyjs::hideElement("graphSettingsWell")
                     shinyjs::hide("Graph")
                     #updateTabsetPanel(session, inputId = "AlgorithmSettings_tabs", selected = "Settings")
                   }
                 }
                 else if(input$whichData == "Generated Ensembles and Emulators")
                   {
                    shinyjs::show("AlgorithmSettings_table")
                    shinyjs::hide("PartitionedData_table")
                    shinyjs::hide("AlgorithmSettings_tabs")
                    shinyjs::hide("PartitionedData_tabs")
                    if(emulation_attrs$ensembleDone == TRUE)
                    {
                      shinyjs::show("Graph")
                      shinyjs::showElement("graphSettingsWell")
                    }
                    else
                    {
                      showModal(modalDialog(
                        title = "Generate Emulators and Ensembles",
                        "Emulators and Ensembles must first be generated"))
                      updateSelectInput(session, inputId = "whichData", selected = "Algorithm Settings - Network Structures")
                    }
                  }
                    
                 }
               )
  
  observeEvent(input$settingsFile,
               {
                 if (!is.null(input$settingsFile)) #Ensure a settings file has been chosen 
                 {
                   dir.create(emulation_attrs$user_dir)
                   file.rename(from=input$settingsFile$datapath, to=file.path(emulation_attrs$user_dir,"analysis_settings.csv"))
                   
                   #These first two lines ensures that the tables are reset, so if the user changing their settings file, the one and new values wont bind together. Instead only the new values will be shown.
                   myValues$table <- NULL
                   measureValues$table <- NULL
                   emulation_attrs$measures <<- c()
                   i <<- 10 #Measures begin at column 10
                   columnNamesMeasures <<- c("Measures")
                   settingsData <- read.csv(file.path(emulation_attrs$user_dir,"analysis_settings.csv"), stringsAsFactors = FALSE)
                   #print(settingsData)
                   parameterList <<- settingsData$Parameter
                   emulation_attrs$sampleMins <- settingsData$Min
                   emulation_attrs$sampleMaxes <- settingsData$Max
                   while (!is.null(settingsData[1, i])) #Keep going along the columns until there are no more measures
                   {
                     emulation_attrs$measures <<- c(emulation_attrs$measures, gsub(" ", "",settingsData[1,i]))#Add one instance of the measure name 
                     i <- i+1 
                     columnNamesMeasures <<- c(columnNamesMeasures, paste0("Measure ", i-10))
                   }
                   myValues$table <- rbind(isolate(myValues$table), cbind(parameterList,emulation_attrs$sampleMins,emulation_attrs$sampleMaxes))
                   measureValues$table <- matrix(c("Measures:", emulation_attrs$measures), nrow = 1, byrow = TRUE)
                   colnames(measureValues$table) <- columnNamesMeasures
                   #print(emulation_attrs$measures)
                   output$measures_table <- renderTable(measureValues$table, striped = TRUE, bordered = TRUE)
                   shinyjs::show("dataInput")
  }})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

