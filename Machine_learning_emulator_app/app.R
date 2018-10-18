
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(spartan)
library(shinyjs)
library(ggplot2)

modelList <- c()
data("sim_data_for_emulation")

# Output measures
measures <<-c()
#measures<-c("Velocity","Displacement","PatchArea")
columnNames <<- c("Measures")
# Mins and max values used in sampling
#sampleMaxes <- cbind(100,0.9,0.5,0.08,1,5)
#sampleMins <-cbind(0,0.1,0.1,0.015,0.1,0.25)
sampleMaxes <- c()
sampleMins <-c()
#parameters<-c("stableBindProbability","chemokineExpressionThreshold","initialChemokineExpressionValue",
#              "maxChemokineExpressionValue","maxProbabilityOfAdhesion","adhesionFactorExpressionSlope")
# Set the algorithm settings. Let's say in this case we're generating a neural 
# network, so we need to feed in some potential structures to examine
networkStructures<-list(c(4),c(3),c(4,3),c(4,4,3),c(4,4),c(4,3,3),c(4,4,4,3),c(4,3,2))
#networkStructures<-list()
PartData <- FALSE
algsDone <- FALSE
ensembleDone <- FALSE
netStruct <- c()
userNetStruct <- c()
structDone <- FALSE
count <- 1
partitionedData <- NULL
partition_data<- NULL

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Machine Learning Emulator Generation App"),
  useShinyjs(),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Settings:"),
        
        fileInput(inputId = "settingsFile",
                  label = "Import Settings File")
        
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
                actionButton(inputId = "clearNetStruct", label = "Clear Network Structure"),
                p(),
                tags$style("#netStructComp {border: 3px solid #0000FF; font-weight: bold;}"),
                actionButton(inputId = "netStructComp", label = "Network Structure Completed")),
      
      wellPanel(id = "models",
                h4("Emulator Models Selection:"),
                
                actionButton(inputId = "SVM", label = "SVM", width = '30%'),
                actionButton(inputId = "RF", label = "RF", width = '30%'),
                actionButton(inputId = "GLM", label = "GLM", width = '30%'),
                actionButton(inputId = "NNET", label = "NNET", width = '30%'),
                actionButton(inputId = "GP", label = "GP", width = '30%'),
                tags$style("#clearModels {border: 2px solid #dd4b39; text-align:center;;}"),
                actionButton(inputId = "clearModels", label = "Clear", width = '30%')
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
      
      wellPanel(id = "partitionDataset",
                h4("Partition the Dataset:"),
                actionButton(inputId = "partitionDataset", label = "Partition")),
      
      wellPanel(id = "algorithmSettings",
                h4("Make the Algorithm Settings:"),
                actionButton(inputId = "algSet", label = "Algorithm Settings")),
      
      wellPanel(id = "makeEnsemble",
                h4("Generate the Emulators and Ensembles:"),
                actionButton(inputId = "ensembles", label = "Ensembles")),
      width = 5),
    
    # Show a plot of the generated distribution
    mainPanel(
      div(tableOutput("parameter_table"),style="font-size:90%"),
      div(tableOutput("measures_table"),style = "font-size:90%"),
      selectInput(inputId = "whichData",
                  label = NULL,
                  choices = c("Partitioned Data", "Algorithm Settings", "Generated Ensembles and Emulators")),
      #div(tableOutput( uiOutput("choiceTable")),style="font-size:90%"),
      tabsetPanel(id = "PartitionedData_tabs", type = "tabs",
                  tabPanel(title = "Training"),
                  tabPanel(title = "Testing"),
                  tabPanel(title = "Validation"),
                  tabPanel(title = "Pre Normed Values")
                  ),
      tabsetPanel(id = "AlgorithmSettings_tabs", type = "tabs",
                  tabPanel(title = "Settings"),
                  tabPanel(title= "Network Structures")),
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
                actionButton(inputId = "showGraph", label = "Show Graph")
      ),
      div(tableOutput("PartitionedData_table"),style="font-size:90%"),
      div(tableOutput("NetStructs_table"), style = "font-size:90%"),
      div(tableOutput("AlgorithmSettings_table"),style="font-size:90%"),
      div(tableOutput("Emulations_table"),style="font-size:90%"),
      imageOutput(outputId = "Graph"),
      width = 7)
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  measureValues <- reactiveValues()
  myValues <- reactiveValues() 
  PartitionedData_table <-reactiveValues()
  AlgorithmSettings_table <-reactiveValues()
  shinyjs::disable("ensembles")
  shinyjs::disable("algSet")
  shinyjs::hide("whichData")
  shinyjs::hide("PartitionedData_table")
  shinyjs::hide("AlgorithmSettings_table")
  shinyjs::hide("NetStructs_table")
  shinyjs::hide("AlgorithmSettings_tabs")
  shinyjs::hide("PartitionedData_tabs")
  shinyjs::hideElement("graphSettingsWell")
  shinyjs::disable("partitionDataset")
  
  observeEvent(input$clearModels,
               {
                 shinyjs::enable("SVM")
                 shinyjs::enable("RF")
                 shinyjs::enable("GLM")
                 shinyjs::enable("NNET")
                 shinyjs::enable("GP")
                 modelList <<- c()
                 print(modelList)
               }
  )  
  observeEvent(input$SVM,
               {
                 modelList <<- c(modelList, "SVM")
                 print(modelList)
                 shinyjs::disable("SVM")
               }
  )
  
  observeEvent(input$RF,
               {
                 modelList <<- c(modelList, "RF")
                 print(modelList)
                 shinyjs::disable("RF")
               }
  )
  
  observeEvent(input$NNET,
               {
                 modelList <<- c(modelList, "NNET")
                 print(modelList)
                 shinyjs::disable("NNET")
               }
  )
  
  observeEvent(input$GLM,
               {
                 modelList <<- c(modelList, "GLM")
                 print(modelList)
                 shinyjs::disable("GLM")
               }
  )
  
  observeEvent(input$GP, 
               {
                 modelList <<- c(modelList, "GP")
                 print(modelList)
                 shinyjs::disable("GP")
               }
  )
  
  observeEvent(input$netStructComp,
               {
                 shinyjs::disable("netStructreComp")
                 shinyjs::disable("addNetStruct")
                 shinyjs::disable("netStructText")
                 structDone <<-TRUE
               }
               )
  
  observeEvent(input$addNetStruct,
               {
                 shinyjs::show("NetStructs_table")
                 networkStructures[[count]]<<-c(input$netStructText)
                 print(networkStructures)
                 updateTextInput(session, inputId = "netStructText", value = "")
                 userNetStruct <-cbind(userNetStruct, networkStructures)
                 output$NetStructs_table <- renderTable(userNetStruct, striped = TRUE, bordered = TRUE)
                 count <<- count+1
               })
  
  observeEvent(input$clearNetStruct,
               {
                 networkStructures<<-list()
                 count <<- 1
               })
  
  observeEvent(input$partitionDataset,
               {
                 shinyjs::hide("NetStructs_table")
                 newData <- input$simData$datapath
                 print(newData)
                 #partitionedData <<- partition_dataset(sim_data_for_emulation, parameterList, percent_train = input$percentTrain, percent_test = input$percentTest, percent_validation = input$percentValidation, normalise = TRUE, sample_mins = sampleMins, sample_maxes = sampleMaxes)
                 #partitionedData <<- partition_dataset(newData, parameterList, percent_train = input$percentTrain, percent_test = input$percentTest, percent_validation = input$percentValidation, normalise = TRUE, sample_mins = sampleMins, sample_maxes = sampleMaxes)
                 print(sampleMins)
                 print(sampleMaxes)
                 partitionedData <<- partition_dataset("/home/fgch500/robospartan/argosFiles/LHCStuff/LHCcombinedParamsAndResults.csv", parameterList, percent_train = 75, percent_test = 15, percent_validation = 10, normalise = TRUE, sample_mins = sampleMins, sample_maxes = sampleMaxes)
                 print(partitionedData)  
                 showModal(modalDialog(
                   title = "Complete",
                   "Data has been successfully partitioned"))
                 PartData <<- TRUE
                 #partition_data <<- get(load("/home/fgch500/robospartan/Machine_learning_emulator_app/partitioned_data.Rda"))
                 preNorms <<- cbind(partitionedData$pre_normed_mins, partitionedData$pre_normed_maxes)
                 colnames(preNorms) <<- c("Pre Normed Mins", "Pre Normed Maxs")
                 updateSelectInput(session, inputId = "whichData", selected = "Partitioned Data")
                 updateTabsetPanel(session, inputId = "PartitionedData_tabs", selected = "Training")
                 shinyjs::show("PartitionedData_table")
                 shinyjs::hide("whichData")
                 shinyjs::show("PartitionedData_tabs")
                 shinyjs::enable("algSet")
               }
               
  )
  
  
  observeEvent(input$PartitionedData_tabs,
               {
                 
                 shinyjs::hide("AlgorithmSettings_table")
                 switch(input$PartitionedData_tabs, "Training" =  output$PartitionedData_table <- renderTable(partitionedData$training, striped = TRUE, bordered = TRUE, rownames = TRUE),
                                                    "Testing" =  output$PartitionedData_table <- renderTable(partitionedData$testing, striped = TRUE, bordered = TRUE, rownames = TRUE),
                                                    "Validation" = output$PartitionedData_table <- renderTable(partitionedData$validation, striped = TRUE, bordered = TRUE, rownames = TRUE),
                                                    "Pre Normed Values" = output$PartitionedData_table <- renderTable(preNorms, striped = TRUE, bordered = TRUE, rownames = TRUE, colnames = TRUE))
                
               })
  
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
                 
                 print(paste0(input$graphSelect, "_", testOrTrainVal, "_", input$measureSelect, ".png"))
                 
                 output$Graph <-  renderImage(list(src = paste0(input$graphSelect, "_", testOrTrainVal, "_", input$measureSelect, ".png") , width = '100%', alt = paste("Image not found")), deleteFile = FALSE)
               })
  
  observeEvent(input$AlgorithmSettings_tabs,
               {
                 if (algsDone == TRUE)
                 {
                   shinyjs::hide("PartitionedData_table")
                   print(str(networkStructures))
                   settings <- cbind(algorithmSettings$num_trees, algorithmSettings$num_of_generations, algorithmSettings$num_of_folds, algorithmSettings$save_emulators, algorithmSettings$save_ensemble, algorithmSettings$plot_test_accuracy)
                   colnames(settings)<-c("Number of Trees", "Number of Generations", "Number of Folds", "Save Emulators (1-True, 0-False)","Save Ensemble (1-True, 0-False)", "Plot Test Accuracy (1-True, 0-False)")
                   if (structDone == TRUE)
                   {
                     for (networkNumber in 1:length(networkStructures))
                     {
                       indivStructure <- paste(unlist(networkStructures[[networkNumber]]), collapse = ', ') #Convert the list element into a string with comma seperation between the values
                       netStruct <-rbind(netStruct, cbind(networkNumber, indivStructure))
                     }
                     colnames(netStruct) <- c("Network", "Structure")
                   }
                   
                   switch(input$AlgorithmSettings_tabs, "Network Structures" =  output$AlgorithmSettings_table <- renderTable(netStruct, striped = TRUE, bordered = TRUE),
                          "Settings" = output$AlgorithmSettings_table <- renderTable(settings, striped = TRUE, bordered = TRUE))
                   #shinyjs::show("AlgorithmSettings_table")
                 }
                 

               })

  
  observeEvent(input$algSet,
               {
                 if (structDone == TRUE)
                 {
                   algorithmSettings<<-emulation_algorithm_settings(network_structures=networkStructures)
                   print(algorithmSettings)
                   shinyjs::enable("ensembles")
                   showModal(modalDialog(
                     title = "Complete",
                     "Algorithm settings have been successfully created"))
                   updateSelectInput(session, inputId = "whichData", selected = "Algorithm Settings")
                   algsDone <<- TRUE
                   shinyjs::show("whichData")
                 }
                 else
                 {
                   showModal(modalDialog(
                     title = "First Complete Network Structure",
                     "Network Structure needs completing before algorithm settings can be produced"))
                 }
               }
  )
  
  observeEvent(input$ensembles,
               {
                 print(length(modelList))
                 if (length(modelList) > 1){
                   showModal(modalDialog(
                     title = "Generating Emulators and Ensembles",
                     "Emulators and Ensembles are being generated..."))
                   print(measures)
                   generated_ensemble<<-generate_emulators_and_ensemble(modelList, parameterList, measures, partitionedData, algorithm_settings = algorithmSettings, normalised=TRUE)
                   print(generated_ensemble)
                   updateSelectInput(session, inputId = "graphSelect", choices = c("Ensemble", modelList))
                   updateSelectInput(session, inputId = "whichData", selected ="Generated Ensembles and Emulators")
                   #load(file = "built_ensemble.Rda")
                   ensembleDone <<- TRUE
                   showModal(modalDialog(
                     title = "Complete",
                     "Emulators and Ensembles have been generated"))
                 }
                 else
                 {
                   showModal(modalDialog(
                     title = "Add More Models",
                     "To generate an ensemble you need at least two types of emulator models"))
                 }
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
                 print(input$whichData)
                 if(input$whichData == "Partitioned Data")
                 {
                   if (PartData == TRUE)
                   {
                     shinyjs::show("PartitionedData_table")
                     shinyjs::hide("AlgorithmSettings_table")
                     shinyjs::hide("AlgorithmSettings_tabs")
                     shinyjs::show("PartitionedData_tabs")
                     shinyjs::hideElement("graphSettingsWell")
                     shinyjs::show("Graph")
                     updateTabsetPanel(session, inputId = "PartitionedData_tabs", selected = "Training")
                   }
                 }
                 else if(input$whichData == "Algorithm Settings")
                 {
                   if (algsDone == TRUE)
                   {
                     shinyjs::show("AlgorithmSettings_table")
                     shinyjs::hide("PartitionedData_table")
                     shinyjs::show("AlgorithmSettings_tabs")
                     shinyjs::hide("PartitionedData_tabs")
                     shinyjs::hideElement("graphSettingsWell")
                     shinyjs::hide("Graph")
                     updateTabsetPanel(session, inputId = "AlgorithmSettings_tabs", selected = "Settings")
                   }
                 }
                 else{
                    shinyjs::hide("AlgorithmSettings_table")
                    shinyjs::hide("PartitionedData_table")
                    shinyjs::hide("AlgorithmSettings_tabs")
                    shinyjs::hide("PartitionedData_tabs")
                    if(ensembleDone == TRUE)
                    {
                      shinyjs::show("Graph")
                      shinyjs::showElement("graphSettingsWell")
                    }
                    else
                    {
                      showModal(modalDialog(
                        title = "Generate Emulators and Ensembles",
                        "Emulators and Ensembles must first be generated"))
                      updateSelectInput(session, inputId = "whichData", selected = "Algorithm Settings")
                    }
                  }
                    
                 }
               )
  
  observeEvent(input$settingsFile,
               {
                 if (!is.null(input$settingsFile)) #Ensure a settings file has been chosen 
                 {
                   #These first two lines ensures that the tables are reset, so if the user changing their settings file, the one and new values wont bind together. Instead only the new values will be shown.
                   myValues$table <- NULL
                   measureValues$table <- NULL
                   i <<- 7 #Measures begin at column 7
                   columnNamesMeasures <<- c("Measures")
                   settingsData <- read.csv(input$settingsFile$datapath, stringsAsFactors = FALSE)
                   print(settingsData)
                   parameterList <<- settingsData$Parameter
                   sampleMins <<- settingsData$Min
                   sampleMaxes <<- settingsData$Max
                   while (!is.null(settingsData[1, i])) #Keep going along the columns until there are no more measures
                   {
                     measures <<- c(measures, gsub(" ", "",settingsData[1,i]))#Add one instance of the measure name 
                     i <- i+1 
                     columnNamesMeasures <<- c(columnNamesMeasures, paste0("Measure ", i-7))
                   }
                   myValues$table <- rbind(isolate(myValues$table), cbind(parameterList,sampleMins,sampleMaxes))
                   measureValues$table <- matrix(c("Measures:", measures), nrow = 1, byrow = TRUE)
                   colnames(measureValues$table) <- columnNamesMeasures
                   print(measures)
                   output$measures_table <- renderTable(measureValues$table, striped = TRUE, bordered = TRUE)
                   updateSelectInput(session, inputId = "measureSelect", choices = measures)
                   shinyjs::enable("partitionDataset")
  }})
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

