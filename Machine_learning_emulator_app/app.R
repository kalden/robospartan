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
#sampleMins <-cbind(0,0.1,0.1,0.015,0.1,0.25)#
sampleMaxes <- c()
sampleMins <-c()
#parameters<-c("stableBindProbability","chemokineExpressionThreshold","initialChemokineExpressionValue",
#              "maxChemokineExpressionValue","maxProbabilityOfAdhesion","adhesionFactorExpressionSlope")
# Set the algorithm settings. Let's say in this case we're generating a neural 
# network, so we need to feed in some potential structures to examine
networkStructures<-list(c(4),c(3),c(4,3),c(4,4,3),c(4,4),c(4,3,3),c(4,4,4,3),c(4,3,2))

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
         wellPanel(id = "models",
                   h4("Models Selection:"),
                   
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
                  h4("Make the Ensembles:"),
                  actionButton(inputId = "ensembles", label = "Ensembles")),
      width = 5),
      
      # Show a plot of the generated distribution
      mainPanel(
        div(tableOutput("measures_table"),style = "font-size:90%"),
      width = 7)
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   measureValues <- reactiveValues()
   shinyjs::disable("ensembles")
   
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
   
   observeEvent(input$partitionDataset,
                {
                  partitionedData <<- partition_dataset(sim_data_for_emulation, parameterList, percent_train = input$percentTrain, percent_test = input$percentTest, percent_validation = input$percentValidation, normalise = TRUE, sample_mins = sampleMins, sample_maxes = sampleMaxes)
                  
                  #partitionedData <<- partition_dataset("/home/fgch500/robospartan/LHCFiles/LHC_AllResults.csv", parameters, percent_train = 75, percent_test = 15, percent_validation = 10, normalise = TRUE, sample_mins = sampleMins, sample_maxes = sampleMaxes)
                  print(partitionedData)  
                  showModal(modalDialog(
                    title = "Complete",
                    "Data has been successfully partitioned"))
                }
   )
   
   
   observeEvent(input$algSet,
                {
                  algorithmSettings<<-emulation_algorithm_settings(network_structures=networkStructures)
                  print(algorithmSettings)
                  shinyjs::enable("ensembles")
                  showModal(modalDialog(
                    title = "Complete",
                    "Algorithm settings have been successfully created"))
                }
   )
   
   observeEvent(input$ensembles,
                {
                  showModal(modalDialog(
                    title = "Generating Emulators and Ensembles",
                    "Emulators and Ensembles are being generated..."))
                  generated_ensemble<-generate_emulators_and_ensemble(modelList, parameterList, measures, partitionedData, algorithm_settings = algorithmSettings, normalised=TRUE)
                  print(generated_ensemble)
                  showModal(modalDialog(
                    title = "Complete",
                    "Emulators and Ensembles have been generated"))
                })
   
   observeEvent(input$settingsFile,
                {
                  if (!is.null(input$settingsFile)) #Ensure a settings file has been chosen 
                  {
                    settingsData <- read.csv(input$settingsFile$datapath, stringsAsFactors = FALSE)
                    parameterList <<- settingsData$Parameter
                    sampleMins <<- settingsData$Min
                    sampleMaxes <<- settingsData$Max
                    while (!is.null(settingsData[1, i])) #Keep going along the columns until there are no more measures
                    {
                      measures <<- c(measures, settingsData[1, i]) #Add one instance of the measure name 
                      i <- i+1 
                    }
                    measureValues$table <- matrix(measures, nrow = 1, byrow = TRUE)
                    rownames(measureValues$table) <- "MEASURES"
                    
                    output$measures_table <- renderTable(measureValues$table, striped = TRUE, bordered = TRUE, colnames = FALSE, rownames = TRUE)
                  }
                })
   
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)

