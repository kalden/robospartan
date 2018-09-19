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
#library(shinycssloaders)



LHCFilePathFull <<- "/home/fgch500/robospartan/LHCFiles"
robustFilePathFull <<- "/home/fgch500/robospartan/robustnessFiles"
eFASTFilePath <<- "/home/fgch500/robospartan/eFASTfiles/eFAST_Sample_Outputs"
filepath <<- ""
lhcAllResults <<- "LHC_AllResults.csv"
lhcParams <<- "Tutorial_Parameters_for_Runs.csv"
lhcSummaryFull <<- "/home/fgch500/robospartan/LHCFiles/LHC_Summary.csv"
lhcSummary <<- "LHC_Summary.csv"
parameters <<- c("stableBindProbability","chemokineExpressionThreshold","initialChemokineExpressionValue",
                 "maxChemokineExpressionValue","maxProbabilityOfAdhesion","adhesionFactorExpressionSlope")
measures <<- c("Velocity", "Displacement")
measure_scale <<- c("microns", "microns/min")
corCoeffOutput <<-"LHC_corCoeffs"
baseline <<- c(50,0.3, 0.2, 0.04, 0.60, 1.0)
minvals <<- c(10, 0.10, 0.10, 0.015, 0.1, 0.25)
maxvals <<- c(100, 0.9, 0.50, 0.08, 0.95, 5.0)
incvals <<- c(10, 0.1, 0.05, 0.005, 0.05, 0.25)
AtestResultsLocation <<- paste0(robustFilePathFull,"/ATest_Results.csv")
ATestSignalLevel <<- 0.23
num_curves=3
num_samples=65
# Define UI for application that draws a histogram
ui <- fluidPage(
   useShinyjs(),
   
   # Application title
   h3("RoboSpartan Analysis Platform"),
   
   #This ensures the graphs image size dynamically changes with the window
   tags$head(tags$style(
     type = "text/css",
     "#Graph img {max-width: 100%; width: 100%; height:auto}"
   )),
   
   tags$head(tags$style(
     type = "text/css",
     "#Graph2 img {max-width: 100%; width: 100%; height:auto}"
   )),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         wellPanel(
           h4("Choose Analysis Type:"),
           selectInput(inputId = "usersAnalysisType", label = "Analysis Type Selection", choices = c("Latin-Hypercube", "Robustness", "eFAST"))
         ),
         wellPanel(
           h4("File Declarations:"),

            fileInput(inputId = "AllResults",
                      label = "All Simulation Results File"),
           
            fileInput(inputId = "ParameterFile",
                      label = "Simulations Parameter File:")
           
         ),
         
         wellPanel(
           h4("Variable names:")
           #GIVE USER THE ABILITY TO TYPE THEIR VARIABLE NAMES IN 
         ),
         
         wellPanel(
           h4(uiOutput("firstChoice")),
           
           #PROBABLY PUT A DROP DOWN FOR EACH ANALYSIS TYPE HERE
           actionButton(inputId = "LHSSummary",
                        label = "Go"),
           hr(),
         
           h4(uiOutput("textChange")),
           
           actionButton(inputId = "LHSGenerate",
                        label = "Go"),

           hr(),
           
           h4("Generate Graphs"),
           
           actionButton(inputId = "GraphButton",
                        label = "Generate"),
           
           actionButton(inputId = "showGraphs",
                        label = "Show Graphs")

         ),
        
      width = 5),
      
      # Show a plot of the generated distribution
      mainPanel(
        selectInput(inputId = "usersAnalysisInput", label = "Graph Analysis Selection", choices = c(parameters, "polarPlot")),
            tabsetPanel(id = "changingTabs", type = "tabs",
                    tabPanel(title = uiOutput("choicePanel"),
                             imageOutput(outputId = "Graph")),
                            
                    tabPanel(title = uiOutput("choicePanel2"),
                             imageOutput(outputId = "Graph2")),
                    
                    tabPanel(title = uiOutput("choicePanel3")),
                            imageOutput(outputId ="Graph3")
                            
        ),
         
         
      width = 7)
   ) 
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)
  observeEvent(input$LHSSummary,
                if (input$usersAnalysisType == "Latin-Hypercube")
                  {showModal(modalDialog(
                    title = "Creating Summary",
                    "Summary files are being created..."))
                    summary <- lhc_generateLHCSummary(filepath, parameters, measures,input$AllResults$datapath, lhcSummaryFull, input$ParameterFile$datapath) 
                    showModal(modalDialog(
                      title = "Complete",
                      "Summary files have been created"))}
                 else if (input$usersAnalysisType == "Robustness"){
                    print(input$AllResults$datapath)
                    results <- "Robustness_Data.csv" #THIS IS ONLY HERE UNTIL THE CHECKING ERROR IS FIXED, ONCE FIXED FILE PATH MUST BE CHANGED TO NULL
                    showModal(modalDialog(
                      title = "Creating Results",
                      "ATest result files are being created..."
                    ))
                    oat_csv_result_file_analysis(robustFilePathFull, "Robustness_Data.csv" , parameters, baseline, measures, "ATest_Results.csv", minvals, maxvals, incvals, PARAMVALS=NULL)
                    showModal(modalDialog(
                      title = "Complete",
                      "ATest result files have been created"
                    ))
                 }
               else if (input$usersAnalysisType == "eFAST"){
                   showModal(modalDialog(
                     title = "Creating Results",
                     "Overall medians result file are being created..."
                   ))
                   efast_get_overall_medians(eFASTFilePath, num_curves, parameters, num_samples, measures )
                   showModal(modalDialog(
                     title = "Complete",
                     "Overall medians result file have been created"
                   ))
               }
                  )
  
  observeEvent(input$LHSGenerate,
               {
                 lhc_generatePRCoEffs(LHCFilePathFull, parameters, measures, lhcSummary, corCoeffOutput)  
                 showModal(modalDialog(
                   title = "Complete",
                   "Coefficients files have been created"))
               })
  
  observeEvent(input$usersAnalysisType,
               if(input$usersAnalysisType == "Robustness")
               {
                 shinyjs::hide("ParameterFile")
                 shinyjs::show("AllResults")
                 shinyjs::hide("LHSGenerate")
                 output$textChange <- renderText("")
               }
               else  if(input$usersAnalysisType == "eFAST")
               {
                 shinyjs::hide("ParameterFile")
                 shinyjs::hide("AllResults")
                 shinyjs::hide("LHSGenerate")
                 output$textChange <- renderText("")
               }
               else if (input$usersAnalysisType == "Latin-Hypercube")
               {
                 shinyjs::show("ParameterFile")
                 shinyjs::show("LHSGenerate")
                 shinyjs::show("AllResults")
                 output$textChange <- renderText("Generate Coefficients")
               }
               )
  
  observeEvent(input$GraphButton,
               {
                 if(input$usersAnalysisType == "Latin-Hypercube")
                 {
                   showModal(modalDialog(
                     title = "Generating Graphs",
                     "Graphs are being generated..."))
                   lhc_graphMeasuresForParameterChange(LHCFilePathFull, parameters, measures, measure_scale, corCoeffOutput, lhcSummary, OUTPUT_TYPE = "PNG")
                   lhc_polarplot(LHCFilePathFull, parameters, measures, corCoeffOutput) #REMEBER TO ADD THIS 
                   showModal(modalDialog(
                     title = "Complete",
                     "Graphs have been generated"))
                 }
                 else if (input$usersAnalysisType == "Robustness")
                 {
                   showModal(modalDialog(
                     title = "Generating Graphs",
                     "Graphs are being generated..."))
                   oat_graphATestsForSampleSize(robustFilePathFull, parameters, measures, ATestSignalLevel, "ATest_Results.csv", baseline, minvals, maxvals, incvals, PARAMVALS=NULL)
                   oat_plotResultDistribution(robustFilePathFull, parameters, measures, measure_scale, "Robustness_Data.csv", baseline, minvals, maxvals, incvals, PARAMVALS=NULL) 
                   showModal(modalDialog(
                     title = "Complete",
                     "Graphs have been generated"))
                   }
                else if (input$usersAnalysisType == "eFAST")
                {
                  showModal(modalDialog(
                    title = "Generating Graphs",
                    "Graphs are being generated..."))
                  efast_run_Analysis(eFASTFilePath, measures, parameters, num_curves, num_samples, 1:length(measures), TTEST_CONF_INT=0.95, GRAPH_FLAG=TRUE, "EFAST_Results.csv")  
                  showModal(modalDialog(
                    title = "Complete",
                    "Graphs have been generated"))
                  }
               })
  output$firstChoice <- renderText({
                        switch(input$usersAnalysisType, "Latin-Hypercube" = "Generate Summary",
                                                        "Robustness" = "Generate ATest Results",
                                                        "eFAST" = "Get Overall Medians")
    })

  
  #These switch function change the two tabs that will be shown to the user so graphs can be displayed neatly
  output$choicePanel <- renderText({
                        switch(input$usersAnalysisInput, "adhesionFactorExpressionSlope"  = paste0(input$usersAnalysisInput, "_Displacement"),
                                                         "maxProbabilityOfAdhesion" = paste0(input$usersAnalysisInput, "_Displacement"),
                                                         "stableBindProbability" = paste0(input$usersAnalysisInput, "_Displacement"),
                                                         "maxChemokineExpressionValue" = paste0(input$usersAnalysisInput, "_Displacement"),
                                                         "initialChemokineExpressionValue" = paste0(input$usersAnalysisInput, "_Displacement"),
                                                         "chemokineExpressionThreshold" = paste0(input$usersAnalysisInput, "_Displacement"),
                                                         "polarPlot" = paste0(input$usersAnalysisInput, "_Displacement"))

  })


  output$choicePanel2 <- renderText({
                        switch(input$usersAnalysisInput, "adhesionFactorExpressionSlope"  = paste0(input$usersAnalysisInput, "_Velocity"),
                                                         "maxProbabilityOfAdhesion" = paste0(input$usersAnalysisInput, "_Velocity"),
                                                         "stableBindProbability" = paste0(input$usersAnalysisInput, "_Velocity"),
                                                         "maxChemokineExpressionValue" = paste0(input$usersAnalysisInput, "_Velocity"),
                                                         "initialChemokineExpressionValue" = paste0(input$usersAnalysisInput, "_Velocity"),
                                                         "chemokineExpressionThreshold" = paste0(input$usersAnalysisInput, "_Velocity"),
                                                         "polarPlot" = paste0(input$usersAnalysisInput, "_Velocity"))
    
  })
  
  output$choicePanel3 <- renderText({
                            if (input$usersAnalysisType == "Robustness")
                              switch(input$usersAnalysisInput, "adhesionFactorExpressionSlope"  = paste0(input$usersAnalysisInput, "_ATestResults"),
                                     "maxProbabilityOfAdhesion" = paste0(input$usersAnalysisInput, "_ATestResults"),
                                     "stableBindProbability" = paste0(input$usersAnalysisInput, "_ATestResults"),
                                     "maxChemokineExpressionValue" = paste0(input$usersAnalysisInput, "_ATestResults"),
                                     "initialChemokineExpressionValue" = paste0(input$usersAnalysisInput, "_ATestResults"),
                                     "chemokineExpressionThreshold" = paste0(input$usersAnalysisInput, "_ATestResults"))
                            else NULL
})
 
                     
                          
                         
  
    

  
observeEvent(input$showGraphs, 
             {
                output$Graph <- renderImage({
                                  if (input$usersAnalysisType == "Latin-Hypercube"){
                                    switch(input$usersAnalysisInput, "adhesionFactorExpressionSlope"  =   list(src = "/home/fgch500/robospartan/LHCFiles/adhesionFactorExpressionSlope_Displacement.png",
                                                                                                               alt = paste("Image has not loaded")),
                                                                     "maxProbabilityOfAdhesion" =   list(src = "/home/fgch500/robospartan/LHCFiles/maxProbabilityOfAdhesion_Displacement.png",
                                                                                                         alt = paste("Image has not loaded")),
                                                                     "stableBindProbability" =   list(src = "/home/fgch500/robospartan/LHCFiles/stableBindProbability_Displacement.png",
                                                                                                      alt = paste("Image has not loaded")),
                                                                     "maxChemokineExpressionValue" =   list(src = "/home/fgch500/robospartan/LHCFiles/maxChemokineExpressionValue_Displacement.png",
                                                                                                         alt = paste("Image has not loaded")),
                                                                     "initialChemokineExpressionValue" =   list(src = "/home/fgch500/robospartan/LHCFiles/initialChemokineExpressionValue_Displacement.png",
                                                                                                              alt = paste("Image has not loaded")),
                                                                     "chemokineExpressionThreshold" = list(src = "/home/fgch500/robospartan/LHCFiles/chemokineExpressionThreshold_Displacement.png",
                                                                                                            alt = paste("Image has not loaded")),
                                                                     "polarPlot" = list(src = "/home/fgch500/robospartan/LHCFiles/polarPlot_Displacement.png",
                                                                                        alt = paste("Image has not loaded")))
                                  }
                                  else if (input$usersAnalysisType == "Robustness"){
                                    print("fsdfhsdf")
                                  }          
                 
                 }, deleteFile = FALSE)
                
                output$Graph2 <- renderImage({
                                  switch(input$usersAnalysisInput, "adhesionFactorExpressionSlope"  =  list(src = "/home/fgch500/robospartan/LHCFiles/adhesionFactorExpressionSlope_Velocity.png",
                                                                                                            alt = paste("Image has not loaded")),
                                                                   "maxProbabilityOfAdhesion" = list(src = "/home/fgch500/robospartan/LHCFiles/maxProbabilityOfAdhesion_Velocity.png",
                                                                                                     alt = paste("Image has not loaded")),
                                                                   "stableBindProbability" =  list(src = "/home/fgch500/robospartan/LHCFiles/stableBindProbability_Velocity.png",
                                                                                                   alt = paste("Image has not loaded")),
                                                                   "maxChemokineExpressionValue" =  list(src = "/home/fgch500/robospartan/LHCFiles/maxChemokineExpressionValue_Velocity.png",
                                                                                                         alt = paste("Image has not loaded")),
                                                                   "initialChemokineExpressionValue" = list(src = "/home/fgch500/robospartan/LHCFiles/initialChemokineExpressionValue_Velocity.png",
                                                                                                            alt = paste("Image has not loaded")),
                                                                   "chemokineExpressionThreshold" = list(src = "/home/fgch500/robospartan/LHCFiles/chemokineExpressionThreshold_Velocity.png",
                                                                                                         alt = paste("Image has not loaded")),
                                                                   "polarPlot" = list(src = "/home/fgch500/robospartan/LHCFiles/polarPlot_Velocity.png",
                                                                                      alt = paste("Image has not loaded"))
                         
                  )
                  
                }, deleteFile = FALSE)
               
             })

}

# Run the application 
shinyApp(ui = ui, server = server)

