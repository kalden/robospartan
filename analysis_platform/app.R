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


#LHCFilePathFull <<- "/home/fgch500/robospartan/LHCFiles"
#robustFilePathFull <<- "/home/fgch500/robospartan/robustnessFiles"
#eFASTFilePath <<- "/home/fgch500/robospartan/eFASTfiles/eFAST_Sample_Outputs"
LHCFilePathFull <<- ""
robustFilePathFull <<- ""
eFASTFilePath <<- ""
filepath <<- ""
lhcAllResults <<- "LHC_AllResults.csv"
lhcParams <<- "Tutorial_Parameters_for_Runs.csv"
lhcSummaryFull <<- "/home/fgch500/robospartan/LHCFiles/LHC_Summary.csv"
lhcSummary <<- "LHC_Summary.csv"
#parameters <<- c("stableBindProbability","chemokineExpressionThreshold","initialChemokineExpressionValue",
                # "maxChemokineExpressionValue","maxProbabilityOfAdhesion","adhesionFactorExpressionSlope")
parameters <<- c()
#measures <<- c("Velocity", "Displacement")
measures <<- c()
#measure_scale <<- c("microns", "microns/min")
measureScale <<- c()
#corCoeffOutput <<-"LHC_corCoeffs"
# baseline <<- c(50,0.3, 0.2, 0.04, 0.60, 1.0)
# minvals <<- c(10, 0.10, 0.10, 0.015, 0.1, 0.25)
# maxvals <<- c(100, 0.9, 0.50, 0.08, 0.95, 5.0)
# incvals <<- c(10, 0.1, 0.05, 0.005, 0.05, 0.25)
baseline <<- c()
minvals <<- c()
maxvals <<- c()
incvals <<- c()
fileName <<- FALSE
columnNames <<- c("Measures")
columnNamesScale <<- c("Measure Scales")
AtestResultsLocation <<- paste0(robustFilePathFull,"/ATest_Results.csv")
parameterList <<-c()

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
          h4("Settings:"),
    
          fileInput(inputId = "settingsFile",
                       label = "Import Settings File")
          
        ),

         wellPanel(id = "analysisWell",
           h4("Choose Analysis Type:"),
           selectInput(inputId = "usersAnalysisType", label = "Analysis Type Selection", choices = c("", "Latin-Hypercube", "Robustness", "eFAST"), selected = NULL)
         ), 
        
        wellPanel(id = "filePath",
                  h4("File Path:"),
                  textInput(inputId = "filePaths", label = uiOutput("changableFile"), value = "" ),
                  tags$style("#filePathFalse {border: 4px solid #dd4b39; float: right;  text-align: center; font-weight: bold;}"),
                
                  textInput(inputId = "filePathFalse", label =NULL, value = "False File Path", width = '100%'),
                  
                  tags$style("#filePathTrue {border: 4px solid #008000; float: right;  text-align:center; font-weight: bold;}"),
                
                  textInput(inputId = "filePathTrue", label =NULL, value = "True File Path", width = '100%')),
        
        
        wellPanel(id = "measuresWell",
                  h4("Measure Scales:"),
                  textInput(inputId = "measureScale",
                            label = "Choose Your Measure Scales:",
                            value = ""),
                  actionButton(inputId = "addMeasureScale", label = "Add to Measure scales"),
                  actionButton(inputId = "clearMeasureScales", label = "Clear All Measure Scales")
                  
        ),
        wellPanel(id = "fileNamesForFunc",
                  textInput(inputId = "ATestFileName", label = "Type a Desired Name for A-Test Results (NO EXTENSION)", value = "ATest_Results", width = '100%', placeholder = ".csv"),
                  textInput(inputId = "eFASTResultsFileName", label = "Type a Desired Name for eFAST Results (NO EXTENSION)", value = "EFAST_Results", width = '100%', placeholder = ".csv"),
                  textInput(inputId = "corCoeffsFileName", label = "Type a Desired Name for Coefficients Results (NO EXTENSION)", value = "LHC_corCoeffs", width = '100%', placeholder = ".csv")),
      
        wellPanel(id = "Extras",
                  h4("Extra Variables:"),
                  numericInput(inputId = "aTestSig", label = "A-Test Signal Level:", value = 0.23, step = 0.01, min = 0),
                  numericInput(inputId = "num_curves", label = "Number of Curves:", value = 3, min = 0),
                  numericInput(inputId = "num_samples", label = "Number of Samples:", value = 65, min = 0),
                  numericInput(inputId = "ttest_conf", label = "T-Test Confidence Interval:", value = 0.95, step = 0.01, max = 1.00, min = 0)),
        
        wellPanel(id = "filesWell",
           h4("File Declarations:"),

            fileInput(inputId = "AllResults",
                      label = "All Simulation Results File"),
           
            fileInput(inputId = "ParameterFile",
                      label = "Simulations Parameter File:")
           
         ),
         
         
         wellPanel(id = "lastWell",
           h4(uiOutput("firstChoice")),
           
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
        div(tableOutput("parameter_table"),style="font-size:90%"),
        div(tableOutput("measures_table"),style = "font-size:90%"),
        div(tableOutput("measureScale_table"),style = "font-size:90%"),
        hr(),

        
        htmlOutput("selectUI"),
       
            tabsetPanel(id = "changingTabs", type = "tabs",
                    tabPanel(title = uiOutput("choicePanel"),
                             imageOutput(outputId = "Graph")),
                            
                    tabPanel(title = uiOutput("choicePanel2"),
                             imageOutput(outputId = "Graph2")),
                    
                    tabPanel(title = uiOutput("choicePanel3"),
                            imageOutput(outputId ="Graph3"))
                            
            ),
        
         
         
      width = 7)
   ) 
)

# Define server logic required to draw ah histogram
server <- function(input, output, session) {
  measureCounter <- 1
  measureScaleCounter <- 1
  myValues <- reactiveValues()
  measureValues <- reactiveValues()
  measureScaleValues <- reactiveValues()
  options(shiny.maxRequestSize=100*1024^2)
  shinyjs::hide("changingTabs")
  shinyjs::hideElement("analysisWell")
  shinyjs::hide("filePathFalse")
  shinyjs::hide("filePathTrue")
  shinyjs::hide("filePath")
  shinyjs::hide("ATestFileName")
  shinyjs::hide("eFASTResultsFileName")
  shinyjs::hide("corCoeffsFileName")
  shinyjs::hide("measureScale")
  shinyjs::hide("addMeasureScale")
  shinyjs::hide("aTestSig")
  shinyjs::hide("num_curves")
  shinyjs::hide("num_samples")
  shinyjs::hide("ttest_conf")
  shinyjs::hideElement("measuresWell")
  shinyjs::hideElement("Extras")
  shinyjs::hideElement("filesWell")
  shinyjs::hideElement("lastWell")
  shinyjs::disable("clearMeasures")
  shinyjs::disable("clearMeasureScales")
  shinyjs::hide("selectUI")
  
  output$parameter_table<-renderTable({
    if(length(myValues$table)>1)
    {
      shinyjs::showElement("analysisWell")
      colnames(myValues$table) <- c("Parameter","Min","Max", "Increment", "Baseline")
      myValues$table
    } 
    
  })
  
  observeEvent(input$filePaths,
               {
                 if(input$filePaths != ""){
                   if(file.exists(input$filePaths))
                   {
                     fileName <<- TRUE
                     shinyjs::hide("filePathFalse")
                     shinyjs::show("filePathTrue")
                     switch(input$usersAnalysisType, "Latin-Hypercube" = LHCFilePathFull <<- input$filePaths,
                                                     "Robustness" = robustFilePathFull <<- input$filePaths,
                                                     "eFAST" = eFASTFilePath <<- input$filePaths)
                     
                   }
                   else
                   {
                     fileName <<- FALSE
                     shinyjs::show("filePathFalse")
                     shinyjs::hide("filePathTrue")
                   }
                 }
               })

  
  observeEvent(input$addMeasureScale,
               {
                 if (input$measureScale != ""){    
                   measureScale <<- c(measureScale, input$measureScale)
                   print(measureScale)
                   updateTextInput(session, inputId = "measureScale", value = "")
                   measureScaleValues$table <- rbind(c("Measure Scales:", measureScale))
                   columnNamesScale <<- c(columnNamesScale, paste0("Measure Scale ", measureScaleCounter))
                   colnames(measureScaleValues$table) <- columnNamesScale
                   output$measureScale_table <- renderTable(measureScaleValues$table, striped = TRUE, bordered = TRUE)
                   measureScaleCounter <<- measureScaleCounter + 1
                 }
                 shinyjs::enable("clearMeasureScales")
               }
               )
  
  observeEvent(input$clearMeasureScales,
               {
                 measureScale <<- c()
                 shinyjs::disable("clearMeasureScales")
                 updateTextInput(session, inputId = "measureScale", value = "")
                 output$measureScale_table <-  NULL
                 measureScaleValues$table
                 columnNamesScale <<- c("Measure Scales")
                 measureScaleCounter <<- 1
               })
  
  observeEvent(input$settingsFile,
               {
                 if (!is.null(input$settingsFile)) #Ensure a settings file has been chosen 
                 {
                   #These first two lines ensures that the tables are reset, so if the user changing their settings file, the one and new values wont bind together. Instead only the new values will be shown.
                   myValues$table <- NULL
                   measureValues$table <- NULL
                   i <<- 7 #Measures begin at column 7
                   settingsData <- read.csv(input$settingsFile$datapath, stringsAsFactors = FALSE)
                   print(settingsData)
                   parameterList <<- settingsData$Parameter
                   minvals <<- settingsData$Min
                   maxvals <<- settingsData$Max
                   incvals <<- settingsData$Increment
                   baseline <<- settingsData$Baseline
                   while (!is.null(settingsData[1, i])) #Keep going along the columns until there are no more measures
                   {
                     measures <<- c(measures, settingsData[1, i]) #Add one instance of the measure name 
                     i <- i+1 
                   }
                   myValues$table <- rbind(isolate(myValues$table), cbind(parameterList,minvals,maxvals,incvals,baseline))
                   measureValues$table <- matrix(measures, nrow = 1, byrow = TRUE)
                   rownames(measureValues$table) <- "MEASURES"
                
                   output$measures_table <- renderTable(measureValues$table, striped = TRUE, bordered = TRUE, colnames = FALSE, rownames = TRUE)
                   
                   
                 }
               }   
               )
  
  
  
  observeEvent(input$LHSSummary,
                if (input$usersAnalysisType == "Latin-Hypercube")
                  {showModal(modalDialog(
                    title = "Creating Summary",
                    "Summary files are being created..."))
                    summary <- lhc_generateLHCSummary(filepath, parameterList, measures,input$AllResults$datapath, lhcSummaryFull, input$ParameterFile$datapath) 
                    showModal(modalDialog(
                      title = "Complete",
                      "Summary files have been created"))}
                 else if (input$usersAnalysisType == "Robustness"){
                    print(input$AllResults$datapath)
                    results <<- "Robustness_Data.csv" #THIS IS ONLY HERE UNTIL THE CHECKING ERROR IS FIXED, ONCE FIXED FILE PATH MUST BE CHANGED TO NULL
                    showModal(modalDialog(
                      title = "Creating Results",
                      "ATest result files are being created..."
                    ))
                    oat_csv_result_file_analysis(filepath, input$AllResults$datapath , parameterList, baseline, measures, paste0(robustFilePathFull, input$ATestFileName, ".csv"), minvals, maxvals, incvals, PARAMVALS=NULL)
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
                   efast_get_overall_medians(eFASTFilePath, num_curves, parameterList, num_samples, measures )
                   showModal(modalDialog(
                     title = "Complete",
                     "Overall medians result file have been created"
                   ))
               }
                  )
  
  observeEvent(input$LHSGenerate,
               {
                 lhc_generatePRCoEffs(LHCFilePathFull, parameterList, measures, lhcSummary, input$corCoeffsFileName)  
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
                 shinyjs::show("filePath")
                 shinyjs::show("ATestFileName")
                 shinyjs::hide("eFASTResultsFileName")
                 shinyjs::hide("corCoeffsFileName")
                 shinyjs::show("measureScale")
                 shinyjs::show("addMeasureScale")
                 shinyjs::showElement("Extras")
                 shinyjs::show("aTestSig")
                 shinyjs::hide("num_curves")
                 shinyjs::hide("num_samples")
                 shinyjs::hide("ttest_conf")
                 shinyjs::showElement("measuresWell")
                 shinyjs::showElement("filesWell")
                 shinyjs::showElement("lastWell")
                 output$textChange <- renderText("")
                 output$selectUI <- renderUI({
                   selectInput(inputId = "usersAnalysisInput", label = "Graph Analysis Selection", choices = c(parameterList))
                 })
               }
               else  if(input$usersAnalysisType == "eFAST")
               {
                
                 shinyjs::hide("ParameterFile")
                 shinyjs::hide("AllResults")
                 shinyjs::hide("LHSGenerate")
                 shinyjs::show("filePath")
                 shinyjs::hide("ATestFileName")
                 shinyjs::show("eFASTResultsFileName")
                 shinyjs::hide("corCoeffsFileName")
                 shinyjs::hide("measureScale")
                 shinyjs::hide("addMeasureScale")
                 shinyjs::showElement("Extras")
                 shinyjs::hide("aTestSig")
                 shinyjs::show("num_curves")
                 shinyjs::show("num_samples")
                 shinyjs::show("ttest_conf")
                 shinyjs::showElement("measuresWell")
                 shinyjs::hideElement("filesWell")
                 shinyjs::showElement("lastWell")
                 output$textChange <- renderText("")
                 output$selectUI <- renderUI({
                   selectInput(inputId = "usersAnalysisInput", label = "Graph Analysis Selection", choices = measures)
                 #updateTabsetPanel(session, output$changingTabs, "tab1")
                  
                 })
                
               }
               else if (input$usersAnalysisType == "Latin-Hypercube")
               {
                 shinyjs::show("ParameterFile")
                 shinyjs::show("LHSGenerate")
                 shinyjs::show("AllResults")
                 shinyjs::show("filePath")
                 shinyjs::hide("ATestFileName")
                 shinyjs::hide("eFASTResultsFileName")
                 shinyjs::show("corCoeffsFileName")
                 shinyjs::show("measureScale")
                 shinyjs::show("addMeasureScale")
                 shinyjs::hideElement("Extras")
                 shinyjs::showElement("measuresWell")
                 shinyjs::showElement("filesWell")
                 shinyjs::showElement("lastWell") 
                 output$textChange <- renderText("Generate Coefficients")
                 output$selectUI <- renderUI({
                   selectInput(inputId = "usersAnalysisInput", label = "Graph Analysis Selection", choices = c(parameterList, "polarPlot"))
                   
                 })
               }
               
               )
  
  observeEvent(input$GraphButton,
               {
                 if(input$usersAnalysisType == "Latin-Hypercube")
                 {
                   showModal(modalDialog(
                     title = "Generating Graphs",
                     "Graphs are being generated..."))
                   lhc_graphMeasuresForParameterChange(LHCFilePathFull, parameterList, measures, measure_scale, input$corCoeffsFileName, lhcSummary, OUTPUT_TYPE = "PNG")
                   lhc_polarplot(LHCFilePathFull, parameterList, measures, input$corCoeffsFileName) 
                   showModal(modalDialog(
                     title = "Complete",
                     "Graphs have been generated"))
                 }
                 else if (input$usersAnalysisType == "Robustness")
                 {
                   showModal(modalDialog(
                     title = "Generating Graphs",
                     "Graphs are being generated..."))
                   oat_graphATestsForSampleSize(robustFilePathFull, parameterList, measures, input$aTestSig, paste0(input$ATestFileName, ".csv"), baseline, minvals, maxvals, incvals, PARAMVALS=NULL)
                   oat_plotResultDistribution(robustFilePathFull, parameterList, measures, measure_scale, "Robustness_Data.csv", baseline, minvals, maxvals, incvals, PARAMVALS=NULL) 
                   showModal(modalDialog(
                     title = "Complete",
                     "Graphs have been generated"))
                   }
                else if (input$usersAnalysisType == "eFAST")
                {
                  showModal(modalDialog(
                    title = "Generating Graphs",
                    "Graphs are being generated..."))
                  efast_run_Analysis(eFASTFilePath, measures, parameterList, input$num_curves, input$num_samples, 1:length(measures), input$ttest_conf, GRAPH_FLAG=TRUE, paste0(input$eFASTResultsFileName, ".csv"))  
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
  
  output$changableFile <- renderText({ 
                          switch(input$usersAnalysisType, "Latin-Hypercube" = "Latin-Hypercube File Path",
                                                          "Robustness" = "Robustness File Path",
                                                          "eFAST" = "eFAST File Path")
  })

  #These switch function change the two tabs that will be shown to the user so graphs can be displayed neatly
  output$choicePanel <- renderText({
                            if (input$usersAnalysisType != "eFAST")
                            {  
                              switch(input$usersAnalysisInput, "adhesionFactorExpressionSlope"  = paste0(input$usersAnalysisInput, "_Displacement"),
                                                               "maxProbabilityOfAdhesion" = paste0(input$usersAnalysisInput, "_Displacement"),
                                                               "stableBindProbability" = paste0(input$usersAnalysisInput, "_Displacement"),
                                                               "maxChemokineExpressionValue" = paste0(input$usersAnalysisInput, "_Displacement"),
                                                               "initialChemokineExpressionValue" = paste0(input$usersAnalysisInput, "_Displacement"),
                                                               "chemokineExpressionThreshold" = paste0(input$usersAnalysisInput, "_Displacement"),
                                                               "polarPlot" = paste0(input$usersAnalysisInput, "_Displacement"))
                            }
                            else "tab1"
    

  })


  output$choicePanel2 <- renderText({
                            if (input$usersAnalysisType != "eFAST")
                            {
                              switch(input$usersAnalysisInput, "adhesionFactorExpressionSlope"  = paste0(input$usersAnalysisInput, "_Velocity"),
                                                               "maxProbabilityOfAdhesion" = paste0(input$usersAnalysisInput, "_Velocity"),
                                                               "stableBindProbability" = paste0(input$usersAnalysisInput, "_Velocity"),
                                                               "maxChemokineExpressionValue" = paste0(input$usersAnalysisInput, "_Velocity"),
                                                               "initialChemokineExpressionValue" = paste0(input$usersAnalysisInput, "_Velocity"),
                                                               "chemokineExpressionThreshold" = paste0(input$usersAnalysisInput, "_Velocity"),
                                                               "polarPlot" = paste0(input$usersAnalysisInput, "_Velocity"))
                            }  
                            else NULL
          
  })
  
  output$choicePanel3 <- renderText({
                            if (input$usersAnalysisType == "Robustness")
                            {
                              switch(input$usersAnalysisInput, "adhesionFactorExpressionSlope"  = paste0(input$usersAnalysisInput, "_ATestResults"),
                                     "maxProbabilityOfAdhesion" = paste0(input$usersAnalysisInput, "_ATestResults"),
                                     "stableBindProbability" = paste0(input$usersAnalysisInput, "_ATestResults"),
                                     "maxChemokineExpressionValue" = paste0(input$usersAnalysisInput, "_ATestResults"),
                                     "initialChemokineExpressionValue" = paste0(input$usersAnalysisInput, "_ATestResults"),
                                     "chemokineExpressionThreshold" = paste0(input$usersAnalysisInput, "_ATestResults"))
                            }   
                            else NULL
})


 observeEvent(input$showGraphs, 
             {
                shinyjs::disable("usersAnalysisType")
                shinyjs::disable("filePath")
                shinyjs::disable("measures")
                shinyjs::disable("measureScale")
                shinyjs::show("selectUI")
                output$Graph <- renderImage({
                                  if (input$usersAnalysisType == "Latin-Hypercube"){
                                    shinyjs::show("changingTabs")
                                    switch(input$usersAnalysisInput, "adhesionFactorExpressionSlope"  =   list(src = paste0(LHCFilePathFull, "/adhesionFactorExpressionSlope_Displacement.png"),
                                                                                                               alt = paste("Image has not loaded")),
                                                                     "maxProbabilityOfAdhesion" =   list(src = paste0(LHCFilePathFull, "/maxProbabilityOfAdhesion_Displacement.png"),
                                                                                                         alt = paste("Image has not loaded")),
                                                                     "stableBindProbability" =   list(src = paste0(LHCFilePathFull, "/stableBindProbability_Displacement.png"),
                                                                                                      alt = paste("Image has not loaded")),
                                                                     "maxChemokineExpressionValue" =   list(src = paste0(LHCFilePathFull, "/maxChemokineExpressionValue_Displacement.png"),
                                                                                                         alt = paste("Image has not loaded")),
                                                                     "initialChemokineExpressionValue" =   list(src = paste0(LHCFilePathFull, "/initialChemokineExpressionValue_Displacement.png"),
                                                                                                              alt = paste("Image has not loaded")),
                                                                     "chemokineExpressionThreshold" = list(src = paste0(LHCFilePathFull, "/chemokineExpressionThreshold_Displacement.png"),
                                                                                                            alt = paste("Image has not loaded")),
                                                                     "polarPlot" = list(src = paste0(LHCFilePathFull, "/polarPlot_Displacement.png"),
                                                                                        alt = paste("Image has not loaded")))
                                  }
                                  else if (input$usersAnalysisType == "Robustness"){
                                    shinyjs::show("changingTabs")
                                    switch(input$usersAnalysisInput, "adhesionFactorExpressionSlope"  =   list(src = "/home/fgch500/robospartan/robustnessFiles/adhesionFactorExpressionSlope_Displacement.png",
                                                                                                               alt = paste("Image has not loaded")),
                                                                     "maxProbabilityOfAdhesion" =   list(src = "/home/fgch500/robospartan/robustnessFiles/maxProbabilityOfAdhesion_Displacement.png",
                                                                                                         alt = paste("Image has not loaded")),
                                                                     "stableBindProbability" =   list(src = "/home/fgch500/robospartan/robustnessFiles/stableBindProbability_Displacement.png",
                                                                                                      alt = paste("Image has not loaded")),
                                                                     "maxChemokineExpressionValue" =   list(src = "/home/fgch500/robospartan/robustnessFiles/maxChemokineExpressionValue_Displacement.png",
                                                                                                            alt = paste("Image has  not loaded")),
                                                                     "initialChemokineExpressionValue" =   list(src = "/home/fgch500/robospartan/robustnessFiles/initialChemokineExpressionValue_Displacement.png",
                                                                                                                alt = paste("Image has not loaded")),
                                                                     "chemokineExpressionThreshold" = list(src = "/home/fgch500/robospartan/robustnessFiles/chemokineExpressionThreshold_Displacement.png",
                                                                                                           alt = paste("Image has not loaded"))
                                                                    )
                                                              
                                  }
                                  else if (input$usersAnalysisType == "eFAST"){
                                    shinyjs::hide("changingTabs") #eFAST doesn't require any of the tabs to be used 
                                    
                                    switch(input$usersAnalysisInput, "Velocity" = list(src = paste0(eFASTFilePath, "/Velocity.png"),
                                                                                       alt = paste("Image has not loaded")),
                                                                     "Displa cement" = list(src = paste0(eFASTFilePath, "/Displacement.png"),
                                                                                           alt = paste("Image has not loaded")))
                                    }
                 
                 }, deleteFile = FALSE)
                
                output$Graph2 <- renderImage({
                                   if (input$usersAnalysisType == "Latin-Hypercube"){
                                      switch(input$usersAnalysisInput, "adhesionFactorExpressionSlope"  =  list(src = paste0(LHCFilePathFull, "/adhesionFactorExpressionSlope_Velocity.png"),
                                                                                                                alt = paste("Image has not loaded")),
                                                                       "maxProbabilityOfAdhesion" = list(src = paste0(LHCFilePathFull, "/maxProbabilityOfAdhesion_Velocity.png"),
                                                                                                         alt = paste("Image has not loaded")),
                                                                       "stableBindProbability" =  list(src = paste0(LHCFilePathFull, "/stableBindProbability_Velocity.png"),
                                                                                                       alt = paste("Image has not loaded")),
                                                                       "maxChemokineExpressionValue" =  list(src = paste0(LHCFilePathFull, "/maxChemokineExpressionValue_Velocity.png"),
                                                                                                             alt = paste("Image has not loaded")),
                                                                       "initialChemokineExpressionValue" = list(src = paste0(LHCFilePathFull, "/initialChemokineExpressionValue_Velocity.png"),
                                                                                                                alt = paste("Image has not loaded")),
                                                                       "chemokineExpressionThreshold" = list(src = paste0(LHCFilePathFull, "/chemokineExpressionThreshold_Velocity.png"),
                                                                                                             alt = paste("Image has not loaded")),
                                                                       "polarPlot" = list(src = paste0(LHCFilePathFull, "/polarPlot_Velocity.png"),
                                                                                          alt = paste("Image has not loaded")))
                                   }
                                   else if (input$usersAnalysisType == "Robustness"){
                                     switch(input$usersAnalysisInput, "adhesionFactorExpressionSlope"  =  list(src = "/home/fgch500/robospartan/robustnessFiles/adhesionFactorExpressionSlope_Velocity.png",
                                                                                                               alt = paste("Image has not loaded")),
                                                                      "maxProbabilityOfAdhesion" = list(src = "/home/fgch500/robospartan/robustnessFiles/maxProbabilityOfAdhesion_Velocity.png",
                                                                                                        alt = paste("Image has not loaded")),
                                                                      "stableBindProbability" =  list(src = "/home/fgch500/robospartan/robustnessFiles/stableBindProbability_Velocity.png",
                                                                                                      alt = paste("Image has not loaded")),
                                                                      "maxChemokineExpressionValue" =  list(src = "/home/fgch500/robospartan/robustnessFiles/maxChemokineExpressionValue_Velocity.png",
                                                                                                            alt = paste("Image has not loaded")),
                                                                      "initialChemokineExpressionValue" = list(src = "/home/fgch500/robospartan/robustnessFiles/initialChemokineExpressionValue_Velocity.png",
                                                                                                               alt = paste("Image has not loaded")),
                                                                      "chemokineExpressionThreshold" = list(src = "/home/fgch500/robospartan/robustnessFiles/chemokineExpressionThreshold_Velocity.png",
                                                                                                            alt = paste("Image has not loaded")))
                                           
                                   }
                         
                  
                  
                }, deleteFile = FALSE)
                
                output$Graph3 <- renderImage({
                                  if (input$usersAnalysisType == "Robustness")
                                  {
                                    switch(input
                                           $usersAnalysisInput, "adhesionFactorExpressionSlope"  =  list(src = "/home/fgch500/robospartan/robustnessFiles/adhesionFactorExpressionSlope.png",
                                                                                                              alt = paste("Image has not loaded")),
                                                                     "maxProbabilityOfAdhesion" = list(src = "/home/fgch500/robospartan/robustnessFiles/maxProbabilityOfAdhesion.png",
                                                                                                       alt = paste("Image has not loaded")),
                                                                     "stableBindProbability" =  list(src = "/home/fgch500/robospartan/robustnessFiles/stableBindProbability.png",
                                                                                                     alt = paste("Image has not loaded")),
                                                                     "maxChemokineExpressionValue" =  list(src = "/home/fgch500/robospartan/robustnessFiles/maxChemokineExpressionValue.png",
                                                                                                           alt = paste("Image has not loaded")),
                                                                     "initialChemokineExpressionValue" = list(src = "/home/fgch500/robospartan/robustnessFiles/initialChemokineExpressionValue.png",
                                                                                                              alt = paste("Image has not loaded")),
                                                                     "chemokineExpressionThreshold" = list(src = "/home/fgch500/robospartan/robustnessFiles/chemokineExpressionThreshold.png",
                                                                                                           alt = paste("Image has not loaded"))
                                                                    )
                                  }
                                  else NULL
                }, deleteFile = FALSE)
               
             })



}

# Run the application 
shinyApp(ui = ui, server = server)

