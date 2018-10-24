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

#LHCFilePathFull <<- "/home/fgch500/robospartan/LHCFiles"
#/home/fgch500/robospartan/argosFiles/LHCStuff
#robustFilePathFull <<- "/home/fgch500/robospartan/robustnessFiles"
#eFASTFilePath <<- "/home/fgch500/robospartan/eFASTfiles"
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
#lhcSummary <- NULL
columnNamesScale <<- c("Measure Scales")
AtestResultsLocation <<- paste0(robustFilePathFull,"/ATest_Results.csv")
parameterList <<-c()
clearMeasureCount <<- 2
#previousChoice <<- NULL
k <<- 0 
graphs <<- c()
disableCounter <<- 0


ui <- fluidPage(
   useShinyjs(),
   # Application title
   h3("RoboSpartan Analysis Platform"),
   
   # #This ensures the graphs image size dynamically changes with the window
   # tags$head(tags$style(
   #   type = "text/css",
   #   "#Graph img {max-width: 100%; width: 100%; height:auto}"
   # )),
   # 
   # tags$head(tags$sFtyle(
   #   type = "text/css",
   #   "#Graph2 img {max-width: 100%; width: 100%; height:auto}"
   # )),
   
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
                  h6(tags$em("Please ensure to not end your file path with a '/' character, else it will count as a false file path")),
                  textInput(inputId = "filePaths", label = uiOutput("changableFile"), value = "" ),
                  tags$style("#filePathFalse {border: 4px solid #dd4b39; float: right;  text-align: center; font-weight: bold;}"),
                
                  textInput(inputId = "filePathFalse", label =NULL, value = "False File Path", width = '100%'),
                  
                  tags$style("#filePathTrue {border: 4px solid #008000; float: right;  text-align:center; font-weight: bold;}"),
                
                  textInput(inputId = "filePathTrue", label =NULL, value = "True File Path", width = '100%')),
        
        
        wellPanel(id = "measuresWell",
                  h4("Measure Scales:"),
                  checkboxInput(inputId = "ifMeasureScale", label = "Click thix box if you would like to choose your measure scales, else they will be 'N/A'"),
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
                  numericInput(inputId = "ttest_conf", label = "T-Test Confidence Interval:", value = 0.95, step = 0.01, max = 1.00, min = 0)),
        
        wellPanel(id = "filesWell",
           h4("File Declarations:"),

            fileInput(inputId = "AllResults",
                      label = "All Simulation Results File")
           
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
        selectInput(inputId = "changingMeasures", label = "Measure Select", choices = ""),
        #tabset panel to append tabs to according how many measures are chosen. 
        # tabsetPanel(id = "changingTabs", type = "tabs",
        #          tabPanel(title = "Remove",
        #                   imageOutput(outputId = "Graph"))
        tags$style("#dispGraph {border: 3px solid #0000FF; font-weight: bold;}"),
        actionButton(inputId = "dispGraph", "Display this Graph"),
        tags$style("#zipGraphs {border: 3px solid #FF8C00; font-weight: bold;}"),
        actionButton(inputId = "zipGraphs", label = "Download all graphs to a ZIP file"),
        imageOutput(outputId = "Graph"),
        

      width = 7)
   ) 
)

# Define server logic required to draw ah histogram
server <- function(input, output, session) {
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
  shinyjs::hide("ttest_conf")
  shinyjs::hideElement("measuresWell")
  shinyjs::hideElement("Extras")
  shinyjs::hideElement("filesWell")
  shinyjs::hideElement("lastWell")
  shinyjs::disable("clearMeasures")
  shinyjs::disable("clearMeasureScales")
  shinyjs::hide("selectUI")
  shinyjs::hide("changingMeasures")
  shinyjs::hide("dispGraph")
  shinyjs::hide("zipGraphs")
  shinyjs::hideElement("lastWell")
  shinyjs::hide("AllResults")
  
  output$parameter_table<-renderTable({
    if(length(myValues$table)>1)
    {
      shinyjs::showElement("analysisWell")
      colnames(myValues$table) <- c("Parameter","Min","Max", "Increment", "Baseline")
      myValues$table
    } 
    
  })
  
  observeEvent(input$zipGraphs,
          {
            if (input$usersAnalysisType == "Latin-Hypercube")
            {
              zipName <<- "LHCGraphs"
              lists <<- c(parameterList, "polarPlot")
              filePathRequried <<- LHCFilePathFull
              for (i in 1:length(lists))
              {
                for (j in 1:length(measures))
                {
                  graphs <<- c(graphs, paste0(LHCFilePathFull, "/", lists[i], "_", measures[j], ".png"))
                }
              }
            }
            else if(input$usersAnalysisType == "Robustness")
            {
              zipName <<- "RobustnessGraphs"
              filePathRequired <<- robustFilePathFull
              for (i in 1:length(parameterList))
              {
                graphs <<- c(graphs, paste0(robustFilePathFull, "/", parameterList[i], ".png"))
                for (j in 1:length(measures))
                {
                  graphs <<- c(graphs, paste0(robustFilePathFull, "/", parameterList[i], measures[j], "_BP.png"))
                }
              }
            } 
            else
            {
              zipName <<- "eFASTGraphs"
              filePathRequired <<- eFASTFilePath
              for (i in 1:length(measures))
              {
                graphs <<- c(graphs, paste0(eFASTFilePath, "/", measures[i], ".png") )
              }
              
            }
            
            zip(zipfile = paste0(filePathRequired, "/", zipName), files = graphs)
          })
  
  
  observeEvent(input$AllResults,
               {
                 if (input$usersAnalysisType != "eFAST") #So for LHC and Robustness
                 {
                   if (input$AllResults$type == "text/csv")
                   {
                     shinyjs::showElement("lastWell")
                   }
                   else
                   {
                     showModal(modalDialog(
                       title = "Wrong File Format",
                       "This file must be a .csv file"))
                     shinyjs::hideElement("lastWell")
                   }
                  #This part makes sure the settings file and results files are correct against each other before allowing the user to progress 
                  resultsFileHeaders <<- c()
                  resultsFileCheck <- read.csv(input$AllResults$datapath, header = FALSE)
                  for (checkerCounter in 1:(length(resultsFileCheck[1, ])))
                  {
                    if (toString(resultsFileCheck[1,checkerCounter]) != "Parameter.of.Interest") #Dont want this header included here
                    {
                      resultsFileHeaders <- c(resultsFileHeaders, gsub( " ", "", toString(resultsFileCheck[1,checkerCounter])))
                    }
                  }
                  settingsCombinedParamsAndMeasures <<- c(measures, parameterList)
                  print(resultsFileHeaders)
                  print(settingsCombinedParamsAndMeasures)
                  #print(intersect(resultsFileHeaders, settingsCombinedParamsAndMeasures))
                  if (length(intersect(resultsFileHeaders, settingsCombinedParamsAndMeasures)) != length(settingsCombinedParamsAndMeasures))
                  {
                    showModal(modalDialog(
                      title = "Error",
                      "Summary file and Results file's parameters and measures do not match"))
                    shinyjs::hideElement("lastWell")
                  }
                 }
                 
                 else
                 {
                   if (input$AllResults$type == "application/zip")
                   {
                     shinyjs::showElement("lastWell")
                   }
                   else
                   {
                     showModal(modalDialog(
                       title = "Wrong File Format",
                       "This file must be a zip file of all eFAST sample outputs"))
                     shinyjs::hideElement("lastWell")
                   }
                 }
                 
               })
  
 
  
  observeEvent(input$filePaths,
               {
                 if(input$filePaths != ""){
                   #ensuring the file exists and the user hasn't placed a '/' character at the end
                   if(file.exists(input$filePaths) && substr(input$filePaths, nchar(input$filePaths), nchar(input$filePaths)) != "/")
                   {
                     fileName <<- TRUE
                     shinyjs::hide("filePathFalse")
                     shinyjs::show("filePathTrue")
                     switch(input$usersAnalysisType, "Latin-Hypercube" = LHCFilePathFull <<- input$filePaths,
                                                     "Robustness" = robustFilePathFull <<- input$filePaths,
                                                     "eFAST" = eFASTFilePath <<- input$filePaths)
                     if(input$usersAnalysisType == "Robustness")
                     {
                       shinyjs::show("AllResults")
                     }
                     else if(input$usersAnalysisType == "eFAST")
                     {
                       shinyjs::show("AllResults")
                       shinyjs::show("lastWell")
                     }
                     else if (input$usersAnalysisType == "Latin-Hypercube")
                     {
                       shinyjs::show("AllResults")
                     }
                     
                     
                   }
                   else
                   {
                     fileName <<- FALSE
                     shinyjs::show("filePathFalse")
                     shinyjs::hide("filePathTrue")
                     shinyjs::hide("AllResults")
                   }
                 }
               })

  
  observeEvent(input$addMeasureScale,
               {
                 if (input$measureScale != ""){    
                   measureScale <<- c(measureScale, input$measureScale)
                   updateTextInput(session, inputId = "measureScale", value = "")
                   #measureScaleValues$table <- rbind(c("Measure Scales:", measureScale))
                   columnNamesScale <<- c(columnNamesScale, paste0("Measure Scale ", measureScaleCounter))
                   #colnames(measureScaleValues$table) <- columnNamesScale
                   required <<- c()
                   x = 0
                   while (x < (length(measures) - length(measureScale)))
                   { 
                     required <- c(required, "REQUIRED")
                     x = x+1
                   }
                   if(length(measureScale) == 1)
                   {
                     measureValues$table <- rbind(measureValues$table, c("Measure Scales:", measureScale, required))
                   }
                   else if (length(measureScale) < length(measures) && length(measureScale) != 1)
                   {
                     measureValues$table[clearMeasureCount,] <- c("Measure Scales:", measureScale, required) 
                   }
                   else if(length(measureScale) == length(measures)) #once all measures have been given measure scales
                   {
                     measureValues$table[clearMeasureCount,] <- c("Measure Scales:", measureScale) #Change the second row, or if the user has cleared measure then change respective to this
                   }
                   else
                   {
                     showModal(modalDialog(
                       title = "Measure Scale Limit Exceeded",
                       "You have already given a measure scale for each measure"))
                   }
                   #output$measureScale_table <- renderTable(measureScaleValues$table, striped = TRUE, bordered = TRUE)
                   measureScaleCounter <<- measureScaleCounter + 1
                   output$measures_table <- renderTable(measureValues$table, striped = TRUE, bordered = TRUE, na = "CLEARED")
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
                 measureValues$table[clearMeasureCount,] <- NA
                 output$measures_table <- renderTable(measureValues$table, striped = TRUE, bordered = TRUE, na = "CLEARED")
                 clearMeasureCount <<- clearMeasureCount + 1
                 })
  
  observeEvent(input$settingsFile,
               {
                 if (!is.null(input$settingsFile)) #Ensure a settings file has been chosen 
                 {
                   #These first two lines ensures that the tables are reset, so if the user changing their settings file, the one and new values wont bind together. Instead only the new values will be shown.
                   myValues$table <- NULL
                   measureValues$table <<- NULL
                   measures <<- c()
                   i <<- 10 #Measures begin at column 10
                   columnNamesMeasures <<- c("Measures")
                   settingsData <- read.csv(input$settingsFile$datapath, stringsAsFactors = FALSE)
                   print(settingsData)
                   parameterList <<- settingsData$Parameter
                   minvals <<- settingsData$Min
                   maxvals <<- settingsData$Max
                   incvals <<- settingsData$Increment
                   baseline <<- settingsData$Baseline
                   num_samples <<- settingsData$number_of_samples[1]
                   num_curves <<- settingsData$number_of_curves[1]
                   print(num_samples)
                   print(num_curves)
                   while (!is.null(settingsData[1, i])) #Keep going along the columns until there are no more measures
                   {
                     measures <<- c(measures,gsub(" ", "",settingsData[1,i])) #Add one instance of the measure name. Removing any whitespace in the name
                     i <- i+1 
                     columnNamesMeasures <<- c(columnNamesMeasures, paste0("Measure ", i-10))
                   }
                   for (j in 1:length(measures))
                   {
                     measureScale <<- c(measureScale, "N/A")
                   }
                   print(measureScale)
                   myValues$table <- rbind(isolate(myValues$table), cbind(parameterList,minvals,maxvals,incvals,baseline))
                   measureValues$table <- matrix(c("Measures:", measures), nrow = 1, byrow = TRUE)
                   colnames(measureValues$table) <- columnNamesMeasures
                   print(measures)
                   output$measures_table <- renderTable(measureValues$table, striped = TRUE, bordered = TRUE)
                   updateSelectInput(session, inputId = "usersAnalysisType", selected = settingsData$analysis_type)
                   shinyjs::disable("usersAnalysisType")
                 }
               }   
               )
  

  observeEvent(input$LHSSummary,
               {
                # if (input$usersAnalysisType == "Latin-Hypercube")
                #   {
                #     showModal(modalDialog(
                #     title = "Creating Summary",
                #     "Summary files are being created..."))
                #     #print(input$AllResults)
                #     checkIfSummaryNeeded <- read.csv(input$AllResults$datapath, stringsAsFactors = FALSE, row.names = NULL)
                #     if (checkIfSummaryNeeded[1,1] == checkIfSummaryNeeded[2,1])
                #     {
                #       print(input$AllResults$name)
                #       print(LHCFilePathFull)
                #       print(lhcSummary)
                #       print(parameterList)
                #       print(measures)
                #       summary <- lhc_generateLHCSummary(LHCFilePathFull, parameterList, measures, input$AllResults$name, lhcSummary) 
                #       showModal(modalDialog(
                #         title = "Complete",
                #         "Summary files have been created"))
                #     }
                #     
                #     else
                #     {
                #       showModal(modalDialog(
                #         title = "No Summary Needed",
                #         "A Summary file is not required as data already summarised"))
                #          lhcSummary <<- input$AllResults$name
                #     }
                #   }
                 if (input$usersAnalysisType == "Latin-Hypercube")
                 {
                   showModal(modalDialog(
                     title = "Creating Summary",
                     "Summary files are being created..."))
                   #print(input$AllResults)
                     print(input$AllResults$name)
                     print(LHCFilePathFull)
                     print(lhcSummary)
                     print(parameterList)
                     print(measures)
                     summary <- lhc_generateLHCSummary(LHCFilePathFull, parameterList, measures, input$AllResults$name, lhcSummary) 
                     showModal(modalDialog(
                       title = "Complete",
                       "Summary files have been created"))
                  
                 }
                    
                   
                 else if (input$usersAnalysisType == "Robustness"){
                    print(input$AllResults$datapath)
                    #results <<- "Robustness_Data.csv" #THIS IS ONLY HERE UNTIL THE CHECKING ERROR IS FIXED, ONCE FIXED FILE PATH MUST BE CHANGED TO NULL
                    showModal(modalDialog(
                      title = "Creating Results",
                      "ATest result files are being created..."
                    ))
                    oat_csv_result_file_analysis(filepath, input$AllResults$datapath , parameterList, baseline, measures, paste0(robustFilePathFull, "/", input$ATestFileName, ".csv"), minvals, maxvals, incvals, PARAMVALS=NULL)
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
                  unzip(input$AllResults$datapath, exdir = eFASTFilePath)
                  # efast_get_overall_medians(eFASTFilePath, num_curves, parameterList, num_samples, measures)
                  efast_get_overall_medians(eFASTFilePath, num_curves, parameterList, num_samples, measures)
                   showModal(modalDialog(
                     title = "Complete",
                     "Overall medians result file have been created"
                   ))  
               }
          }
                  )
  
  observeEvent(input$LHSGenerate,
               { 
                 print(parameterList)
                 print(measures)
                 #lhcSummary <<- "LHC_Summary.csv"
                 print(lhcSummary)
                 lhc_generatePRCoEffs(LHCFilePathFull, parameterList, measures, lhcSummary, input$corCoeffsFileName)  
                #lhc_generatePRCoEffs("/home/fgch500/robospartan/LHCFiles", parameterList, measures, "LHC_Summary.csv", "LHC_corCoeffs")  
                 showModal(modalDialog(
                   title = "Complete",
                   "Coefficients files have been created"))
               })
  
  observeEvent(input$ifMeasureScale,
               {
                 if(input$ifMeasureScale == TRUE)
                 {
                   shinyjs::show("measureScale")
                   shinyjs::show("addMeasureScale")
                   shinyjs::show("clearMeasureScales")
                   measureScale <<- c()
                   
                 }
                 else
                 {
                   disableCounter <<- disableCounter + 1
                   shinyjs::hide("measureScale")
                   shinyjs::hide("addMeasureScale")
                   shinyjs::hide("clearMeasureScales")
                   if (disableCounter > 1) #ensures that it does not disable this when the app starts
                   {
                     shinyjs::disable("ifMeasureScale")
                   }
                  
                   for (j in 1:length(measures))
                   {
                     measureScale <<- c(measureScale, "N/A")
                   }
                 }
               }
              )
  
  observeEvent(input$usersAnalysisType,
               if(input$usersAnalysisType == "Robustness")
               {
                 shinyjs::hide("LHSGenerate")
                 shinyjs::show("filePath")
                 shinyjs::show("ATestFileName")
                 shinyjs::hide("eFASTResultsFileName")
                 shinyjs::hide("corCoeffsFileName")
                 shinyjs::showElement("Extras")
                 shinyjs::show("aTestSig")
                 shinyjs::hide("ttest_conf")
                 shinyjs::showElement("measuresWell")
                 shinyjs::showElement("filesWell")
                 output$textChange <- renderText("")
                 output$selectUI <- renderUI({
                   selectInput(inputId = "usersAnalysisInput", label = "Graph Analysis Selection", choices = c(parameterList))
                 })
               }
               else  if(input$usersAnalysisType == "eFAST")
               {
                 shinyjs::hide("LHSGenerate")
                 shinyjs::show("filePath")
                 shinyjs::hide("ATestFileName")
                 shinyjs::show("eFASTResultsFileName")
                 shinyjs::hide("corCoeffsFileName")
                 shinyjs::showElement("Extras")
                 shinyjs::hide("aTestSig")
                 shinyjs::show("ttest_conf")
                 shinyjs::hideElement("measuresWell")
                 shinyjs::showElement("filesWell")
                 output$textChange <- renderText("")
                 output$selectUI <- renderUI({
                   selectInput(inputId = "usersAnalysisInput", label = "Graph Analysis Selection", choices = measures)
                  
                 })
                
               }
               else if (input$usersAnalysisType == "Latin-Hypercube")
               {
                 shinyjs::show("LHSGenerate")
                 shinyjs::show("filePath")
                 shinyjs::hide("ATestFileName")
                 shinyjs::hide("eFASTResultsFileName")
                 shinyjs::show("corCoeffsFileName")
                 shinyjs::hideElement("Extras")
                 shinyjs::showElement("measuresWell")
                 shinyjs::showElement("filesWell")
                 output$textChange <- renderText("Generate Coefficients")
                 output$ selectUI <- renderUI({
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
                   lhc_graphMeasuresForParameterChange(LHCFilePathFull, parameterList, measures, measureScale, input$corCoeffsFileName, lhcSummary, OUTPUT_TYPE = "PNG")
                   #POLAR PLOT CURRENTLY GIVING AN ERROR 
                   lhc_polarplot(LHCFilePathFull, parameterList, measures, input$corCoeffsFileName) 
                   #lhc_graphMeasuresForParameterChange(LHCFilePathFull, parameterList, measures, measure_scale, "LHC_corCoeffs", lhcSummary, OUTPUT_TYPE = "PNG")
                   #lhc_polarplot(LHCFilePathFull, parameterList, measures, "LHC_corCoeffs") 
                   showModal(modalDialog(
                     title = "Complete",
                     "Graphs have been generated"))
                 }
                 else if (input$usersAnalysisType == "Robustness")
                 {
                   showModal(modalDialog(
                     title = "Generating Graphs",
                     "Graphs are being generated..."))
                   print(input$AllResults$datapath)
                   oat_graphATestsForSampleSize(robustFilePathFull, parameterList, measures, input$aTestSig, paste0(input$ATestFileName, ".csv"), baseline, minvals, maxvals, incvals, PARAMVALS=NULL, output_types = c("png"))
                   #oat_plotResultDistribution(robustFilePathFull, parameterList, measures, measure_scale, "Robustness_Data.csv", baseline, minvals, maxvals, incvals, PARAMVALS=NULL, output_types = c("png")) 
                   print(baseline)
                   oat_plotResultDistribution(robustFilePathFull, parameterList, measures, measureScale, input$AllResults$name, baseline, minvals, maxvals, incvals, PARAMVALS=NULL, output_types = c("png")) 
                   
                   showModal(modalDialog(
                     title = "Complete",
                     "Graphs have been generated"))
                   
                   }
                else if (input$usersAnalysisType == "eFAST")
                {
                  showModal(modalDialog(
                    title = "Generating Graphs",
                    "Graphs are being generated..."))
                  print(input$ttest_conf)
                  print(measures)
                  print(parameterList)
                  efast_run_Analysis(eFASTFilePath, measures, parameterList, num_curves, num_samples, 1:length(measures), TTEST_CONF_INT = input$ttest_conf, GRAPH_FLAG=TRUE, paste0(input$eFASTResultsFileName, ".csv"), output_types = c("png")) 
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
  
  observeEvent(input$usersAnalysisInput,
               {
                 shinyjs::hide("Graph")
               })
  
  observeEvent(input$changingMeasures, shinyjs::hide("Graph"))
  
  observeEvent(input$dispGraph,
               {
                 #shinyjs::show("showGraph")
                 shinyjs::show("Graph")
                 if(input$changingMeasures == "A-Test Results")
                 {
                   output$Graph <- renderImage(list(src = paste0(robustFilePathFull, "/", input$usersAnalysisInput, ".png") , width = '100%', alt = paste("Image not found")), deleteFile = FALSE)
                 }
                 else
                 {
                   output$Graph <- switch(input$usersAnalysisType, "Latin-Hypercube" = renderImage(list(src = paste0(LHCFilePathFull, "/", input$usersAnalysisInput, "_", input$changingMeasures, ".png") , width = '100%', alt = paste("Image not found")), deleteFile = FALSE),
                                          "Robustness" = renderImage(list(src = paste0(robustFilePathFull, "/", input$usersAnalysisInput, input$changingMeasures, "_BP.png") , width = '100%', alt = paste("Image not found")), deleteFile = FALSE),
                                          "eFAST" = renderImage(list(src = paste0(eFASTFilePath, "/", input$changingMeasures, ".png"), width = '100%', alt = paste("Image not found")), deleteFile = FALSE))
                   
                 }
                 
               })

 observeEvent(input$showGraphs, 
             {
                shinyjs::show("dispGraph")
                shinyjs::disable("filePath")
                shinyjs::disable("measures")
                shinyjs::disable("measureScale")
                shinyjs::show("selectUI")
                shinyjs::show("changingTabs")
                shinyjs::show("changingMeasures")
                shinyjs::show("zipGraphs")

                if (input$usersAnalysisType == "eFAST")
                {
                  shinyjs::hide("selectUI") #Parameters are not required for eFAST graphs
                }
                if (input$usersAnalysisType == "Robustness")
                {
                  measures<<- c(measures, "A-Test Results")
                }
                updateSelectInput(session, inputId = "changingMeasures", choices = measures)

             })



}

# Run the application 
shinyApp(ui = ui, server = server)

