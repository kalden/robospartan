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


ui <- fluidPage(
   useShinyjs(),
   # Application title
   h3("RoboSpartan Analysis Platform"),
  
   
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
                  h4("Simulation Results File for Parameter Sets:"),
                  
                  fileInput(inputId = "filePaths",
                            label = uiOutput("changableFile"))),
        
        
        wellPanel(id = "measuresWell",
                  h4("Measure Scales:"),
                  checkboxInput(inputId = "ifMeasureScale", label = "Click thix box if you would like to choose your measure scales, else they will be 'N/A'"),
                  textInput(inputId = "measureScale",
                            label = "Choose Your Measure Scales:",
                            value = ""),
                  actionButton(inputId = "addMeasureScale", label = "Add to Measure scales"),
                  actionButton(inputId = "clearMeasureScales", label = "Clear All Measure Scales")
                  
        ),
      
        wellPanel(id = "Extras",
                  h4("Analysis Specific Variables:"),
                  numericInput(inputId = "aTestSig", label = "A-Test Signal Level:", value = 0.23, step = 0.01, min = 0),
                  numericInput(inputId = "ttest_conf", label = "T-Test Confidence Interval:", value = 0.95, step = 0.01, max = 1.00, min = 0)),
        

         wellPanel(id = "lastWell",
           h4(uiOutput("firstChoice")),
           
           actionButton(inputId = "LHSSummary",
                        label = "Go")

         ),
        
        tags$a(href="javascript:history.go(0)", 
               popify(tags$i(class="fa fa-refresh fa-5x"),
                      title = "Reload", 
                      content = "Clear definitions and restart app",
                      placement = "right")),
        
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
        tags$style("#zip_analysis {border: 3px solid #FF8C00; font-weight: bold;}"),
        
        downloadButton(outputId = "zip_analysis", label = "Download Analysis in Zip File"),
        
        imageOutput(outputId = "Graph"),
        

      width = 7)
   ) 
)

# Define server logic required to draw ah histogram
server <- function(input, output, session) {
  
  # Attributes that Finlay previously put above have been moved here, in a reactive variable
  analysis_attrs<-reactiveValues()
  
  analysis_attrs$lhcSummary <- "LHC_Summary.csv"
  analysis_attrs$measures <- c()
  analysis_attrs$measureScale <- c()
  analysis_attrs$baseline <- c()
  analysis_attrs$minvals <- c()
  analysis_attrs$maxvals <- c()
  analysis_attrs$incvals <- c()
  
  analysis_attrs$columnNamesScale <- c("Measure Scales")
  analysis_attrs$parameterList <-c()
  analysis_attrs$graphs <- c()
  analysis_attrs$disableCounter <- 0
  analysis_attrs$ATestFileName <- "ATest_Results.csv"
  analysis_attrs$eFASTResultsFileName <- "eFAST_Analysis_Results.csv"
  analysis_attrs$corCoeffsFileName <- "LHC_Correlation_Coefficients.csv"

  # We're going to have a specific directory named with the date and time, incase of multiple users
  analysis_attrs$user_dir <- file.path(getwd(),paste0("rs_analysis_",gsub(" ","_",gsub(":","_",toString(Sys.time())))))
  # Also stored as non-reactive so can delete at the end of the session
  user_dir<-file.path(getwd(),paste0("rs_analysis_",gsub(" ","_",gsub(":","_",toString(Sys.time())))))
  
  analysis_attrs$measureScaleCounter <- 1
  myValues <- reactiveValues()
  measureValues <- reactiveValues()
  measureScaleValues <- reactiveValues()
  options(shiny.maxRequestSize=100*1024^2)
  shinyjs::hide("changingTabs")
  shinyjs::hideElement("analysisWell")
  
  shinyjs::hide("filePath")
  
  shinyjs::hide("measureScale")
  shinyjs::hide("addMeasureScale")
  shinyjs::hide("aTestSig")
  shinyjs::hide("ttest_conf")
  shinyjs::hideElement("measuresWell")
  shinyjs::hideElement("Extras")

  shinyjs::hideElement("lastWell")
  shinyjs::disable("clearMeasures")
  shinyjs::disable("clearMeasureScales")
  shinyjs::hide("selectUI")
  shinyjs::hide("changingMeasures")
  shinyjs::hide("dispGraph")
  shinyjs::hide("zip_analysis")
  shinyjs::hideElement("lastWell")


  
  output$parameter_table<-renderTable({
    if(length(myValues$table)>1)
    {
      shinyjs::showElement("analysisWell")
      colnames(myValues$table) <- c("Parameter","Min","Max", "Increment", "Baseline")
      myValues$table
    } 
    
  })
  
  
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
  
  observeEvent(input$filePaths,
               {
                 
                 if (input$usersAnalysisType != "eFAST") #So for LHC and Robustness
                 {
                   # Move the uploaded file into this sessions folder
                   file.rename(input$filePaths$datapath, to=file.path(analysis_attrs$user_dir,"execution_results.csv"))
                   
                   if (input$filePaths$type == "text/csv")
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
                   resultsFileHeaders <- c()
                   resultsFileCheck <- read.csv(file.path(analysis_attrs$user_dir,"execution_results.csv"), header = FALSE)
                   for (checkerCounter in 1:(length(resultsFileCheck[1, ])))
                   {
                     if (toString(resultsFileCheck[1,checkerCounter]) != "Parameter.of.Interest") #Dont want this header included here
                     {
                       resultsFileHeaders <- c(resultsFileHeaders, gsub( " ", "", toString(resultsFileCheck[1,checkerCounter])))
                     }
                   }
                   
                   settingsCombinedParamsAndMeasures <- c(analysis_attrs$measures, analysis_attrs$parameterList)
                   
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
                   #eFAST Analysis
                   # Unzip the results file
                   
                   if (input$filePaths$type == "application/zip")
                   {
                     unzip(input$filePaths$datapath, exdir = analysis_attrs$user_dir)
                    
                     shinyjs::show("lastWell")
                      
                   }
                   else
                   {
                     showModal(modalDialog(
                       title = "Wrong Format",
                       "eFAST results should be uploaded as a zip file containing one CSV file per curve-parameter result pair"))
                   }
                 }
               })

  
  observeEvent(input$addMeasureScale,
               {
                 for (i in nrow(measureValues$table))
                 {
                   if (is.na(measureValues$table[i, ]))
                   {
                     measureValues$table <- measureValues$table[-2, ]
                   }
                 }
                 
                 if (input$measureScale != ""){    
                   analysis_attrs$measureScale <- c(analysis_attrs$measureScale, input$measureScale)
                   updateTextInput(session, inputId = "measureScale", value = "")
                  
                   analysis_attrs$columnNamesScale <- c(analysis_attrs$columnNamesScale, paste0("Measure Scale ", analysis_attrs$measureScaleCounter))
                  
                   required <- c()
                   x = 0
                   while (x < (length(analysis_attrs$measures) - length(analysis_attrs$measureScale)))
                   { 
                     required <- c(required, "REQUIRED")
                     x = x+1
                   }
                   if(length(analysis_attrs$measureScale) == 1)
                   {
                     measureValues$table <- rbind(measureValues$table, c("Measure Scales:", analysis_attrs$measureScale, required))
                   }
                   else if (length(analysis_attrs$measureScale) < length(analysis_attrs$measures) && length(analysis_attrs$measureScale) != 1)
                   {
                     measureValues$table[2, ] <- c("Measure Scales:", analysis_attrs$measureScale, required) 
                   }
                   else if(length(analysis_attrs$measureScale) == length(analysis_attrs$measures)) #once all measures have been given measure scales
                   {
                     measureValues$table[2, ] <- c("Measure Scales:", analysis_attrs$measureScale) #Change the second row, or if the user has cleared measure then change respective to this
                   }
                   else
                   {
                     showModal(modalDialog(
                       title = "Measure Scale Limit Exceeded",
                       "You have already given a measure scale for each measure"))
                   }
                   #output$measureScale_table <- renderTable(measureScaleValues$table, striped = TRUE, bordered = TRUE)
                   analysis_attrs$measureScaleCounter <- analysis_attrs$measureScaleCounter + 1
                   output$measures_table <- renderTable(measureValues$table, striped = TRUE, bordered = TRUE, na = "CLEARED")
                 }
                 shinyjs::enable("clearMeasureScales")
                 
               }
               )
  
  observeEvent(input$clearMeasureScales,
               {
                 analysis_attrs$measureScale <- c()
                 shinyjs::disable("clearMeasureScales")
                 updateTextInput(session, inputId = "measureScale", value = "")
                 output$measureScale_table <-  NULL
                 measureScaleValues$table
                 analysis_attrs$columnNamesScale <- c("Measure Scales")
                 analysis_attrs$measureScaleCounter <- 1
                 measureValues$table[2,] <- NA
                 output$measures_table <- renderTable(measureValues$table, striped = TRUE, bordered = TRUE, na = "CLEARED")
                 
                 
                 })
  
  observeEvent(input$settingsFile,
               {
                 if (!is.null(input$settingsFile)) #Ensure a settings file has been chosen 
                 {
                   dir.create(analysis_attrs$user_dir)
                   file.rename(from=input$settingsFile$datapath, to=file.path(analysis_attrs$user_dir,"analysis_settings.csv"))
                   
                   #These first two lines ensures that the tables are reset, so if the user changing their settings file, the one and new values wont bind together. Instead only the new values will be shown.
                   myValues$table <- NULL
                   measureValues$table <- NULL
                   analysis_attrs$measures <- c()
                   i <- 10 #Measures begin at column 10
                   columnNamesMeasures <- c("Measures")
                   settingsData <- read.csv(file.path(analysis_attrs$user_dir,"analysis_settings.csv"), stringsAsFactors = FALSE)
                   
                   analysis_attrs$parameterList <- settingsData$Parameter
                   analysis_attrs$minvals <- settingsData$Min
                   analysis_attrs$maxvals <- settingsData$Max
                   analysis_attrs$incvals <- settingsData$Increment
                   analysis_attrs$baseline <- settingsData$Baseline
                   analysis_attrs$num_samples <- settingsData$number_of_samples[1]
                   analysis_attrs$num_curves <- settingsData$number_of_curves[1]
               
                   while (!is.null(settingsData[1, i])) #Keep going along the columns until there are no more measures
                   {
                     analysis_attrs$measures <- c(analysis_attrs$measures,gsub(" ", "",settingsData[1,i])) #Add one instance of the measure name. Removing any whitespace in the name
                     i <- i+1 
                     columnNamesMeasures <- c(columnNamesMeasures, paste0("Measure ", i-10))
                   }

                   myValues$table <- rbind(isolate(myValues$table), cbind(analysis_attrs$parameterList,analysis_attrs$minvals,analysis_attrs$maxvals,analysis_attrs$incvals,analysis_attrs$baseline))
                   measureValues$table <- matrix(c("Measures:", analysis_attrs$measures), nrow = 1, byrow = TRUE)
                   colnames(measureValues$table) <- columnNamesMeasures

                   output$measures_table <- renderTable(measureValues$table, striped = TRUE, bordered = TRUE)
                   updateSelectInput(session, inputId = "usersAnalysisType", selected = settingsData$analysis_type)
                   shinyjs::disable("usersAnalysisType")
                 }
               }   
               )
  

  observeEvent(input$LHSSummary,
               {
                 if (length(analysis_attrs$measures) != length(analysis_attrs$measureScale))
                 {
                   analysis_attrs$measureScale <- c()
                   for (j in 1:length(analysis_attrs$measures))
                   {
                     analysis_attrs$measureScale <- c(analysis_attrs$measureScale, "N/A")
                   }
                 }
                 
                 if (input$usersAnalysisType == "Latin-Hypercube")
                 {
                   showModal(modalDialog(
                     title = "Creating Summary",
                     "Summary files are being created..."))
                     
                     # Can't put analysis_attrs$[attribute] into spartan call, as spartan complains it can't find analysis_attrs. So need a local copy
                     params<-analysis_attrs$parameterList
                     lhc_res_name<-"execution_results.csv"
                     lhc_sum_dir<-analysis_attrs$user_dir
                     lhc_sum_name<-analysis_attrs$lhcSummary
                     coeffs_file_name<-analysis_attrs$corCoeffsFileName
                     measures<-analysis_attrs$measures
                     
                     
                     measure_scale_loc<-analysis_attrs$measureScale
                     summary <- lhc_generateLHCSummary(lhc_sum_dir,params,measures,lhc_res_name,lhc_sum_name, check_done=TRUE)
                     
                     # KA: Have taken Finlay's code and condensed into one button - so now calculate coefficients
                     lhc_generatePRCoEffs(lhc_sum_dir, params, measures, lhc_sum_name, coeffs_file_name, check_done=TRUE)
                     
                     # Make the graphs
                     simDataset <-read.csv(file.path(lhc_sum_dir,lhc_sum_name), header = TRUE)
                     lister<-NULL
                     dataRemoved<-FALSE
                     for (i in 1:(length(simDataset[1, ])))
                     {
                       if(min(simDataset[i]) == max(simDataset[i]))
                       {
                         lister <- c(lister, colnames(simDataset[i]))
                         dataRemoved <- TRUE
                       }
                     }
                     listerString <- toString(lister)
                     lhc_graphMeasuresForParameterChange(lhc_sum_dir, params, measures, measure_scale_loc, coeffs_file_name, lhc_sum_name, OUTPUT_TYPE = "PNG", check_done=TRUE)
                     lhc_polarplot(lhc_sum_dir, params, measures, coeffs_file_name) 
                     
                     if (dataRemoved == TRUE)
                     {
                       showModal(modalDialog(
                         title = "Complete",
                         paste0("Graphs have been generated. The measure(s) ", listerString,", have not been analysed. This is due to their values all being identical.")))
                       analysis_attrs$measures <- analysis_attrs$measures[!analysis_attrs$measures %in% lister] #Remove the non partitioned measures as these will not be included in the graph options
                       
                     }
                     else 
                     {
                       showModal(modalDialog(
                         title = "Complete",
                         "Analysis complete, and graphs ready to view in browser"))
                     }
                 }
                    
                   
                 else if (input$usersAnalysisType == "Robustness"){
                    
                    #results <<- "Robustness_Data.csv" #THIS IS ONLY HERE UNTIL THE CHECKING ERROR IS FIXED, ONCE FIXED FILE PATH MUST BE CHANGED TO NULL
                    showModal(modalDialog(
                      title = "Creating Results",
                      "ATest result files are being created..."
                    ))
                    
                    # Can't put analysis_attrs$[attribute] into spartan call, as spartan complains it can't find analysis_attrs. So need a local copy
                    params<-analysis_attrs$parameterList
                    oat_res_name<-"execution_results.csv"
                    oat_res_dir<-analysis_attrs$user_dir
                    oat_a_test_sum_name<-analysis_attrs$ATestFileName
                    measures<-analysis_attrs$measures
                    
                    oat_csv_result_file_analysis(oat_res_dir, oat_res_name , params, analysis_attrs$baseline, analysis_attrs$measures, analysis_attrs$ATestFileName, analysis_attrs$minvals, analysis_attrs$maxvals, analysis_attrs$incvals, PARAMVALS=NULL)
                    oat_graphATestsForSampleSize(oat_res_dir, params, analysis_attrs$measures, input$aTestSig, analysis_attrs$ATestFileName, analysis_attrs$baseline, analysis_attrs$minvals, analysis_attrs$maxvals, analysis_attrs$incvals, PARAMVALS=NULL, output_types = c("png"))
                    oat_plotResultDistribution(oat_res_dir, params, analysis_attrs$measures, analysis_attrs$measureScale, oat_res_name, analysis_attrs$baseline, analysis_attrs$minvals, analysis_attrs$maxvals, analysis_attrs$incvals, PARAMVALS=NULL, output_types = c("png")) 
                    
                    showModal(modalDialog(
                      title = "Complete",
                      "Graphs have been generated"))
                    
                    showModal(modalDialog(
                      title = "Complete",
                      "ATest result files have been created, and graphs ready for viewing in browser"
                    ))
                 }
               else if (input$usersAnalysisType == "eFAST"){
                 
                  showModal(modalDialog(
                     title = "Creating Results",
                     HTML("Performing eFAST Analysis.<br> This may take some time..")
                   ))
                 
                  efast_get_overall_medians(analysis_attrs$user_dir, analysis_attrs$num_curves, analysis_attrs$parameterList, analysis_attrs$num_samples, 
                                            analysis_attrs$measures, check_done=TRUE)
                  
                  
                  efast_run_Analysis(analysis_attrs$user_dir, analysis_attrs$measures, analysis_attrs$parameterList, analysis_attrs$num_curves, 
                                     analysis_attrs$num_samples, 1:length(analysis_attrs$measures), TTEST_CONF_INT = input$ttest_conf, GRAPH_FLAG=TRUE, 
                                     analysis_attrs$eFASTResultsFileName, output_types = c("png"), check_done = TRUE) 
                  
                   showModal(modalDialog(
                     title = "Complete",
                     "Analysis results ready for download and graphs ready for viewing in browser"
                   ))  
               }
                 
                 shinyjs::show("dispGraph")
                 shinyjs::disable("filePath")
                 shinyjs::disable("measures")
                 shinyjs::disable("measureScale")
                 shinyjs::show("selectUI")
                 shinyjs::show("changingTabs")
                 shinyjs::show("changingMeasures")
                 shinyjs::show("zip_analysis")
                 
                 if (input$usersAnalysisType == "eFAST")
                 {
                   shinyjs::hide("selectUI") #Parameters are not required for eFAST graphs
                 }
                 if (input$usersAnalysisType == "Robustness")
                 {
                   analysis_attrs$measures<- c(analysis_attrs$measures, "A-Test Results")
                 }
                 updateSelectInput(session, inputId = "changingMeasures", choices = analysis_attrs$measures)
          }
                  )
  
  observeEvent(input$ifMeasureScale,
               {
                 if(input$ifMeasureScale == TRUE)
                 {
                   shinyjs::show("measureScale")
                   shinyjs::show("addMeasureScale")
                   shinyjs::show("clearMeasureScales")
                   analysis_attrs$easureScale <- c()
                   
                 }
                 else
                 {
                   analysis_attrs$disableCounter <- analysis_attrs$disableCounter + 1
                   shinyjs::hide("measureScale")
                   shinyjs::hide("addMeasureScale")
                   shinyjs::hide("clearMeasureScales")
                   if (analysis_attrs$disableCounter > 1) #ensures that it does not disable this when the app starts
                   {
                     shinyjs::disable("ifMeasureScale")
                   }
                  
                   for (j in 1:length(analysis_attrs$measures))
                   {
                     analysis_attrs$measureScale <- c(analysis_attrs$measureScale, "N/A")
                   }
                 }
               }
              )
  
  observeEvent(input$usersAnalysisType,
               if(input$usersAnalysisType == "Robustness")
               {
                
                 shinyjs::show("filePath")
                 shinyjs::showElement("Extras")
                 shinyjs::show("aTestSig")
                 shinyjs::hide("ttest_conf")
                 shinyjs::showElement("measuresWell")
                 
                 output$selectUI <- renderUI({
                   selectInput(inputId = "usersAnalysisInput", label = "Graph Analysis Selection", choices = c(analysis_attrs$parameterList))
                 })
               }
               else  if(input$usersAnalysisType == "eFAST")
               {
                 
                 shinyjs::show("filePath")
                 shinyjs::showElement("Extras")
                 shinyjs::hide("aTestSig")
                 shinyjs::show("ttest_conf")
                 shinyjs::hideElement("measuresWell")
                 
                
                 output$selectUI <- renderUI({
                   selectInput(inputId = "usersAnalysisInput", label = "Graph Analysis Selection", choices = analysis_attrs$measures)
                  
                 })
                
               }
               else if (input$usersAnalysisType == "Latin-Hypercube")
               {
                 
                 shinyjs::show("filePath")
                shinyjs::hideElement("Extras")
                 shinyjs::showElement("measuresWell")
                 
                 output$ selectUI <- renderUI({
                   selectInput(inputId = "usersAnalysisInput", label = "Graph Analysis Selection", choices = c(analysis_attrs$parameterList, "polarPlot"))
                   
                 })
               }
               
               )
  
  output$firstChoice <- renderText({
                          switch(input$usersAnalysisType, "Latin-Hypercube" = "Perform LHC Correlation Analysis",
                                                          "Robustness" = "Generate ATest Results",
                                                          "eFAST" = "Perform eFAST Analysis")
    })
  
  
  output$changableFile <- renderText({ 
                          switch(input$usersAnalysisType, "Latin-Hypercube" = "Latin-Hypercube Executions Results File (CSV)",
                                                          "Robustness" = "Robustness Executions Results File (CSV)",
                                                          "eFAST" = "eFAST Summary Files (ZIP)")
  })
  
  observeEvent(input$usersAnalysisInput,
               {
                 shinyjs::hide("Graph")
               })
  
  observeEvent(input$changingMeasures, shinyjs::hide("Graph"))
  
  observeEvent(input$dispGraph,
               {
                shinyjs::show("Graph")
                 if(input$changingMeasures == "A-Test Results")
                 {
                   output$Graph <- renderImage(list(src = file.path(analysis_attrs$user_dir, paste0(input$usersAnalysisInput, ".png")) , width = '100%', alt = paste("Image not found")), deleteFile = FALSE)
                 }
                 else
                 {
                   output$Graph <- switch(input$usersAnalysisType, "Latin-Hypercube" = renderImage(list(src = file.path(analysis_attrs$user_dir, paste0(input$usersAnalysisInput, "_", input$changingMeasures, ".png")) , width = '100%', alt = paste("Image not found")), deleteFile = FALSE),
                                          "Robustness" = renderImage(list(src = file.path(analysis_attrs$user_dir, paste0("/BP_", input$usersAnalysisInput, "_", input$changingMeasures, ".png")) , width = '100%', alt = paste("Image not found")), deleteFile = FALSE),
                                          "eFAST" = renderImage(list(src = file.path(analysis_attrs$user_dir, paste0(input$changingMeasures, ".png")), width = '100%', alt = paste("Image not found")), deleteFile = FALSE))
                   
                 }
                 
               })

 
 
 # Delete the user directory when the session ends
  session$onSessionEnded(function() {
     unlink(user_dir,recursive=TRUE)
   })



}

# Run the application 
shinyApp(ui = ui, server = server)

