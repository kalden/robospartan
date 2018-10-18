#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyjs)
library(DT)
library(spartan)
library(DBI)
library(RMySQL)
library(readr) #Required for wrtie_csv on MacOS
library(spartanDB)

source("/home/fgch500/robospartan/modify_argos_xml.R")
parameters<-c()
mins<-c()
maxs<-c()
baselines<-c()
increments<-c()
curves<-c()
directory <- ""
measures <- c()
columnNames <<- c("Measures")
wholeNumbers <- c()
Decimal_or_Rounded <- c()

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  useShinyjs(),
  
   # Application title
   h3("Generate Parameter Sets Using Different Sampling Techniques"),
   
   # LHC: Parameters
   # Number of Samples
   # Minimum for each parameter
   # Maximum for each parameter
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
       
        wellPanel(
          
          h4("Analysis Technique:"),
          selectInput(inputId = "analysisType",
                      label = NULL,
                      choices = c("Latin-Hypercube","Robustness", "eFAST")),
          
          h4("Declare Parameters:"),
          
          textInput(inputId = "parameter",
                    label = "Parameter:",
                    value = "a"),
          
          numericInput(inputId = "min",
                    label = "Minimum:",
                    value = 0, min = 0),
          
          numericInput(inputId = "max",
                    label = "Maximum:",
                    value = 0, min = 0),
          
          numericInput(inputId = "robustnessIncrement",
                       label = "Increment",
                       value = 0, min = 0),
          
          numericInput(inputId = "baseline",
                       label = "Calibrated Baseline",
                       value = 0, min = 0),
          
          checkboxInput(inputId = "wholeNumber", label = "Values Rounded to a Whole Number"),
          
          
          #,textInput('txt','','Text')
          actionButton(inputId = "addParameter",
                        label = 'Add Parameter'),
          actionButton(inputId = "clearParameter",
                       label = 'Clear All')
          
        ),
        
        wellPanel(id = "measuresWell",
                  h4("Measures:"),
                  textInput(inputId = "measures",
                            label = "Choose Your Measures:",
                            value = "NULL"),
                  actionButton(inputId = "addMeasure", label = "Add to Measures"),
                  actionButton(inputId = "clearMeasures", label = "Clear All Measures")
        ),
        
        
       wellPanel(
         
         h4("Settings:"),
         
         numericInput(inputId = "numSamples",
                      label = "Number of Samples",
                      value = 65, min = 0),
         
         numericInput(inputId = "numCurves",
                      label = "Number of Curves",
                      value = 1, min = 0),
         
         selectInput(inputId = "algorithm",
                     label = "Sampling Algorithm",
                     choices = c("normal","optimal")),
         
         numericInput(inputId = "numExecutions",
                      label = "Number of Replica Executions",
                      value = 400, min = 0),

         actionButton(inputId = "createSample",
                   label = 'Create Sample'),
         hr(),
          
         fileInput(inputId = "argosFiles",
                   label = "Argos File To Modify:"),
         
         textInput(inputId = "argosDirectory",
                   label = "Type the full file directory where you wish the ARGoS file to save:",
                   value = "/home/fgch500/robospartan/argosFiles"),
         
         tags$style("#argosDirNotComplete {border: 4px solid #dd4b39; float: right;  text-align: center; font-weight: bold;}"),
         
         textInput(inputId = "argosDirNotComplete", label =NULL, value = "False File Path", width = '100%'),
         
         tags$style("#argosDirComplete {border: 4px solid #008000; float: right;  text-align:center; font-weight: bold;}"),
         
         textInput(inputId = "argosDirComplete", label =NULL, value = "True File Path", width = '100%'),
         
         hr(), hr(), hr(),
         
         textInput(inputId = "zipDirectory",
                   label = "Type the full file directory where you wish the ZIP file to save:",
                   value = "/home/fgch500/robospartan/argosFilesZip"),
         
         tags$style("#zipDirNotComplete {border: 4px solid #dd4b39; float: right;  text-align: center; font-weight: bold;}"),
         
         textInput(inputId = "zipDirNotComplete", label =NULL, value = "False File Path", width = '100%'),
         
         tags$style("#zipDirComplete {border: 4px solid #008000; float: right;  text-align:center; font-weight: bold;}"),
         
         textInput(inputId = "zipDirComplete", label =NULL, value = "True File Path", width = '100%'),
         
         hr(), hr(), hr(),
         
         textInput(inputId = "zipName", label = "Enter the name you'd like the zip file saved as", value = "argosZIP", width = '100%'),
         
         actionButton(inputId = "createARGoSFiles",
                   label = 'Modify ARGoS Files'),
         
         actionButton(inputId = "cluster",
                      label = "Add to Cluster")
       ),
      
      wellPanel(
        
        h4("Database:"),
      
        fileInput(inputId = "DBSettings",
                  label = "Database Settings File:"),
        
        actionButton(inputId = "createDB",
                     label = "Make Database"),
        
        actionButton(inputId = "deleteDB",
                     label = "Delete Database"),
        
        br(), br(),
        
        textInput(inputId = "description",
                  label = "Experiment Description:",
                  value = ""),
        
        actionButton(inputId = "addToDB",
                  label = 'Add Experiment to Database')
        
        
      ),
      
      width = 5),
      
      # Show a plot of the generated distribution
      mainPanel(id = "main",
        
        # Output the parameters as they are entered:
        htmlOutput("table_header"),
        div(tableOutput("parameter_table"),style="font-size:90%"),
        div(tableOutput("measures_table"),style = "font-size:90%"),
        
        hr(),
        
        # Now output the sample once generated, with a download button
        htmlOutput("sample_header"),
        div(DT::dataTableOutput(outputId = "sample"),style="font-size:90%"),
        br(),
        downloadButton(outputId = "settingFile", label = "Download settings"),
        downloadButton(outputId = "lhc_sample", label = "Download data"),
        
        
        width=7
      )
   )
   
)

# Define server logic 
server <- function(input, output, session) {
  
  measureCounter <- 1
  measureValues <- reactiveValues()
  myValues <- reactiveValues()
  myValues$sampleGenerated<-FALSE
  shinyjs::hide("createSample") #initialise the button to be hidden until parameters are added
  shinyjs::disable("createARGoSFiles")#Hide the ability to create an argos file until the user has created their sample
  shinyjs::hide("addToDB")
  shinyjs::hide("createDB")
  shinyjs::hide("cluster")
  shinyjs::hide("argosDirectory")
  shinyjs::hide("zipDirectory")
  shinyjs::hide("argosDirNotComplete")
  shinyjs::hide("argosDirComplete")
  shinyjs::hide("zipDirNotComplete")
  shinyjs::hide("zipDirComplete")
  shinyjs::hide("zipName")
  shinyjs::disable("clearMeasures")
  #shinyjs::hideElement("main")
  shinyjs::hide("settingFile")
  shinyjs::hide("wholeNumber")
  sampleCreated <<- FALSE #Flag to determine whether a sample has been created
  #### Hide the sample table if not generated yet
  observe({
    shinyjs::hide("lhc_sample")
    
    if(myValues$sampleGenerated==TRUE)
      shinyjs::show("lhc_sample")
      
  })
  
  observeEvent(
    input$analysisType,
    {   
        print(input$ analysisType)
        #Robustness analysis technique - parameter requires param, min, max and increment. No settings required.
        if(input$analysisType == "Robustness"){
          shinyjs::show("robustnessIncrement")
          shinyjs::hide("numSamples")
          shinyjs::hide("algorithm")
          shinyjs::hide("numCurves") 
          shinyjs::show("baseline")
          shinyjs::hide("wholeNumber")
        }  
        #Latin-Hypercube analysis technique - parameter requires param, min, max. Settings are sample number and algorithm type.
        else if(input$analysisType == "Latin-Hypercube"){
          shinyjs::hide("robustnessIncrement")
          shinyjs::show("numSamples")
          shinyjs::show("algorithm")
          shinyjs::hide("numCurves") 
          shinyjs::hide("baseline")
          shinyjs::show("wholeNumber")
        }  
        #eFAST analysis technique - parameter requires requires param, min, max. Settings require additional 
        else{
          shinyjs::hide("robustnessIncrement")
          shinyjs::show("numSamples")
          shinyjs::hide("algorithm")
          shinyjs::show("numCurves") 
          shinyjs::hide("baseline")
          shinyjs::show("wholeNumber")
        } 
    })
  
  observeEvent(input$addMeasure,
               {
                 if (input$measures != ""){
                   measures <<- c(measures, input$measures)
                   print(measures)
                   updateTextInput(session, inputId = "measures", value = "")
                   measureValues$table <- rbind(c("Measures:", measures))
                   columnNames <<- c(columnNames, paste0("Measure ", measureCounter))
                   colnames(measureValues$table) <- columnNames
                   output$measures_table <- renderTable(measureValues$table, striped = TRUE, bordered = TRUE)
                   shinyjs::enable("clearMeasures")
                   measureCounter <<- measureCounter + 1
                   print (measureCounter)
                 }
               })
  
  observeEvent(input$clearMeasures,
               {
                 measures <<- c()
                 updateTextInput(session, inputId = "measures", value = "")
                 shinyjs::disable("clearMeasures")
                 output$measures_table <-  NULL
                 measureValues$table
                 columnNames <<- c("Measures")
                 measureCounter <<- 1
               })
  
  
  # Download generated sample
  output$lhc_sample <- downloadHandler(
    filename = function() {
      paste0(input$analysisType,".csv")
    },
    content = function(file) { 
      colnames(result) <- columnNames
      write_csv(data.frame(result), path = file) 
      }
  )
  
  #Create a settings .csv file
  output$settingFile <- downloadHandler(
    filename = function() {
      "Settings.csv"
    },
    content = function(file) { 
      write_csv(cbind(data.frame(myValues$table), data.frame(measureValues$table)), path = file) 
    }
  )
  
  observeEvent(input$argosFiles,
      {
        shinyjs::show("argosDirectory")
        shinyjs::show("zipDirectory")
      })
  
  observeEvent(input$argosDirectory,
               {
                 if(!is.null(input$argosFiles$datapath)){
                   if(file.exists(input$argosDirectory))
                   {
                     shinyjs::enable("createARGoSFiles")
                     shinyjs::hide("argosDirNotComplete")
                     shinyjs::show("argosDirComplete")
                     
                   }
                   else
                   {
                     shinyjs::disable("createARGoSFiles")
                     shinyjs::show("argosDirNotComplete")
                     shinyjs::hide("argosDirComplete")
                   }
                 }
               })
  
  observeEvent(input$zipDirectory,
               {
                 if(!is.null(input$argosFiles$datapath)){
                   if(file.exists(input$zipDirectory))
                   {
                     shinyjs::enable("createARGoSFiles")
                     shinyjs::hide("zipDirNotComplete")
                     shinyjs::show("zipDirComplete")
                     shinyjs::show("zipName")
                     
                   }
                   else
                   {
                     shinyjs::disable("createARGoSFiles")
                     shinyjs::show("zipDirNotComplete")
                     shinyjs::hide("zipDirComplete")
                     shinyjs::hide("zipName")
                   }
                 }
               })
  #Make sure the zip file has a desired names
  observeEvent(input$zipName,
               {
                 if (input$zipName == "")
                 {
                   shinyjs::disable("createARGoSFiles")
                 }
                 else
                 {
                   shinyjs::enable("createARGoSFiles")
                 }
               }
               )
                 
  #Modify ARGoS files
  observeEvent(input$createARGoSFiles,  
     {
      if (!is.null(input$argosFiles$datapath) && sampleCreated){
         #directory <<- "/home/fgch500/robospartan/argosFiles" #Working directory
         directory <<- input$argosDirectory
         #zipLocation <-  "/home/fgch500/robospartan/argosFilesZip/ARGoSFilesZip"  #File destination followed by folder and file name where the zipped file should go
         zipLocation <-  input$zipDirectory  #File destination followed by folder and file name where the zipped file should go 
         zipName <- input$zipName
         print(result)
         filesToModify <- input$argosFiles$datapath
         showModal(modalDialog(
           title = "Creating ARGoS Files",
           "ARGoS files are being created..."))
         make_argos_file_from_sample(filesToModify, directory, parameters, result, paste0(zipLocation,zipName))
         shinyjs::show("cluster")
      }
       
      else {
          showModal(modalDialog(
            title = "No Sample found",
            "You must first create a sample with your chosen parameters and values"))
      }
     } 
  )
      
  #### Action when Create Sample is pressed
  observeEvent(
    input$createSample,
    {
      result <<- NULL
      sampleCreated <<- TRUE
      #measures <<- c("Velocity", "Displacement")
      shinyjs::enable("createARGoSFiles") #allow the user to create argos files using the sample results 
      shinyjs::show("settingFile")
      
      if(input$analysisType == "Latin-Hypercube" && is.integer(input$numSamples)) 
      {
        myValues$sample <<- lhc_generate_lhc_sample(FILEPATH=NULL, parameters, input$numSamples, mins, maxs, input$algorithm)
        myValues$sampleGenerated<<-TRUE
        columnNames <<- c(parameters)
        result<<-myValues$sample #required when the user wishes to download the analysis
        if (length(wholeNumbers > 0)) #if the user has wanted any of their parameters to be set to being rounded to whole numbers
        {
          for (i in wholeNumbers)
          {
            result [ ,i] <<- round(result[ ,i])
          }
        }
        output$sample_header <- renderUI({ h4("Generated Sample:") })
        output$sample <- DT::renderDataTable(
          DT::datatable(data = result, 
                        options = list(pageLength = 10, searching=FALSE), 
                        rownames = FALSE, colnames = columnNames))
      }
      
      else if(input$analysisType == "eFAST" && is.integer(input$numSamples))
      {
        myValues$sample <<- efast_generate_sample(FILEPATH = NULL, input$numCurves, input$numSamples, parameters, mins, maxs, write_csv = FALSE, return_sample = TRUE)
        myValues$sampleGenerated<<-TRUE
        print("eFAST can work")
        
        for(param in 1:length(parameters)) #iterate through each parameter chosen
        {
          for(c in 1:input$numCurves) #iterate for the number of curves chosen 
          {  
            result <<- rbind(result, cbind(myValues$sample[,  , param,c], parameters[param], c))
          }
        }
        columnNames <<- c(parameters, "Parameter of Interest", "Curve")
        output$sample_header <- renderUI({ h4("Generated Sample:") })
        output$sample <- DT::renderDataTable(
          DT::datatable(data = result,
                        options = list(pageLength = 10, searching=FALSE),
                        rownames = FALSE, colnames = columnNames))
       }
       
      else if(input$analysisType == "Robustness")
      {
        myValues$sample <<- oat_parameter_sampling(FILEPATH = NULL, parameters, baselines, mins, maxs, increments, write_csv = FALSE, return_sample = TRUE)
        myValues$sampleGenerated<<-TRUE
        for(param in 1:length(myValues$sample))
        {
          result <<- rbind(result, cbind(myValues$sample[[param]], parameters[param]))
        }
        columnNames <<- c(parameters, "Parameter of Interest")
        output$sample_header <- renderUI({ h4("Generated Sample:") })
        output$sample <- DT::renderDataTable(
          DT::datatable(data = result,
                        options = list(pageLength = 10, searching=FALSE),
                        rownames = FALSE, colnames = columnNames))
      }  
      
      else #this case gets called when latin-hypercube or eFAST have incorrect number of samples
      {
        showModal(modalDialog(
          title = "Incorrect number of samples",
          "Number of samples should be numeric"))
        shinyjs::disable("createARGoSFiles") 
        
      }
    }
  )

  observeEvent(
    input$DBSettings,
    {
      rmysql.settingsfile<-input$DBSettings$datapath #Use the user's selected datapath
      rmysql.db<-"spartan_ppsim"
      dblink<<-dbConnect(MySQL(),default.file=rmysql.settingsfile,group=rmysql.db)
      shinyjs::show("addToDB")
      shinyjs::show("createDB")
    })
  
  #Write a bash script to send the simulation to the cluster
  observeEvent(
    input$cluster, 
    {
      sink("lhc_cluster_argos.bash")
      cat("#$-S /bin/bash", "\n")
      cat("#$-cwd", "\n")
      cat(paste0("#$-t 1-",input$numSamples), "\n")
      cat("#$-l h_vmen=8G", "\n")
      cat("#$-l h_rt=23:59:59", "\n", "\n")
      cat(paste0("parameterFilePath = '",directory, "'"), "\n", "\n") #Directory where the ARGoS files are created
      cat(paste0("for i in {1..", input$numExecutions, "}"), "\n")
      cat("do", "\n")
      cat("\t", "if [ ! -d $parameterOutputPath/$SGE_TASKID/ ]; then", "\n")
      cat("\t", "\t", "mkdir $parametersOutputPath/$SGE_TASK_ID", "\n")
      cat("\t", "fi", "\n", "\n")
      cat("\t", "if [ ! -d $parameterOutputPath/$SGE_TASKID/$i ]; then", "\n")
      cat("\t", "\t", "mkdir $parametersOutputPath/$SGE_TASK_ID/$i", "\n")
      cat("\t", "fi", "\n", "\n")
      cat("\t", "argos3 -c $parameterFilePath/argos_experiment_set_$SGE_TASK_ID.argos", "\n")
      cat("done")
      sink()
    }
  )
    
  #### Action when Add to Database is Pressed
  observeEvent(
    input$addToDB,
    {
      if(!is.null(myValues$sample))
      {
        if(input$description!="")
        {
          if(input$analysisType == "Latin-Hypercube" && is.integer(input$numSamples)) 
          {
            add_existing_lhc_sample_to_database(dblink, myValues$sample, experiment_description="Original ppsim lhc dataset")
          }
          
          else if(input$analysisType == "eFAST" && is.integer(input$numSamples))
          {
            add_existing_efast_sample_to_database(dblink, parameters, input$numCurves, parameters_r_object=myValues$sample, experiment_description="Original PPSim eFAST")
          }
          
          else if(input$analysisType == "Robustness")
          {
            add_existing_robustness_sample_to_database(dblink, parameters, parameters_r_object=myValues$sample, experiment_description="Original PPSim Robustness")
          
          }  
        }
        else
        {
          print(is.null(myValues$sample))
          showModal(modalDialog(
            title = "No Experiment Description",
            "You need to enter a description of this experiment for storage in the database"))
        }
      }
      else
      {
        showModal(modalDialog(
          title = "No Sample Generated",
          "You need to generate a sample before one can be added to a experiment database"))
      }
      
    }
  )
  
  observeEvent(input$createDB, 
               create_database_structure(dblink, parameters, measures)) #Allow the user to create a database based on their parameters 
  observeEvent(input$deleteDB, delete_database_structure(dblink)) #Allows the user to delete database structure
  
  #### Action when Clear Parameter is pressed
  observeEvent({
    input$clearParameter},
    {
      shinyjs::hideElement("main")
      shinyjs::hide("createSample") #Make it so the create sample is once again hidden from the user
      shinyjs::disable("createARGoSFiles") 
      sampleCreated <<- FALSE
      parameters<<-c()
      mins<<-c()
      maxs<<-c()
      increments<<-c()
      baselines<<-c()
      wholeNumbers <<- c()
      myValues$sampleGenerated<-FALSE
    
      myValues$table<-NULL
      
      #change values back to the default
      updateTextInput(session, "parameter", value = "")     
      updateTextInput(session, "min", value = 0)
      updateTextInput(session, "max", value = 0)
      updateTextInput(session, "baseline", value = 0)
      updateTextInput(session, "robustnessIncrement", value = 0)
      updateCheckboxInput(session, "wholeNumber", value = FALSE)
      
      # Need to clear the generated sample to:
      if(!is.null(myValues$sample))
      {
        myValues$sample<-NULL
        output$sample_header <- renderUI({ "" })
        output$sample <- NULL
        myValues$sampleGenerated<-FALSE
      }
    }
  )
  
  #### Action when Add Parameter is pressed
  observeEvent(
    input$addParameter,
    {
    shinyjs::showElement("main")
    if(input$addParameter > 0){
      inputTest <- list(input$min, input$max)
      positiveNumber <<- TRUE
      for(positiveTest in 1:length(inputTest)) #checking the min and max values are positive numbers
      { 
        if (inputTest[positiveTest] < 0)
        {  
          positiveNumber <<- FALSE  
        } 
      }
      if(input$parameter != "" && is.numeric(input$min) && is.numeric(input$max) && positiveNumber == TRUE)
      {
        if(input$min < input$max)
        {
          parameter<-isolate(input$parameter)
          min<-isolate(input$min)
          max<-isolate(input$max)
          
          parameters<<-c(parameters,parameter)
          
          if (input$wholeNumber == TRUE)
          {
            wholeNumbers <<- c(wholeNumbers, length(parameters))
            wholeNumbersBool <<- "Whole Number"
          }
          else 
          {
            wholeNumbersBool <<- "Decimal"  
          }

          if (input$analysisType != "Robustness")
          {
            baseline <- "N/A"
            increment <- "N/A"
          }
          else 
          {
            baseline <- isolate(input$baseline)
            increment <- isolate(input$robustnessIncrement)
            wholeNumbersBool <<- "N/A"
          }
          
          mins<<-c(mins,as.numeric(min))
          maxs<<-c(maxs,as.numeric(max))
          baselines<<-c(baselines,as.numeric(baseline))
          increments<<-c(increments, as.numeric(increment))
          
          Decimal_or_Rounded <<- c(Decimal_or_Rounded, wholeNumbersBool)
          
          myValues$table <- rbind(isolate(myValues$table), cbind(parameter,min,max,increment,baseline))
          
          #Change option boxes back to default 
          updateTextInput(session, "parameter", value = "")     
          updateTextInput(session, "min", value = 0)
          updateTextInput(session, "max", value = 0)
          updateTextInput(session, "baseline", value = 0)
          updateTextInput(session, "robustnessIncrement", value = 0)
          updateCheckboxInput(session, "wholeNumber", value = FALSE)
          
          shinyjs::show("createSample") #Allow the user to click the show sample button
          
        }
        else
        {
          updateTextInput(session, "min", value = "")
          updateTextInput(session, "max", value = "")
          showModal(modalDialog(
            title = "Error in Range",
            "Minimum of the sampling range needs to be less than the maximum"))
        }
      }
      else
      {
        # Either parameter is blank, or min/max are not numeric, or value is negative
        updateTextInput(session, "min", value = "")
        updateTextInput(session, "max", value = "")
        showModal(modalDialog(
          title = "Error in Input",
          "Either parameter name is blank, minimum or maximum are not numeric, or a value is negative"))
      }
    }
      }
  )
 
  #### Render the parameter table each time the add parameter button is pressed
  output$parameter_table<-renderTable({
    if(length(myValues$table)>1)
    {
      #This is the column names for the settings file
      colnames(myValues$table) <- c("Parameter","Min","Max", "Increment", "Baseline")
      
      #This part changes only what the user sees on screen and not what gets downloaded in the settings file, as there is no need to know about rounding in the settings file.
      forUserToSee <- cbind(myValues$table, Decimal_or_Rounded)
      colnames(forUserToSee) <- c("Parameter","Min","Max", "Increment", "Baseline", "Decimal/Whole Number")
      forUserToSee #show the user the table
    }
    
  })
  
  #### Change the parameter table header when a sample is generated
    output$table_header <- renderUI({
    if(length(myValues$table)>1)
    {
      h4("Parameters Declared For", input$analysisType, ":")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

