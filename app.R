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
library(readr) #Required for wrtie_csv on MacOS
#library(spartanDB)

# Attributes moved from here to server, as these were common across all users of the app

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  shinyjs::useShinyjs(),
  
   # Application title
   h3("RoboSpartan Sampling: Generate Parameter Sets Using Different Sampling Techniques"),
   
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
          h6(tags$em("Note: eFAST will automatically have a dummy parameter declared")),
          h6(tags$em("Note: Do Not Use Any Spaces When Declaring Parameters or Measures")),
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
                   label = "ARGoS File To Modify:"),
         
         #textInput(inputId = "argosDirectory",
        #           label = "Type the full file directory where you wish the ARGoS file to save:"),
         
         #tags$style("#argosDirNotComplete {border: 4px solid #dd4b39; float: right;  text-align: center; font-weight: bold;}"),
         
        # textInput(inputId = "argosDirNotComplete", label =NULL, value = "False File Path", width = '100%'),
         
         #tags$style("#argosDirComplete {border: 4px solid #008000; float: right;  text-align:center; font-weight: bold;}"),
         
         #textInput(inputId = "argosDirComplete", label =NULL, value = "True File Path", width = '100%'),
         
         #hr(), hr(), hr(),
         
        # textInput(inputId = "zipDirectory",
         #          label = "Type the full file directory where zip of ARGoS Files should be saved:"),
        
        #h6(tags$em("Please ensure to not end your file path with a '/' character, else it will count as a false file path")),  
         
         #tags$style("#zipDirNotComplete {border: 4px solid #dd4b39; float: right;  text-align: center; font-weight: bold;}"),
         
         #textInput(inputId = "zipDirNotComplete", label =NULL, value = "False File Path", width = '100%'),
         
         #tags$style("#zipDirComplete {border: 4px solid #008000; float: right;  text-align:center; font-weight: bold;}"),
         
         #textInput(inputId = "zipDirComplete", label =NULL, value = "True File Path", width = '100%'),
         
         #hr(), hr(), hr(),
         
         #textInput(inputId = "zipName", label = "Enter the name you'd like the zip file saved as", value = "argos_sample_files.zip", width = '100%'),
         
         #actionButton(inputId = "createARGoSFiles",
        #           label = 'Modify ARGoS Files'),
        
         downloadButton(outputId = "createARGoSFiles", label = "Download Modified ARGoS Files"),

         #actionButton(inputId = "setVariables",
        #              label = "Set Variables for Simulation Runs"),
         
        
        downloadButton(outputId = "cluster", label = "Generate SGE Cluster Script")
        #actionButton(inputId = "cluster",
        #              label = "Generate SGE Cluster Script")
       ),
      
      ## Database section not included in this release
      #wellPanel(
        
      #  h4("Database:"),
      
      #  fileInput(inputId = "DBSettings",
      #            label = "Database Settings File:"),
        
      #  actionButton(inputId = "createDB",
      #               label = "Make Database"),
        
      #  actionButton(inputId = "deleteDB",
      #               label = "Delete Database"),
        
      #  br(), br(),
        
      #  textInput(inputId = "description",
      #            label = "Experiment Description:",
      #            value = ""),
        
      #  actionButton(inputId = "addToDB",
      #            label = 'Add Experiment to Database')
        
        
      #),
      
      width = 4),
      
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
       
        
        width=8
      )
   )
   
)

# Define server logic 
server <- function(input, output, session) {
  
  source("modify_argos_xml.R", local=TRUE)
  
  # Attributes now within server function
  
  measureValues <- reactiveValues()
  myValues <- reactiveValues()
  myValues$sampleGenerated<-FALSE
  myValues$parameters <- c()
  myValues$measures <- c()
  myValues$mins<-c()
  myValues$maxs<-c()
  myValues$baselines<-c()
  myValues$increments<-c()
  #myValues$curves<-c()
  #myValues$directory <- ""
  myValues$columnNames <- c("Measures")
  myValues$wholeNumbers <- c()
  myValues$Decimal_or_Rounded <- c()
  #myValues$resultReplicas <- c()
  myValues$measureCounter <- 1
  myValues$displayed_result<-NULL
  
  shinyjs::hide("createSample") #initialise the button to be hidden until parameters are added
  #shinyjs::disable("createARGoSFiles")#Hide the ability to create an argos file until the user has created their sample
  shinyjs::hide("argosFiles")
  shinyjs::hide("createARGoSFiles") 
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
  #shinyjs::hide("setVariables")
  shinyjs::disable("clearMeasures")
  #shinyjs::hideElement("main")
  shinyjs::hide("settingFile")
  shinyjs::hide("wholeNumber")
  sampleCreated <- FALSE #Flag to determine whether a sample has been created
  
  rv <- reactiveValues(download_flag = 0)
  
  #### Hide the sample table if not generated yet
  observe({
    shinyjs::hide("lhc_sample")
    
    if(myValues$sampleGenerated==TRUE)
    {
      shinyjs::show("lhc_sample")
      shinyjs::show("argosFiles")
    }
      
  })
  
  
  observeEvent(
    input$analysisType,
    {
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
                   myValues$measures <- c(myValues$measures, input$measures)
                   updateTextInput(session, inputId = "measures", value = "")
                   measureValues$table <- rbind(c("Measures:", myValues$measures))
                   myValues$columnNames <- c(myValues$columnNames, paste0("Measure ", myValues$measureCounter))
                   colnames(measureValues$table) <- myValues$columnNames
                   output$measures_table <- renderTable(measureValues$table, striped = TRUE, bordered = TRUE)
                   shinyjs::enable("clearMeasures")
                   myValues$measureCounter <- myValues$measureCounter + 1
                   
                 }
               })
  
  observeEvent(input$clearMeasures,
               {
                 myValues$measures <- c()
                 updateTextInput(session, inputId = "measures", value = "")
                 shinyjs::disable("clearMeasures")
                 output$measures_table <-  NULL
                 measureValues$table
                 myValues$columnNames <- c("Measures")
                 myValues$measureCounter <- 1
                 
               })
  
  
  # Download generated sample
  output$lhc_sample <- downloadHandler(
    filename = function() {
      paste0(input$analysisType,"Data.csv")
    },
    content = function(file) { 
      colnames(myValues$displayed_result) <- myValues$columnNames
      write_csv(data.frame(myValues$displayed_result), path = file) 
      }
  )
  
  #Create a settings .csv file
  output$settingFile <- downloadHandler(
    filename = function() {
      "Settings.csv"
    },
    content = function(file) { 
      analysis_type = input$analysisType
      if (input$analysisType == "Robustness")
      {
        number_of_samples = "N/A"
      }
      else
      {
        number_of_samples = input$numSamples
      }
      
      if (input$analysisType == "eFAST")
      {
        number_of_curves = input$numCurves
      }
      else
      {
        number_of_curves = "N/A"
      }
      #write_csv(cbind(input$analysisType, data.frame(myValues$table), data.frame(measureValues$table)), path = file) 
      write_csv(cbind(analysis_type, number_of_samples, number_of_curves, data.frame(myValues$table), data.frame(measureValues$table)), path = file) 
    }
  )
  
  observeEvent(input$argosFiles,
      {
        #shinyjs::show("argosDirectory")
        #shinyjs::show("zipDirectory")
        shinyjs::show("createARGoSFiles")
      })
  
  observeEvent(input$argosDirectory,
               {
                  if(!is.null(input$argosFiles$datapath)){
                   if(file.exists(input$argosDirectory) && substr(input$argosDirectory, nchar(input$argosDirectory), nchar(input$argosDirectory)) != "/")
                   {
                     shinyjs::show("createARGoSFiles")
                     shinyjs::hide("argosDirNotComplete")
                     shinyjs::show("argosDirComplete")
                     
                   }
                   else
                   {
                     shinyjs::hide("createARGoSFiles")
                     shinyjs::show("argosDirNotComplete")
                     shinyjs::hide("argosDirComplete")
                   }
                 }
               })
  
  observeEvent(input$zipDirectory,
               {
                 if(!is.null(input$argosFiles$datapath)){
                   if(file.exists(input$zipDirectory) && substr(input$zipDirectory, nchar(input$zipDirectory), nchar(input$zipDirectory)) != "/")
                   {
                     shinyjs::show("createARGoSFiles")
                     shinyjs::hide("zipDirNotComplete")
                     shinyjs::show("zipDirComplete")
                     shinyjs::show("zipName")
                     
                   }
                   else
                   {
                     shinyjs::hide("createARGoSFiles")
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
                 
  # Used to show message once download is created (as this can take time)
  #observeEvent(rv$download_flag, {
  #  shinyjs::alert("File downloaded!")
  #}, ignoreInit = TRUE)
  
  # Create ARGoSFiles now done as a download button
  output$createARGoSFiles <- downloadHandler(
    filename = function() {
      paste0(input$analysisType,"Modified_ARGoS_Files.zip")
    },
    content = function(file) { 
      
      # We're going to have a specific directory named with the date and time, incase of multiple users
      argos_files_directory<-file.path(getwd(),paste0("argosFiles_",gsub(" ","_",gsub(":","_",toString(Sys.time())))))
      
      
      if(!file.exists(argos_files_directory))
        dir.create(argos_files_directory)
      
      #directory <<- "/home/fgch500/robospartan/argosFiles" #Working directory
      #dir.create(file.path(input$argosDirectory, "experiments"))
      #dir.create(file.path(input$argosDirectory, "logs"))
      #dir.create(file.path(input$argosDirectory, "Results"))
      #directory <<- paste0(input$argosDirectory, "/experiments")
      
      #zipLocation <-  "/home/fgch500/robospartan/argosFilesZip/ARGoSFilesZip"  #File destination followed by folder and file name where the zipped file should go
      #zipLocation <-  input$zipDirectory  #File destination followed by folder and file name where the zipped file should go 
      #zipName <- input$zipName
      #print(result)
      #filesToModify <- 
      #showModal(modalDialog(
      #  title = "Creating ARGoS Files",
      #  "ARGoS files are being created..."))
      #print(input$numExecutions)
      #print(argos_files_directory)
      make_argos_file_from_sample(input$argosFiles$datapath, argos_files_directory, myValues$parameters, myValues$displayed_result)
      
      #current_wd<-getwd()
      #setwd(directory)
      zip(zipfile = file, dir(argos_files_directory, full.names = TRUE), flags="-qjr")
      #showModal(modalDialog(
      #  title = "Zip File Created",
      #  "A Zip file of ARGoS files has been created at:       ", file))
      
      for(s in 1:nrow(myValues$sample)) #Remove all XML files once they've been zipped
      {
        file.remove(file.path(argos_files_directory, paste0("argos_experiment_set_",s,".argos")))
      }
      # Remove the generated directory
      unlink(argos_files_directory,recursive=TRUE, force=TRUE)
      
      # Change the wd back
      #setwd(current_wd)
      
      shinyjs::show("cluster")
    })
  
  
  #Modify ARGoS files
  # No longer used
  #observeEvent(input$createARGoSFiles,  
  #   {
  #    if (!is.null(input$argosFiles$datapath) && sampleCreated){
  #      if(!file.exists(file.path(getwd(),"argosFiles")))
  #        dir.create(file.path(getwd(),"argosFiles"))
        
  #      directory<-file.path(getwd(),"argosFiles")
         #directory <<- "/home/fgch500/robospartan/argosFiles" #Working directory
         #dir.create(file.path(input$argosDirectory, "experiments"))
         #dir.create(file.path(input$argosDirectory, "logs"))
         #dir.create(file.path(input$argosDirectory, "Results"))
         #directory <<- paste0(input$argosDirectory, "/experiments")
         
         #zipLocation <-  "/home/fgch500/robospartan/argosFilesZip/ARGoSFilesZip"  #File destination followed by folder and file name where the zipped file should go
  #       zipLocation <-  input$zipDirectory  #File destination followed by folder and file name where the zipped file should go 
  #       zipName <- input$zipName
         #print(result)
  #       filesToModify <- input$argosFiles$datapath
  #       showModal(modalDialog(
  #         title = "Creating ARGoS Files",
  #         "ARGoS files are being created..."))
         #print(input$numExecutions)
  #       make_argos_file_from_sample(filesToModify, directory, parameters, result, paste0(zipLocation, "/", zipName), input$numExecutions)
  #       shinyjs::show("cluster")
  #       #shinyjs::show("setVariables")
  #    }
       
  #    else {
  ##        showModal(modalDialog(
  #          title = "No Sample found",
  #          "You must first create a sample with your chosen parameters and values"))
  #    }
  #   } 
  #)
      
  #### Action when Create Sample is pressed
  observeEvent(input$createSample,
    {
      complete <- FALSE
      if (length(myValues$measures) > 0)
      {
        myValues$displayed_result
        sampleCreated <- TRUE
        #measures <<- c("Velocity", "Displacement")
        shinyjs::enable("createARGoSFiles") #allow the user to create argos files using the sample results 
        shinyjs::show("settingFile")
        
        if(input$analysisType == "Latin-Hypercube" && is.integer(input$numSamples)) 
        {
          myValues$sample <- lhc_generate_lhc_sample(FILEPATH=NULL, myValues$parameters, input$numSamples, myValues$mins, myValues$maxs, input$algorithm)
          myValues$sampleGenerated<-TRUE
          myValues$columnNames <- c(myValues$parameters)
          myValues$displayed_result<-myValues$sample #required when the user wishes to download the analysis
          if (length(myValues$wholeNumbers > 0)) #if the user has wanted any of their parameters to be set to being rounded to whole numbers
          {
            for (i in myValues$wholeNumbers)
            {
              myValues$displayed_result [ ,i] <- round(myValues$displayed_result[ ,i])
            }
          }
          #for (lines in 1:input$numSamples)
          #{
            #for (replicas in 1:input$numExecutions)
            #{
              #resultReplicas <<- rbind(resultReplicas, result[lines, ])
              
            #}
          #}
         
          output$sample_header <- renderUI({ h4("Generated Sample:") })
          output$sample <- DT::renderDataTable(
            DT::datatable(data = myValues$displayed_result, 
                          options = list(pageLength = 10, searching=FALSE), 
                          rownames = FALSE, colnames = myValues$columnNames))
          
          complete <- TRUE
        }
        
        else if(input$analysisType == "eFAST" && is.integer(input$numSamples))
        {
          if (length(myValues$parameters) > 1)
          {
            if (input$numSamples >= 65)
            {
              
              #Adding the dummy parameter
              myValues$parameters <- c(myValues$parameters, "Dummy")
              myValues$mins <- c(myValues$mins, 0)
              myValues$maxs <- c(myValues$maxs, 1)
              
              myValues$sample <- efast_generate_sample(FILEPATH = NULL, input$numCurves, input$numSamples, myValues$parameters, myValues$mins, myValues$maxs, write_csv = FALSE, return_sample = TRUE)
              myValues$sampleGenerated<-TRUE
              
              for(param in 1:length(myValues$parameters)) #iterate through each parameter chosen
              {
                for(c in 1:input$numCurves) #iterate for the number of curves chosen
                {
                  myValues$sample[ , myValues$wholeNumbers,param,c] <- round(myValues$sample[,myValues$wholeNumbers, param, c ])

                  myValues$displayed_result <- rbind(myValues$displayed_result, cbind(myValues$sample[,  , param,c], myValues$parameters[param], c))
                }
              }
              showModal(modalDialog(
                title = "Generating data table",
                "Data table is currently being generated"))
              #for (lines in 1:nrow(result))
              #{
              #  for (replicas in 1:input$numExecutions)
              #  {
              #    resultReplicas <<- rbind(resultReplicas, result[lines,  ])
              #  }
              #}
              myValues$columnNames <- c(myValues$parameters, "Parameter of Interest", "Curve")
              output$sample_header <- renderUI({ h4("Generated Sample:") })
              output$sample <- DT::renderDataTable(
                DT::datatable(data = myValues$displayed_result,
                              options = list(pageLength = 10, searching=FALSE),
                              rownames = FALSE, colnames = myValues$columnNames))
              showModal(modalDialog(
                title = "Data table generated",
                "Data table has been generated"))
              
              complete <- TRUE
            }
            else
            {
              showModal(modalDialog(
                title = "Incorrect number of samples",
                "eFAST requires at least 65 samples"))
              shinyjs::disable("createARGoSFiles") 
            }
          }
          else
          {
            showModal(modalDialog(
              title = "Incorrect number of parameters",
              "eFAST requires at least 2 parameters"))
            shinyjs::disable("createARGoSFiles") 
          }
        
          
        }
        
        else if(input$analysisType == "Robustness")
        {
          myValues$sample <- oat_parameter_sampling(FILEPATH = NULL, myValues$parameters, myValues$baselines, myValues$mins, myValues$maxs, myValues$increments, write_csv = FALSE, return_sample = TRUE)
          myValues$sampleGenerated<-TRUE
          for(param in 1:length(myValues$sample))
          {
            myValues$displayed_result <- rbind(myValues$displayed_result, cbind(myValues$sample[[param]], myValues$parameters[param]))
          }
          
          #for (lines in 1:nrow(result))
          #{
          #  for (replicas in 1:input$numExecutions)
          #  {
          #    resultReplicas <<- rbind(resultReplicas, result[lines, ])
          #  }
          #}
          myValues$columnNames <- c(myValues$parameters, "Parameter of Interest")
          output$sample_header <- renderUI({ h4("Generated Sample:") })
          output$sample <- DT::renderDataTable(
            DT::datatable(data = myValues$displayed_result,
                          options = list(pageLength = 10, searching=FALSE),
                          rownames = FALSE, colnames = myValues$columnNames))
          complete <- TRUE
          
        }  
        
        else #this case gets called when latin-hypercube or eFAST have incorrect number of samples
        {
          showModal(modalDialog(
            title = "Incorrect number of samples",
            "Number of samples should be numeric"))
          shinyjs::disable("createARGoSFiles") 
          
        }
      }
      else
      {
        showModal(modalDialog(
          title = "No Measures",
          "You must have at least one measure defined before creating the sample"))
      }
    if (complete == TRUE)
    {
      shinyjs::disable("addParameter")
      shinyjs::disable("clearParameter")
      shinyjs::disable("analysisType")
      shinyjs::hide("measuresWell")
      shinyjs::disable("createSample")
    }
    }
     
  )

  observeEvent(
    input$DBSettings,
    {
      rmysql.settingsfile<-input$DBSettings$datapath #Use the user's selected datapath
      rmysql.db<-"spartan_ppsim"
      dblink<-dbConnect(MySQL(),default.file=rmysql.settingsfile,group=rmysql.db)
      shinyjs::show("addToDB")
      shinyjs::show("createDB")
    })
  
  
  
  # Generate and download the SGE cluster scripts
  output$cluster <- downloadHandler(
    filename = function() {
      paste0(input$analysisType,"Cluster_Scripts.zip")
    },
    content = function(file) { 
  
      # We're going to have a specific directory named with the date and time, incase of multiple users
      cluster_script_directory<-file.path(getwd(),paste0("cluster_scripts_",gsub(" ","_",gsub(":","_",toString(Sys.time())))))
      
      if(!file.exists(cluster_script_directory))
        dir.create(cluster_script_directory)
      
      
      # Creates the scripts in the current directory, zips these two up, then offers for download
      sink(file.path(cluster_script_directory,paste0(input$analysisType, "_cluster_argos.sh")))
      
      # KA: Incorporated variables script into this one, to save the need for two scripts
      cat("#!/bin/bash","\n")
      cat("#$-cwd","\n")
      cat(paste0("#$-t 1-", input$numSamples),"\n")
      cat("#$-l h_vmem=8G","\n")
      cat("#$-l h_rt=05:00:00","\n")
      cat("#$ -o ./logs/","\n")
      cat("#$ -e ./logs/","\n")
      cat("","\n")
      
      # Old variables script here
      cat("length=1000", "\n") #Allow user to choose length
      cat("expName=argos_experiment_set_", "\n", "\n") #Again for future allow user to choose this
      cat("initialRunNumber=1", "\n")
      cat(paste0("finalRunNumber=", input$numSamples), "\n")
      cat("stepRunNumber=1", "\n", "\n")
      cat("initialSeedIterationNumber=1", "\n")
      cat(paste0("finalSeedIterationNumber=", input$numExecutions), "\n")
      cat("stepSeedIterationNumber=1", "\n", "\n")
      cat(paste0("OutFileName=\"", input$analysisType, "NoParameters.csv\""),"\n")
      cat(paste0("DataFileName=\"", input$analysisType, "Data.csv\""),"\n")
      cat(paste0("CombinedOutputFileName=\"", input$analysisType, "CombinedParamsAndResults.csv\""), "\n")
      cat(paste0("processedFileName=\"", input$analysisType, "_final_row.csv\"","\n"))
      
      cat("# Parameter","\n")
      cat("function analyseParameters() {","\n")
      cat("	for ((i= $initialSeedIterationNumber; i <= $finalSeedIterationNumber; i = i + $stepSeedIterationNumber))","\n")
      cat("	do","\n")
      cat("		# Directory","\n")
      cat("		if [ -d Results/$expName/$i ]","\n")
      cat("			then echo -e \"\nDirectory replica excecution $i exists\n\" >> Results/Logs/$FILE.txt","\n")
      cat("			else mkdir Results/$expName/$i","\n")
      cat("		fi","\n")
      cat("		analyseIterations \"$1\"","\n")
      cat("		echo -n \".\"","\n")
      cat("	done","\n")
      cat("}","\n")
      cat("","\n")
      cat("# Iteration","\n")
      cat("function analyseIterations() {","\n")
      cat("","\n")
      cat("		filename=${expName}$1","\n")
      cat("","\n")
      cat("		modifyFiles \"$1\"","\n")
      cat("","\n")
      cat("		# Directory","\n")
      cat("		if [ -d Results/$expName/$i/$1 ]","\n")
      cat("			then echo -e \"Directory iteration $1 exists \n\" >> Results/Logs/$FILE.txt","\n")
      cat("			else mkdir Results/$expName/$i/$1","\n")
      cat("		fi","\n")
      cat("","\n")
      cat("		#------------Copy simulation----------#","\n")
      cat("		cp -R experiments/${expName}${1}.argos Results/$expName/$i/$1","\n")
      cat("		#------------Run simulation----------#","\n")
      cat("		argos3 -c experiments/${expName}${1}.argos","\n")
      cat("","\n")
      cat("}","\n")
      cat("","\n")
      cat("function modifyFiles() {","\n")
      cat("","\n")
      cat("	# Simulation parameters","\n")
      cat("	perl -i -pe 's|(experiment length=)\".*?\"|$1\"'\"$length\"'\"|g' experiments/$filename.argos # Change experiment length","\n")
      cat("	perl -i -pe 's|(random_seed=)\".*?\"|$1\"'\"$i\"'\"|g' experiments/$filename.argos # Change seed","\n")
      cat("	","\n")
      cat("	#change file path for the data to get saved to ","\n")
      cat("	sed -i 's|file_path=\"[^\"]*\"|file_path=\"./Results/argos_experiment_set_/'$i'/'$1'/\"|g' experiments/$filename.argos","\n")
      cat("","\n")
      cat("	","\n")
      cat("}","\n")
      cat("","\n")
      cat("","\n")
      #cat("# Variables","\n")
      #cat("source ./Latin-Hypercube_variables.sh","\n")
      #cat("","\n")
      cat("FILE=$expName","\n")
      cat("","\n")
      #cat("VAR=\"Output\n\"","\n")
      cat("","\n")
      cat("#Create a results folder to store all results if there is not already one","\n")
      cat("if [ ! -d Results ] ","\n")
      cat("	then mkdir Results","\n")
      cat("fi","\n")
      cat("","\n")
      cat("","\n")
      cat("# Directory","\n")
      cat("if [ -d Results/$expName ]","\n")
      cat("	then echo -e \"\nDirectory type $expName exists\n\" >> Results/Logs/$FILE.txt","\n")
      cat("	else mkdir Results/$expName ","\n")
      cat("fi","\n")
      cat("clear","\n")
      cat("","\n")
      cat("","\n")
      cat("mkdir ./logs")
      cat("module load argos3/3.0.0","\n")
      cat("analyseParameters \"$SGE_TASK_ID\"","\n")
      cat("echo \"End\"","\n")
      cat("","\n")
      sink()
      
      # Creates the scripts in the current directory, zips these two up, then offers for download
      sink(file.path(cluster_script_directory,"processDataAndCombineResults.sh"))
      
      cat("#!/bin/bash","\n")
      cat("","\n")
      #cat("#-----Import variables-----#","\n")
      #cat("source ./Latin-Hypercube_variables.sh","\n")
      #cat("","\n")
      
      # Old variables script here
      cat("length=1000", "\n") #Allow user to choose length
      cat("expName=argos_experiment_set_", "\n", "\n") #Again for future allow user to choose this
      cat("initialRunNumber=1", "\n")
      cat(paste0("finalRunNumber=", input$numSamples), "\n")
      cat("stepRunNumber=1", "\n", "\n")
      cat("initialSeedIterationNumber=1", "\n")
      cat(paste0("finalSeedIterationNumber=", input$numExecutions), "\n")
      cat("stepSeedIterationNumber=1", "\n", "\n")
      cat(paste0("OutFileName=\"", input$analysisType, "NoParameters.csv\""),"\n")
      cat(paste0("DataFileName=\"", input$analysisType, "Data.csv\""),"\n")
      cat(paste0("CombinedOutputFileName=\"", input$analysisType, "CombinedParamsAndResults.csv\""), "\n")
      cat(paste0("processedFileName=\"", input$analysisType, "_final_row.csv\"","\n"))
      
      cat("function analyseParameters () {","\n")
      cat("	#-----Check all parameters-----#","\n")
      cat("	for ((i= $initialSeedIterationNumber; i <= $finalSeedIterationNumber; i = i + $stepSeedIterationNumber))","\n")
      cat("	do","\n")
      cat("		echo \"GOT HERE\"","\n")
      cat("		#-----Check if parameter directory exists-----#","\n")
      cat("		if [ -d Results/$expName/$i/ ]","\n")
      cat("			then","\n")
      cat("				","\n")
      cat("				 analyseIterations ","\n")
      cat("			else","\n")
      cat("				echo -e \"Directory parameter '$i' does not exist\n \"","\n")
      cat("		fi","\n")
      cat("	done","\n")
      cat("	wait","\n")
      cat("	paste -d, Results/$expName/*.csv > Results/$expName.csv","\n")
      cat("	rm -f Results/$expName/*.csv","\n")
      cat("}","\n")
      cat("","\n")
      cat("function analyseIterations () {","\n")
      cat("	#-----Check all iterations-----#","\n")
      cat("	for ((j= $initialRunNumber; j <= $finalRunNumber; j = j + $stepRunNumber))","\n")
      cat("	do","\n")
      cat("		#-----Check if iteration exists-----#","\n")
      cat("		if [ -d Results/$expName/$i/$j/ ] ","\n")
      cat("			then","\n")
      cat("				analyseFiles","\n")
      cat("		fi","\n")
      cat("	done	","\n")
      cat("	","\n")
      cat("}","\n")
      cat("","\n")
      cat("","\n")
      cat("","\n")
      cat("function analyseFiles () {","\n")
      cat("","\n")
      cat("	#Copy head and tail from each file into a new file that only has header and final row","\n")
      cat("	head -n 1 Results/$expName/$i/$j/global.csv >> Results/$expName/$i/$j/$processedFileName","\n")
      cat("	tail -n 1 Results/$expName/$i/$j/global.csv >> Results/$expName/$i/$j/$processedFileName","\n")
      cat("	","\n")
      cat("}","\n")
      cat("","\n")
      cat("function moveFiles(){","\n")
      cat("	#If there is a previous file of the same name remove it ","\n")
      cat("	if [ -f $OutFileName ]","\n")
      cat("		then rm -rf $OutFileName","\n")
      cat("	fi","\n")
      cat("			","\n")
      cat("	if [ -d tempDir ]","\n")
      cat("		then echo \"Directory Already Exists\"","\n")
      cat("		else mkdir tempDir","\n")
      cat("	fi","\n")
      cat("	for ((i= $initialSeedIterationNumber; i <= $finalSeedIterationNumber; i = i + $stepSeedIterationNumber))","\n")
      cat("	","\n")
      cat("	do","\n")
      cat("		iterateThroughFiles","\n")
      cat("	done	","\n")
      cat("}","\n")
      cat("","\n")
      cat("#Once the parameter number folder has been accessed, iterate through all the files in this, copying the results from this into a temporary folder","\n")
      cat("function iterateThroughFiles(){","\n")
      cat("	for ((j= $initialRunNumber; j <= $finalRunNumber; j = j +$stepRunNumber))","\n")
      cat("	do	","\n")
      cat("		echo $expName/$i/$j","\n")
      cat("		cp -R Results/$expName/$i/$j/final_row.csv tempDir/${i}_${j}.csv","\n")
      cat("	done","\n")
      cat("}","\n")
      cat("","\n")
      cat("","\n")
      cat("function combineCSVFiles(){","\n")
      cat("	","\n")
      cat("	c=0                                       # Reset a counter","\n")
      cat("	for ((j= $initialRunNumber; j <= $finalRunNumber; j = j +$stepRunNumber))","\n")
      cat("	","\n")
      cat("	do","\n")
      cat("		for ((i= $initialSeedIterationNumber; i <= $finalSeedIterationNumber; i = i + $stepSeedIterationNumber))","\n")
      cat("		do","\n")
      cat("			for filename in ./tempDir/${i}_${j}.csv; do ","\n")
      cat("				 echo $filename","\n")
      cat("				 if [ \"$filename\"  != \"$OutFileName\" ] ;      # Avoid recursion ","\n")
      cat("				 then ","\n")
      cat("				   if [[ $c -eq 0 ]] ; then ","\n")
      cat("				      head -1  $filename >   $OutFileName # Copy header if it is the first file","\n")
      cat("				   fi","\n")
      cat("				   tail -n +2  $filename >>  $OutFileName # Append from the 2nd line each file","\n")
      cat("				   c=$(( $c + 1 ))                        # Increase the counter","\n")
      cat("				 fi","\n")
      cat("			done","\n")
      cat("		done","\n")
      cat("	done","\n")
      cat("","\n")
      cat("	echo \"Would you like to delete the temporary folder of files? (y/n)\"","\n")
      cat("	read tempANS","\n")
      cat("","\n")
      cat("	if [ \"$tempANS\" == y ]","\n")
      cat("	then ","\n")
      cat("		rm -rf tempDir","\n")
      cat("	fi","\n")
      cat("	","\n")
      cat("	","\n")
      cat("	#Remove commas from end of line","\n")
      cat("	for commaRemove in $OutFileName ","\n")
      cat("		do","\n")
      cat("		cat $commaRemove | sed 's/,$//' > tmp.tmp","\n")
      cat("		mv tmp.tmp $commaRemove","\n")
      cat("	done","\n")
      cat("","\n")
      cat("","\n")
      cat("	echo \"Would you like to combine the parameter file with results file? (y/n)\"","\n")
      cat("	read ANS","\n")
      cat("	","\n")
      cat("	if [ \"$ANS\" == y ]","\n")
      cat("	then ","\n")
      cat("		paste -d , $DataFileName $OutFileName > $CombinedOutputFileName","\n")
      cat("	fi","\n")
      cat("}","\n")
      cat("","\n")
      cat("#-----Check if type directory exists-----#","\n")
      cat("clear","\n")
      cat("echo \"Processing data!\"","\n")
      cat("if [ -d Results/$expName/ ]","\n")
      cat("	then 	","\n")
      cat("		echo \"Start!\"","\n")
      cat("		analyseParameters","\n")
      cat("		echo \"End!\"","\n")
      cat("        	else ","\n")
      cat("        		echo -e \"Directory type '$expName' does not exist\n \"","\n")
      cat("fi","\n")
      cat("moveFiles","\n")
      cat("combineCSVFiles","\n")
      cat("","\n")
      sink()
      
      zip(zipfile = file, dir(cluster_script_directory, full.names = TRUE), flags="-qjr")
      
      file.remove(file.path(cluster_script_directory,"processDataAndCombineResults.sh"))
      file.remove(file.path(cluster_script_directory,paste0(input$analysisType, "_cluster_argos.sh")))
      unlink(cluster_script_directory,recursive=TRUE,force=TRUE)
    }
  )
  
  
  
  
  
  #Write a bash script to send the simulation to the cluster
  # KA - no longer used
  #observeEvent(
  #  input$cluster, 
  #  {
  #    if(!file.exists(file.path(getwd(),"cluster_scripts")))
  #      dir.create(file.path(getwd(),"cluster_scripts"))
      
      # Script generation part was previously here, now above
      
     
      
      # Zip these two together
      #current_wd<-getwd()
      #setwd(file.path(getwd(),"cluster_scripts"))
      
      
      #setwd(current_wd)
      
    
      
      
  #    showModal(modalDialog(
  #      title = "Complete",
  #      "Cluster files made"))
  #  }
  #)
  
  observeEvent(input$setVariables,
    {
      sink(paste0(input$zipDirectory, "/", input$analysisType, "_variables.sh"))
      cat("#$-S /bin/bash", "\n", "\n")
      cat("export length=1000", "\n") #Allow user to choose length
      cat("export expName=argos_experiment_set_", "\n", "\n") #Again for future allow user to choose this
      cat("export initialRunNumber=1", "\n")
      cat(paste0("export finalRunNumber=", input$numSamples), "\n")
      cat("export stepRunNumber=1", "\n", "\n")
      cat("export initialSeedIterationNumber=1", "\n")
      cat(paste0("export finalSeedIterationNumber=", input$numExecutions), "\n")
      cat("export stepSeedIterationNumber=1", "\n", "\n")
      cat(paste0("export OutFileName=\"", input$analysisType, "NoParameters.csv\""),"\n")
      cat(paste0("export DataFileName=\"", input$analysisType, "Data.csv\""),"\n")
      cat(paste0("export CombinedOutputFileName=\"", input$analysisType, "CombinedParamsAndResults.csv\""), "\n")
      cat(paste0("export processedFileName=\"", input$analysisType, "_final_row.csv\""))
      sink()
      
      showModal(modalDialog(
        title = "Complete",
        "Cluster variables file made"))
      
    })
    
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
            add_existing_efast_sample_to_database(dblink, myValues$parameters, input$numCurves, parameters_r_object=myValues$sample, experiment_description="Original PPSim eFAST")
          }
          
          else if(input$analysisType == "Robustness")
          {
            add_existing_robustness_sample_to_database(dblink, myValues$parameters, parameters_r_object=myValues$sample, experiment_description="Original PPSim Robustness")
          
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
               create_database_structure(dblink, myValues$parameters, myValues$measures)) #Allow the user to create a database based on their parameters 
  observeEvent(input$deleteDB, delete_database_structure(dblink)) #Allows the user to delete database structure
  
  #### Action when Clear Parameter is pressed
  observeEvent(
    input$clearParameter,
    {
      myValues$Decimal_or_Rounded <- c()
      shinyjs::hideElement("main")
      shinyjs::hide("createSample") #Make it so the create sample is once again hidden from the user
      shinyjs::hide("createARGosFiles")
      shinyjs::disable("createARGoSFiles") 
      sampleCreated <- FALSE
      myValues$parameters<-c()
      myValues$mins<-c()
      myValues$maxs<-c()
      myValues$increments<-c()
      myValues$baselines<-c()
      myValues$wholeNumbers <- c()
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
      positiveNumber <- TRUE
      for(positiveTest in 1:length(inputTest)) #checking the min and max values are positive numbers
      { 
        if (inputTest[positiveTest] < 0)
        {  
          positiveNumber <- FALSE  
        } 
      }
      if(input$parameter != "" && is.numeric(input$min) && is.numeric(input$max) && positiveNumber == TRUE)
      {
        if(input$min < input$max)
        {
          parameter<-isolate(input$parameter)
          min<-isolate(input$min)
          max<-isolate(input$max)
          
          myValues$parameters<-c(myValues$parameters,parameter)
          
          if (input$wholeNumber == TRUE)
          {
            myValues$wholeNumbers <- c(myValues$wholeNumbers, length(myValues$parameters))
            wholeNumbersBool <- "Whole Number"
          }
          else 
          {
            wholeNumbersBool <- "Decimal"  
          }

          if (input$analysisType != "Robustness")
          {
            baseline <- NA
            increment <- NA
          }
          else 
          {
            baseline <- isolate(input$baseline)
            increment <- isolate(input$robustnessIncrement)
            wholeNumbersBool <- "N/A"
          }
          
          myValues$mins<-c(myValues$mins,as.numeric(min))
          myValues$maxs<-c(myValues$maxs,as.numeric(max))
          myValues$baselines<-c(myValues$baselines,as.numeric(baseline))
          myValues$increments<-c(myValues$increments, as.numeric(increment))
          
          ###Decimal_or_Rounded <<- c(Decimal_or_Rounded, wholeNumbersBool)
          
          myValues$table <- rbind(isolate(myValues$table), cbind(parameter,min,max,increment,baseline, wholeNumbersBool))
          
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
      colnames(myValues$table) <- c("Parameter","Min","Max", "Increment", "Baseline", "Decimal/Whole Number")
      
      #This part changes only what the user sees on screen and not what gets downloaded in the settings file, as there is no need to know about rounding in the settings file.
      #forUserToSee <- cbind(myValues$table, c(Decimal_or_Rounded))
      #colnames(forUserToSee) <- c("Parameter","Min","Max", "Increment", "Baseline", "Decimal/Whole Number")
      myValues$table
      #forUserToSee #show the user the table
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

