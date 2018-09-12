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
#source("/home/kja505/Documents/spartanDB/R/generate_parameter_sample_in_db.R")
#source("/home/kja505/Documents/spartanDB/R/database_utilities.R")
parameters<-c()
mins<-c()
maxs<-c()


# Define UI for application that draws a histogram
ui <- fluidPage(
   
  useShinyjs(),
  
   # Application title
   h3("Generate Parameter Sets using different sampling techniques"),
   
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
                    value = ""),
          
          numericInput(inputId = "min",
                    label = "Minimum:",
                    value = ""),
          
          numericInput(inputId = "max",
                    label = "Maximum:",
                    value = ""),
          
          numericInput(inputId = "robustnessIncrement",
                       label = "Increment",
                       value = ""),
          
          numericInput(inputId = "baseline",
                       label = "Calibrated Baseline",
                       value = ""),
          
          #,textInput('txt','','Text')
          actionButton(inputId = "addParameter",
                        label = 'Add Parameter'),
          actionButton(inputId = "clearParameter",
                       label = 'Clear All')
          #,textOutput('list'),
          
          #actionButton(inputId = "addParameter2",
          #             label = "Add Parameter2")
        ),
        
        #wellPanel(
        #  
        #  htmlOutput("table_header2"),
        #  div(tableOutput("parameter_table2"),style="font-size:90%")
        #),
         
        #htmlOutput("parameter_list"),
        
       wellPanel(
         
         h4("Settings:"),
         
         numericInput(inputId = "numSamples",
                      label = "Number of Samples",
                      value = 500),
         
         numericInput(inputId = "numCurves",
                      label = "Number of Curves",
                      value = 1),
         
         selectInput(inputId = "algorithm",
                     label = "Sampling Algorithm",
                     choices = c("normal","optimal"))
         
      ),
      
       actionButton(inputId = "createSample",
                   label = 'Create Sample'),
        br(),
      
        actionButton(inputId = "createARGoSFiles",
                   label = 'Create ARGoS Files'),
        br(),
      
      wellPanel(
        
        textInput(inputId = "description",
                  label = "Experiment Description:",
                  value = ""),
        
        actionButton(inputId = "addToDB",
                  label = 'Add Experiment to Database')
      ),
      
      width = 5),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        # Output the parameters as they are entered:
        htmlOutput("table_header"),
        div(tableOutput("parameter_table"),style="font-size:90%"),
        
        hr(),
        
        # Now output the sample once generated, with a download button
        htmlOutput("sample_header"),
        div(DT::dataTableOutput(outputId = "sample"),style="font-size:90%"),
        downloadButton(outputId = "lhc_sample", label = "Download data"),
        
        width=7
      )
   )
   
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  myValues <- reactiveValues()
  myValues$sampleGenerated<-FALSE
  shinyjs::hide("createSample") #initialise the button to be hidden until parameters are added
  #### Hide the sample table if not generated yet
  observe({
    shinyjs::hide("lhc_sample")
    shinyjs::show("robustnessIncrement")
    
    if(myValues$sampleGenerated==TRUE)
      shinyjs::show("lhc_sample")
  })
 
  observeEvent(
    input$analysisType,
    {   
        print(input$analysisType)
        #Robustness analysis technique - parameter requires param, min, max and increment. No settings required.
        if(input$analysisType == "Robustness"){
          shinyjs::show("robustnessIncrement")
          shinyjs::hide("numSamples")
          shinyjs::hide("algorithm")
          shinyjs::hide("numCurves") 
          shinyjs::show("baseline")
        }  
        #Latin-Hypercube analysis technique - parameter requires param, min, max. Settings are sample number and algorithm type.
        else if(input$analysisType == "Latin-Hypercube"){
          shinyjs::hide("robustnessIncrement")
          shinyjs::show("numSamples")
          shinyjs::show("algorithm")
          shinyjs::hide("numCurves") 
          shinyjs::hide("baseline")
        }  
        #eFAST analysis technique - parameter requires requires param, min, max. Settings require additional 
        else{
          shinyjs::hide("robustnessIncrement")
          shinyjs::show("numSamples")
          shinyjs::hide("algorithm")
          shinyjs::show("numCurves") 
          shinyjs::hide("baseline")
          
        } 
    })
  
  # Download generated sample
  output$lhc_sample <- downloadHandler(
    filename = function() {
      paste0("lhc_sample.csv")
    },
    content = function(file) { 
      write_csv(data.frame(myValues$sample), path = file) 
      }
  )
  
  #### Action when Create Sample is pressed
  observeEvent(
    input$createSample,
    {
      if(input$analysisType == "Latin-Hypercube" && is.integer(input$numSamples)) 
      {
        myValues$sample <<- lhc_generate_lhc_sample(FILEPATH=NULL, parameters, input$numSamples, mins, maxs, input$algorithm)
        myValues$sampleGenerated<<-TRUE
        
        #print(head(myValues$sample))
        output$sample_header <- renderUI({ h4("Generated Sample:") })
        output$sample <- DT::renderDataTable(
          DT::datatable(data = myValues$sample, 
                        options = list(pageLength = 10, searching=FALSE), 
                        rownames = FALSE))
      }
      
      else if(input$analysisType == "eFAST" && is.integer(input$numSamples))
      {
        print("eFAST can work")
      }
      
      else if(input$analysisType == "Robustness")
      {
        myValues$sample <<- oat_parameter_sampling(FILEPATH = NULL, parameters, input$baseline, mins, maxs, input$robustnessIncrement, write_csv = FALSE, return_sample = TRUE)
        print("Robustness can work")
      }  
      
      else #this case gets called when latin-hypercube or eFAST have incorrect number of samples
      {
        showModal(modalDialog(
          title = "Incorrect number of samples",
          "Number of samples should be numeric"))
      }
      
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
          rmysql.settingsfile<-"/home/kja505/Documents/sql_settings/newspaper_search_results.cnf"
          rmysql.db<-"spartan_ppsim"
          dblink<-dbConnect(MySQL(),default.file=rmysql.settingsfile,group=rmysql.db)
          experiment_id <- setup_experiment(dblink,"LHC","2018-08-22","robospartan test sample 3")
        
          add_parameter_set_to_database(dblink, myValues$sample,experiment_id)
        
          showModal(modalDialog(
            title = "Important message",
            paste("Experiment ID: ",experiment_id,sep="")))
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
  
  
  #### Action when Clear Parameter is pressed
  observeEvent(
    input$clearParameter,
    {
      shinyjs::hide("createSample") #Make it so the create sample is once again hidden from the user
      parameters<<-c()
      mins<<-c()
      maxs<<-c()
      myValues$sampleGenerated<-FALSE
    
      myValues$table<-NULL
      updateTextInput(session, "parameter", value = "")     
      updateTextInput(session, "min", value = "")
      updateTextInput(session, "max", value = "")
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
    if(input$addParameter > 0){
  
      if(input$parameter != "" && is.numeric(input$min) && is.numeric(input$max))
      {
        if(input$min < input$max)
        {
          parameter<-isolate(input$parameter)
          min<-isolate(input$min)
          max<-isolate(input$max)
          
          parameters<<-c(parameters,parameter)
          
          mins<<-c(mins,as.numeric(min))
          maxs<<-c(maxs,as.numeric(max))
          myValues$table <- rbind(isolate(myValues$table), cbind(parameter,min,max))
  
          updateTextInput(session, "parameter", value = "")     
          updateTextInput(session, "min", value = "")
          updateTextInput(session, "max", value = "")
          
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
        # Either parameter is blank, or min/max are not numeric
        updateTextInput(session, "min", value = "")
        updateTextInput(session, "max", value = "")
        showModal(modalDialog(
          title = "Error in Input",
          "Either parameter name is blank, or minimum or maximum are not numeric"))
      }
    }
      }
  )
 
  #### Render the parameter table each time the add parameter button is pressed
  output$parameter_table<-renderTable({
    if(length(myValues$table)>1)
    {
      colnames(myValues$table) <- c("Parameter","Min","Max")
      myValues$table
    }
    
  })
  
  #### Change the parameter table header when a sample is generated
    output$table_header <- renderUI({
    if(length(myValues$table)>1)
    {
      h4("Parameters Declared:")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

