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
parameters<-c()
mins<-c()
maxs<-c()


# Define UI for application that draws a histogram
ui <- fluidPage(
   
  useShinyjs(),
  
   # Application title
   h3("Generate Parameter Sets using Latin-Hypercube Sampling"),
   
   # LHC: Parameters
   # Number of Samples
   # Minimum for each parameter
   # Maximum for each parameter
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
       
        
        wellPanel(
          
          h4("Declare Parameters:"),
          
          textInput(inputId = "parameter",
                    label = "Parameter:",
                    value = ""),
          
          textInput(inputId = "min",
                    label = "Minimum:",
                    value = ""),
          
          textInput(inputId = "max",
                    label = "Maximum:",
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
      
      actionButton(inputId = "addToDB",
                   label = 'Add Experiment to Database')
      
      ,width = 5),
      
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
  
  
  observe({
    shinyjs::hide("lhc_sample")
    
    if(myValues$sampleGenerated==TRUE)
      shinyjs::show("lhc_sample")
  })

  myValues <- reactiveValues()
  myValues$sampleGenerated<-FALSE
  
  # New plot title
  #new_plot_title <- eventReactive(
  #  eventExpr = input$update_plot_title, 
  #  valueExpr = { toTitleCase(input$plot_title) },
  #  ignoreNULL = FALSE
  #)
  
  # Download file
  output$lhc_sample <- downloadHandler(
    filename = function() {
      paste0("lhc_sample.csv")
    },
    content = function(file) { 
      write_csv(data.frame(myValues$sample), path = file) 
      }
  )
  
  observeEvent(
    input$createSample,
    {
      #print(parameters)
      #print(mins)
      #print(maxs)
      myValues$sample<<-lhc_generate_lhc_sample(FILEPATH=NULL, parameters, input$numSamples, mins, maxs, input$algorithm)
      myValues$sampleGenerated<<-TRUE
      
      #print(head(myValues$sample))

    }
  )
  
  observeEvent(
    input$createSample,
    {
      
      print(head(myValues$sample))
      output$sample_header <- renderUI({ h4("Generated Sample:") })
      output$sample <- DT::renderDataTable(
        DT::datatable(data = myValues$sample, 
                      options = list(pageLength = 10, searching=FALSE), 
                      rownames = FALSE))
      
    }
  )
  
  observeEvent(
    input$clearParameter,
    {
      parameters<<-c()
      mins<<-c()
      maxs<<-c()
      myValues$sampleGenerated<-FALSE
      print(parameters)
      print(mins)
      print(maxs)
      myValues$table<-NULL
      updateTextInput(session, "parameter", value = "")     
      updateTextInput(session, "min", value = "")
      updateTextInput(session, "max", value = "")
    }
  )
  
  observeEvent(
    input$addParameter,
    {
    if(input$addParameter > 0){
      if(input$parameter != "" && input$min!="" && input$max!="")
      {
        print(paste(input$parameter,input$min,input$max))
        parameter<-isolate(input$parameter)
        min<-isolate(input$min)
        max<-isolate(input$max)
        #myValues$params <- c(isolate(myValues$params), parameter)
        parameters<<-c(parameters,parameter)
        print(parameters)
        mins<<-c(mins,as.numeric(min))
        maxs<<-c(maxs,as.numeric(max))
        print(mins)
        print(maxs)
        #myValues$mins <- c(isolate(myValues$mins), min)
        #myValues$maxs <- c(isolate(myValues$maxs), max)
        #myValues$mins <- c(isolate(myValues$mins), isolate(input$min))
        #myValues$maxs <- c(isolate(myValues$maxs), isolate(input$max))
        #myValues$table <- rbind(isolate(myValues$table), cbind(isolate(input$parameter),isolate(input$min),isolate(input$max)))
        myValues$table <- rbind(isolate(myValues$table), cbind(parameter,min,max))
        #print(myValues$table)
        #print(myValues$params)
        #print(myValues$mins)
        #print(myValues$maxs)
        updateTextInput(session, "parameter", value = "")     
        updateTextInput(session, "min", value = "")
        updateTextInput(session, "max", value = "")
      }
    }
      }
  )
 
  output$parameter_table<-renderTable({
    if(length(myValues$table)>1)
    {
      colnames(myValues$table) <- c("Parameter","Min","Max")
      myValues$table
    }
    
  })
  
    output$table_header <- renderUI({
    if(length(myValues$table)>1)
    {
      h4("Parameters Declared:")
    }
  })

  
  
   #output$distPlot <- renderPlot({
  #    # generate bins based on input$bins from ui.R
  #    x    <- faithful[, 2] 
  #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
  #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

