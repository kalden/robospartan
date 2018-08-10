#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
parameters<-c()
mins<-c()
maxs<-c()

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Generate Parameter Sets using Latin-Hypercube Sampling"),
   
   # LHC: Parameters
   # Number of Samples
   # Minimum for each parameter
   # Maximum for each parameter
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        wellPanel(
          
          textOutput("parameter_list"),
          
          textInput(inputId = "parameter",
                    label = "Parameter:",
                    value = ""),
          
          textInput(inputId = "min",
                    label = "Minimum:",
                    value = ""),
          
          textInput(inputId = "max",
                    label = "Maximum:",
                    value = "")
          
          ,textInput('txt','','Text')
          ,actionButton('add','add')
          ,verbatimTextOutput('list'),
          
          actionButton(inputId = "addParameter",
                       label = "Add Parameter")
        ),
         
        sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         
         numericInput(inputId = "numSamples",
                      label = "Number of Samples",
                      value = 500),
         
         selectInput(inputId = "algorithm",
                     label = "Sampling Algorithm",
                     choices = c("normal","optimal"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  myValues <- reactiveValues()
  observe({
    if(input$add > 0){
      myValues$dList <- c(isolate(myValues$dList), isolate(input$txt))
    }
  })
  output$list<-renderPrint({
    myValues$dList
  })

  
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

