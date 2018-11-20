#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)#

measures <<- c()

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Table Tester"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        wellPanel(id = "measuresWell",
                  h4("Measures:"),
                  textInput(inputId = "measures",
                            label = "Choose Your Measures:",
                            value = ""),
                  actionButton(inputId = "addMeasure", label = "Add to Measures"),
                  actionButton(inputId = "clearMeasures", label = "Clear All Measures"),
                  hr(),
                  textInput(inputId = "measureScale",
                            label = "Choose Your Measure Scales:",
                            value = ""),
                  actionButton(inputId = "addMeasureScale", label = "Add to Measure scales"),
                  actionButton(inputId = "clearMeasureScales", label = "Clear All Measure Scales")
                  
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        div(tableOutput("measures_table"))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  measureValues <- reactiveValues()
  measureValues$table <- matrix(c("", ""), ncol = 1, byrow = TRUE)
  
  observeEvent(input$addMeasure,
               {
                 if (input$measures != ""){
                   measures <<- c(measures, input$measures)
                   print(measures)
                   updateTextInput(session, inputId = "measures", value = "")
                   #measureValues$table <- cbind(measures)
                   rownames(measureValues$table) <- c("MEASURES", "MEASURE SCALES")
                  
                   #columnNames <<- c(columnNames, "Measure Name")
                   output$measures_table <- renderTable(measureValues$table, rownames = TRUE, colnames = FALSE, spacing = "m", striped = TRUE)
                   #shinyjs::enable("clearMeasures")
                 }
               })

}

# Run the application 
shinyApp(ui = ui, server = server)

