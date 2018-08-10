require(shiny)
runApp(list(
  ui=fluidPage(
    h1('Example')
    ,textInput('txt','','Text')
    ,actionButton('add','add')
    ,verbatimTextOutput('list')
  )
  
  ,server=function(input,output,session) {
    myValues <- reactiveValues()
    observe({
      if(input$add > 0){
        myValues$dList <- c(isolate(myValues$dList), isolate(input$txt))
      }
    })
    output$list<-renderPrint({
      myValues$dList
    })
  }
  
))