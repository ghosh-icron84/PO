library(shiny)

#options(shiny.maxRequestSize = 1000*1024^2)

shinyServer(function(input, output, session) {
 
  ## Input text of keywords##
  interms <- reactive({
    input$update1
    isolate({ input$selection })
  }) 
  ## Input text of File ##
  fileterms <- reactive({
   input$update1
  isolate ({
    
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  
    read.delim(inFile$datapath, header = FALSE, sep="\t")
             
      })
  }) 
  
## Text Analysis algorithm ##  
  terms <- reactive({
   input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTerm(input$selection)
      })
    })
  }) 
  
  terms1 <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTerm(fileterms())
      })
    })
  })
## Checkbox updating ##
   observe({
     if(input$sel == "selection"){
    ch <- names(terms()) 
    cho <- as.vector(ch, mode = "character")
    if(input$update == 1)
    { updateCheckboxGroupInput(session, "check", choices = cho,
                               selected = "" )  } }
    else if (input$sel == "file1"){
      ch <- names(terms1()) 
    cho <- as.vector(ch, mode = "character")
    if(input$update == 1)
    { updateCheckboxGroupInput(session, "check", choices = cho,
                               selected = "" )  } }  
      
      })
   
## Adding selected keywords from checkbox to list ##  
  addterms <- reactive ({        
    input$update2
    isolate({ input$check })
  })
 
  ## If action is file upload, then fileinput, else words input ## 
  observe ({
  if (input$sel == "file1") 
  
  output$data1 <- renderTable({
    fileterms()
  })
 else  if (input$sel == "selection") 
 
  output$data1 <- renderText({
  interms()
})
  })
  
  #output$data <- renderText({
   # v <- terms()
    #x <- names(v) 
  #})
  
 output$data2 <- renderText ({
   addterms()
  })
 observe({
   input$refresh
   isolate
   { invalidateLater(1000 * 60 * 5, session)}
   })
   
   
  ## Download words and save to csv ## 
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$check, '.csv', sep='') },
    content = function(file) {
      write.csv(addterms(), file = "Keywords.csv", row.names=FALSE)
    }
  )  
  
  
}) 

