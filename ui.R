library(shiny)

fluidPage (theme = "bootstrap.css", 
        
  
headerPanel(tags$head(h4(strong("Knowledge Search and Similarity Scoring System"),align = "center" )), 
         
                        tags$head(tags$img(src="image.png",align = "right", height = "60px"))),
  br(),
  
  sidebarLayout(
    
    sidebarPanel( 
      
      actionButton("refresh", "Refresh",  class="btn btn-info"),br(), br(),
      
      radioButtons("sel","Select action", selected ="file1",
                   c("File Upload" = "file1","Insert Keywords" = "selection")),
                     
      
       fileInput("file1", 'Upload File',accept=c('text/csv','text/comma-separated-values',
                                                 'text/tab-separated-values','text/plain',
                                                 '.csv','.tsv')), 
      
      textInput("selection", h5(strong("Insert Keywords:")),
                  value = words),
     actionButton("update1", "Submit",  class="btn btn-info"),
     
      actionButton("update", "Generate",  class="btn btn-info"),
    br(), br(),
    
    br(),
    
    fluidRow( 
            column(3,
      checkboxGroupInput("check", 
                         label = h5(strong("Word List")), 
                         choices = cho,
                         selected = "")),
      column(9,
             actionButton("update2", "Add",  class="btn btn-info", align = "right"))),
    dateInput("date1",label = "From", Sys.Date()),
    dateInput("date2",label = "To", Sys.Date())),
 
       
    mainPanel(
      
      tabsetPanel(id = 1,
                  tabPanel(h5(strong("Text Input")), tableOutput("data1"))
                  ),
      
                  br(),
      
                  #hr(),
            #hr(), hr(), hr(), 
        #tabsetPanel(id = 2,
                  #tabPanel(h5(strong("Output text")), textOutput("data"))),
      
        #actionButton("update3", "Add to Checkbox",  class="btn btn-info"),
           br(), br(),
      tabsetPanel(id = 3,
                  tabPanel(h5(strong("Key words")), textOutput("data2"))),
     # downloadButton('downloadData', 'Download', class="btn btn-info"),
      actionButton("sear", "Search",  class="btn btn-info")
      )
    )
)
  

    
   
  



  

 