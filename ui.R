library(shiny)


shinyUI(fluidPage(
    
    # Application title
    headerPanel(h5(tags$b(("UPDATE LME SHEET")))),
    
    
    fluidRow(
      column(3,
             radioButtons("sel","Select action", selected ="ins",
                          c("Insert" = "ins","Edit Row" = "cha",
                            "Delete Last Row" = "del-las",
                            "Delete a Row"   = "del-a")),
             numericInput("R", "Row number", 1),
             dateInput("date1",label = "Trade Date", Sys.Date()+1),
             dateInput("date2", label = "Expiry Date", Sys.Date()+2)
      ),
      column(3,
             numericInput("num2",label = "Strike Price", 2000),
             numericInput("s", label = "Underlying Price", 2041.5),
             numericInput("num6", label = "Assign_3MLME", 1900),
             numericInput("num4", label = "Delta_Eq", 5),
             numericInput("vol", label = "Volatility", 0.23)),
      column(3, 
             numericInput("int", label = "Interest Rate", 0.1),
             selectInput("select1", label = "Option", 
                         choices = list("CALL", "PUT"),
                         selected = "CALL"),
             selectInput("select2", label = "Position", 
                         choices = list("BUY","SELL"),
                         selected = "SELL"),
             selectInput("checkGroup",label = "Commodity",choices = list("LEAD","ALUMINIUM","COPPER"),selected="LEAD"),
             numericInput("num5", label = "Lot size",10)),
      
      column(3,
             selectInput("text", label = "Broker Name",choices = list("TTM","IFCS","JEFF"),selected="JEFF"),
             numericInput("num1", label = "Current_3MLME", 1500),
             numericInput("div", label = "Dividend", 0.1),   
             actionButton("action",label = tags$b("SUBMIT")),
             actionButton("refresh", label = tags$b("Refresh")))
      
    ),
    mainPanel(
      downloadButton('downloadData', 'Download'),
      tabsetPanel(id="tab1",
                  tabPanel(tags$b("LME Sheet"),tableOutput("DATA"))
                  
                  
                  
      )
    )
  ))
  
  