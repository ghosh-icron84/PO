library(shiny)
source('function.R', local = TRUE)

shinyServer(function(input,output) {
  
  mydataL <- reactive({
    Names<-c("TRADE.DATE","EXP.DATE","COMMODITY","OPTION","POSITION","LOT","ASSIGN_3MLME","STRIKE.PRICE","UNDERLYING_PRICE","DELTA_EQ","BROKER","VOLATILITY","INT_RATE","DIVIDEND","CURRENT_3MLME","TIMES","VARA","VARB","VARC","VARD","VARE","VARF","VARG","VARH","a","b","CALL_PRICE","PUT_PRICE")
    if(input$action == 0)
      return(mydataRead())
    else
      isolate({
        orig.date1<-as.Date(input$date1,origin = "1900-01-01")
        orig.date2<-as.Date(input$date2,origin = "1900-01-01")
        new.date1<-as.character(format(orig.date1, format=c("%Y-%m-%d")))
        new.date2<-as.character(format(orig.date2, format=c("%Y-%m-%d")))
        t <- TIMES
        vara <- VARA
        varb <- VARB
        varc <- VARC
        vard <- VARD
        vare <- VARE
        varf <- VARF
        varg <- VARG
        varh <- VARH
        aa <- a
        bb <- b
        cp <- CALL_PRICE 
        pp <- PUT_PRICE
        t = (-1)*(as.numeric(difftime(Sys.Date(),as.POSIXlt(input$date2),units="days")))/365
        varb = input$vol*(sqrt(t))
        vara = t*(input$int-input$div+((input$vol*input$vol)/2))
        varc = ((log((input$s/input$num2),base = exp(1)))+vara)/varb
        vard = varc+((-1)*varb)
        vare = pnorm(varc,mean=0,sd=1)
        varf = pnorm(vard,mean=0,sd=1)
        aa = pnorm((-1*vard),mean=0,sd=1)
        bb = pnorm((-1*varc),mean=0,sd=1)
        varg = input$num2*(exp((-1)*input$int*t))
        varh = input$s*(exp((-1)*input$div*t))
        cp = (varh*vare)-(varg*varf)
        pp = (varg*aa)-(varh*bb)
        newentry<-list(new.date1,new.date2,input$checkGroup,input$select1,input$select2,input$num5,input$num6,input$num2,input$s,input$num4,input$text,input$vol,input$int,input$div,input$num1,t,vara,varb,varc,vard, vare,varf,varg,varh,aa,bb,cp,pp)
        
        if (input$sel == "ins") 
          newRow(mydataRead(),newentry,Names)
        if (input$sel == "cha")
          changeRow(mydataRead(),newentry,input$R)
        if (input$sel == "del-las")
          deleteLastRow(mydataRead())
        if (input$sel == "del-a")
          deleteARow(mydataRead(),input$R)
        di<-mydataRead()
        return(di)
      })
     })
  observe({
    if(input$refresh==0) return(NULL)
    else
    isolate({
  
      invalidateLater(10000, session)
    }) 
  }) 
  output$DATA<- renderTable({
    mydataL()
  })
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$sel, '.csv', sep='') },
    content = function(file) {
      write.csv(mydataL(), file, row.names=FALSE)
    }
  )
  
  
})
