library(shiny)
library(ggplot2)

dt =  read.csv('/Volumes/兴趣/electricity_pay/epay/elec_use.csv')
ui <- fluidPage(
  
  titlePanel("702的电费缴纳⚡"),
  #tags$img(height = 100, width = 100, src = "Logo"),
  sidebarLayout(
    sidebarPanel( 
      textInput(inputId = "date", label = "日期",  value = "yyyy/m/d"),
      numericInput(inputId = "A", label = "A室电表数", value = 3000.0, min = 0, max = 10000),
      numericInput(inputId = "B", label = "B室电表数", value = 3000.0, min = 0, max = 10000),
      numericInput(inputId = "C", label = "C室电表数", value = 3000.0, min = 0, max = 10000), 
      actionButton("Add", "Add",style = "margin-top:10px; margin-left:0px; color: #fff; background-color: #5b89f7; border-color: #5b89f7"),  
      actionButton("Del", "Del",style = "margin-top:10px; margin-left:100px; color: #fff; background-color: #5b89f7; border-color: #5b89f7"),
      actionButton("Save", "Save",style = "margin-top:10px; margin-left:100px; color: #fff; background-color: #5b89f7; border-color: #5b89f7") ,
      tags$div(
        HTML(paste("*press", tags$span(style="border: 1px solid #ccc; border-radius: 3px;
                                       opacity: 0.85; font:13px sans-serif ", "Add"), 
                   "to check the bill,", tags$span(style="border: 1px solid #ccc; border-radius: 3px;
                                       opacity: 0.85; font:13px sans-serif ", "Del"), 
                   "to delete the recent record, and", tags$span(style="border: 1px solid #ccc; border-radius: 3px;
                                       opacity: 0.85; font:13px sans-serif ", "Save"), 
                   "to save historicial data.", 
                   sep = " "))
      ), 
      # h5("*press Add to check the bill, Del to delete the recent record, 
      # Save to save historicial data")
      
      #h5("*press Add to check the bill, Del to delete the recent record, 
      # Save to save historic data", style = 'margin-left:0px; color:blue')  
      ),
    
    mainPanel(tabsetPanel(
      tabPanel("本次应缴费", tableOutput(outputId = "result"),
               textOutput(outputId = "rule")
      ),
      tabPanel("历史电表数据", tableOutput(outputId = "table"), 
               plotOutput(outputId = "chart", width = "400px", height = "300px")
      ) )
      )   
    )
)


server <- function(input, output){ 
  rv <- reactiveValues(
    df = read.csv('/Volumes/兴趣/electricity_pay/epay/elec_use.csv')
  )
  observeEvent(input$Add, {
    rv$df <- rbind(rv$df , data.frame(Date = input$date, 
                                     A = input$A, 
                                     B = input$B, 
                                     C = input$C
                                     ))
  }) 
  observeEvent(input$Del, {
    rv$df <- rv$df[-nrow(rv$df),] 
    
  })
  
  observeEvent(input$Save, { 
    write.csv(rv$df, file = "/Volumes/兴趣/electricity_pay/epay/elec_use.csv",row.names = FALSE)
  })
  
  
     
  output$result <- renderTable({
    cbind(Date = rv$df[nrow(rv$df),1],(rv$df[nrow(rv$df),-1] -rv$df[nrow(rv$df)-1,-1])*0.64+c(10,20,40) )
  })   
  
  output$rule <- renderText({
    "*缴费标准：空调费用 ( 0.64元/度 ) + 基础费用 ( A 10元 / B 20元 / C 40元 )"
  })
    
  output$table <- renderTable({
      rv$df 
    })   
  
  output$chart <- renderPlot( { 
      plot(as.Date(rv$df[,1]),rv$df[,2]-3952.50, type = "l" , col = 2,
           xlab="date",ylab="electricity usage(from 2021/9/17) ",
           main = "空调用电量累计", family='STXihei')
      lines(as.Date(rv$df[,1]),rv$df[,3]-7495.20 , col = 3)
      lines(as.Date(rv$df[,1]),rv$df[,4]-5805.70 , col = 4)
      legend("topleft", legend = LETTERS[1:3], lwd = 1, col=c(2,3,4) )  
  })   
  
}

shinyApp(ui = ui, server = server)
