#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rhandsontable)
###############################################################################################
###############################################################################################
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("702的电费缴纳"),
   
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel( 
            #textInput(inputId = "date", label = "Date", value = "yyyy/mm/dd"),
            dateInput("date", Sys.Date(), label="date",format = "yyyy-mm-dd"), 
            numericInput("A", "A", 10, min = 0, max = 500),
            numericInput("B", "B", 10, min = 0, max = 500),
            numericInput("C", "C", 10, min = 0, max = 500),
            actionButton("runButton","Change Dataframes")
            ),
        
         
        # Main panel for displaying outputs ----
        mainPanel(  
            # Button
            # downloadButton("downloadData", "Download") ,
            # tableOutput("table")  
            
            tabsetPanel(
                tabPanel("OldTab", rHandsontableOutput('Old')),
                tabPanel("NewTab", DT::dataTableOutput("New"))
            ))
    )
)
###############################################################################################
###############################################################################################
# Define server logic required to draw a histogram
server <- function(input, output,session) {
    # output$table <- renderTable({ 
    #     data.frame(date = as.character(input$date),#as.character(as.Date(input$date,origin='1970-1-1'))
    #                A=input$A,
    #                B=input$B,
    #                C=input$C )  
    #     })
    # output$downloadData <- downloadHandler(
    #     filename = "record.csv" ,
    #     content = function(file) { write.csv(output$table, file, row.names = FALSE) }
    #     )
    values <- reactiveValues()
    output$Old <- renderRHandsontable({
        rhandsontable(
            
        
            )
    })
    
    observeEvent(input$runButton, {
        values$data <-  hot_to_r(rbind(input$Old,
                                 c(as.character(input$date), input$A, B=input$B, C=input$C)))
    })
    
    output$NewIris <- DT::renderDataTable({
        values$data
    })
    }
###############################################################################################
###############################################################################################
# Run the application 
shinyApp(ui = ui, server = server)
