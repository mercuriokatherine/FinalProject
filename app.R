library(shiny)
library(ggplot2)

ui <- fluidPage(
  # *Input() functions,
    titlePanel("Arson Data for Juveniles from 2012-2015"),
    
    
    # Create a new Row in the UI for selectInputs
    fluidRow(
      column(4,selectInput("Year","Year:", c("All",
                           unique(as.character(full$Year))))),
      
      column(4, selectInput("STATE","STATE:", c("All",
                           unique(as.character(full$STATE))))),
             
      column(4, selectInput("AGE","AGE:", c("All",
                           unique(as.character(sort((full$AGE))))))),

             
    fluidRow(DT::dataTableOutput("full"))))
    
server <- function(input, output) {
    output$full <- DT::renderDataTable(DT::datatable({
      data <- full
      if (input$Year != "All") {
        data <- data[data$Year == input$Year]
      }
      if (input$STATE != "All") {
        data <- data[data$STATE == input$STATE]
      }
      if (input$AGE != "All") {
        data <- data[data$AGE == input$AGE]
      }
      data
    }))
    
  }
shinyApp(server = server, ui = ui)