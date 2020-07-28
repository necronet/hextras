library(shiny)
source('preprocessing.R', local = TRUE)

VALID_MIME <- c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", "application/vnd.ms-excel")

ui <- fluidPage( 
    titlePanel("Generador de horas extras"),
    mainPanel(
      fluidRow(column(12,
                      fileInput("file1", "Archivo excel de origen",
                                multiple = FALSE,
                                accept = VALID_MIME))),
      fluidRow(
        column(12, 
               tableOutput("contents"))
        )
    )
  )


server <- function(input, output) {
  output$contents <- renderTable({
    
    req(input$file1)
    
    tryCatch(
      {
        processedData <- processTimeClock(input$file1$datapath, strickColumns=T) %>% 
                          storeInDatabase() %>% timeTable()
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        print("This is an error")
        stop(safeError(e))
      }
    )
    return(processedData)
    
  })
}

shinyApp(ui, server)
