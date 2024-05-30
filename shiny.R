library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(lubridate)
library(shinyWidgets)




#### Shiny ####
##################### ui ######################
ui <- navbarPage("OFR User Input Data Base App", 
                 theme = shinythemes::shinytheme("flatly"),
                 tabPanel("Main Page", 
                          fluidPage(
                            titlePanel(
                              div(class = "row", 
                                  div(class = "col-sm-8",
                                      "Data Upload and Manipulation App"), 
                                  div(class = "col-sm-4",
                                      img(src = "VenturaFoodsLogo.png", height = "60px", align = "right"))
                              )
                            ),
                            tags$head(tags$link(rel = "shortcut icon", href = "www/VenturaFoodsLogo.png")),
                            
                            fileInput("file1", "Choose Excel File", accept = c(".xlsx")),
                            tableOutput("data1")
                          )
                 )
)


###################### server ######################
server <- function(input, output) {
  rv <- reactiveValues(data1 = NULL)
  
  observeEvent(input$file1, {
    rv$data1 <- read_excel(input$file1$datapath)
  })
  
  output$data1 <- renderTable({
    rv$data1
  })
}

shinyApp(ui = ui, server = server)

