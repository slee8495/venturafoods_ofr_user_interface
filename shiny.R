library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(lubridate)
library(shinyWidgets)
library(readxl)
library(janitor)

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
                            actionButton("upload_button", "Upload OFR Today's Data")
                          )
                 ),
                 tabPanel("Daily Update",
                          tabsetPanel(
                            tabPanel("Today's Data",
                                     fluidPage(
                                       titlePanel(
                                         div(class = "row",
                                             div(class = "col-sm-8", 
                                                 span("Today's Data"),
                                                 span(textOutput("current_date"), style = "margin-left: 20px; font-size: 20px;")
                                             )
                                         )
                                       ),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll; height: calc(100vh - 100px);',
                                           DTOutput("data2"))
                                     )
                            ),
                            tabPanel("User-Input",
                                     fluidPage(
                                       titlePanel("User-Input"),
                                       pickerInput("campus_filter", "Filter by Campus No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
                                       dateRangeInput("date_filter", "Filter by Shortage Date:",
                                                      start = Sys.Date() - 30, end = Sys.Date()),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll; height: calc(100vh - 100px);',
                                           DTOutput("data3"))
                                     )
                            )
                          )
                 )
)

###################### server ######################
server <- function(input, output, session) {
  rv <- reactiveValues(data1 = NULL, processed_data1 = NULL, processed_data2 = NULL)
  
  ofr_1st_data <- function(df) {
    df %>%
      janitor::clean_names() %>%
      dplyr::mutate(item_no = gsub("-", "", item_no)) %>%
      dplyr::mutate(shortage_date_2 = as.double(shortage_date)) %>%
      dplyr::mutate(ref = paste0(campus_no, "_", shortage_date_2, "_", item_no)) %>%
      dplyr::relocate(ref) %>%
      dplyr::mutate(across(ends_with("date"), as.Date)) %>%
      dplyr::select(-shortage_date_2)
  }
  
  ofr_2nd_data <- function(df) {
    df %>%
      dplyr::group_by(ref, campus_no, shortage_date, item_no) %>%
      dplyr::summarise(order_shortage_case_qty = sum(order_shortage_case_no, na.rm = TRUE), .groups = "drop") 
  }
  
  observeEvent(input$file1, {
    rv$data1 <- read_excel(input$file1$datapath)
    rv$processed_data1 <- ofr_1st_data(rv$data1)
    rv$processed_data2 <- ofr_2nd_data(rv$processed_data1)
    
    updatePickerInput(session, "campus_filter", choices = unique(rv$processed_data2$campus_no), selected = unique(rv$processed_data2$campus_no))
  })
  
  filtered_data2 <- reactive({
    data <- rv$processed_data2
    if (!is.null(input$campus_filter) && length(input$campus_filter) > 0) {
      data <- data %>% filter(campus_no %in% input$campus_filter)
    }
    if (!is.null(input$date_filter)) {
      data <- data %>% filter(shortage_date >= input$date_filter[1] & shortage_date <= input$date_filter[2])
    }
    data
  })
  
  observeEvent(input$upload_button, {
    output$data2 <- renderDT({
      datatable(rv$processed_data1, options = list(scrollX = TRUE, pageLength = 10))
    })
    output$data3 <- renderDT({
      datatable(filtered_data2(), options = list(scrollX = TRUE, pageLength = 10))
    })
  })
  
  output$current_date <- renderText({
    format(Sys.Date(), "%Y-%m-%d")
  })
}

shinyApp(ui = ui, server = server)
