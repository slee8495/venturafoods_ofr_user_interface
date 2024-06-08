library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(lubridate)
library(shinyWidgets)
library(readxl)
library(janitor)
library(writexl)

source("data.R")

# Load initial data from the saved files or from the initial processing
load_data <- function() {
  list(
    processed_data1 = ofr_1st_data,
    processed_data2 = ofr_2nd_data %>% mutate(reason_code = "", comment = "", submitted_date = Sys.Date())
  )
}

data <- load_data()

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
                            div(style = "color: blue; margin-top: 10px;", "Only an authorized user can initially upload the data once per day.")
                          )
                 ),
                 tabPanel("User Input Dashboard",
                          tabsetPanel(
                            tabPanel("Database",
                                     fluidPage(
                                       titlePanel(
                                         div(class = "row",
                                             div(class = "col-sm-8",
                                                 span("Database"),
                                                 br(),
                                                 span(textOutput("current_date"), style = "margin-left: 20px; font-size: 20px;")
                                             )
                                         )
                                       ),
                                       fluidRow(
                                         column(2, pickerInput("db_campus_filter", "Campus No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE))),
                                         column(2, pickerInput("db_item_no_filter", "Item No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE))),
                                         column(2, dateRangeInput("db_date_filter", "Shortage Date:", start = NULL, end = NULL)),
                                         column(2, dateRangeInput("db_uploaded_date_filter", "Database Uploaded Date:", start = NULL, end = NULL))
                                       ),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll; height: calc(100vh - 100px);',
                                           DTOutput("data2"))
                                     )
                            ),
                            tabPanel("User-Input",
                                     fluidPage(
                                       titlePanel("User-Input"),
                                       fluidRow(
                                         column(2, actionButton("save_button", "Save", style = "background-color: lightblue; color: black;")),
                                       ),
                                       fluidRow(
                                         column(2, pickerInput("campus_filter", "Campus No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE))),
                                         column(2, pickerInput("item_no_filter", "Item No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE))),
                                         column(2, dateRangeInput("date_filter", "Shortage Date:", start = NULL, end = NULL)),
                                         column(2, dateRangeInput("uploaded_date_filter", "Database Uploaded Date:", start = NULL, end = NULL))
                                       ),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll; height: calc(100vh - 100px);',
                                           DTOutput("data3"))
                                     )
                            )
                          )
                 ),
                 tabPanel("Master Database",
                          fluidPage(
                            titlePanel("Master Database"),
                            fluidRow(
                              column(2, downloadButton("download_rds_button", "Download as .rds", style = "background-color: coral; color: black;")),
                              column(2, downloadButton("download_xlsx_button", "Download as .xlsx", style = "background-color: lightblue; color: black;")),
                              column(2, downloadButton("download_csv_button", "Download as .csv", style = "background-color: lightblue; color: black;"))
                            ),
                            br(),
                            fluidRow(
                              column(2, pickerInput("master_campus_filter", "Campus No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE))),
                              column(2, pickerInput("master_item_no_filter", "Item No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE))),
                              column(2, dateRangeInput("master_date_filter", "Shortage Date:", start = NULL, end = NULL)),
                              column(2, dateRangeInput("master_uploaded_date_filter", "Database Uploaded Date:", start = NULL, end = NULL))
                            ),
                            div(style = 'overflow-x: scroll; overflow-y: scroll; height: calc(100vh - 100px);',
                                DTOutput("master_data"))
                          )
                 ),
                 tags$head(
                   tags$style(HTML("
                                   #data3 table.dataTable tr td:nth-child(8),
                                   #data3 table.dataTable tr td:nth-child(9),
                                   #data3 table.dataTable tr td:nth-child(10) {
                                     background-color: lightgreen !important;
                                   }
                                   "))
                 )
)

###################### server ######################
server <- function(input, output, session) {
  # Load backend data on session start
  rv <- reactiveValues(
    processed_data1 = data$processed_data1,
    processed_data2 = data$processed_data2,
    user_input_backend_data = data$processed_data2, # Initialize with processed_data2
    master_data = left_join(data$processed_data1, data$processed_data2 %>% select(ref, reason_code, comment, submitted_date), by = "ref") # Initialize master data
  )
  
  # Update filters on session start
  observe({
    updatePickerInput(session, "campus_filter", choices = unique(rv$processed_data2$campus_no), selected = unique(rv$processed_data2$campus_no))
    updatePickerInput(session, "item_no_filter", choices = unique(rv$processed_data2$item_no), selected = unique(rv$processed_data2$item_no))
    updateDateRangeInput(session, "date_filter", start = min(rv$processed_data2$shortage_date, na.rm = TRUE), end = max(rv$processed_data2$shortage_date, na.rm = TRUE))
    updateDateRangeInput(session, "uploaded_date_filter", start = min(rv$processed_data2$database_uploaded_date, na.rm = TRUE), end = max(rv$processed_data2$database_uploaded_date, na.rm = TRUE))
    
    updatePickerInput(session, "master_campus_filter", choices = unique(rv$master_data$campus_no), selected = unique(rv$master_data$campus_no))
    updatePickerInput(session, "master_item_no_filter", choices = unique(rv$master_data$item_no), selected = unique(rv$master_data$item_no))
    updateDateRangeInput(session, "master_date_filter", start = min(rv$master_data$shortage_date, na.rm = TRUE), end = max(rv$master_data$shortage_date, na.rm = TRUE))
    updateDateRangeInput(session, "master_uploaded_date_filter", start = min(rv$master_data$database_uploaded_date, na.rm = TRUE), end = max(rv$master_data$database_uploaded_date, na.rm = TRUE))
    
    updatePickerInput(session, "db_campus_filter", choices = unique(rv$processed_data1$campus_no), selected = unique(rv$processed_data1$campus_no))
    updatePickerInput(session, "db_item_no_filter", choices = unique(rv$processed_data1$item_no), selected = unique(rv$processed_data1$item_no))
    updateDateRangeInput(session, "db_date_filter", start = min(rv$processed_data1$shortage_date, na.rm = TRUE), end = max(rv$processed_data1$shortage_date, na.rm = TRUE))
    updateDateRangeInput(session, "db_uploaded_date_filter", start = min(rv$processed_data1$database_uploaded_date, na.rm = TRUE), end = max(rv$processed_data1$database_uploaded_date, na.rm = TRUE))
  })
  
  # Reactive data filtering with checks for empty data frames and presence of 'ref' column
  filtered_data2 <- reactive({
    data <- rv$processed_data2
    if (nrow(data) == 0 || !"ref" %in% colnames(data)) return(data)
    if (!is.null(input$campus_filter) && length(input$campus_filter) > 0) {
      data <- data %>% filter(campus_no %in% input$campus_filter)
    }
    if (!is.null(input$item_no_filter) && length(input$item_no_filter) > 0) {
      data <- data %>% filter(item_no %in% input$item_no_filter)
    }
    if (!is.null(input$date_filter)) {
      data <- data %>% filter(shortage_date >= input$date_filter[1] & shortage_date <= input$date_filter[2])
    }
    if (!is.null(input$uploaded_date_filter)) {
      data <- data %>% filter(database_uploaded_date >= input$uploaded_date_filter[1] & database_uploaded_date <= input$uploaded_date_filter[2])
    }
    data
  })
  
  filtered_master_data <- reactive({
    data <- rv$master_data
    if (nrow(data) == 0 || !"ref" %in% colnames(data)) return(data)
    if (!is.null(input$master_campus_filter) && length(input$master_campus_filter) > 0) {
      data <- data %>% filter(campus_no %in% input$master_campus_filter)
    }
    if (!is.null(input$master_item_no_filter) && length(input$master_item_no_filter) > 0) {
      data <- data %>% filter(item_no %in% input$master_item_no_filter)
    }
    if (!is.null(input$master_date_filter)) {
      data <- data %>% filter(shortage_date >= input$master_date_filter[1] & shortage_date <= input$master_date_filter[2])
    }
    if (!is.null(input$master_uploaded_date_filter)) {
      data <- data %>% filter(database_uploaded_date >= input$master_uploaded_date_filter[1] & database_uploaded_date <= input$master_uploaded_date_filter[2])
    }
    data
  })
  
  filtered_db_data <- reactive({
    data <- rv$processed_data1
    if (nrow(data) == 0 || !"ref" %in% colnames(data)) return(data)
    if (!is.null(input$db_campus_filter) && length(input$db_campus_filter) > 0) {
      data <- data %>% filter(campus_no %in% input$db_campus_filter)
    }
    if (!is.null(input$db_item_no_filter) && length(input$db_item_no_filter) > 0) {
      data <- data %>% filter(item_no %in% input$db_item_no_filter)
    }
    if (!is.null(input$db_date_filter)) {
      data <- data %>% filter(shortage_date >= input$db_date_filter[1] & shortage_date <= input$db_date_filter[2])
    }
    if (!is.null(input$db_uploaded_date_filter)) {
      data <- data %>% filter(database_uploaded_date >= input$db_uploaded_date_filter[1] & database_uploaded_date <= input$db_uploaded_date_filter[2])
    }
    data
  })
  
  # Render Data Tables
  output$data2 <- renderDT({
    datatable(filtered_db_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$data3 <- renderDT({
    datatable(filtered_data2(), editable = list(target = "cell", columns = c(8, 9, 10)), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$master_data <- renderDT({
    datatable(filtered_master_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Observe cell edits and update reactive data
  observeEvent(input$data3_cell_edit, {
    info <- input$data3_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    
    if (j == 7) {  # Column index for 'reason_code'
      rv$processed_data2[i, "reason_code"] <<- v
    } else if (j == 8) {  # Column index for 'comment'
      rv$processed_data2[i, "comment"] <<- v
    } else if (j == 9) {  # Column index for 'submitted_date'
      rv$processed_data2[i, "submitted_date"] <<- as.Date(v)
    }
  })
  
  # Save changes from User-Input to User-Input Backend Data and update Master Database
  observeEvent(input$save_button, {
    if (nrow(rv$processed_data2) > 0 && "ref" %in% colnames(rv$processed_data2)) {
      # Merge User-Input data to backend data
      updated_backend_data <- rv$user_input_backend_data %>%
        filter(!ref %in% filtered_data2()$ref) %>%
        bind_rows(filtered_data2())
      
      # Update reactive value
      rv$user_input_backend_data <- updated_backend_data
      
      # Update master data with the new user input
      rv$master_data <- left_join(rv$processed_data1, rv$user_input_backend_data %>% select(ref, reason_code, comment, submitted_date), by = "ref")
      
      # Re-render master data table
      output$master_data <- renderDT({
        datatable(filtered_master_data(), options = list(scrollX = TRUE, pageLength = 10))
      })
    }
  })
  
  # Provide Download Links for each file type
  output$download_rds_button <- downloadHandler(
    filename = function() {
      paste("master_data", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(filtered_master_data(), file)
    }
  )
  
  output$download_xlsx_button <- downloadHandler(
    filename = function() {
      paste("master_data", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(filtered_master_data(), file)
    }
  )
  
  output$download_csv_button <- downloadHandler(
    filename = function() {
      paste("master_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(filtered_master_data(), file)
    }
  )
  
  output$current_date <- renderText({
    format(Sys.Date(), "%Y-%m-%d")
  })
}

shinyApp(ui = ui, server = server)
