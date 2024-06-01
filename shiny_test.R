library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(lubridate)
library(shinyWidgets)
library(readxl)
library(janitor)
library(openxlsx)

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
                            actionButton("upload_button", "Upload OFR Today's Data"),
                            div(style = "color: blue; margin-top: 10px;", "Only an authorized user can initially upload the data once per day.")
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
                                       actionButton("deploy_button", "Deploy"),
                                       pickerInput("campus_filter", "Filter by Campus No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
                                       dateRangeInput("date_filter", "Filter by Shortage Date:", start = NULL, end = NULL),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll; height: calc(100vh - 100px);',
                                           DTOutput("data3"))
                                     )
                            ),
                            tabPanel("User-Input Dashboard",
                                     fluidPage(
                                       titlePanel("User-Input Dashboard"),
                                       pickerInput("dashboard_campus_filter", "Filter by Campus No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
                                       dateRangeInput("dashboard_date_filter", "Filter by Shortage Date:", start = NULL, end = NULL),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll; height: calc(100vh - 100px);',
                                           DTOutput("dashboard_data"))
                                     )
                            )
                          )
                 ),
                 tabPanel("Data Base",
                          fluidPage(
                            titlePanel("Data Base"),
                            h4(span("**It refreshes daily at 23:59 PM**", style = "color: blue;")),
                            tabsetPanel(
                              tabPanel("User-Input Data Base",
                                       fluidPage(
                                         pickerInput("user_input_db_campus_filter", "Filter by Campus No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
                                         dateRangeInput("user_input_db_date_filter", "Filter by Shortage Date:", start = NULL, end = NULL),
                                         downloadButton("download_user_input_csv", "Download CSV"),
                                         downloadButton("download_user_input_xlsx", "Download XLSX"),
                                         div(style = 'overflow-x: scroll; overflow-y: scroll; height: calc(100vh - 100px);',
                                             DTOutput("user_input_database_data"))
                                       )
                              ),
                              tabPanel("Master Data Base",
                                       fluidPage(
                                         pickerInput("master_db_campus_filter", "Filter by Campus No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
                                         dateRangeInput("master_db_date_filter", "Filter by Shortage Date:", start = NULL, end = NULL),
                                         downloadButton("download_master_csv", "Download CSV"),
                                         downloadButton("download_master_xlsx", "Download XLSX"),
                                         div(style = 'overflow-x: scroll; overflow-y: scroll; height: calc(100vh - 100px);',
                                             DTOutput("master_database_data"))
                                       )
                              )
                            )
                          )
                 ),
                 tags$head(
                   tags$style(HTML("
                                   #data3 table.dataTable tr td:nth-child(7),
                                   #data3 table.dataTable tr td:nth-child(8),
                                   #data3 table.dataTable tr td:nth-child(9),
                                   #dashboard_data table.dataTable tr td:nth-child(7),
                                   #dashboard_data table.dataTable tr td:nth-child(8),
                                   #dashboard_data table.dataTable tr td:nth-child(9),
                                   #user_input_database_data table.dataTable tr td:nth-child(7),
                                   #user_input_database_data table.dataTable tr td:nth-child(8),
                                   #user_input_database_data table.dataTable tr td:nth-child(9),
                                   #master_database_data table.dataTable tr td:nth-child(7),
                                   #master_database_data table.dataTable tr td:nth-child(8),
                                   #master_database_data table.dataTable tr td:nth-child(9) {
                                     background-color: lightgreen !important;
                                   }
                                   "))
                 )
)

###################### server ######################
server <- function(input, output, session) {
  rv <- reactiveValues(data1 = NULL, processed_data1 = NULL, processed_data2 = NULL, user_input_data = NULL, user_input_database_data = data.frame(), master_database_data = data.frame(), last_upload_date = NULL)
  
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
      dplyr::summarise(order_shortage_case_qty = sum(order_shortage_case_no, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(reason_code = "", comment = "", submitted_date = Sys.Date())
  }
  
  observeEvent(input$upload_button, {
    if (is.null(rv$last_upload_date) || rv$last_upload_date != Sys.Date()) {
      rv$data1 <- read_excel(input$file1$datapath)
      rv$processed_data1 <- ofr_1st_data(rv$data1)
      rv$processed_data2 <- ofr_2nd_data(rv$processed_data1)
      
      rv$last_upload_date <- Sys.Date()
      
      updatePickerInput(session, "campus_filter", choices = unique(rv$processed_data2$campus_no), selected = unique(rv$processed_data2$campus_no))
      updatePickerInput(session, "dashboard_campus_filter", choices = unique(rv$processed_data2$campus_no), selected = unique(rv$processed_data2$campus_no))
      updatePickerInput(session, "user_input_db_campus_filter", choices = unique(rv$processed_data2$campus_no), selected = unique(rv$processed_data2$campus_no))
      updatePickerInput(session, "master_db_campus_filter", choices = unique(rv$processed_data2$campus_no), selected = unique(rv$processed_data2$campus_no))
      
      updateDateRangeInput(session, "date_filter", start = min(rv$processed_data2$shortage_date), end = max(rv$processed_data2$shortage_date))
      updateDateRangeInput(session, "dashboard_date_filter", start = min(rv$processed_data2$shortage_date), end = max(rv$processed_data2$shortage_date))
      updateDateRangeInput(session, "user_input_db_date_filter", start = min(rv$processed_data2$shortage_date), end = max(rv$processed_data2$shortage_date))
      updateDateRangeInput(session, "master_db_date_filter", start = min(rv$processed_data2$shortage_date), end = max(rv$processed_data2$shortage_date))
    } else {
      showModal(modalDialog(
        title = "Upload Error",
        "Data has already been uploaded for today.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
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
  
  filtered_dashboard_data <- reactive({
    data <- rv$user_input_data
    if (!is.null(input$dashboard_campus_filter) && length(input$dashboard_campus_filter) > 0) {
      data <- data %>% filter(campus_no %in% input$dashboard_campus_filter)
    }
    if (!is.null(input$dashboard_date_filter)) {
      data <- data %>% filter(shortage_date >= input$dashboard_date_filter[1] & shortage_date <= input$dashboard_date_filter[2])
    }
    data
  })
  
  filtered_user_input_database_data <- reactive({
    data <- rv$user_input_database_data
    if (!is.null(input$user_input_db_campus_filter) && length(input$user_input_db_campus_filter) > 0) {
      data <- data %>% filter(campus_no %in% input$user_input_db_campus_filter)
    }
    if (!is.null(input$user_input_db_date_filter)) {
      data <- data %>% filter(shortage_date >= input$user_input_db_date_filter[1] & shortage_date <= input$user_input_db_date_filter[2])
    }
    data
  })
  
  filtered_master_database_data <- reactive({
    data <- rv$master_database_data
    if (!is.null(input$master_db_campus_filter) && length(input$master_db_campus_filter) > 0) {
      data <- data %>% filter(campus_no %in% input$master_db_campus_filter)
    }
    if (!is.null(input$master_db_date_filter)) {
      data <- data %>% filter(shortage_date >= input$master_db_date_filter[1] & shortage_date <= input$master_db_date_filter[2])
    }
    data
  })
  
  output$data2 <- renderDT({
    datatable(rv$processed_data1, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$data3 <- renderDT({
    datatable(filtered_data2(), editable = list(target = "cell", columns = c(6, 7, 8)), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  observeEvent(input$data3_cell_edit, {
    info <- input$data3_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    
    if (j == 6) {  # Column index for 'reason_code'
      rv$processed_data2[i, "reason_code"] <<- v
    } else if (j == 7) {  # Column index for 'comment'
      rv$processed_data2[i, "comment"] <<- v
    } else if (j == 8) {  # Column index for 'submitted_date'
      rv$processed_data2[i, "submitted_date"] <<- as.Date(v)
    }
  })
  
  observeEvent(input$deploy_button, {
    rv$user_input_data <- filtered_data2()
  })
  
  output$dashboard_data <- renderDT({
    datatable(filtered_dashboard_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  observe({
    invalidateLater(60000, session) # check every 60 seconds
    if (format(Sys.time(), "%H:%M") == "23:59") {
      rv$user_input_database_data <- bind_rows(rv$user_input_database_data, rv$user_input_data)
      
      # Perform left join to add extra columns to Today's Data
      today_data_with_extras <- left_join(rv$processed_data1, rv$user_input_data %>% select(ref, reason_code, comment, submitted_date), by = "ref")
      
      rv$master_database_data <- bind_rows(rv$master_database_data, today_data_with_extras)
      
      rv$user_input_data <- NULL
    }
  })
  
  # Render User-Input Data Base
  output$user_input_database_data <- renderDT({
    datatable(filtered_user_input_database_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Render Master Data Base
  output$master_database_data <- renderDT({
    datatable(filtered_master_database_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Download User-Input Data Base as CSV
  output$download_user_input_csv <- downloadHandler(
    filename = function() {
      paste("user_input_database_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$user_input_database_data, file, row.names = FALSE)
    }
  )
  
  # Download User-Input Data Base as XLSX
  output$download_user_input_xlsx <- downloadHandler(
    filename = function() {
      paste("user_input_database_data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(rv$user_input_database_data, file)
    }
  )
  
  # Download Master Data Base as CSV
  output$download_master_csv <- downloadHandler(
    filename = function() {
      paste("master_database_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$master_database_data, file, row.names = FALSE)
    }
  )
  
  # Download Master Data Base as XLSX
  output$download_master_xlsx <- downloadHandler(
    filename = function() {
      paste("master_database_data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(rv$master_database_data, file)
    }
  )
  
  output$current_date <- renderText({
    format(Sys.Date(), "%Y-%m-%d")
  })
}

shinyApp(ui = ui, server = server)