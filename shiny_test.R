library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(lubridate)
library(shinyWidgets)
library(readxl)
library(janitor)
library(writexl)

# Load the data and functions from data.R
source("data.R")

# Load initial data from the saved files or from the initial processing
load_data <- function() {
  list(
    processed_data1 = ofr_1st_data, # Processed initial data
    processed_data2 = ofr_2nd_data, # Processed second data
    master_data = ofr_master_data,  # Combined and cleaned master data
    live_database = live_database   # The new live database to be used in the app
  )
}

data <- load_data()

# Ensure submitted_date is in Date format if it exists
if ("submitted_date" %in% colnames(data$live_database)) {
  data$live_database$submitted_date <- as.Date(data$live_database$submitted_date, origin = "1970-01-01") # Adjust the origin if necessary
} else {
  data$live_database <- data$live_database %>%
    mutate(submitted_date = NA_Date_)
}

# Add missing columns reason_code and comment with appropriate NA values
if (!"reason_code" %in% colnames(data$live_database)) {
  data$live_database <- data$live_database %>%
    mutate(reason_code = NA_character_)
}

if (!"comment" %in% colnames(data$live_database)) {
  data$live_database <- data$live_database %>%
    mutate(comment = NA_character_)
}

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
                            tabPanel("Today's Database",
                                     fluidPage(
                                       titlePanel(
                                         div(class = "row",
                                             div(class = "col-sm-8",
                                                 span(textOutput("current_date"), style = "margin-left: 20px; font-size: 20px;")
                                             )
                                         )
                                       ),
                                       br(),
                                       fluidRow(
                                         column(2, pickerInput("db_campus_filter", "Campus No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE))),
                                         column(2, pickerInput("db_item_no_filter", "Item No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE))),
                                         column(2, dateRangeInput("db_date_filter", "Shortage Date:", start = NULL, end = NULL))
                                       ),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll; height: calc(100vh - 100px);',
                                           DTOutput("data2"))
                                     )
                            ),
                            tabPanel("User-Input",
                                     fluidPage(
                                       br(),
                                       fluidRow(
                                         column(2, actionButton("save_button", "Send to Database", style = "background-color: lightblue; color: black;")),
                                       ),
                                       br(),
                                       fluidRow(
                                         column(2, pickerInput("campus_filter", "Campus No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE))),
                                         column(2, pickerInput("item_no_filter", "Item No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE))),
                                         column(2, dateRangeInput("date_filter", "Shortage Date:", start = NULL, end = NULL))
                                       ),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll; height: calc(100vh - 100px);',
                                           DTOutput("data3"))
                                     )
                            ),
                            tabPanel("User Input Dashboard", # New tab to show non-editable data
                                     fluidPage(
                                       br(),
                                       br(),
                                       fluidRow(
                                         column(12,
                                                DTOutput("user_input_dashboard_data")
                                         )
                                       )
                                     )
                            )
                          )
                 ),
                 tabPanel("Master Database",
                          tabsetPanel(
                            tabPanel("Live Database", # New tab for Live Database
                                     br(),
                                     fluidPage(
                                       fluidRow(
                                         column(2, downloadButton("download_rds_button_live", "For an authorized user", style = "background-color: coral; color: black;"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(2, pickerInput("live_campus_filter", "Campus No:", choices = unique(data$live_database$campus_no), selected = unique(data$live_database$campus_no), multiple = TRUE, options = list(`actions-box` = TRUE))),
                                         column(2, pickerInput("live_item_no_filter", "Item No:", choices = unique(data$live_database$item_no), selected = unique(data$live_database$item_no), multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE))),
                                         column(2, dateRangeInput("live_date_filter", "Shortage Date:", start = min(data$live_database$shortage_date, na.rm = TRUE), end = max(data$live_database$shortage_date, na.rm = TRUE))
                                         )
                                       ),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll; height: calc(100vh - 100px);',
                                           DTOutput("live_data"))
                                     )
                            ),
                            tabPanel("Master Database", # Existing Master Database tab
                                     br(),
                                     fluidPage(
                                       fluidRow(
                                         column(2, downloadButton("download_xlsx_button_master", "Download as .xlsx", style = "background-color: lightblue; color: black;")),
                                         column(2, downloadButton("download_csv_button_master", "Download as .csv", style = "background-color: lightblue; color: black;"))
                                       ),
                                       br(),
                                       fluidRow(
                                         column(2, pickerInput("master_campus_filter", "Campus No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE))),
                                         column(2, pickerInput("master_item_no_filter", "Item No:", choices = NULL, selected = NULL, multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE))),
                                         column(2, dateRangeInput("master_date_filter", "Shortage Date:", start = NULL, end = NULL))
                                       ),
                                       div(style = 'overflow-x: scroll; overflow-y: scroll; height: calc(100vh - 100px);',
                                           DTOutput("master_data"))
                                     )
                            )
                          )
                 ),
                 tags$head(
                   tags$style(HTML("
                                   #data3 table.dataTable tr td:nth-child(7),
                                   #data3 table.dataTable tr td:nth-child(8),
                                   #data3 table.dataTable tr td:nth-child(9) {
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
    master_data = data$master_data, # Use the combined and cleaned master data
    live_data = data$live_database # Initialize live data with live_database from data.R
  )
  
  # Define the dropdown choices for the reason_code column
  reason_code_choices <- c("Code1", "Code2", "Code3", "Code4")  # Update with actual choices
  
  # Convert reason_code_choices to a JSON format for JavaScript use
  reason_code_choices_js <- jsonlite::toJSON(reason_code_choices, auto_unbox = TRUE)
  
  # Update filters on session start
  observe({
    updatePickerInput(session, "campus_filter", choices = unique(rv$processed_data2$campus_no), selected = unique(rv$processed_data2$campus_no))
    updatePickerInput(session, "item_no_filter", choices = unique(rv$processed_data2$item_no), selected = unique(rv$processed_data2$item_no))
    updateDateRangeInput(session, "date_filter", start = min(rv$processed_data2$shortage_date, na.rm = TRUE), end = max(rv$processed_data2$shortage_date, na.rm = TRUE))
    
    updatePickerInput(session, "master_campus_filter", choices = unique(rv$master_data$campus_no), selected = unique(rv$master_data$campus_no))
    updatePickerInput(session, "master_item_no_filter", choices = unique(rv$master_data$item_no), selected = unique(rv$master_data$item_no))
    updateDateRangeInput(session, "master_date_filter", start = min(rv$master_data$shortage_date, na.rm = TRUE), end = max(rv$master_data$shortage_date, na.rm = TRUE))
    
    updatePickerInput(session, "db_campus_filter", choices = unique(rv$processed_data1$campus_no), selected = unique(rv$processed_data1$campus_no))
    updatePickerInput(session, "db_item_no_filter", choices = unique(rv$processed_data1$item_no), selected = unique(rv$processed_data1$item_no))
    updateDateRangeInput(session, "db_date_filter", start = min(rv$processed_data1$shortage_date, na.rm = TRUE), end = max(rv$processed_data1$shortage_date, na.rm = TRUE))
    
    updatePickerInput(session, "live_campus_filter", choices = unique(rv$live_data$campus_no), selected = unique(rv$live_data$campus_no))
    updatePickerInput(session, "live_item_no_filter", choices = unique(rv$live_data$item_no), selected = unique(rv$live_data$item_no))
    updateDateRangeInput(session, "live_date_filter", start = min(rv$live_data$shortage_date, na.rm = TRUE), end = max(rv$live_data$shortage_date, na.rm = TRUE))
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
    data
  })
  
  filtered_live_data <- reactive({
    data <- rv$live_data
    if (nrow(data) == 0 || !"ref" %in% colnames(data)) return(data)
    if (!is.null(input$live_campus_filter) && length(input$live_campus_filter) > 0) {
      data <- data %>% filter(campus_no %in% input$live_campus_filter)
    }
    if (!is.null(input$live_item_no_filter) && length(input$live_item_no_filter) > 0) {
      data <- data %>% filter(item_no %in% input$live_item_no_filter)
    }
    if (!is.null(input$live_date_filter)) {
      data <- data %>% filter(shortage_date >= input$live_date_filter[1] & shortage_date <= input$live_date_filter[2])
    }
    data
  })
  
  # Render Data Table with editable dropdowns for reason_code column for "User-Input" tab
  output$data3 <- renderDT({
    datatable(
      rv$processed_data2,  # Use rv$processed_data2 directly for user input
      options = list(scrollX = TRUE, pageLength = 10, 
                     columnDefs = list(list(targets = 7, render = JS(
                       sprintf(
                         "function(data, type, row, meta) {
                           if (type === 'display') {
                             var select = '<select class=\"form-control dt-editable\" onchange=\"$(this).trigger(\'change\')\" data-row=\"' + row + '\" data-col=\"' + meta.col + '\">';
                             var options = %s;
                             for (var i = 0; i < options.length; i++) {
                               select += '<option value=\"' + options[i] + '\"' + (data === options[i] ? ' selected' : '') + '>' + options[i] + '</option>';
                             }
                             select += '</select>';
                             return select;
                           }
                           return data;
                         }",
                         reason_code_choices_js
                       )
                     )))
      ),
      callback = JS(
        "table.on('change', '.dt-editable', function() {
          var $select = $(this);
          var value = $select.val();
          var row = $select.data('row');
          var col = $select.data('col');
          table.cell(row, col).data(value).draw();
          Shiny.setInputValue('edited_cells', {row: row, col: col, value: value}, {priority: 'event'});
        });"
      ),
      editable = TRUE  # Allow editing
    )
  })
  
  # Handle the edits and update the reactive value
  observeEvent(input$edited_cells, {
    info <- input$edited_cells
    
    # Extract the row, column, and value from the input
    i <- info$row + 1  # JavaScript is 0-indexed; R is 1-indexed
    j <- info$col + 1
    v <- info$value
    
    # Print debug info
    print(paste("Row:", i, "Column:", j, "Value:", v))
    
    # Determine correct indices for reason_code, comment, and submitted_date
    reason_code_index <- which(colnames(rv$processed_data2) == "reason_code")
    comment_index <- which(colnames(rv$processed_data2) == "comment")
    submitted_date_index <- which(colnames(rv$processed_data2) == "submitted_date")
    
    # Update the correct column based on the index
    if (j == reason_code_index) {
      rv$processed_data2[i, "reason_code"] <- v
    } else if (j == comment_index) {
      rv$processed_data2[i, "comment"] <- v
    } else if (j == submitted_date_index) {
      rv$processed_data2[i, "submitted_date"] <- as.Date(v)
    }
  })
  
  # Save changes from User-Input and update User Input Dashboard and live_data
  observeEvent(input$save_button, {
    if (nrow(rv$processed_data2) > 0 && "ref" %in% colnames(rv$processed_data2)) {
      # Update the User Input Dashboard to show the latest changes
      output$user_input_dashboard_data <- renderDT({
        datatable(rv$processed_data2, options = list(scrollX = TRUE, pageLength = 10))
      })
      
      # Perform VLOOKUP-style update for live_data from user_input_backend_data
      rv$live_data <- rv$live_data %>%
        left_join(rv$processed_data2 %>% select(ref, reason_code, comment, submitted_date), by = "ref") %>%
        mutate(
          reason_code = ifelse(is.na(reason_code.y), reason_code.x, reason_code.y),
          comment = ifelse(is.na(comment.y), comment.x, comment.y),
          submitted_date = as.Date(ifelse(is.na(submitted_date.y), submitted_date.x, submitted_date.y), origin = "1970-01-01") # Ensure correct Date conversion
        ) %>%
        select(-ends_with(".x"), -ends_with(".y"))
      
      showNotification("Changes saved and User Input Dashboard updated.", type = "message")
    }
  })
  
  # Render "Today's Data" table
  output$data2 <- renderDT({
    datatable(filtered_db_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Render "User-Input Dashboard" table
  output$user_input_dashboard_data <- renderDT({
    datatable(rv$processed_data2, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Render "Master Database" table
  output$master_data <- renderDT({
    datatable(filtered_master_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Render "Live Database" table
  output$live_data <- renderDT({
    datatable(filtered_live_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Download handlers for master data in Master Database tab
  output$download_xlsx_button_master <- downloadHandler(
    filename = function() {
      paste("master_data", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(filtered_master_data(), file)
    }
  )
  
  output$download_csv_button_master <- downloadHandler(
    filename = function() {
      paste("master_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(filtered_master_data(), file)
    }
  )
  
  # Download handler for .rds file in Live Database tab
  output$download_rds_button_live <- downloadHandler(
    filename = function() {
      paste("live_data", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(rv$live_data, file)
    }
  )
  
  output$current_date <- renderText({
    format(Sys.Date(), "%Y-%m-%d")
  })
}



shinyApp(ui = ui, server = server)
