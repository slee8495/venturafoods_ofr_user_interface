library(tidyverse)
library(magrittr)
library(openxlsx)
library(readxl)
library(writexl)
library(reshape2)
library(skimr)
library(janitor)
library(lubridate)
library(rio)

### Daily Processing ###
#################################################################### Read Files ####################################################################
ofr_original <- read_excel("data-xlsx/05302024.xlsx")
ofr_master_database <- readRDS("master_data.rds")
####################################################################################################################################################

# ofr_1st_data %>%
#   dplyr::mutate(database_uploaded_date = Sys.Date(), reason_code = "", comment = "", submitted_date = Sys.Date()) -> temp_data
# 
# saveRDS(temp_data, "master_data.rds") 


### Functions ###
#### ofr 1st data manipulation ####
ofr_1st_data <- function(df) {
  df %>%
    janitor::clean_names() %>%
    dplyr::mutate(item_no = gsub("-", "", item_no)) %>%
    dplyr::mutate(shortage_date = as.Date(shortage_date)) %>% 
    dplyr::mutate(shortage_date_2 = as.double(shortage_date)) %>%
    dplyr::mutate(ref = paste0(campus_no, "_", shortage_date_2, "_", item_no)) %>%
    dplyr::relocate(ref) %>%
    dplyr::mutate(across(ends_with("date"), as.Date)) %>%
    dplyr::select(-shortage_date_2)
}

# Use the function to create the initial processed data
ofr_1st_data <- ofr_1st_data(ofr_original)


#### ofr_master_data ####
ofr_master_data_function <- function(df, initial_data) {
  # Clean the incoming master data
  df_cleaned <- df %>%
    janitor::clean_names() %>%
    dplyr::relocate(ref) %>%
    dplyr::mutate(across(ends_with("date"), as.Date)) 
  
  # Combine initial data with the cleaned incoming data
  combined_data <- bind_rows(initial_data, df_cleaned)
  
  # Prioritize rows where either reason_code or comment is not empty
  combined_data <- combined_data %>%
    arrange(desc(reason_code != ""), desc(comment != "")) %>% 
    distinct(ref, sales_order_no, .keep_all = TRUE)
  
  return(combined_data)
}




# Use the function to combine the initial data with the master database
ofr_master_data <- ofr_master_data_function(ofr_master_database, ofr_1st_data)


### ofr 2nd data manipulation ####
ofr_2nd_data_function <- function(df) {
  df %>%
    janitor::clean_names() %>%  # Ensure clean column names for consistency
    dplyr::group_by(campus_no, shortage_date, item_no, database_uploaded_date) %>%
    dplyr::summarise(order_shortage_case_qty = sum(order_shortage_case_no, na.rm = TRUE), .groups = "drop") %>%
    
    dplyr::mutate(shortage_date_2 = as.double(shortage_date)) %>%
    
    dplyr::mutate(reason_code = "", comment = "", submitted_date = Sys.Date()) %>% 
    dplyr::mutate(ref = paste0(campus_no, "_", shortage_date_2, "_", item_no)) %>%
    
    dplyr::select(-shortage_date_2) %>%
    dplyr::relocate(ref)
}

# Use the function
ofr_2nd_data <- ofr_2nd_data_function(ofr_master_data)


# live Database
live_database <- ofr_master_data %>% 
  dplyr::select(-reason_code, -comment, -submitted_date) 


