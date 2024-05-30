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


specific_date <- as.Date("2024-05-29")

### Daily Processing ###
#################################################################### Read Files ####################################################################
ofr_original <- read_excel("data-xlsx/05302024.xlsx")
####################################################################################################################################################


### Functions ###
#### ofr 1st data manipultation ####
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

# Use the function
ofr_1st_data <- ofr_1st_data(ofr_original)


#### ofr 2nd data manipultation ####

ofr_2nd_data <- function(df) {
  df %>%
    dplyr::group_by(ref, campus_no, shortage_date, item_no) %>%
    dplyr::summarise(order_shortage_case_qty = sum(order_shortage_case_no, na.rm = TRUE), .groups = "drop") 
}

# Use the function
ofr_2nd_data <- ofr_2nd_data(ofr_1st_data)







