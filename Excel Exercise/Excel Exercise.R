# Define data path ####

data_folder <- "T:/Projects/71 - SSB PCOS/Data/"

# Load packages ####

if (!require(pacman)) install.packages("pacman")
library(pacman)

p_load("openxlsx", "dplyr")

# Load data ####

current_year <- 2022

data_final <- readRDS(paste0(data_folder, "Final/PCOS ", current_year, " Final Dataset.RDS"))

table_1_data <- readRDS(paste0(data_folder, "Trend/", current_year - 1, "/table_1_data.RDS"))

# Functions ####

## Function to calculate weighted percentage ####

f_weighted_pct <- function(data, col, val) {
  sum(data$W3[data[[col]] == val]) / sum(data$W3) * 100
}

### Test our function ####

f_weighted_pct(data_final, "PCOS1", "No")
f_weighted_pct(data_final, "PCOS1", "Yes")

## Function to return unweighted n ####

f_return_n <- function(data, col) {
  data %>%
    filter(!is.na(data[[col]])) %>%
    nrow()

  # result <- data
  # result <- filter(result, !is.na(result[[col]]))
  # result <- nrow(result)
}

### Test our function ####

f_return_n(data_final, "PCOS1")
f_return_n(data_final, "TrustNISRA2")

# Analyse data ####

## Use above functions along with mutate() command to add a new column ####

table_1_data <- table_1_data %>%
  mutate(new = c(
    f_weighted_pct(data_final, "PCOS1", "Yes"),
    f_weighted_pct(data_final, "PCOS1", "No"),
    f_weighted_pct(data_final, "PCOS1", "Don't know"),
    f_return_n(data_final, "PCOS1")
  ))

### Rename new column ####

names(table_1_data) <- sub("new", current_year, names(table_1_data))

# Excel stylings ####

title_style <- createStyle(
  fontSize = 14,
  textDecoration = "bold"
)

header_style <- createStyle(
  textDecoration = "bold",
  halign = "right"
)

header_style_left <- createStyle(
  textDecoration = "bold",
)

number_style <- createStyle(
  numFmt = "#,##0",
  halign = "right"
)

# Create Excel Workbook ####

## Define a work book object and add a sheet ####
wb <- createWorkbook()

modifyBaseFont(wb, fontSize = 12, fontName = "Arial")

addWorksheet(wb,
  sheetName = "Heard_of_NISRA"
)

## Write title to first row of sheet ####
writeData(wb, "Heard_of_NISRA",
  x = paste0("Table 1: Awareness of NISRA, 2009-", current_year),
  startCol = 1,
  startRow = 1
)

### Change styling of title ####
addStyle(wb, "Heard_of_NISRA",
  style = title_style,
  rows = 1,
  cols = 1
)

## Write data frame to sheet ####
writeDataTable(wb, "Heard_of_NISRA",
  x = table_1_data,
  startCol = 1,
  startRow = 2,
  withFilter = FALSE,
  tableStyle = "none",
  headerStyle = header_style
)

### Change styling of table ####

addStyle(wb,
  sheet = "Heard_of_NISRA",
  style = header_style_left,
  rows = 2,
  cols = 1
)

addStyle(wb,
  sheet = "Heard_of_NISRA",
  style = number_style,
  rows = 3:6,
  cols = 2:11,
  gridExpand = TRUE
)

### Change width of first column ####

setColWidths(wb, "Heard_of_NISRA", cols = 1, widths = 21)

# Save Workbook out to local folder ####

saveWorkbook(wb, "Excel-Coding.xlsx", overwrite = TRUE)
