if (!require(pacman)) install.packages("pacman")
library(pacman)

p_load("dplyr", "plotly", "janitor")

current_year <- 2022
# Change to your remote data folder location
data_folder <- "T:/Projects/71 - SSB PCOS/Data/" 

# Load final data set for current year
data_final <- readRDS(paste0(data_folder, "Final/PCOS ", current_year, " Final Dataset.RDS"))

# Produce data frame for Awareness of NISRA by year by reading data from previous year
# And then binding on new row using current_year and
# the Weighted Percentage of AwareNISRA2 obtained from data_final
aware_nisra_data <- readRDS(paste0(data_folder, "Trend/", current_year - 1, "/aware_nisra_data.RDS")) %>%
  bind_rows(data.frame(
    year = current_year,
    pct = sum(data_final$W3[data_final$AwareNISRA2 == "Yes"]) / sum(data_final$W3) * 100
  ))

# Extract a single value from new data frame
heard_of_nisra <- aware_nisra_data$pct[aware_nisra_data$year == current_year]


## Exercise 1a
# All of the below comamnds come from the dplyr package
# Help with these commands can be found at https://dplyr.tidyverse.org/ (Ctrl + click link to follow)

# 1. Use data_final to create a new data frame called aware_by_age
# 2. Use group_by() and summarise() to return the weighted totals
#    for each age group AGE2 and each response to AwareNISRA2.
# 3. Use group_by() and mutate() to return variable pct, which is the weighted percentage for each row
# 4. Use filter() to select only the "Yes" rows
# 5. Use select() to select out the columns AGE2 and pct


aware_by_age <- data_final %>%
  group_by(AGE2, AwareNISRA2) %>%
  summarise(W3 = sum(W3)) %>%
  group_by(AGE2) %>%
  mutate(pct = W3 / sum(W3) * 100) %>%
  filter(AwareNISRA2 == "Yes") %>%
  select(AGE2, pct)

## Exercise 2

# 1. Create value aware_youngest which is first value of the pct column of aware_by_age

aware_youngest <- round_half_up(aware_by_age$pct[1])

# 2. Create value aware_oldest which is first value of the pct column of aware_by_age

aware_oldest <- round_half_up(aware_by_age$pct[nrow(aware_by_age)])
