if (!require(pacman)) install.packages("pacman")
library(pacman)

p_load("dplyr", "plotly", "janitor")

current_year <- 2022
data_folder <- "T:/Projects/71 - SSB PCOS/Data/"

data_final <- readRDS(paste0(data_folder, "Final/PCOS ", current_year, " Final Dataset.RDS"))

aware_nisra_data <- readRDS(paste0(data_folder, "Trend/", current_year - 1, "/aware_nisra_data.RDS")) %>%
  bind_rows(data.frame(
    year = current_year,
    pct = sum(data_final$W3[data_final$AwareNISRA2 == "Yes"]) / sum(data_final$W3) * 100
  ))

heard_of_nisra <- round_half_up(aware_nisra_data$pct[aware_nisra_data$year == current_year])

aware_by_age <- data_final %>%
  group_by(AGE2, AwareNISRA2) %>%
  summarise(W3 = sum(W3)) %>%
  group_by(AGE2) %>%
  mutate(pct = W3 / sum(W3) * 100) %>%
  filter(AwareNISRA2 == "Yes") %>%
  select(AGE2, pct)

aware_youngest <- round_half_up(aware_by_age$pct[1])

aware_oldest <- round_half_up(aware_by_age$pct[nrow(aware_by_age)])
