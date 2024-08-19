# Load necessary libraries
library(dplyr)
library(magrittr)

# Define the generic function to merge and assign IMD quintiles
merge_and_assign_quintiles <- function(data, start_year = 2017, end_year = 2023) {
  # Read the IMD data
  IMD <- read.csv("../IMD/IMD_interpolated.csv")

  # Merge the provided data with IMD data
  merged_data <- merge(data, IMD, by = c("Practice.Code", "Year"), all.x = TRUE)

  merged_data %>%
    head() %>%
    print()

  # Assign IMD to quintiles
  for (year in start_year:end_year) {
    print(paste("Processing Year:", year))

    # Assign IMD to quintiles
    merged_data[merged_data$Year == year, "IMD_quintile"] <- merged_data[merged_data$Year == year, ] %>%
      mutate(IMD_quintile = cut(IMD,
        breaks = quantile(IMD, probs = seq(0, 1, 0.2), na.rm = TRUE),
        include.lowest = TRUE,
        labels = c("1", "2", "3", "4", "5")
      )) %>%
      pull(IMD_quintile)
  }

  return(merged_data)
}
