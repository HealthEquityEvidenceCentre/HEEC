# Load necessary libraries
library(dplyr)
library(magrittr)

# Define the generic function to merge and assign IMD quintiles
merge_and_assign_quintiles <- function(data, start_year = 2017, end_year = 2024) {
  # Read the IMD data
  IMD <- read.csv("../IMD/IMD_interpolated.csv")

  # Merge the provided data with IMD data
  merged_data <- merge(data, IMD, by = c("Practice.Code", "Year"), all.x = TRUE)

  # Assign IMD to quintiles for each year
  for (year in start_year:end_year) {
    # Apply ntile to create IMD quintiles
    merged_data[merged_data$Year == year, "IMD_quintile"] <- merged_data[merged_data$Year == year, ] %>%
      mutate(IMD_quintile = ntile(IMD, 5)) %>%
      pull(IMD_quintile)

    # Print the number of practices in each quintile
    print(paste("Year:", year))
    print(table(merged_data[merged_data$Year == year, "IMD_quintile"]))
  }

  return(merged_data)
}

# Define the generic function to match CCG.Code in df with CCG.Code in CCG_ICB and return ICB.Code
clean_icb_name <- function(data) {
  # Clean up ICB.NAME by removing "NHS " and " Integrated Care Board"
  data$ICB.NAME <- gsub("NHS ", "", data$ICB.NAME)
  data$ICB.NAME <- gsub(" Integrated Care Board", "", data$ICB.NAME)

  return(data)
}
