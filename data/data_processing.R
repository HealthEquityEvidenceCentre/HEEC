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
assign_icb_name <- function(data) {
  # Read the CCG to ICB mapping data
  ccg_icb_df <- read.csv("../CCG_ICB_code.csv")

  ccg_icb_df %>%
    head() %>%
    print()

  # Match CCG_Code in data with CCG.Code in ccg_icb_df and return ICB.NAME
  data$ICB.NAME <- ccg_icb_df[match(data$CCG_Code, ccg_icb_df$CCG.Code), ]$ICB.NAME

  # Handle NAs by matching ICS_Code with ICB.Code
  # data[is.na(data$ICB.NAME), ]$ICB.NAME <- ccg_icb_df[match(data[is.na(data$ICB.NAME), ]$ICB_Code, ccg_icb_df$ICB.Code), ]$ICB.NAME

  # Clean up ICB.NAME by removing "NHS " and " Integrated Care Board"
  # data$ICB.NAME <- gsub("NHS ", "", data$ICB.NAME)
  # data$ICB.NAME <- gsub(" Integrated Care Board", "", data$ICB.NAME)

  # Select relevant columns for the final dataset
  # data <- data[, c("Practice.Code", "Practice.Name", "ICB.NAME", "Year", "overall_pct", "continuity_pct", "access_pct", "trust_pct")]

  return(data)
}

assign_icb_name2 <- function(data) {
  payments <- read.csv("../../data/payments/payments.csv")

  # match Practice.Code in data with Practice.Code in payments and return ICB.NAME
  data$ICB.NAME <- payments[match(data$Practice.Code, payments$Practice.Code), ]$ICB.NAME

  return(data)
}
