# Load necessary libraries
library(dplyr)

# Load the dataset (replace with your actual file path)
file_path <- '/mnt/data/dataset.csv'
getwd()
output_path <- '/Users/shengzhang/Download'
# Open a file connection to save the results
file_conn <- file(output_path, open = "wt")
setwd("/Users/shengzhang/Downloads/Dissertations/Results From Mike/Verification/7. Summary Stats")
df <- read.csv("Results_KL.csv")

# Extract Theta and SE columns
theta_cols <- grep("Theta", colnames(df), value = TRUE)
se_cols <- grep("SE", colnames(df), value = TRUE)

# Get unique participant IDs
participants <- unique(df$Simulation)

# Initialize an empty data frame to store final results
results <- data.frame(Participant = integer(), FinalItem = character(), FinalRound = integer(), SumEV = double(), SumCurrentV = double(), SumCurrentSE = double(), ItemsAdministered = integer(), RMSE = double(), StoppingRuleMet = character(), FinalTheta0 = double(), FinalTheta1 = double(), FinalTheta2 = double(), FinalTheta3 = double(), FinalSE0 = double(), FinalSE1 = double(), FinalSE2 = double(), FinalSE3 = double(), stringsAsFactors = FALSE)

# Loop through each participant
for (s in participants) {
  simulee <- df[df$Simulation == s, ]  # Select data for the current participant
  max_items <- nrow(simulee)
  items_administered <- 0
  stopping_rule_met <- "No"
  final_item <- ""
  final_round <- 0
  final_sumEV <- 0
  final_sumCurrentV <- 0
  final_sumCurrentSE <- 0
  rmse <- 0
  final_theta <- numeric(length(theta_cols))
  final_se <- numeric(length(se_cols))
  
  # Loop through each item for the participant
  for (i in 1:max_items) {
    EV <- as.numeric(simulee[i, theta_cols])
    current_SE <- as.numeric(simulee[i, se_cols])
    sumEV <- sum(EV)
    sumCurrentV <- sum(current_SE^2)
    sumCurrentSE <- sum(current_SE)
    
    # Calculate RMSE using true theta values from the first row
    true_theta <- as.numeric(simulee[1, theta_cols])
    rmse <- sqrt(mean((EV - true_theta)^2))
    
    item <- simulee$ItemSelectio[i]
    items_administered <- items_administered + 1
    final_item <- item
    final_round <- i
    final_sumEV <- sumEV
    final_sumCurrentV <- sumCurrentV
    final_sumCurrentSE <- sumCurrentSE
    final_theta <- EV
    final_se <- current_SE
    
    # Stopping rule: all four SEs are below 0.3
    if (all(current_SE < 0.4)) {
      stopping_rule_met <- "Yes"
      cat(sprintf("Participant %d stopped after %d items (SEs below 0.4)\n", s, items_administered))
      break
    }
  }
  
  # Save the final result for the participant after the loop
  result <- data.frame(
    Participant = s,
    FinalItem = final_item,
    FinalRound = final_round,
    SumEV = final_sumEV,
    SumCurrentV = final_sumCurrentV,
    SumCurrentSE = final_sumCurrentSE,
    ItemsAdministered = items_administered,
    RMSE = rmse,
    StoppingRuleMet = stopping_rule_met,
    FinalTheta0 = final_theta[1],
    FinalTheta1 = final_theta[2],
    FinalTheta2 = final_theta[3],
    FinalTheta3 = final_theta[4],
    FinalSE0 = final_se[1],
    FinalSE1 = final_se[2],
    FinalSE2 = final_se[3],
    FinalSE3 = final_se[4],
    stringsAsFactors = FALSE
  )
  results <- rbind(results, result)
}

# Save the final summarized results to a CSV file
write.csv(results, "Final results_KL_0.4.csv", row.names = FALSE)
cat("Participant Theta and SE final results saved to results.csv\n")
