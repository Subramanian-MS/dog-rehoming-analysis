createsample(201898183)

print(mysample)
head(mysample)
tail(mysample)


save(mysample, file = "mysample.RData")
table(mysample$Breed)



# 1.Data Cleaning

# Here Data cleaning is done to Remove rows where 'Rehomed' is 99999 or 'Breed' is NA
data_cleaned <- mysample[mysample$Rehomed != 99999 & !is.na(mysample$Breed), ]


# Calculate removals for each condition
removed_rehomed <- nrow(mysample[mysample$Rehomed == 99999, ])
removed_breed <- nrow(mysample[is.na(mysample$Breed), ])
total_removed <- nrow(mysample) - nrow(data_cleaned)

# Percentages of removals
removed_rehomed_pct <- (removed_rehomed / nrow(mysample)) * 100
removed_breed_pct <- (removed_breed / nrow(mysample)) * 100


# Display results for removed rows
cat("Removed due to 'Rehomed' == 99999:", removed_rehomed, "(", removed_rehomed_pct, "%)\n")
cat("Removed due to missing 'Breed':", removed_breed, "(", removed_breed_pct, "%)\n")
cat("Total removed rows:", total_removed, "(", (total_removed / nrow(mysample)) * 100, "%)\n")

# Save the cleaned dataset
save(data_cleaned, file = "data_cleaned.RData")

# Load the cleaned dataset
load("data_cleaned.RData")



library(dplyr)

# Split the dataset by Breed
breed_samples <- split(data_cleaned, data_cleaned$Breed)

# Summary statistics by breed
Rehomed_summary_table <- data_cleaned %>% group_by(Breed) %>%
  summarise(
    Count = n(),
    Mean_Rehomed = mean(Rehomed, na.rm = TRUE),
    SD_Rehomed = sd(Rehomed, na.rm = TRUE),
    Median_Rehomed = median(Rehomed, na.rm = TRUE),
    IQR_Rehomed = IQR(Rehomed, na.rm = TRUE),
  )

# View summary table

print(Rehomed_summary_table)
Rehomed_summary_table = data.frame(Rehomed_summary_table)
pq<- write.table(Rehomed_summary_table, file = "Rehomed_summary_table.csv", sep = ",")
save(pq, file = "Rehomed Summary Table")
#Summary Visited Table
Visited_summary_table <- data_cleaned %>% group_by(Breed) %>%
  summarise(
    Count = n(),
    Mean_Visited = mean(Visited, na.rm = TRUE),
    SD_Visited = sd(Visited, na.rm = TRUE),
    Median_Visited = median(Visited, na.rm = TRUE),
    IQR_Visited = IQR(Visited, na.rm = TRUE),
  )

print(Visited_summary_table)
Visited_summary_table = data.frame(Visited_summary_table)

#Summary Health Table
Health_summary_table <- data_cleaned %>% group_by(Breed) %>%
  summarise(
    Count = n(),
    Mean_Health = mean(Health, na.rm = TRUE),
    SD_Health = sd(Health, na.rm = TRUE),
    Median_Health = median(Health, na.rm = TRUE),
    IQR_Health = IQR(Health, na.rm = TRUE),
  )

print(Health_summary_table)
Health_summary_table = data.frame(Health_summary_table)

library(dplyr)
#Data Exploration
Breed_group1 <- data_cleaned %>% filter(data_cleaned$Breed == "Bichon Frise")
head(Breed_group1)
Breed_group1$Breed
min(Breed_group1$Rehomed)
max(Breed_group1$Rehomed)

Breed_group2 <- data_cleaned %>% filter(data_cleaned$Breed == "Border Collie")
head(Breed_group2)
Breed_group2$Breed
min(Breed_group1$Rehomed)
max(Breed_group1$Rehomed)

Breed_group3 <- data_cleaned %>% filter(data_cleaned$Breed == "Dobermann")
head(Breed_group3)
Breed_group2$Breed
min(Breed_group1$Rehomed)
max(Breed_group1$Rehomed)




library(ggplot2)
#Visualising rehoming time for each breed
#Box Plot for Rehomed
ggplot(data_cleaned, aes(x = Breed, y = Rehomed, fill = Breed)) + geom_boxplot() +
  labs(
    title = "Rehoming Time for each breed ",
    x = "Breed",
    y = "Rehomed time (weeks)"
  ) +
  theme_minimal()

# Histogram for Rehoming Time
# a. Bichon Frise
ggplot(Breed_group1, aes(x = Rehomed, fill = Breed)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") +
  labs(
    title = "Distribution of Rehoming Time for Bichon Frise ",
    x = "Rehoming Time (weeks)",
    y = "Count"
  ) +
  theme_minimal()

#b. Border Collie
ggplot(Breed_group2, aes(x = Rehomed, fill = Breed)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") +
  labs(
    title = "Distribution of Rehoming Time for Border Collie Breed",
    x = "Rehoming Time (weeks)",
    y = "Count"
  ) +
  theme_minimal()

#c. Dobermann
ggplot(Breed_group3, aes(x = Rehomed, fill = Breed)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") +
  labs(
    title = "Distribution of Rehoming Time for Dobermann Breed",
    x = "Rehoming Time (weeks)",
    y = "Count"
  ) +
  theme_minimal()

# Density plot
# a. Bichon Frise
ggplot(Breed_group1, aes(x = Rehomed, color = Breed, fill = Breed)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Density Plot of Rehoming Time for Bichon Frise ",
    x = "Rehoming Time (week)",
    y = "Density"
  ) +
  theme_minimal()

#b. Border Collie
ggplot(Breed_group2, aes(x = Rehomed, color = Breed, fill = Breed)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Density Plot of Rehoming Time for Border Collie Breed",
    x = "Rehoming Time (week)",
    y = "Density"
  ) +
  theme_minimal()

#c. Dobermann
ggplot(Breed_group3, aes(x = Rehomed, color = Breed, fill = Breed)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Density Plot of Rehoming Time for Dobermann Breed",
    x = "Rehoming Time (week)",
    y = "Density"
  ) +
  theme_minimal()

# STEP3  Load necessary libraries
library(ggplot2)
library(fitdistrplus)
library(MASS)

# Load the cleaned dataset
load("data_cleaned.RData")

# a. Bichon Frise
#Fit Normal distribution
fit_normal1 <- fitdist(Breed_group1$Rehomed, "norm")

#Fit exponential distribution
fit_exponential1 <- fitdist(Breed_group1$Rehomed, "exp")

plot(fit_normal1)

plot(fit_exponential1)


# b. Border Collie
fit_normal2 <- fitdist(Breed_group2$Rehomed, "norm")

#Fit exponential distribution
fit_exponential2 <- fitdist(Breed_group2$Rehomed, "exp")

plot(fit_normal2)

plot(fit_exponential2)


#c. Dobermann
fit_normal3 <- fitdist(Breed_group3$Rehomed, "norm")

#Fit exponential distribution
fit_exponential3 <- fitdist(Breed_group3$Rehomed, "exp")

plot(fit_normal3)

plot(fit_exponential3)

# Estimate Parameters for Each Distribution
print("\nStep: Parameter Estimation")

# Function to estimate parameters for Normal and Exponential distributions
estimate_parameters <- function(data, column) {
  rehomed_times <- na.omit(data[[column]])
  
  # Fit a Normal distribution
  normal_fit <- fitdist(rehomed_times, "norm")
  
  # Fit an Exponential distribution
  exponential_fit <- fitdist(rehomed_times, "exp")
  
  # Extract parameters
  normal_params <- normal_fit$estimate
  exponential_params <- exponential_fit$estimate
  
  return(list(
    Normal = normal_params,
    Exponential = exponential_params
  ))
}

# Estimate parameters for each breed
parameters_by_breed <- lapply(breed_samples, estimate_parameters, column = "Rehomed")

# Display parameter estimates
for (breed in names(parameters_by_breed)) {
  cat("\nBreed:", breed, "\n")
  cat("Normal Distribution Parameters (Mean, SD):", 
      paste(names(parameters_by_breed[[breed]]$Normal), parameters_by_breed[[breed]]$Normal, sep = " = ", collapse = ", "), "\n")
  cat("Exponential Distribution Parameter (Rate):", 
      paste(names(parameters_by_breed[[breed]]$Exponential), parameters_by_breed[[breed]]$Exponential, sep = " = ", collapse = ", "), "\n")
}

# Function to perform goodness-of-fit tests for each breed
perform_tests <- function(data, column) {
  rehomed_times <- na.omit(data[[column]])
  rehomed_times <- jitter(rehomed_times)  # Add jitter if there are ties
  
  # Perform Shapiro-Wilk Test (Normality)
  shapiro <- shapiro.test(rehomed_times)
  
  # Perform Kolmogorov-Smirnov Test (Exponential)
  ks_exp <- ks.test(rehomed_times, "pexp", rate = 1 / mean(rehomed_times))
  
  # Perform Chi-squared Goodness-of-Fit Test (Normal Distribution)
  breaks <- seq(min(rehomed_times), max(rehomed_times), length.out = 10)
  observed <- hist(rehomed_times, breaks = breaks, plot = FALSE)$counts
  expected <- diff(pnorm(breaks, mean = mean(rehomed_times), sd = sd(rehomed_times))) * length(rehomed_times)
  
  # Ensure expected frequencies are valid
  expected[expected < 1] <- 1
  
  chi_sq <- chisq.test(x = observed, p = expected / sum(expected))
  
  # Return results
  return(list(
    shapiro = shapiro,
    ks_exp = ks_exp,
    chi_sq = chi_sq
  ))
}

# Apply the function for each breed
results_by_breed <- lapply(breed_samples, perform_tests, column = "Rehomed")



# Function to summarize test results
summarize_test_results <- function(results) {
  shapiro <- results$shapiro$p.value
  ks_exp <- results$ks_exp$p.value
  chi_sq <- results$chi_sq$p.value
  
  cat("Shapiro-Wilk Test (Normality): p =", shapiro, "\n")
  cat("Kolmogorov-Smirnov Test (Exponential): p =", ks_exp, "\n")
  cat("Chi-squared Goodness-of-Fit (Normal): p =", chi_sq, "\n")
  
  # Recommendations based on p-values
  if (shapiro > 0.05 && chi_sq > 0.05) {
    cat("Recommendation: The Normal distribution fits the data well.\n")
  } else if (ks_exp > 0.05) {
    cat("Recommendation: The Exponential distribution fits the data well.\n")
  } else {
    cat("Recommendation: None of the tested distributions fit the data well.\n")
  }
}

# Apply and display results for each breed
for (breed in names(results_by_breed)) {
  cat("\nBreed:", breed, "\n")
  summarize_test_results(results_by_breed[[breed]])
}





# 4. Load required libraries
library(dplyr)

# Confidence interval function
calculate_confidence_interval <- function(data, column, hypothesized_mean, alpha = 0.05) {
  if (!all(column %in% names(data))) {
    stop(paste("Column", column, "not found in the dataset."))
  }
  if (!is.numeric(data[[column]])) {
    stop(paste("Column", column, "must be numeric."))
  }
  
  n <- length(data[[column]])
  if (n < 2) {
    return(list(
      test_type = NA,
      mean = NA,
      lower_bound = NA,
      upper_bound = NA,
      within_interval = NA
    ))
  }
  
  sample_mean <- mean(data[[column]], na.rm = TRUE)
  sample_sd <- sd(data[[column]], na.rm = TRUE)
  
  # Decide test type
  if (n >= 30) {
    test_type <- "z-test"
    z_critical <- qnorm(1 - alpha / 2)
    margin_of_error <- z_critical * (sample_sd / sqrt(n))
  } else {
    test_type <- "t-test"
    t_critical <- qt(1 - alpha / 2, df = n - 1)
    margin_of_error <- t_critical * (sample_sd / sqrt(n))
  }
  
  # Confidence interval
  lower_bound <- sample_mean - margin_of_error
  upper_bound <- sample_mean + margin_of_error
  
  # Result
  within_interval <- hypothesized_mean >= lower_bound && hypothesized_mean <= upper_bound
  
  return(list(
    test_type = test_type,
    mean = sample_mean,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    within_interval = within_interval
  ))
}

# Apply to each breed
hypothesized_value <- 27  # Define hypothesized mean dynamically
results <- lapply(names(breed_samples), function(breed_name) {
  breed_data <- breed_samples[[breed_name]]
  ci <- calculate_confidence_interval(breed_data, "Rehomed", hypothesized_mean = hypothesized_value)
  
  # Format results
  data.frame(
    Breed = breed_name,
    Test_Type = ci$test_type,
    Sample_Mean = round(ci$mean, 2),
    CI_Lower = round(ci$lower_bound, 2),
    CI_Upper = round(ci$upper_bound, 2),
    Includes_27 = ifelse(ci$within_interval, "Yes", "No")
  )
})

# Combine results into a single data frame
results_df <- do.call(rbind, results)

# Print results
print("Confidence Interval Results:")
print(results_df)

ggplot(results_df, aes(x = Breed, y = Sample_Mean)) +
  geom_point(size = 3, aes(color = "Sample Mean"), show.legend = TRUE) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, color = "Confidence Interval"), 
                width = 0.2, size = 1.2, show.legend = TRUE) +
  geom_hline(yintercept = hypothesized_value, linetype = "dashed", color = "black", aes(color = "Reference Line"), 
             show.legend = TRUE) +
  labs(title = "Confidence Intervals for Mean Rehoming Time by Breed",
       x = "Breed", 
       y = "Mean Rehoming Time (Weeks)",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(
    values = c("Sample Mean" = "#377eb8", 
               "Confidence Interval" = "#e69f00", 
               "Reference Line" = "#333333"),
    labels = c("Sample Mean", "95% Confidence Interval", "Hypothesized Mean (27)")
  )





#STEP 5 Load the cleaned data
load("data_cleaned.RData")

# Split the cleaned data by Breed
breed_samples <- split(data_cleaned, data_cleaned$Breed)

# Function to calculate 95% confidence interval for difference in means
compare_breeds <- function(breed1_data, breed2_data) {
  # Extract rehomed times for both breeds
  rehomed_breed1 <- na.omit(breed1_data$Rehomed)
  rehomed_breed2 <- na.omit(breed2_data$Rehomed)
  
  # Perform a two-sample t-test (Welch's t-test by default, assuming unequal variances)
  test_result <- t.test(rehomed_breed1, rehomed_breed2, conf.level = 0.95)
  
  # Return the test result
  return(data.frame(
    Mean_Breed1 = mean(rehomed_breed1),
    Mean_Breed2 = mean(rehomed_breed2),
    Difference = test_result$estimate[1] - test_result$estimate[2],
    CI_Lower = test_result$conf.int[1],
    CI_Upper = test_result$conf.int[2],
    P_Value = test_result$p.value
  ))
}

# Compare Bichon Frise vs. Border Collie
result_bichon_border <- compare_breeds(breed_samples$`Bichon Frise`, breed_samples$`Border Collie`)

# Compare Bichon Frise vs. Dobermann
result_bichon_dobermann <- compare_breeds(breed_samples$`Bichon Frise`, breed_samples$`Dobermann`)

# Compare Border Collie vs. Dobermann
result_border_dobermann <- compare_breeds(breed_samples$`Border Collie`, breed_samples$`Dobermann`)

comparison_results <- rbind(result_bichon_border, result_bichon_dobermann, result_border_dobermann)
# Combine the results into a data frame for visualization
comparison_results <- data.frame(
  Comparison = c("Bichon Frise vs. Border Collie", 
                 "Bichon Frise vs. Dobermann", 
                 "Border Collie vs. Dobermann"),
  Mean_Difference = c(result_bichon_border$Difference, 
                      result_bichon_dobermann$Difference, 
                      result_border_dobermann$Difference),
  CI_Lower = c(result_bichon_border$CI_Lower, 
               result_bichon_dobermann$CI_Lower, 
               result_border_dobermann$CI_Lower),
  CI_Upper = c(result_bichon_border$CI_Upper, 
               result_bichon_dobermann$CI_Upper, 
               result_border_dobermann$CI_Upper),
  P_Value = c(result_bichon_border$P_Value, 
              result_bichon_dobermann$P_Value, 
              result_border_dobermann$P_Value)
)

# Add formatted label for CI and p-value
comparison_results$Label <- paste0(
  round(comparison_results$Mean_Difference, 2), " (",
  round(comparison_results$CI_Lower, 2), ", ", 
  round(comparison_results$CI_Upper, 2), 
  "), p = ", round(comparison_results$P_Value, 3)
)

library(ggplot2)

# Updated Caterpillar Plot Code with Adjusted Spacing
ggplot(comparison_results, aes(x = reorder(Comparison, Mean_Difference), y = Mean_Difference)) +
  geom_point(size = 3, color = "darkblue") +  # Larger points for emphasis
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.3, color = "darkred") +  # Thicker error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +  # Reference line
  geom_text(
    aes(
      label = paste0(
        sprintf("%.2f", Mean_Difference), 
        " (", sprintf("%.2f", CI_Lower), ", ", sprintf("%.2f", CI_Upper), "), p = ", sprintf("%.3f", P_Value)
      )
    ), 
    nudge_y = 1.5,  # Adjust text position above the points
    size = 3,  # Adjust text size
    color = "black"
  ) +  # Proper placement for text
  labs(
    title = "Rehoming Time Comparison Across Breeds",
    subtitle = "Mean differences with 95% Confidence Intervals",
    x = "Breed Comparison", 
    y = "Mean Difference in Rehoming Time (Weeks)"
  ) +
  theme_minimal() +  # Clean background theme
  theme(
    axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 10),  # Horizontal labels
    axis.text.y = element_text(size = 10),  # Clear y-axis text
    plot.title = element_text(size = 16, face = "bold"),  # Larger title for impact
    plot.subtitle = element_text(size = 12),  # Subtitle for context
    axis.title.x = element_text(size = 12, face = "bold"),  # Emphasis on axis titles
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 10, 10, 20)  # Add padding for labels
  )



