### SETTING UP THE ENVIRONMENT ###

#Install and load required packages
install.packages("readxl")
install.packages("tidyr")
install.packages("dplyr")
install.packages("stats")

library(readxl)
library(tidyr)
library(dplyr)
library(stats)

#Load data from Excel
file_path <- "C:/Users/.../data_survey.xlsx"
data <- read_excel(file_path)
head(data)


### PREPROCESSING DATA - SEE BELOW FOR ACTUAL TESTS ###

# We need to reorganize the data to be able to do the calculations 
df_long <- data[rep(row.names(data), each = 10), ] # Each participant gives 10 fairness scores (2 questions with 5 occupations)

occupation_order <- c("babysitters", "carpenters", "engineers", "judges", "psychologists") # Names of the five occupations
occupation_order_repeated <- rep(occupation_order, 114) # Repeated twice (57 participants in total)
df_long$Occupation <- occupation_order_repeated # New dataframe called df_long 

question_order <- c("1", "2") # Names of the two questions
question_order_repeated <- rep(question_order, 285) # Repeated 285 times (57 x 5)
df_long$Question <- question_order_repeated


# Arrange this dataframe by Participant and Question
df_ordered <- df_long %>%
  arrange(Participant, Occupation)


# Now that the format is correct, the values need to be organized as well 
participant_values <- list()

# Define the current column names, where 1 and 2 refer to the question at hand
steps <- c("Fairness1_babysitters", "Fairness2_babysitters", "Fairness1_carpenters", 
           "Fairness2_carpenters", "Fairness1_engineers", "Fairness2_engineers", 
           "Fairness1_judges", "Fairness2_judges", "Fairness1_psychologists", 
           "Fairness2_psychologists")


# Loop over the results for each unique participant
for (participant in unique(data$Participant)) {
  # Select only the relevant data for the current participant
  participant_df <- subset(data, Participant == participant)
  participant_step_values <- list()
  
  # Check all columns (5 x 2)
  for (step in 1:10) {
    col_name <- steps[step]
    values <- participant_df[[col_name]]
    participant_step_values[[col_name]] <- values
  }
  
  participant_values[[participant]] <- participant_step_values
}

# Add these values as a new column to the ordered long dataframe 
flat_list <- unlist(participant_values, recursive = FALSE)
df_ordered$Score <- flat_list


# Rename columns in long dataframe 
final_columns <- c("Participant", "Gender", "Age", "Occupation", "Question", "Score")
data_final <- df_ordered[final_columns]
data_final$'Score' <- as.numeric(data_final$'Score') # Needed in order to be able to do calculations 
data_final$'Score' <- as.numeric(data_final$'Gender') # Needed in order to be able to do calculations 


# Reshape the data once again to have the responses from each round in separate columns
responses_wide <- pivot_wider(data = data_final, 
                              id_cols = c(Participant, Gender, Age, Occupation),
                              names_from = Question,
                              values_from = Score)

# Save different data formats 
save(responses_wide, data_final, data, file="survey_data.Rda")



### ANALYSIS Q1 AND Q2 ###

### Check for differences within each occupation ###
occupations <- unique(responses_wide$'Occupation')
results <- list()
cohen_d <- list()
mean_diff <-list()

for (occupation in occupations) {
  # Filter data for the current occupation
  data_subset <- responses_wide %>% filter(Occupation == occupation)
  
  # Perform Wilcoxon signed-rank test
  wilcox_test <- wilcox.test(data_subset$`1`, data_subset$`2`, paired = TRUE, exact = FALSE, correct = TRUE)
  results[[occupation]] <- wilcox_test
  
  # Calculate Cohen's d
  mean_diff[[occupation]] <- mean(data_subset$'1')- mean(data_subset$'2')
  cohen_d[[occupation]] <- mean_diff[[occupation]] / sqrt((56*var(data_subset$'1') + 56*var(data_subset$'2'))/(56+56-2))
}

# Print results
for (occupation in occupations) {
  print(paste("Occupation:", occupation))
  print(results[[occupation]])
  print(paste("Cohen's d:", cohen_d[[occupation]]))
  print(paste("Mean diff:", mean_diff[[occupation]]))
}


### Check for differences across occupations ###
data_Q1 <- filter(data_final, Question == "1")
pairwise1 <- pairwise.wilcox.test(data_Q1$Score, data_Q1$Occupation, p.adjust.method = "none")
print(pairwise1)


data_Q2 <- filter(data_final, Question == "2")
pairwise2 <- pairwise.wilcox.test(data_Q2$Score, data_Q2$Occupation, p.adjust.method = "none")
print(pairwise2)



### Check for differences between answers given by men and women ###
results <- list()

# Perform Wilcoxon rank-sum test for each combination of occupation and question
for (occupation in unique(data_final$Occupation)) {
  for (question in unique(data_final$Question)) {
    # Filter data for the current occupation and question
    data_subset <- filter(data_final, Occupation == occupation, Question == question)
    data_subset <- filter(data_subset, Gender != 2) # Gender=2 not included, because this is the ``other" category
    
    test_result <- wilcox.test(Score ~ Gender, data = data_subset)
    results[[paste(occupation, question, sep = "_")]] <- test_result
  }
}

# Print results
for (result in names(results)) {
  print(paste("Occupation and Question:", result))
  print(results[[result]])
}




### ANALYSIS Q3 AND Q4 ###
# Q3 #
count_accuracy <- 29  
count_equality <- 28  
total_count <- count_accuracy + count_equality  

# Perform binomial test and print results
binom_test1 <- binom.test(x = count_accuracy, n = total_count, p = 0.5) # p=0.5 because we want to check for equal probabilities
print("Binomial Test:")
print(binom_test1)


# Q4 #
count_accuracy <- 27  
count_equality <- 30  
total_count <- count_accuracy + count_equality  

# Perform binomial test and print results
binom_test2 <- binom.test(x = count_accuracy, n = total_count, p = 0.5) # p=0.5 because we want to check for equal probabilities
print("Binomial Test:")
print(binom_test2)



### Check for differences between men and women ###
# Q3 #
data_subset <- filter(data, Gender != 2) # Gender=2 not included, because this is the ``other" category
contingency_table <- table(data_subset$Gender, data_subset$Chatbot)
print(contingency_table)

# Perform the Chi-Square test and print results
chi_square_test <- chisq.test(contingency_table)
print("Chi-Square Test:")
print(chi_square_test)


# Q4 #
data_subset <- filter(data, Gender != 2) # Gender=2 not included, because this is the ``other" category
contingency_table <- table(data_subset$Gender, data_subset$Service)
print(contingency_table)

# Perform the Chi-Square test and print results
chi_square_test <- chisq.test(contingency_table)
print("Chi-Square Test:")
print(chi_square_test)
