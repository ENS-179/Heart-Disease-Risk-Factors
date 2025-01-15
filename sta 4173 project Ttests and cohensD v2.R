df <- data.frame(heart_disease_health_indicators_BRFSS2015)
attach(df)

# Summary of dataset
str(df)
summary(df)
names(df)

# Hypothesis 1: Higher age, smoking, and high blood pressure are significantly associated with a greater likelihood of heart disease.

# Subsets for individuals with and without heart disease
heart_disease_yes <- df[df$HeartDiseaseorAttack == 1, ]
heart_disease_no <- df[df$HeartDiseaseorAttack == 0, ]
# Function to calculate Cohen's d
cohen_d <- function(group1, group2) {
  n1 <- length(group1)
  n2 <- length(group2)
  mean1 <- mean(group1, na.rm = TRUE)
  mean2 <- mean(group2, na.rm = TRUE)
  sd1 <- sd(group1, na.rm = TRUE)
  sd2 <- sd(group2, na.rm = TRUE)
  sp <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  d <- (mean1 - mean2) / sp
  return(d)
}
# Outlining the variables to test
t_test_vars <- c("Age", "BMI", "GenHlth", "MentHlth", "PhysHlth", "Education", "Income")

# Generating a results table
t_test_results_table <- data.frame(
Variable = character(),
t_Statistic = numeric(),
p_Value = numeric(),
Cohen_d = numeric(),
stringsAsFactors = FALSE
)

# Looping for t-tests and Cohen's d
for (var in t_test_vars) {
  t_test_result <- t.test(heart_disease_yes[[var]], heart_disease_no[[var]], var.equal = TRUE)
  cohen_d_value <- cohen_d(heart_disease_yes[[var]], heart_disease_no[[var]])
  # Adding results to the correct table
  t_test_results_table <- rbind(t_test_results_table, data.frame(
  Variable = var,
  t_Statistic = round(t_test_result$statistic, 2),
  p_Value = round(t_test_result$p.value, 4),
  Cohen_d = round(cohen_d_value, 2)
))
}

t_test_results_table

