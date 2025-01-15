install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggcorrplot")
install.packages("DataExplorer")

library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(DataExplorer)

data <- data.frame(heart_disease_health_indicators_BRFSS2015_2)

# overview of the dataset
str(data)
summary(data)

# missing values
missing_values <- colSums(is.na(data))
print(missing_values)

# summary stats
summary_stats <- data %>%
  summarise(
    BMI_mean = mean(BMI, na.rm = TRUE),
    MentHlth_mean = mean(MentHlth, na.rm = TRUE),
    PhysHlth_mean = mean(PhysHlth, na.rm = TRUE),
    Age_mean = mean(Age, na.rm = TRUE)
  )
print(summary_stats)

# correlation matrix
numeric_data <- select_if(data, is.numeric)
cor_matrix <- cor(numeric_data, use = "complete.obs")
ggcorrplot(cor_matrix, title = "Correlation Matrix", lab = TRUE, lab_size = 3)

# bar plots
ggplot(data, aes(x = factor(Smoker))) + geom_bar() + ggtitle("Distribution of Smoker")
ggplot(data, aes(x = factor(Stroke))) + geom_bar() + ggtitle("Distribution of Stroke")
ggplot(data, aes(x = factor(HeartDiseaseorAttack))) + geom_bar() + ggtitle("Distribution of Heart Disease or Attack")
ggplot(data, aes(x = factor(HighBP))) + geom_bar() + ggtitle("Distribution of High Blood Pressure")
ggplot(data, aes(x = factor(Diabetes))) + geom_bar() + ggtitle("Distribution of Diabetes")


# histograms
ggplot(data, aes(x = BMI)) + geom_histogram(bins = 30, fill = "blue", color = "black") + ggtitle("Distribution of BMI")
ggplot(data, aes(x = MentHlth)) + geom_histogram(bins = 30, fill = "blue", color = "black") + ggtitle("Distribution of Mental Health")
ggplot(data, aes(x = PhysHlth)) + geom_histogram(bins = 30, fill = "blue", color = "black") + ggtitle("Distribution of Physical Health")
ggplot(data, aes(x = Age)) + geom_histogram(bins = 30, fill = "blue", color = "black") + ggtitle("Distribution of Age")

# box plots
ggplot(data, aes(x = factor(HeartDiseaseorAttack), y = BMI)) + geom_boxplot() + ggtitle("Boxplot of BMI by Heart Disease or Attack")
ggplot(data, aes(x = factor(HeartDiseaseorAttack), y = MentHlth)) + geom_boxplot() + ggtitle("Boxplot of Mental Health by Heart Disease or Attack")
ggplot(data, aes(x = factor(HeartDiseaseorAttack), y = PhysHlth)) + geom_boxplot() + ggtitle("Boxplot of Physical Health by Heart Disease or Attack")
ggplot(data, aes(x = factor(HeartDiseaseorAttack), y = Age)) + geom_boxplot() + ggtitle("Boxplot of Age by Heart Disease or Attack")

# pairs plots
pairs(select(numeric_data, BMI, MentHlth, PhysHlth, Age), main = "Pairs Plot")

create_report(data)


