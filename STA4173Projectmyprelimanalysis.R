# Install necessary packages if they are not already installed
packages <- c("ggplot2", "dplyr", "gridExtra", "reshape2", "corrplot", "tidyr", "tibble", "DataExplorer")
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
invisible(lapply(packages, install_if_missing))

library(ggplot2)
library(dplyr)
library(gridExtra)
library(reshape2)
library(corrplot) 
library(tidyr)
library(tibble)
library(DataExplorer)

df <- data.frame(heart_disease_health_indicators_BRFSS2015)
attach(df)

#Summary of dataset
summary(df)
names(df)
ncol(df)
nrow(df)
head(df)
str(df)

# Proportion of heart disease cases
ggplot(df, aes(x = as.factor(HeartDiseaseorAttack))) + geom_bar() + labs(title = "Distribution of Heart Disease Cases",
       x = "Heart Disease or Attack", y = "Count")

##################################################################################################################
# Loop through each feature to make histogram for each feature
num_features <- ncol(df)
num_cols <- 3
num_rows <- ceiling(num_features / num_cols)
plots <- list()
for (i in seq_along(names(df))) {
  column <- names(df)[i]
  
  p <- ggplot(df, aes_string(x = column)) +
    geom_histogram(bins = 20, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", column), x = column, y = "Frequency") +
    theme_minimal(base_size = 10)
  
  plots[[i]] <- p
}
grid.arrange(grobs = plots, ncol = num_cols)
##################################################################################################################
# Correlation matrix of all features
correlation_matrix <- cor(df, use = "complete.obs")

melted_corr <- as.data.frame(correlation_matrix) %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value")

ggplot(data = melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  # Display correlation values
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap")

# Sort and display correlation values for each variable
for (column in colnames(correlation_matrix)) {
  sorted_correlations <- as.data.frame(correlation_matrix) %>%
    select(all_of(column)) %>%
    arrange(desc(!!sym(column)))
  
  print(paste("Correlations with", column, ":"))
  print(sorted_correlations)
}
##################################################################################################################

# Create a list to store the plots
heart_disease_plots <- list()

# Loop through each column, except HeartDiseaseorAttack
for (feature in names(df)) {
  if (feature != "HeartDiseaseorAttack") {
    
    # Check if the feature is numeric or categorical
    if (is.numeric(df[[feature]])) {
      # Plot numeric feature as a box plot
      p <- ggplot(df, aes(x = as.factor(HeartDiseaseorAttack), y = .data[[feature]])) +
        geom_boxplot(fill = "lightblue") +
        labs(title = paste("Heart Disease vs", feature),
             x = "Heart Disease or Attack",
             y = feature) +
        theme_minimal()
      
    } else {
      # Plot categorical feature as a bar plot
      p <- ggplot(df, aes(x = as.factor(HeartDiseaseorAttack), fill = as.factor(.data[[feature]]))) +
        geom_bar(position = "dodge") +
        labs(title = paste("Heart Disease vs", feature),
             x = "Heart Disease or Attack",
             fill = feature) +
        theme_minimal()
    }
    
    heart_disease_plots[[feature]] <- p
  }
}

# Display all plots in a grid
do.call("grid.arrange", c(heart_disease_plots, ncol = 3))
##################################################################################################################

create_report(df)

