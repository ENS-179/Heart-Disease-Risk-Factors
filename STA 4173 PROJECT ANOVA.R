library(tidyverse)

data <- data.frame(heart_disease_health_indicators_BRFSS2015_2)

data$Age <- as.factor(data$Age)

# BMI
anova_bmi <- aov(BMI ~ Age, data = data)
summary(anova_bmi)

# Phyiscal Health
anova_phys <- aov(PhysHlth ~ Age, data = data)
summary(anova_phys)

# Mental Health
anova_ment <- aov(MentHlth ~ Age, data = data)
summary(anova_ment)


# Tukey's for BMI
tukey_bmi <- TukeyHSD(anova_bmi)
print(tukey_bmi)

# Tukey's for Physical Health
tukey_phys <- TukeyHSD(anova_phys)
print(tukey_phys)

# Tukey's for MentL Health
tukey_ment <- TukeyHSD(anova_ment)
print(tukey_ment)


# Boxplot for BMI by Age
ggplot(data, aes(x = Age, y = BMI)) +
  geom_boxplot(aes(fill = Age), show.legend = FALSE) +
  labs(
    title = "Boxplot of BMI by Age Group",
    x = "Age Group",
    y = "BMI"
  ) +
  theme_minimal()

# Boxplot for Physical Health by Age
ggplot(data, aes(x = Age, y = PhysHlth)) +
  geom_boxplot(aes(fill = Age), show.legend = FALSE) +
  labs(
    title = "Boxplot of PhysHlth by Age Group",
    x = "Age Group",
    y = "PhysHlth"
  ) +
  theme_minimal()

# Boxplot for Mental Health by Age
ggplot(data, aes(x = Age, y = MentHlth)) +
  geom_boxplot(aes(fill = Age), show.legend = FALSE) +
  labs(
    title = "Boxplot of MentHlth by Age Group",
    x = "Age Group",
    y = "MentHlth"
  ) +
  theme_minimal()














