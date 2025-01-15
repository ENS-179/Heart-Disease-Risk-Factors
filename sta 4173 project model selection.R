library(MASS)

heart_data <- data.frame(heart_disease_health_indicators_BRFSS2015)
attach(heart_data)


model <- glm(HeartDiseaseorAttack ~ ., data = heart_data, family = binomial)
summary(model)

stepwise_model <- stepAIC(model, direction = "both")
summary(stepwise_model)

# Model after stepwise selection
model <- glm(HeartDiseaseorAttack ~ HighBP + HighChol + CholCheck + Smoker + 
                     Stroke + Diabetes + PhysActivity + Veggies + HvyAlcoholConsump + 
                     NoDocbcCost + GenHlth + MentHlth + DiffWalk + Sex + Age + 
                     Income, data = heart_data, family = binomial)
