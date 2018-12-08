
#------------------------Assignment 17 -----------------------------
# Weight Lifting Exercise Analysis

# Import Data Set

data_set <- read.csv("E:/Data Analytics with RET/Assignment/Example_WearableComputing_weight_lifting_exercises_biceps_curl_variations.csv")
View(data_set)

# remove irrelevant collumns viz. name, cvtd_timestamp, new_window
data <- data_set[,-c(1,4,5)]
View(data)
str(data)

# 2. Perform the below given activities:
# a. Create classification model using logistic regression model
# the target variable variable is multiple level 

sum(is.na(data))  # there are no missing values

# spliting the data set for train and test

library(caTools)
set.seed(123)
split = sample.split(data$classe, SplitRatio = 0.7) 

train = subset(data, split == TRUE)            # train data
test = subset(data, split == FALSE)            # test data

library(nnet) ; library(MASS)
model <- multinom(classe ~., data = train)
summary(model)

# stepAIC(model, direction = "backward")

final <- multinom(classe ~ raw_timestamp_part_1 + num_window + roll_belt + pitch_belt + 
                    yaw_belt + total_accel_belt + gyros_belt_x + gyros_belt_y + 
                    gyros_belt_z + accel_belt_x + accel_belt_y + accel_belt_z + 
                    magnet_belt_x + magnet_belt_y + magnet_belt_z + roll_arm + 
                    pitch_arm + yaw_arm + total_accel_arm + gyros_arm_x + gyros_arm_y + 
                    gyros_arm_z + accel_arm_x + accel_arm_y + accel_arm_z + magnet_arm_y + 
                    magnet_arm_z + roll_dumbbell + pitch_dumbbell + yaw_dumbbell + 
                    gyros_dumbbell_x + gyros_dumbbell_z + accel_dumbbell_x + 
                    accel_dumbbell_y + accel_dumbbell_z + magnet_dumbbell_x + 
                    magnet_dumbbell_y + magnet_dumbbell_z + roll_forearm + pitch_forearm + 
                    yaw_forearm + total_accel_forearm + gyros_forearm_x + gyros_forearm_y + 
                    gyros_forearm_z + accel_forearm_x + accel_forearm_y + accel_forearm_z + 
                    magnet_forearm_x + magnet_forearm_y + magnet_forearm_z, data = train)
final
summary(final)

# Predictions
predicted <- predict(final, newdata= test)

# ---------------------------------------------------------------------

# b. Goodness of Fit
library(car)
chisq.test(table(test$classe), prop.table(table(predicted)))

# --------------------------------------------------------------------------
# c. Report the accuracy measures
# Accuracy
conf <- table(test$classe, predicted)
OAA <- (conf[1,1]+conf[2,2]+conf[3,3]+conf[4,4]+conf[5,5]) / sum(conf) 
OAA

# --------------------------------------------------------------------------
# d. Report the variable importance

coef(final)
library(caret)
varImp(final)

# --------------------------------------------------------------------------
# e. Report the unimportant variables
install_github("riv","tomasgreif")
install_github("woe","tomasgreif")

library(devtools); library(woe); library(riv)

iv_df <- iv.mult(train, y = "classe", summary= FALSE, verbose = TRUE)

variables <- c(colnames(train[,-56]))
imp_variables <- names(as.data.frame(coef(final)))

unimportant_variables <- setdiff(variables, imp_variables)
unimportant_variables

# --------------------------------------------------------------------------
# f. Interpret the results

# 1. Model execution output shows some iteration history and includes the final negative log-likelihood 49.43.
# This value is multiplied by two as shown in the model summary as the Residual Deviance 98.86

# 2. The summary output has a block of coefficients and another block of standard errors. 
# Each blocks has row of values corresponding to each category of DV
# and each column represents the predictor
# and the values show the coefficients and standard errors

library(pROC)
m <- multiclass.roc(as.numeric(classe) ~ as.numeric(predicted) , data = test)
# AUC = 0.9977

# --------------------------------------------------------------------------
# g. Visualize the results

plot <- plot(conf, col = topo.colors(6))

library(ggplot2)
ggplot(data = as.data.frame(conf), mapping = aes(x = predicted,y = Var1)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red", trans = "log")

#-------------------------------------------------------------------------
