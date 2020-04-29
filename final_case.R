library(tidyverse)
library(ggplot2)
library(plotly)
library(corrplot)
library(data.table)
library(broom)
library(caTools)
library(caret)
library(gridExtra)
library(gmodels)
library(fastDummies)
library(naivebayes)


# Set working directory
setwd("~/Desktop/Fall 2019 Academics/Bus 256_Marketing Analytics/Final case")

# Set Seed
set.seed(12345)

# Part 1 ------------------------------------------------------------------
# import the data
dta_bank <- fread("Bank Case.csv")

# Part 2 ------------------------------------------------------------------
# a) Variable description
# i) age: age of the client (numeric)
# ii) job: type of occupation of the client (categorical) 
# iii) marital: marital status  of the client (categorical)
# iv) education: eduacation status of the client (categorical)
# v) default: if the client has previously failed ailure to repay a debt including interest or principal on a loan (categorical)
# vi) housing: if the client has a housing loan (categorical)
# vii) loan: if the client has a personal loan (categorical)
# viii) contact: mode of communication with the client(categorical)
# ix) month:contact month with the client (categorical)
# x) day_of_week: contact day of the week with the client (categorical)
# xi) duration: duration of conversation with the client in seconds (numeric)
# xii) y: if the client has subscribed a bank term deposit (categorical)

# b) outlier detection; Numeric variables
age_sd <- sd(dta_bank$age)
duration_sd <- sd(dta_bank$duration)

 outlier_dta_bank <- dta_bank %>% 
   mutate(outlier_age = ifelse(age > 4*age_sd, "yes", "no"),
          outlier_duration = ifelse(duration > 4*duration_sd, "yes", "no"))

table(outlier_dta_bank$outlier_age)
# Based on this defition of outlier, there are 16142 obseravtions ~ 39.2% observations that are outliers
# in the age variable
table(outlier_dta_bank$outlier_duration)
# Based on this defition of an outlier, there are 858 obseravtions ~ 2.0% observations that are outliers
# in the duration variable

# Part 3 ------------------------------------------------------------------

# Create a corr-plot using the package corrplot.

dta_bank_cor <- dta_bank %>% 
  select(age, duration) %>% 
  as.matrix()

corrplot(dta_bank_cor, is.corr = FALSE,method = "circle")

# Part 4 ------------------------------------------------------------------
 dta_bank <- dta_bank %>% 
  mutate(y = ifelse(y == "yes", 1, 0))

       #  month = factor(month, levels = c("mar", "apr", "may", "jun", "jul",
        #                                  "aug", "sep", "oct", "nov", "dec")),
        # day_of_week = factor(day_of_week, levels = c("mon", "tue", "wed", "thu", "fri")),
         #job = factor(job, levels = c("unemployed", "services", "admin.", "blue-collar",
          #                            "technician", "retired", "management","housemaid",
          #                            "self-employed", "unknown", "entrepreneur", "student")))


# a) Use: rmarkdown to rwite the equation

# b
lm <- lm(y ~., data = dta_bank) %>% 
  tidy()
lm <-  data.frame(lm)
lm

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

lm <- lm %>% 
  mutate(prob = logit2prob(lm$estimate))
# i) Best time to perform telemarketing tasks: monday(), tuesday (44.44% lower), wednesday(44.49% lower) 
# (baseline is Monday)
# jul, nov, and may
# ii) Best income groups (top 3): jobblue-collar (65.74% higher), jobentrepreneur(65.47% higher), jobself-employed (63.61% higher)
# relative to unemployed
# iii) Potential concerns of omitted variable Bias: 
# Length of relationship with the bank (yrs)
# Type of caller(relationship manager or random sales agent)
# macroeconomic indiactors i.e inflation, bank interest rates, exchange rates (2008-2009 was the peak of the financial crisis with Portugal worst affected)


# Predictive Modelling ----------------------------------------------------

# 1: We split the data into training, validating, and test data in order to 
# improve and evaluate the perfomance of our predictive model without overfiiting the data. 
# More specifically, we use the train data to fit our model using the parameters in the data. 
# In addition, we use the valid data to chose parameters and tune the model in the train data 
# to avoid overfitting. Lastly, we use the the test dat the the perfomance and accuracy of 
# our model developed in the training data

# 2: In this predictive exercise, we need to drop the duration variable. This is because
# if duration = 0, then y = "no". However, the bank agent do not know he duration of
# each call prior to making the call as duration depeends on the clinnt's willingness
# and availability to talk with the agent. After the call, y is now known regadrdless of 
# of the duration. Thus, the duration does not provide reliable information to the bank
# since its main to to call the right customers at the right times. This variable should
# be dropped

# 3: Underfitting: Underfitting refers to a model that can neither model the training data nor 
# generalize to new data. A model that underfits the data often has low Root Meaan Sample Predictor 
# Error which is the expected value of the squared error made by predicting Y for
# an observation in the training data

# Overfitting: Overfitting refers to a model that too closely fit to a limited set of data points.
# In this case the model gets trained with so much of data such that it starts learning from the noise and 
# inaccurate data entries in our data set. A model that is overfit model has higher accuracy on the
# training data than on the test data

# 4: The No Free Lunch#  Theorem states that averaged over all problems, no optimization algorithm is 
# expected to perform better than any other optimization algorithm. In machine learning,
# this implies that although classifier can perfom better on one particular data,
# the perfomance of the classifiers is the same when we averaged over all the problems possible

# 5 Write their structural equations in Rmarkdown


# Sampling---------------------------------------------------------------------
dta_bank <- dta_bank %>% 
  select(-duration)
# Split data into train, validation and  test datasets: 80%:10%:10%

# Split into training and test
dta_bank_sample <-  sample.split(dta_bank$y, SplitRatio = .9)

# Test data: 10%
test_dta_bank <-  subset(dta_bank,  dta_bank_sample == FALSE)

# Split training data into training and validation
train_valid_dta_bank <- subset(dta_bank,  dta_bank_sample == TRUE)

train_valid_sample_dta_bank <-  sample.split(train_valid_dta_bank$y, SplitRatio = .9)

# Train data: 80%
train_dta_bank <- subset(train_valid_dta_bank, train_valid_sample_dta_bank == TRUE)

# Validation data: 10%
valid_dta_bank <- subset(train_valid_dta_bank, train_valid_sample_dta_bank == FALSE)

# remove used data
rm(train_valid_sample_dta_bank, train_valid_dta_bank, dta_bank_sample)

# lm_1 --------------------------------------------------------------------
# Estimated model
lm_1 <- lm(y~age + factor(month),  data = train_dta_bank) 
summary(lm_1)

# Valid data
# Predict values: Valid data
lm_1_pred_valid <- predict(lm_1, newdata = valid_dta_bank, type ="response")

# Compute a confusion matrix
confus_mat_lm_1_valid <- confusionMatrix(
  data = factor(as.numeric(lm_1_pred_valid>=0.5)), 
  reference = factor(valid_dta_bank$y))

# Accuracy
accuracy_valid_lm_1 <- round(((confus_mat_lm_1_valid$table[1,1] + confus_mat_lm_1_valid$table[2,2])/
                                sum(confus_mat_lm_1_valid$table))*100,3)

# Test data
# Predict values: Test data
lm_1_pred <- predict(lm_1, newdata = test_dta_bank, type ="response")

# Compute a confusion matrix
confus_mat_lm_1 <- confusionMatrix(
                                  data = factor(as.numeric(lm_1_pred>=0.5)), 
                                  reference = factor(test_dta_bank$y))
# Print confusion matrix
confus_mat_lm_1$table
accuracy_test_lm_1 <- round(((confus_mat_lm_1$table[1,1] + confus_mat_lm_1$table[2,2])/
                     sum(confus_mat_lm_1$table))*100,3)

# RMSPE
RMSPE_lm_1_test <- round(RMSE(lm_1_pred,test_dta_bank$y),3)
RMSPE_lm_1_valid <- round(RMSE(lm_1_pred_valid,valid_dta_bank$y),3)

# Remove used data sets
rm(list = ls(pattern = "^lm_1"))

# lm2 ---------------------------------------------------------------------
# estimated model
lm_2 = lm(y~age+ age*age+ I(age^2) + I(age^3)+factor(month),data= train_dta_bank)
summary(lm_2)
# Valid data
# Predict values: Valid data
lm_2_pred_valid <- predict(lm_2, newdata = valid_dta_bank, type ="response")

# Compute a confusion matrix
confus_mat_lm_2_valid <- confusionMatrix(
  data = factor(as.numeric(lm_2_pred_valid>=0.5)), 
  reference = factor(valid_dta_bank$y))

# Accuracy
accuracy_valid_lm_2 <- round(((confus_mat_lm_2_valid$table[1,1] + confus_mat_lm_2_valid$table[2,2])/
                                sum(confus_mat_lm_2_valid$table))*100,3)

# Test data
# Predict values: Test data
lm_2_pred <- predict(lm_2, newdata = test_dta_bank, type ="response")

# Compute a confusion matrix
confus_mat_lm_2 <- confusionMatrix(
  data = factor(as.numeric(lm_2_pred>=0.5)), 
  reference = factor(test_dta_bank$y))
# Print confusion matrix
confus_mat_lm_2$table
accuracy_test_lm_2 <- round(((confus_mat_lm_2$table[1,1] + confus_mat_lm_2$table[2,2])/
                               sum(confus_mat_lm_2$table))*100,3)

# RMSPE
RMSPE_lm_2_test <- round(RMSE(lm_2_pred,test_dta_bank$y),3)
RMSPE_lm_2_valid <- round(RMSE(lm_2_pred_valid,valid_dta_bank$y),3)

# Remove used data sets
rm(list = ls(pattern = "^lm_2"))

# lm_3 --------------------------------------------------------------------
# estimated model
lm_3 = lm(y~.,data= train_dta_bank)
summary(lm_3)
# Valid data
# Predict values: Valid data
lm_3_pred_valid <- predict(lm_3, newdata = valid_dta_bank, type ="response")

# Compute a confusion matrix
confus_mat_lm_3_valid <- confusionMatrix(
  data = factor(as.numeric(lm_3_pred_valid>=0.5)), 
  reference = factor(valid_dta_bank$y))

# Accuracy
accuracy_valid_lm_3 <- round(((confus_mat_lm_3_valid$table[1,1] + confus_mat_lm_3_valid$table[2,2])/
                                sum(confus_mat_lm_3_valid$table))*100,3)

# Test data
# Predict values: Test data
lm_3_pred <- predict(lm_3, newdata = test_dta_bank, type ="response")

# Compute a confusion matrix
confus_mat_lm_3 <- confusionMatrix(
  data = factor(as.numeric(lm_3_pred>=0.5)), 
  reference = factor(test_dta_bank$y))
# Print confusion matrix
confus_mat_lm_3$table
accuracy_test_lm_3 <- round(((confus_mat_lm_3$table[1,1] + confus_mat_lm_3$table[2,2])/
                               sum(confus_mat_lm_3$table))*100,3)

# RMSPE
RMSPE_lm_3_test <- round(RMSE(lm_3_pred,test_dta_bank$y),3)
RMSPE_lm_3_valid <- round(RMSE(lm_3_pred_valid,valid_dta_bank$y),3)

# Remove used data sets
rm(list = ls(pattern = "^lm_3"))

# lm_4 --------------------------------------------------------------------
# lm4 = lm(y~.^2, data=????)

# estimated model: Since age is the on continous varaible which can be squared
lm_4 <-  lm(y ~. + I(age^2), data= train_dta_bank)
summary(lm_4)

# Valid data
# Predict values: Valid data
lm_4_pred_valid <- predict(lm_4, newdata = valid_dta_bank, type ="response")

# Compute a confusion matrix
confus_mat_lm_4_valid <- confusionMatrix(
  data = factor(as.numeric(lm_4_pred_valid>=0.5)), 
  reference = factor(valid_dta_bank$y))

# Accuracy
accuracy_valid_lm_4 <- round(((confus_mat_lm_4_valid$table[1,1] + confus_mat_lm_4_valid$table[2,2])/
                                sum(confus_mat_lm_4_valid$table))*100,3)

# Test data
# Predict values: Test data
lm_4_pred <- predict(lm_4, newdata = test_dta_bank, type ="response")

# Compute a confusion matrix
confus_mat_lm_4 <- confusionMatrix(
  data = factor(as.numeric(lm_4_pred>=0.5)), 
  reference = factor(test_dta_bank$y))
# Print confusion matrix
confus_mat_lm_4$table
accuracy_test_lm_4 <- round(((confus_mat_lm_4$table[1,1] + confus_mat_lm_4$table[2,2])/
                               sum(confus_mat_lm_4$table))*100,3)

# RMSPE
RMSPE_lm_4_test <- round(RMSE(lm_4_pred,test_dta_bank$y),3)
RMSPE_lm_4_valid <- round(RMSE(lm_4_pred_valid,valid_dta_bank$y),3)

# Remove used data sets
rm(list = ls(pattern = "^lm_4"))

# Collect all errors and accurracies
model <- c(1:4)
accuracy_test <- c(accuracy_test_lm_1, accuracy_test_lm_2, accuracy_test_lm_3, accuracy_test_lm_4)
accuracy_valid <- c(accuracy_valid_lm_1, accuracy_valid_lm_2, accuracy_valid_lm_3, accuracy_valid_lm_4)
RMSPE_test <- c(RMSPE_lm_1_test,RMSPE_lm_2_test, RMSPE_lm_3_test, RMSPE_lm_4_test)
RMSPE_valid <- c(RMSPE_lm_1_valid,RMSPE_lm_2_valid, RMSPE_lm_3_valid, RMSPE_lm_4_valid)

model_sum <- cbind(model,accuracy_valid,accuracy_test,RMSPE_valid,RMSPE_test )

# a) Model 2 marginnaly overfits the data. This is becuase the accuracy on this model in the 
# the validation data (88.784%) is greater than its accuracy in the test data (88.832%).
# However, this overfitting by only a small margin since the difference the two accuracies is only 0.048

# b) Model 1 underfits the data. This model has the lowest accuracy in the test data of 88.784%
# and the highest RMSPE of 0.304. This suggest that we can tune this model to become more complex.
# At the current state, this model the model is unable to capture the relationship between the the Xs and t
# the Y as compare to the other models

# c) Yes, the model that fits the training data the best is model 4 since it has the lowest RMSPE
# This model also has the highest predictive power (accuracy) of 89.479% 

# d) Please see the results of the confusion marix above

# e) Based on these results, the best model of this data is model 4. This model is balanced
# as it neither underfits nor overfits the data. This model achieves the highest accuracy and 
# lowest RMSPE

# Improving the predictive power ------------------------------------------

# Visualizations ----------------------------------------------------------------------
dta_bank_plots <- fread("Bank Case.csv")
# Age
p1 <- ggplot(data = dta_bank_plots, aes(x=y, y=age, fill = y)) + geom_boxplot()+
  ggtitle("Age and Subscription") + xlab("Term deposit subscription") +
  theme(
    legend.position = "none",
    panel.border = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))

# Age Squared 
p2 <- ggplot(data = dta_bank_plots, aes(x=y, y=age^2, fill = y)) + geom_boxplot()+
  xlab("Term deposit subscription") +
  theme(
    legend.position = "none",
    panel.border = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))

# Age cubed
p3 <- ggplot(data = dta_bank_plots, aes(x=y, y=age^3, fill = y)) + geom_boxplot()+
  xlab("Term deposit subscription") +
  theme(
    legend.position = "none",
    panel.border = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")) 

# Combine age plots
age_plots <- grid.arrange(p1,p2, p3)
age_plots

# There appears to be no difference in the median age in teh yes and no categories
# with the mean being around 38 years in both categories.
# This entails that there is not linear relationship  between the cllent's
# subscription to the laon and their age. This also applies to age^2 and age^3

# Call duration
p4 <- ggplot(data = dta_bank_plots, aes(x=y, y=duration, fill = y)) + geom_boxplot() +
  ggtitle("Call Duration") + xlab("Duration") + ylab("Subscrption") +
  theme(
    legend.position = "none",
    panel.border = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"))
   
# Job type
p5 <- ggplot(data = dta_bank_plots, aes(x=job, fill =y)) + geom_bar() + 
  ggtitle("Job type") + 
  scale_fill_manual(values = c("light blue","navy")) +
  theme(
  panel.border = element_blank(),  
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "grey"))

# Marital status
p6 <- ggplot(data = dta_bank_plots, aes(x=marital, fill =y)) + geom_bar() + 
  ggtitle("Marital status") + 
  scale_fill_manual(values = c("light blue","navy")) +
  theme(
    panel.border = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"))

# Education
p7 <- ggplot(data = dta_bank_plots, aes(x=education, fill =y)) + geom_bar() + 
  ggtitle("Education status") + 
  scale_fill_manual(values = c("light blue","navy")) +
  theme(
    panel.border = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"))

# Default
p8 <- ggplot(data = dta_bank_plots, aes(x=default, fill =y)) + geom_bar() + 
  ggtitle("Default status") + 
  scale_fill_manual(values = c("light blue","navy")) +
  theme(
    panel.border = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"))

# Housing loan
p9 <- ggplot(data = dta_bank_plots, aes(x=housing, fill =y)) + geom_bar() + 
  ggtitle("Housing loan status") + 
  scale_fill_manual(values = c("light blue","navy")) +
  theme(legend.position = "none",
    panel.border = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"))

# Personal Loan 
p10 <- ggplot(data = dta_bank_plots, aes(x=loan, fill =y)) + geom_bar() + 
  ggtitle("Personal loan status") + 
  scale_fill_manual(values = c("light blue","navy")) +
  theme(legend.position = "right",
    panel.border = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"))

# Combine loan plots
loan_plots <- grid.arrange(p9,p10, nrow= 1)

# Contact status
p11 <- ggplot(data = dta_bank_plots, aes(x=contact, fill =y)) + geom_bar() + 
  ggtitle("Contact status") + 
  scale_fill_manual(values = c("light blue","navy")) +
  theme(
    panel.border = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"))

# Month of the year
p12 <- ggplot(data = dta_bank_plots, aes(x=month, fill =y)) + geom_bar() + 
  ggtitle("Month") + 
  scale_fill_manual(values = c("light blue","navy")) +
  theme(
    panel.border = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"))

# Day of week
p13 <- ggplot(data = dta_bank_plots, aes(x=day_of_week, fill =y)) + geom_bar() + 
  ggtitle("Day of week") + 
  scale_fill_manual(values = c("light blue","navy")) +
  theme(
    panel.border = element_blank(),  
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"))




rm(list = ls(pattern = "^p"))
# KNN ---------------------------------------------------------------------
# Convert to factors

dta_bank_knn <- fread("Bank Case.csv") %>% 
  mutate(y = ifelse(y == "yes", 1, 0),
  # Normalize numeric variable      
          age = scale(age))%>% 
  # Remove duration variable
  select(-duration)

dta_bank_knn<- fastDummies::dummy_cols(dta_bank_knn) %>% 
  select(-job, -marital, -education, -default,-housing, -loan, -contact, -month, -day_of_week ) %>% 
  select(y, everything())

# Sampling KNN---------------------------------------------------------------------
# Split data into train, validation and  test datasets: 80%:10%:10%

# Split into training and test
dta_bank_sample <-  sample.split(dta_bank_knn$y, SplitRatio = .9)

# Test data: 10%
test_dta_bank <-  subset(dta_bank_knn,  dta_bank_sample == FALSE)

# Split training data into training and validation
train_valid_dta_bank <- subset(dta_bank_knn,  dta_bank_sample == TRUE)

train_valid_sample_dta_bank <-  sample.split(train_valid_dta_bank$y, SplitRatio = .9)

# Train data: 80%
train_dta_bank <- subset(train_valid_dta_bank, train_valid_sample_dta_bank == TRUE)

# Validation data: 10%
valid_dta_bank <- subset(train_valid_dta_bank, train_valid_sample_dta_bank == FALSE)

# remove used data
rm(train_valid_sample_dta_bank, train_valid_dta_bank, dta_bank_sample)

# create labels for training, valid, and test data
train_dta_labels = train_dta_bank[, 1] 
valid_dta_labels = valid_dta_bank[, 1] 
test_dta_labels  = test_dta_bank[, 1]

# Training model on validating data                            
knn_valid_pred = class::knn(train = train_dta_bank,
                           cl    = train_dta_labels,
                           test  = valid_dta_bank,
                             k   = 1)

# Evaluating performance on validating data                          
k1_valid_conf_mat  =   gmodels::CrossTable(x    = valid_dta_labels, 
                                           y    = knn_valid_pred,
                                     prop.chisq = TRUE)

# Accuracy
k1_valid_accuracy <- round(((k1_valid_conf_mat$t[1,1] + k1_valid_conf_mat$t[2,2])/
                   sum(k1_valid_conf_mat$t))*100,3)

# Training model on test data                            
knn_test_pred = class::knn(train = train_dta_bank,
                            cl = train_dta_labels,
                            test  = test_dta_bank,
                            k  = 1)

# Evaluating performance on training data                          
k1_test_conf_mat  =   gmodels::CrossTable(x    = test_dta_labels, 
                                           y    = knn_test_pred,
                                                 prop.chisq = TRUE)

# Accuracy
k1_test_accuracy <- round(((k1_test_conf_mat$t[1,1] + k1_test_conf_mat$t[2,2])/
                              sum(k1_test_conf_mat$t))*100,3)

# RMSPE
RMSPE_knn_test <- round(RMSE(as.numeric(knn_test_pred),test_dta_bank$y),3)
RMSPE_knn_valid <- round(RMSE(as.numeric(knn_valid_pred),valid_dta_bank$y),3)

# Remove used data sets
rm(list = ls(pattern = "^knn"))

# Naive bayes -------------------------------------------------------------

# Estimate model: Validation data
NBclassifier_valid <- naivebayes::naive_bayes(formula      = factor(y)~.,
                                     usekernel = T,
                                     data      = train_dta_bank)

valid_pred <- predict(NBclassifier_valid,newdata = valid_dta_bank)

# Compute a confusion matrix 
confus_mat_valid <- confusionMatrix(
  data = factor(valid_pred), 
  reference = factor(valid_dta_bank$y))

NB_valid_accuracy <- round(((confus_mat_valid$table[1,1] + confus_mat_valid$table[2,2])/
                               sum(confus_mat_valid$table))*100,3)


# Estimate model: Test data
NBclassifier_test <- naivebayes::naive_bayes(formula      = factor(y)~.,
                                        usekernel = T,
                                        data      = train_dta_bank)

test_pred <- predict(NBclassifier_test ,newdata = test_dta_bank)

# Compute a confusion matrix 
confus_mat_test <- confusionMatrix(
  data = factor(test_pred), 
  reference = factor(test_dta_bank$y))

NB_test_accuracy <- round(((confus_mat_test$table[1,1] + confus_mat_test$table[2,2])/
                              sum(confus_mat_test$table))*100,3)

# RMSPE
RMSPE_NB_test <- round(RMSE(as.numeric(test_pred),test_dta_bank$y),3)
RMSPE_NB_valid <- round(RMSE(as.numeric(valid_pred),valid_dta_bank$y),3)

# Compile results
model <- c("KNN", "Naive Bayes")
KNN <-  c(k1_valid_accuracy,k1_test_accuracy, RMSPE_knn_valid, RMSPE_NB_test)
NB <- c(NB_valid_accuracy, NB_test_accuracy, RMSPE_NB_valid, RMSPE_NB_test)
Metric <- c("Accuracy", "Accuracy", "RMSPE", "RMSPE")
Data <- c("Validating", "Training", "Validating", "Validating")

# Results tables
model <- cbind(Data,Metric, as.numeric(KNN), as.numeric(NB))

# I applied NB classifier and KNN on on model 4 which I had identified as the most balanced
# model. We notice that the accauracy increases to 


# Causal Questions --------------------------------------------------------
 
# 1a) In marketing it would be better to use a causal analyis approach when 
# we want to determine whether a particular X variable (eg ad exposure or expenditure) affects the 
# the dependent y variable (purchase decision or sales) respectively.
# In particular, we might want to assess the impact of an advertising
# campaign on consumer purchasing decision.

# 1b) Biased estimates in linear regression entail that our estimates either over-estimate or
# understimate that true value of the population parameter. Apart from data collection errors,
# the most common cause of biased coefficient estimates in linear regression is ommitted variable bias
# This occurs when there are variables that both affect the dependent variable and are correlated with the
# variables that are currently in the model. In this situation, the estimated coefficient are biased
# and we cannot make accurance inference on causality

# 2) Examples of causal analyis in marketing
#  i ) Y = Purchase decision (yes or no), X =  ad exposure of shoes on Google (aategorical), 
# Company = Nike
# ii)  Y = Sales volume (numeric), X = marketing or ad expenditure on TVs & Social Media (numeric), 
# Company = Coca Cola
# iii) Y = Sales volume (numeric), X = iphones color designs (categorical),
# Company = Apple

# 3) Ommited variable
 # i) customer income : Customers with higher income are more likely to purchase Nike Shoes
# and are alos more likely to have internet see the add on Google. Also Google is banned in some
# countries hence all this can result to an upward bias on X estimates

 # ii) Customer age: Coke consumption varies widely across age groups bases on health choices
 # and use of TVs and social media also varies widelly across age groups. As result, 
 # age is correlated with bothe Y and X in this model

# iii) Price of the iphone: Different designs of iphones (Gold or Silver coloer) require different raw
# materials hence price here is corrlated with the X variable. Also, Price is corrlated with sale volume
# since it might be that more expensive iphones have better features. As a results, price is an ommited 
# variable which can lead to biased estimates n the X variable.








