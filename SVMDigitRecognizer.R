########################### SVM Digit Recogniser #################################

#set the working directory
setwd("D:\\PG_Diploma\\SVM\\SVM Dataset")

########### Business Understanding ###########

#The objective is to develop a model using Support Vector Machine which would correctly 
#classify the handwritten digits (0-9) based on the pixel values given as features.

#####################################################################
# 1. Data understanding
#####################################################################

#Here, both the datasets are loaded and the structure of the datasets are analysed.

#load the required .csv files
mnist_train <- read.csv("mnist_train.csv",header=F,stringsAsFactors = F)
mnist_test <- read.csv("mnist_test.csv",header=F,stringsAsFactors = F)

#understanding dimensions and structure of dataset
dim(mnist_train)
dim(mnist_test)

str(mnist_train)
str(mnist_test)

#From the above commands, it is clear that the train and test datasets have 60000 and 10000 rows (instances) respectively.
#Both the train and test datasets have 785 columns (pixel attributes) ,each.
 
#####################################################################
# 2. Data Preparation
#####################################################################

#Here the required packages are loaded and data is checked for missing and NA values.
#Thus, data is converted into a format suitable for analysis.

#installing required packages
library(readr)
library(dplyr)
library(ggplot2)
library(kernlab)
library(caret)
library(caTools)
library(gridExtra)

#renaming the first column in the dataset
names(mnist_train)[1]<-"digit"
names(mnist_test)[1]<-"digit"

#printing the first few rows
head(mnist_train)
head(mnist_test)

#exploring the data
summary(mnist_train)
summary(mnist_test)

#check for missing values
sapply(mnist_train, function(x) length(which(x == '')))
sapply(mnist_test, function(x) length(which(x == '')))

#checking for NA values
sapply(mnist_train, function(x) sum(is.na(x)))
sapply(mnist_test, function(x) sum(is.na(x)))

#checking for duplicate rows
sum(duplicated(mnist_train))
sum(duplicated(mnist_test))

#checking whether there is difference in columns in both the csv files
setdiff(length(colnames(mnist_train)),length(colnames(mnist_test)))

#Thus, all anomalies and redundancies are eliminated and data is thus suitable for analysis.

#####################################################################
# 3. Exploratory Data Analysis (EDA)
#####################################################################

#In this step, we are analysing the different types of digits and their distribution among the pixel attributes

#changing output variable in each dataset into factor type for ease of analysis
mnist_train$digit <- as.factor(mnist_train$digit)
mnist_test$digit <- as.factor(mnist_test$digit)

#analysing the different factor levels of digit variable in each dataset
summary(mnist_train$digit)
summary(mnist_test$digit)

#analysing the distribution of different factor levels of digit variable in each dataset
plot(mnist_train$digit)
plot(mnist_test$digit)

#In both the train and test data set the attributes are 
#almost evenly distributed among all the factor classes 

#####################################################################
# 4. Model preparation
#####################################################################

#Here, preparation of test and train data and scaling of pixel attributes is carried out

#scaling the pixel values in train and test dataset
#pixel values range from 0 to 255
mnist_train[,-1] <- mnist_train[,-1]/255
mnist_test[,-1] <- mnist_test[,-1]/255
#this is done to convert all pixel attributes to 0 or 1
#thus ensuring consistency in analysis

#preparing train and test data for model building
set.seed(100)
indices <- sample.split(mnist_train$digit, SplitRatio = 0.15)
mnist_train1 <- mnist_train[indices,]

#changing output variable in the new dataset into factor type for ease of analysis
mnist_train1$digit <- as.factor(mnist_train1$digit)

#analysing the distribution of different factor levels of digit variable in the sample dataset w.r.t. original train data
plot(mnist_train1$digit)
plot(mnist_train$digit)

#It is clear that, the distributions of both the sample data and the original train data are comparable.

#here, only 15% of both the total training data is considered for ease of analysis 
#as very large amount of data is present in the train dataset
#however, the modelling is done on the entire test data to check accuracy.

#####################################################################
# 5. Model building, validation and evaluation
#####################################################################

#Here, after building models based on training data, the appropriate hyper parameters
#(C and sigma) are selected using 5-fold cross-validation for the different types of kernels.
#For cross-validation, the train function from caret package is used.

########### Linear model - SVM ###########

# Linear (vanilladot) kernel 
model_linear <- ksvm(digit ~ ., data = mnist_train1, scaled=FALSE, kernel = "vanilladot")

# Predicting the model results 
Eval_linear <- predict(model_linear, mnist_test,type="response")

# confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,mnist_test$digit)

#Accuracy - 91.7%
#Sensitivity range - (86.24% - 98.41%)
#Specificity range - (98.64% - 99.50%)

########### Hyperparameter tuning and Cross Validation for linear SVM ###########

trainControl <- trainControl(method="cv", number=5)
# Number - Number of folds 
# Method - cross validation
# Thus, 5-fold cross-validation is carried out for each of the kernels

metric <- "Accuracy"

set.seed(100)

# making a grid of C values. 
grid <- expand.grid(C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.svm_linear <- train(digit ~., data=mnist_train1, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm_linear)
# Best tune at C=1, Accuracy - 91.02%

# Plotting "fit.svm" results
plot(fit.svm_linear)

# Validating the model results (after cross validation) on test data

evaluate_linear_test<- predict(fit.svm_linear, mnist_test)
confusionMatrix(evaluate_linear_test, mnist_test$digit)

#Accuracy - 91.7%
#Sensitivity range - (94.25% - 99.3%)
#Specificity range - (99.53% - 99.81%) 

########### Non-linear (Radial) model - SVM ###########

# Radial (RBFdot) kernel
model_RBF <- ksvm(digit ~ ., data = mnist_train1, scaled=FALSE, kernel = "rbfdot")

# Predicting the model results 
Eval_RBF <- predict(model_RBF, mnist_test)

# confusion matrix - Radial Kernel
confusionMatrix(Eval_RBF,mnist_test$digit)

#Accuracy - 95.81%
#Sensitivity range - (92.86% - 99.12%)
#Specificity range - (99.42% - 99.74%)

########### Hyperparameter tuning and Cross Validation for non-linear (Radial) SVM ###########

# Making grid of "sigma" and C values. 
grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1))

# Performing 5-fold cross validation
fit.svm_radial <- train(digit ~., data=mnist_train1, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm_radial)
# Best tune at sigma = 0.03 & C=4, Accuracy - 96.61%

# Plotting model results
plot(fit.svm_radial)

# Validating the model results (after cross validation) on test data

evaluate_radial<- predict(fit.svm_radial, mnist_test)
confusionMatrix(evaluate_radial, mnist_test$digit)

#Accuracy - 96.88%
#Sensitivity range - (94.25% - 99.3%)
#Specificity range - (99.53% - 99.81%)

########### Non-linear (Polynomial) model - SVM ###########

# Polynomial (Polydot) kernel (2nd degree polynomial)
model_poly <- ksvm(digit ~ ., data = mnist_train1, scaled=FALSE, kernel = "polydot", kpar=list(degree=2))

# Predicting the model results 
Eval_poly <- predict(model_poly, mnist_test)

# confusion matrix - Polynomial Kernel
confusionMatrix(Eval_poly,mnist_test$digit)

#Accuracy - 96.16%
#Sensitivity range - (94.56% - 98.85%) 
#Specificity range - (99.41% - 99.73%)

########### Hyperparameter tuning and Cross Validation for non-linear (Polynomial) SVM ###########

# Making grid of degree, scale and C values. 
grid <- expand.grid(C=seq(1, 5, by=1),degree=seq(1, 5, by=1),scale=c(-2,-1,1,2))
# scale value 0 can be ignored as it implies no scaling

# Performing 5-fold cross validation
fit.svm_poly <- train(digit ~., data=mnist_train1, method="svmPoly", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm_poly)
# Best tune at degree = 2, scale =2 & C=1, Accuracy - 95.93%

# Plotting model results
plot(fit.svm_poly)

# Validating the model results (after cross validation) on test data
evaluate_poly<- predict(fit.svm_poly, mnist_test)
confusionMatrix(evaluate_poly, mnist_test$digit)

#Accuracy - 96.15%
#Sensitivity range - (93.56% - 98.85%) 
#Specificity range - (99.41% - 99.73%) 

#####################################################################
#Thus, appropriate hyperparameters are chosen using 5-fold cross-validation 
#(using train function from caret package) and hyperparameter tuning:
#1)C for linear kernel
#2)C and sigma for radial kernel
#3)C, scale and degree for polynomial kernel
#####################################################################


#####################################################################
#From the above observations, it is clear that:
#1)For linear kernel, C=1 gives the highest accuracy (91.02%)
#2)For radial kernel, C=4 and sigma=0.03 gives the highest accuracy (96.61%)
#3)For polynomial kernel, C=1, scale=2 and degree=2 gives the highest accuracy (95.93%)
#####################################################################


#####################################################################
#6. Final selected model
#####################################################################

#fit.svm_radial <- train(digit ~., data=mnist_train, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)
#Hyperparameters for the above model: C=4, sigma=0.03 (training accuracy - 96.61%, test accuracy - 96.88%)
#The radial kernel model is chosen for svm digit recognition since it has 
#the highest train and test accuracy after cross-validation.
#The train and test accuracies are comparable, so chances of overfitting are less.

########### Assumptions and notes made regarding the assignment ###########

#1. In our case manual selection of c values (explictly stating c values like C=1 and C=10 in ksvm) is not done for linear kernel 
#as it is taken care of by cross validation and the kernel = "vanilladot" parameter in the ksvm function.
#2. The distribution of the sample data (15% of training data) is proportional to the entire training data.
#3. Scaling is done since all pixel values are converted into range[0,1] for consistency in analysis.
#4. The warnings obtained (if any) after train function during cross-validation can be ignored as they do not affect the
#running and accuracy of the model.
#5. The three kinds of kernel used in this assignment are vanilladot, rbfdot and polydot.
#6. 5-fold Cross validation is done for all the above three kernels to compare accuracy.
#7. All the required parameter values for svm models are derived using cross validation rather than by manually.

########### Conclusions and inferences drawn from the assignment ###########
#1. The non-linear kernel accuracies (rbfdot and polydot) are much higher than the linear (vanilladot) kernel accuracy. 
#2. Less chances of overfitting occur in the non-linear kernels as the train and test accuracy differences are <0.3% in both cases.
#In other words, both the train and test accuracies of both non-linear kernels are comparable.
#3. The parameters for each kernel selected by cross validation that give the highest accuracies are:
#Linear (vanilladot) - C=1, Accuracy - 91.02%
#Radial (RBFdot) - sigma = 0.03 , C=4 , Accuracy - 96.61%
#Polynomial (polydot) - degree = 2, scale =2 , C=1, Accuracy - 95.93%
#4. The radial (rbfdot) kernel is best suited for svm digit recognition as it has the highest train and test accuracy of
#96.61% and 96.88% respectively.
#5. To further improve the accuracy of svm models, techniques like boosting and principal component analysis (PCA) can be carried out.
#However, these techniques are beyond the scope of this assignment.