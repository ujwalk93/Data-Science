##------------Bank Marketing Analysis---------------------##

########################################################
## Business Understanding:- Prospect Profiling
#The business objective is to achieve 80% of total responders at the minimum possible cost. 
#The total number of responders is the total number of prospects who responded, 
#from the available data of about 45,000 data points.
########################################################

########################################################
# The procedure followed in this assignment is:
# 1. Data Understanding, Preparation and EDA
# 2. Model building, evaluation based on test data and
# finding probabilities of response for entire original
# dataset based on the same.
# 3. Creating a new dataframe with relevant variables and
# finding the average cost of call for each prospect
# 4. Find the number of top X% prospects you should target to meet the business objective
# Report the average call duration for targeting the top X% prospects to the CMO
# 5. Creating a lift chart (no of prospects contacted vs response rate)
########################################################

#Loading bank marketing data in the working directory. 

bank_data<- read.csv("D:\\PG_Diploma\\Elective\\bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset

summary(bank_data)
#-------------------------------------------------------

########################################################
#1. Data cleaning, understanding and EDA
########################################################

# Checking response rate of prospect customer

response <- 4640/(36548+4640)
response
#0.1126542 (response rate)

# Checking missing values

sum(is.na(bank_data))

#-------------------------------------------------------

# Loading ggplot2 library
library(ggplot2)

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(bank_data$age,seq(0,1,0.01))

# Box plot 

boxplot(bank_data$age)

# Capping the upper values of age with 71.

bank_data[(which(bank_data$age>71)),]$age <- 71

# Binning the age variable and store it into "binning.age".

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)

# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values

agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

#-------------------------------------------------------

# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

##--------------------------------------------------------  

# Checking structure of dataset

str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job

levels(bank_data$job)

# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 

str(bank_data)

# Checking Marital status

summary(bank_data$marital)

# Let's replace Unknown level to married

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")

# Let's see the education variables

plot_response(bank_data$education,"Education")

# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")

#-------------------------------------------------------
# Let's see the default variable

table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 

summary(bank_data$housing)

plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------

#  Next variable is Contact, Let's see the response rate of each mode 

summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

#-------------------------------------------------------

# Next variable is "Month" i.e contact month. 

plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Let's do the same of "day_of_week" variable

plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------

# Now, Let's see the "duration" variable: Which is Quantitative variable

# Let's check the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

# Let's see the summary of this variable once 

summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)

bank_data <- bank_data[,-22]

## Definitely the outlier is present in the dataset

# So let's check the percentile distribution of duration 

quantile(bank_data$duration,seq(0,1,0.01))

# So, capping the duration seconds at 99% which is 1271.3sec 

bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

#-------------------------------------------------------

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 

summary(bank_data$campaign)

# Let's see the percentile distribution of this variable

boxplot(bank_data$campaign)

quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14

bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot

ggplot(bank_data,aes(campaign))+geom_histogram()

#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary

summary(bank_data$pdays)

levels(bank_data$pdays)

# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"

# Also,lets see the respose rate of each levels. 

plot_response(bank_data$pday,"Pday")

# Number of prospects under each category

table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never_contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More_than_3_times"

summary(bank_data$previous)

plot_response(bank_data$previous,"Previous_contacts")

# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')

summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")
#-------------------------------------------------------

########################################################
#2.  Model Building and Evaluation Using Logistic Regression
########################################################

# Required Packages

library(caret)
library(caTools)
library(dummies)

#---------------------------------------------------------    

# Removing binning variables 

bank_data <- bank_data[, -21]

#-------------------------------------------------------
#A. creating dummy variables by converting all categorical variables into numeric ones
#-------------------------------------------------------

bank_data$response <- as.integer(bank_data$response)

k1 <- bank_data

#using inbuilt function from 'dummies' library to create dummy variables for model building

bank_data <- dummy.data.frame(bank_data)

#converting response as factor type for ease of analysis

bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

#-------------------------------------------------------
#B. splitting into train and test data for model building
#-------------------------------------------------------

set.seed(100)

split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)

train <- bank_data[split_indices, ]

test <- bank_data[!split_indices, ]

nrow(train)/nrow(bank_data)

nrow(test)/nrow(bank_data)

#removing 'duration' column from the training and testing data sets as it is not required for model building
train_bank <- train[,-45]
test_bank <- test[,-45]
#duration is the 45th column of the training and testing data sets (in that order)

#rename incorrectly formatted variable
names(train_bank)[3] <- "jobblue_collar"
names(test_bank)[3] <- "jobblue_collar"
names(bank_data)[3] <- "jobblue_collar"

#-------------------------------------------------------
#C. model building - variable selection
#-------------------------------------------------------

#loading required libraries 

library(MASS)
library(car)

# Building the first model

logistic_1 <- glm(response ~ ., family = "binomial", data = train_bank)

summary(logistic_1)

# Using stepwise algorithm for removing insignificant variables 

logistic_2 <- stepAIC(logistic_1, direction = "both")

#checking vif of the final selected variables

vif(logistic_2) 

# stepAIC has removed some variables and only the following ones remain

logistic_3 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                  monthaug + monthdec + monthjun + monthmar + monthmay + monthnov +
		  day_of_weekfri + day_of_weekmon + day_of_weekthu + day_of_weektue +
		  campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days +
		  previousLess_than_3_times + poutcomefailure + emp.var.rate + cons.price.idx +
		  cons.conf.idx + euribor3m + nr.employed + educationTertiary_Education +
 		  jobblue_collar + jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_3 

vif(logistic_3)

summary(logistic_3)

#emp.var.rate is removed as it has the highest VIF (107.443)

logistic_4 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                  monthaug + monthdec + monthjun + monthmar + monthmay + monthnov +
		  day_of_weekfri + day_of_weekmon + day_of_weekthu + day_of_weektue +
		  campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days +
		  previousLess_than_3_times + poutcomefailure + cons.price.idx +
		  cons.conf.idx + euribor3m + nr.employed + educationTertiary_Education +
 		  jobblue_collar + jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_4 

vif(logistic_4)

summary(logistic_4)

#euribor3m is removed as it has the highest VIF (82.881)

logistic_5 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                  monthaug + monthdec + monthjun + monthmar + monthmay + monthnov +
		  day_of_weekfri + day_of_weekmon + day_of_weekthu + day_of_weektue +
		  campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days +
		  previousLess_than_3_times + poutcomefailure + cons.price.idx +
		  cons.conf.idx + nr.employed + educationTertiary_Education +
 		  jobblue_collar + jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_5 

vif(logistic_5)

summary(logistic_5)

#previousLess_than_3_times is removed as it has high VIF (10.72) and low significance (p-value = 0.053 > 0.05)

logistic_6 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                  monthaug + monthdec + monthjun + monthmar + monthmay + monthnov +
		  day_of_weekfri + day_of_weekmon + day_of_weekthu + day_of_weektue +
		  campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days +
		  poutcomefailure + cons.price.idx + cons.conf.idx + nr.employed + 
		  educationTertiary_Education + jobblue_collar + jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_6

vif(logistic_6)

summary(logistic_6)

#Now all variables have VIF <2, hence remove variables based on p-values
#cons.price.idx is removed since it has the highest p-value (0.253)

logistic_7 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                  monthaug + monthdec + monthjun + monthmar + monthmay + monthnov +
		  day_of_weekfri + day_of_weekmon + day_of_weekthu + day_of_weektue +
		  campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days +
		  poutcomefailure + cons.conf.idx + nr.employed + educationTertiary_Education + 
		  jobblue_collar + jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_7

vif(logistic_7)

summary(logistic_7)

#Now all variables have VIF <2, hence remove variables based on p-values
#educationTertiary_Education is removed since it has the highest p-value (0.1643)

logistic_8 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                  monthaug + monthdec + monthjun + monthmar + monthmay + monthnov +
		  day_of_weekfri + day_of_weekmon + day_of_weekthu + day_of_weektue +
		  campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days +
		  poutcomefailure + cons.conf.idx + nr.employed + jobblue_collar + 
		  jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_8

vif(logistic_8)

summary(logistic_8)

#Now all variables have VIF <2, hence remove variables based on p-values
#day_of_weekthu is removed since it has the highest p-value (0.1657)

logistic_9 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                  monthaug + monthdec + monthjun + monthmar + monthmay + monthnov +
		  day_of_weekfri + day_of_weekmon + day_of_weektue +
		  campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days +
		  poutcomefailure + cons.conf.idx + nr.employed + jobblue_collar + 
		  jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_9

vif(logistic_9)

summary(logistic_9)

#Now all variables have VIF <2, hence remove variables based on p-values
#day_of_weektue is removed since it has the highest p-value (0.2492)

logistic_10 <-  glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                  monthaug + monthdec + monthjun + monthmar + monthmay + monthnov +
		  day_of_weekfri + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
		  pdaysContacted_after_10days + poutcomefailure + cons.conf.idx + nr.employed + 
		  jobblue_collar + jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_10

vif(logistic_10)

summary(logistic_10)

#Now all variables have VIF <2, hence remove variables based on p-values
#day_of_weekfri is removed since it has the highest p-value (0.07824)

logistic_11 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                  monthaug + monthdec + monthjun + monthmar + monthmay + monthnov +
		  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
		  pdaysContacted_after_10days + poutcomefailure + cons.conf.idx + nr.employed + 
		  jobblue_collar + jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_11

vif(logistic_11)

summary(logistic_11)

#Now all variables have VIF <2, hence remove variables based on p-values
#loanno is removed since it has the highest p-value (0.05279)

logistic_12 <- glm(formula = response ~ age + jobretired + contactcellular + 
                  monthaug + monthdec + monthjun + monthmar + monthmay + monthnov +
		  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
		  pdaysContacted_after_10days + poutcomefailure + cons.conf.idx + nr.employed + 
		  jobblue_collar + jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_12

vif(logistic_12)

summary(logistic_12)

#Now all variables have VIF <2, hence remove variables based on p-values
#monthaug is removed since it has the highest p-value (0.0533)

logistic_13 <- glm(formula = response ~ age + jobretired + contactcellular + 
                  monthdec + monthjun + monthmar + monthmay + monthnov +
		  day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
		  pdaysContacted_after_10days + poutcomefailure + cons.conf.idx + nr.employed + 
		  jobblue_collar + jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_13

vif(logistic_13)

summary(logistic_13)

#Now all variables have VIF <2 and p-value less then 0.05, but the number of 
#variables is still too large. So we will remove the variables till all p-values are less than 0.001 
#monthdec is removed since it has the highest p-value (0.01676)

logistic_14 <- glm(formula = response ~ age + jobretired + contactcellular + 
                  monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
		  campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
		  poutcomefailure + cons.conf.idx + nr.employed + 
		  jobblue_collar + jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_14

vif(logistic_14)

summary(logistic_14)

#Now all variables have VIF <2 and p-value less then 0.05, but the number of 
#variables is still too large. So we will remove the variables till all p-values are less than 0.001 
#age is removed since it has the highest p-value (0.02)

logistic_15 <- glm(formula = response ~ jobretired + contactcellular + 
                  monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
		  campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
		  poutcomefailure + cons.conf.idx + nr.employed + 
		  jobblue_collar + jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_15

vif(logistic_15)

summary(logistic_15)

#Now all variables have VIF <2 
#jobretired is removed since it has the highest p-value (0.1468 > 0.05)

logistic_16 <- glm(formula = response ~ contactcellular + monthjun + monthmar +
		  monthmay + monthnov + day_of_weekmon + campaign + 
		  pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
		  poutcomefailure + cons.conf.idx + nr.employed + 
		  jobblue_collar + jobservices, family = "binomial", data = train_bank)

# checking vif for logistic_16

vif(logistic_16)

summary(logistic_16)

#Now all variables have VIF<=2 and p-value less then 0.05, but the number of 
#variables is still too large. So we will remove the variables till all p-values are less than 0.001 
#jobservices is removed since it has the highest p-value (0.00827)

logistic_17 <- glm(formula = response ~ contactcellular + monthjun + monthmar +
		  monthmay + monthnov + day_of_weekmon + campaign + 
		  pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
		  poutcomefailure + cons.conf.idx + nr.employed + 
		  jobblue_collar, family = "binomial", data = train_bank)

# checking vif for logistic_17

vif(logistic_17)

summary(logistic_17)

#Now all variables have VIF<2 and p-value less then 0.001
#Hence, logistic_17 is chosen as the final model

#The major assumption is that p-values of each variable in the final model are less than 0.001

#the method for eliminating variables after each model, is as follows:
#1. If the variables have high VIF and/or low signifance(p-value>0.05), the variable can be removed
#2. If the variables all have p-values less than 0.05 (or 0.01 if applicable), the variable having the highest VIF is considered (if VIF>3)
#3. If the variables all have VIF less than 2, the variable having the highest p-value is considered if the p-value is more than 0.05
#4. Still, if variables are remaining, they are eliminated in order of p-values until all have p-value<0.001
#5. This means finally variable selection is based on high VIF and then p-values

#-------------------------------------------------------
#D. Model Evaluation on Test Data
#-------------------------------------------------------

# Predicting probabilities of response for the test data

predictions_logit <- predict(logistic_17, newdata = test_bank[, -60], type = "response")
summary(predictions_logit)

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test_bank$response, positive = "yes")

conf

#-------------------------------------------------------
#E. Let's find out the optimal probalility cutoff 
#-------------------------------------------------------

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_bank$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.12)] 
cutoff
#0.07929293

# Let's choose the cutoff value of 7.93% for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.0793, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, test_bank$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#accuracy 
#0.7635966
sens
#sensitivity
#0.6702586 
spec
#specificity 
#0.7754469

#Let's choose the cutoff value of 7.4% for final model as all the three lines 
#converge closer to that point as shown in the graph.

predicted_response <- factor(ifelse(predictions_logit >= 0.074, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, test_bank$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
#accuracy 
#0.7169796 
sens
#sensitivity
#0.6975575
spec
#specificity
#0.7194455 

#accuracy, sensitivity and specificity all are around 70-72%

#Thus, the values of the important evaluation metrics, 
#accuracy, sensitivity and specificity are mentioned in comments above.

#7.93% is the cutoff found but the optimal probability cutoff is 7.4% 
#all the three lines converge closer to that point as shown in the graph.

########################################################
#For model evaluation and for finding cutoff we used the test data frame test_bank.
#However we now have to again find the predicted response based on entire dataset
#hence, the original data frame bank_data is involved in this step 
########################################################

#-------------------------------------------------------
#F. Finding and analysing predicted response and probability of responses for the enitire original data set
#-------------------------------------------------------

#Appending the probabilities and response variables to the original data

bank_data$predicted_probs <- predict(logistic_17, newdata = bank_data[, -61], type = "response")

bank_data$predicted_response <- factor(ifelse(bank_data$predicted_probs >= 0.50, "yes", "no"))

#finding the response rate
#success rate= Y/Y+N
response_rate <- table(bank_data$response)[2]/(table(bank_data$response)[1] + table(bank_data$response)[2])
response_rate
#0.112654

#-------------------------------------------------------
#G. Sorting the probabilities in decreasing order of probability of response.
#-------------------------------------------------------

bank_data <- bank_data[order(bank_data$predicted_probs, decreasing = T), ]

########################################################
#3. Creating new dataframe "test_predictions" and calculating cost of call for each propect
########################################################

#-------------------------------------------------------
#A. Adding required columns from the original data set
#-------------------------------------------------------

test_predictions <- bank_data[, c("response", "predicted_probs", "predicted_response","duration")]

#-------------------------------------------------------
#B. generating unique response ID for analysis
#-------------------------------------------------------

test_predictions$prospectID <- seq.int(nrow(test_predictions))

#-------------------------------------------------------
#C. calculate the cost of call for each prospect using the below formula
#Cost per call (INR) = 0.033*(duration_in_seconds) + 0.8
#-------------------------------------------------------

test_predictions$costOFcall <- (0.033*test_predictions$duration)+0.8

#checking the structure of the new data frame
str(test_predictions)
#thus, all the six required variables are present in the new data frame created.

########################################################
#4. Filtering top X% prospects to be targeted and determine the average call duration for the same
########################################################

# Loading dplyr package 
require(dplyr)
library(dplyr)

#-------------------------------------------------------
#A. Create a Table containing cumulative responses, gain, cumulative lift and total prospects
#-------------------------------------------------------

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)),
	   Total_Prospects= cumsum(total))
  return(gaintable)
}

bank_data$response <- as.factor(ifelse(bank_data$response =="yes",1,0))

LG = lift(bank_data$response, bank_data$predicted_probs, groups = 10)
#View the table details
LG

#-------------------------------------------------------
#From the table obtained, we can find the top X% prospects to be targeted
#-------------------------------------------------------

#Gain Chart 
plot(LG$Total_Prospects,LG$Gain,col="red",type="l",main="Gain Chart",xlab="number of prospects contacted",ylab = "% of positive response")

#Thus, as expected the model has increasing gain with respect to 
#the total number of prospects as shown in the above gain chart.

str(bank_data)
#duration is the 45th variable in the original bank_data data frame
#at 80% gain, in the 5th decile, we will get around 3758 responsers

#-------------------------------------------------------
#B. Determine the average call duration for the top X% prospects determined above
#-------------------------------------------------------

mean(bank_data[0:3758,45])
#289.0375

#For the top 50% responders, we will get 80% responses
#there are 41188 columns,
#therefore, number of responders to be contacted is 41188 * 0.5
#i.e. 20594.
#hence, the value of X is 50.
#Thus the average call duration for top 50% respondents is 289.0375 seconds.

########################################################
#5. Creating the lift chart
########################################################

#The ratio: response rate using the model/ response rate without using the model is nothing but the cumulative lift
#This effectively means we have to plot the lift chart with x-axis as number of prospects contacted and y-axis as
#cumulative lift (total prospects vs cumlift)

#-------------------------------------------------------
#Lift Chart (total prospects vs cumlift)
#-------------------------------------------------------

plot(LG$Total_Prospects,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="number of prospects contacted",ylab = "cumulative lift")

#Thus, as expected the model has decreasing cumulative lift with respect to 
#the total number of prospects as shown in the above lift chart.

########################################################
#The following are the assumptions made in this assignment.
#1. The model building is carried out without using the variable duration.
#2. The dummy variables are created using the inbuilt dummy.data.frame function
# present in the 'dummies' package.
#3. The cutoff is calculated and model evalution is carried out based on the test data.
#4. However, the predicted responses and the respective probability of responses are again
# calculated based on the entire original data set for later analysis as mentioned in relevant
# comments.
#5. The cutoff values, accuracy, sensitivity and specificity are low since the variable duration is
# not taken into consideration during model building and evaluation (on the test data set).
#6. The cutoff value 7.4% is chosen instead of 7.93% as all the three lines converge closer to that point as shown in the graph.
#7. The cost of call for each prospect is found out using the below formula
# Cost per call (INR) = 0.033*(duration_in_seconds) + 0.8
########################################################

########################################################
#Note: The value of X determined may vary with different final logistic regression models.
########################################################