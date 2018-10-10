#Step 1: Loading the data to dataframe 
#setting the working directory 
#setwd("C:/Users/sithota/Documents/eda_gramener_case_study")

#Load the data file
loan <- read.csv("loan.csv", header=T, stringsAsFactors = FALSE)


#Step 2: Cleaning the Data 
#Remove the columns with all the row values as NA
loan <- loan[,colSums(is.na(loan))<nrow(loan)]

#Remove columns with all rows with 0 value
loan <- loan[,!is.na(colSums(loan != 0)) & colSums(loan != 0) > 0]

#Remove pymnt_plan column as it has same value n in all rows
which(loan$pymnt_plan != "n")
#Remove initial_list_status column as it has same value f in all rows
which(loan$initial_list_status != "f")
#Remove policy_code column as it has same value 1 in all rows
which(loan$policy_code != 1)
#Remove application_type column as it has same value INDIVIDUAL in all rows
which(loan$application_type != "INDIVIDUAL")

loan <- loan[ , -which(names(loan) %in% c("pymnt_plan","initial_list_status",
                                          "policy_code","application_type"))]

#write.csv(loan,"loan_clean.csv")

#loading all the required libraries
library(ggplot2)
library(stringr)
library(GGally)
library(tidyr)
library(dplyr)
#library(lubridate)

#understanding the data based on business perspective
#from the given .csv file, there are 3 types of data used for analysis in this case study
#1. Data based on customer information and demographics such as emp_title, emp_length, home_ownership, zip_code and addr_state
#2. Data based on loan characteristics such as loan_amnt, funded_amnt, installment, loan_status and int_rate
#3. Data based on customer behaviour such as last_pymnt_amnt, application_type, open_acc, total_pymnt and delinq_amnt

#business perspective
#The company wants to identify defaulters and 'risky' loan applicants so that such loans can be reduced to cut the credit loss.
#However, customer behaviour data is neglected as it cannot be collected at the time of application.
#It also wants to identify the driving factors behind the loan default and use this knowledge for portfolio and risk assessment.
#Thus, the main objective of this case study is to identify 'risky' loan applicants using Exploratory Data Analysis (EDA).

#check if all rows of term are in months
sum(str_detect(loan$term,"months"))
#remove sub string month from column term
loan$term <- as.integer(gsub(" months","",loan$term))

#convert all characters types to factor by sapply (for ease of analysis)
loan[sapply(loan, is.character)] <- lapply(loan[sapply(loan, is.character)], as.factor)

#remove % from interest rate(inst_rate)
loan$int_rate <- as.numeric(gsub("%","",loan$int_rate))

#check if any NA values in any columns
sapply(loan, function(x) any(is.na(x))) # no NA values

#check if blank values in any columns
sapply(loan, function(x) any(x == ''))  #emp_title, desc, revol_util, next_pymnt_d, last_credit_pull_d

#check if any negative values are present
sapply(loan, function(x) any(is.numeric(x) && x < 0)) #no negative values 
#i.e. no outliers

#use custom function for date conversion to appropriate format
customdate <- function(date) {
str_replace_all(date,pattern = "[/]",replacement = "-")
date<- paste("01", date, sep = "-")
date <- as.Date(date, format = "%d-%b-%y")
}

#convert date columns into appropriate format for ease of analysis
loan$issue_d <- customdate(loan$issue_d)
loan$earliest_cr_line <- customdate(loan$earliest_cr_line)
loan$last_pymnt_d <- customdate(loan$last_pymnt_d)
loan$next_pymnt_d <- customdate(loan$next_pymnt_d)
loan$last_credit_pull_d <- customdate(loan$last_credit_pull_d)
#Fill blank spaces in emp_title with unknown

#convert loan term as factor
loan$term <- as.factor(loan$term)

#Remove next_pymnt_d as it is not useful for our analysis to know estimated date 
#of next payment 

#Step 3: Univariate and Segmented Univariate Analysis and deriving relevant insights and conclusions

#filter data with loan status as charged off as we need to analyse the factors leading to charged off
loan_defaulters <- subset(loan, loan_status == "Charged Off")

#segmented univariate analysis w.r.t.  loan_amnt
summary(loan$loan_amnt)
summary(loan_defaulters$loan_amnt)
#Number of loans and charged off loans are more for loan amount 10000
ggplot(loan, aes(x = loan_amnt,fill=loan_status)) + geom_histogram(binwidth = 500)
ggplot(loan_defaulters, aes(x = loan_amnt,fill=loan_status)) + geom_histogram(binwidth = 500)

#univariate analysis on annual income
summary(loan$annual_inc) # better to remove outliers for 99% for better visualization
summary(loan_defaulters$annual_inc) # better to remove outliers for 99% for better visualization

#segmented univariate analysis with respect to loan defaulters

#loan grades B,C and D have more number of charged off loans
ggplot(loan_defaulters, aes(x=loan_defaulters$grade,fill=loan_status)) + geom_bar() + ggtitle("Grade vs charged off loans") + labs(x="Grade", y= "charged off loans")

#loan subgrade B5 has the most number of charged off loans
ggplot(loan_defaulters, aes(x=loan_defaulters$sub_grade,fill=loan_status)) + geom_bar() + ggtitle("Sub-grade vs charged off loans") + labs(x="Sub-Grade", y= "charged off loans")

#loans applied by people employed for 10+ years have  the most number of charged off loans
ggplot(loan_defaulters, aes(x=loan_defaulters$emp_length,fill=loan_status)) + geom_bar() + ggtitle("Employment length vs charged off loans") + labs(x="Employment Length", y= "charged off loans")

#loans having term of 36 months have the highest number of charged-off loans. 
ggplot(loan_defaulters, aes(x=loan_defaulters$term,fill=loan_status)) + geom_bar() + ggtitle("Term to Repay vs charged off loans") + labs(x="Term to Repay", y= "charged off loans")

#non-verified type of loan applicants have the highest number of charged-off loans. 
ggplot(loan_defaulters, aes(x=loan_defaulters$verification_status,fill=loan_status)) + geom_bar() + ggtitle("Verification status vs charged off loans") + labs(x="Verification Status", y= "charged off loans")

#mortgaged and rented ownership types have the highest number of charged-off loans.
ggplot(loan_defaulters, aes(x=loan_defaulters$home_ownership ,fill=loan_status)) + geom_bar() + ggtitle("Home Ownership vs charged off loans") + labs(x="Home Ownership", y= "charged off loans")

#univariate analysis with respect to type of driver variables

#as installments increase the defauters are increasing
ggplot(loan, aes(x = installment, fill = loan_status)) + geom_histogram(binwidth = 500, position = "fill") + ggtitle("Installments vs Number of loans") + labs(x="Installments", y= "Number of loans")

#defaulters are more with more interest rates
ggplot(loan, aes(x = int_rate, fill = loan_status)) + geom_histogram(binwidth = 0.5, position = "fill") + ggtitle("Interest Rate vs Number of loans") + labs(x="Interest Rate", y= "Number of loans")

#small business is the most risky
ggplot(loan, aes(x = purpose, fill = loan_status)) + geom_bar(position = "fill") + ggtitle("Purpose vs Number of loans") + labs(x="Purpose", y= "Number of loans")

#more percent of defaulters for loan amount > 25K
#ggplot(loan, aes(x = loan_amnt, fill = loan_status)) + geom_histogram(binwidth = 500, position = "fill") + ggtitle("Loan amount vs Number of loans") + labs(x="loan amount", y= "Number of loans")

#No Relation Columns

#no much relationship between defaulters and term
#ggplot(loan, aes(x = term, fill = loan_status)) + geom_bar()+ ggtitle("Term vs Number of loans") + labs(x="Term", y= "Number of loans")

#no relationship with home_ownership
#ggplot(loan, aes(x = home_ownership, fill = loan_status)) + geom_bar(position = "fill") + ggtitle("Home Ownership vs Number of loans") + labs(x="Home Ownership", y= "Number of loans")

#no much relationship with verfication status
#ggplot(loan, aes(x = verification_status, fill = loan_status)) + geom_bar(position = "fill") + ggtitle("Verification Status vs Number of loans") + labs(x="Verification Status", y= "Number of loans")

#no much relationship zip code
#ggplot(loan, aes(x = zip_code, fill = loan_status)) + geom_bar(position = "fill") + ggtitle("Zip Code vs Number of loans") + labs(x="Zip Code", y= "Number of loans")

# Step 4: Bivariate analysis and deriving relevant insights and conclusions

#loan_purpose vs Funded_Amt
ggplot(loan, aes(x = loan$purpose, y = loan$funded_amnt))+geom_boxplot()+ ggtitle("LOAN PURPOSE VS FUNDED AMOUNT")
#From this graph, there are Major five products has high funded amount
#debtconsolidation, smallbusiness,home_improvement,creditcard & house

#Term vs funded_amt
#from this graph, it clearly shows that 60 months term loan has a larger funded amount.
ggplot(loan, aes(x = as.factor(loan$term), y = loan$funded_amnt))+geom_boxplot()+ggtitle("LOAN TERM VS FUNDED AMOUNT")

#DTI vs Annual_Inc
#deriving metric to indicate high, medium and low types of annual income
loan$annual_inc_status <- ifelse(loan$annual_inc <= 5000, "Low", 
                                 ifelse(loan$annual_inc > 50000 & loan$annual_inc < 100000, "Medium","High"))
loan$annual_inc_status <- as.factor(loan$annual_inc_status)
#from this graph, it clearly shows DTI more than 20 and above will lead to defaulters
#Defaulters are majorly in High and Medium salary earners and surprisingly low salary earners dont have any DTI 
ggplot(loan, aes(x = loan$annual_inc_status, y = loan$dti, fill = loan$loan_status))+geom_bar(stat = "identity") + ggtitle("ANNUAL INCOME VS LOAN STATUS")

#Int_Rate vs DTI Ratio
#deriving metric to indicate high, medium, extreme and low types of interest rates
intrate_status <- ifelse(loan$int_rate > 5 & loan$int_rate < 10, "Low", 
                         ifelse(loan$int_rate > 10 & loan$int_rate < 15, "Medium", 
                                ifelse(loan$int_rate > 15 & loan$int_rate < 20, "High","Extreme")))
loan$intrate_status <- as.factor(intrate_status)
#from this graph, Major defaulter fall under high and medium interest rate with high dti
ggplot(loan, aes(x = loan$intrate_status, y = loan$dti, fill = loan$loan_status))+geom_bar(stat = "identity")+ ggtitle("INTEREST RATE VS LOAN STATUS ")



#MultiVariate Analysis 
ggcorr(loan_defaulters, label = TRUE, label_size = 3,hjust = 0.5, size = 2.5, color = "black", layout.exp = 1) + ggtitle("CORRELATION MATRIX")
