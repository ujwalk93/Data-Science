#---------------BFSI Capstone Project---------------#
library(ROSE)
library(MASS)
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(e1071)
library(caTools)
library(Information)
library(knitr)
library(GGally)
library(randomForest)

#set up the working directory
setwd("D://PG_Diploma//Project")

#loading both the rquired csv files
demo <- read.csv(file = "Demographic data.csv",header = T,stringsAsFactors = F,na.strings = c(""," ","NA",NA))
credit <- read.csv(file = "Credit Bureau data.csv",header = T,stringsAsFactors = F,na.strings = c(""," ","NA",NA))

#################################################################
##################### 1. Data Understanding #####################
#################################################################

#checking the structure and summary of both the data sets
str(demo)
str(credit)

summary(demo)
summary(credit)

#checking the number of rows of each dataset
nrow(demo) # 71295 rows 
nrow(credit) # 71295 rows

#checking the number of cols of each dataset
ncol(demo) # 12 cols
ncol(credit) # 19 cols

#######################################################################
################## 2. Data cleaning and preparation ###################
#######################################################################

#check uniqueness of application ID
length(unique(tolower(demo$Application.ID))) # 71292 rows
length(unique(tolower(credit$Application.ID))) # 71292 rows

demo[duplicated(demo$Application.ID),]$Application.ID
credit[duplicated(credit$Application.ID),]$Application.ID

#3 duplicates application IDs are present

#Removing the duplicate entry for these application id
demo<- demo[-which(duplicated(demo$Application.ID) == T), ]
credit<- credit[-which(duplicated(credit$Application.ID) == T), ]

#checking whether there is difference in columns in both the datasets
setdiff(demo$Application.ID,credit$Application.ID)

#identical application IDs across both the datasets

#merging the two datasets into a master file called master_data
#By joining based on "Application.ID", "Performance.Tag" we ensure that there is no data mismatch in the master dataset
master_data <- merge(demo,credit, by=c("Application.ID", "Performance.Tag"), all = F)

#checking the structure and summary of the master file obtained
str(master_data)

summary(master_data)

rejected_applicants<-master_data[which(is.na(master_data$Performance.Tag)),]

nrow(rejected_applicants)/nrow(master_data)*100 #1.998822% ~ 2% data are having no lebel or target value

#removing rows where Performance Tag is not applicable or not given
master_data <- master_data[!is.na(master_data$Performance.Tag) == TRUE,]

nrow(master_data) #69867 records

#check for NA values
missing_df <- data.frame(sapply(master_data, function(x) sum(is.na(x))))
colnames(missing_df) <- c('missing_counts')
missing_df$column <- rownames(missing_df)
ggplot(missing_df,aes(column,missing_counts))+geom_col()+coord_flip()


# Gender : 2 
# Marital.Status..at.the.time.of.application. : 6 
# No.of.dependents : 3 
# Education : 118 
# Profession : 13 
# Type.of.residence : 8 
# Avgas.CC.Utilization.in.last.12.months : 1023 
# No.of.trades.opened.in.last.6.months : 1 
# Presence.of.open.home.loan : 272 
# Outstanding.Balance : 272 


# Application ID :	Unique ID of the customers
# Age :	Age of customer
# Gender :	Gender of customer
# Marital Status :	Marital status of customer (at the time of application)
# No of dependents :	No. of childrens of customers
# Income :	Income of customers
# Education :	Education of customers
# Profession :	Profession of customers
# Type of residence :	Type of residence of customers
# No of months in current residence :	No of months in current residence of customers
# No of months in current company :	No of months in current company of customers
# Performance Tag :	Status of customer performance (" 1 represents "Default")
# No of times 90 DPD or worse in last 6 months :	Number of times customer has not payed dues since 90days in last 6 months
# No of times 60 DPD or worse in last 6 months :	Number of times customer has not payed dues since 60 days last 6 months
# No of times 30 DPD or worse in last 6 months :	Number of times customer has not payed dues since 30 days days last 6 months
# No of times 90 DPD or worse in last 12 months :	Number of times customer has not payed dues since 90 days days last 12 months
# No of times 60 DPD or worse in last 12 months :	Number of times customer has not payed dues since 60 days days last 12 months
# No of times 30 DPD or worse in last 12 months :	Number of times customer has not payed dues since 30 days days last 12 months
# Avgas CC Utilization in last 12 months :	Average utilization of credit card by customer
# No of trades opened in last 6 months :	Number of times the customer has done the trades in last 6 months
# No of trades opened in last 12 months :	Number of times the customer has done the trades in last 12 months
# No of PL trades opened in last 6 months :	No of PL trades in last 6 month  of customer
# No of PL trades opened in last 12 months :	No of PL trades in last 12 month  of customer
# No of Inquiries in last 6 months (excluding home & auto loans) :	Number of times the customers has inquired in last 6 months
# No of Inquiries in last 12 months (excluding home & auto loans) :	Number of times the customers has inquired in last 12 months
# Presence of open home loan :	Is the customer has home loan (1 represents "Yes")
# Outstanding Balance :	Outstanding balance of customer
# Total No of Trades :	Number of times the customer has done total trades
# Presence of open auto loan :	Is the customer has auto loan (1 represents "Yes")

######### Outliers detection through Multivariate approach #####################

# Create a variable/vector/collection of the column names you want to remove outliers on.
features <- colnames(subset(master_data, select=3:29))

# Create a variable to store the row id's to be removed
Outliers <- c()

# Loop through the list of columns you specified
for(i in features){
  
  # Get the Min/Max values
  max <- quantile(master_data[,i],0.75, na.rm=TRUE) + (IQR(master_data[,i], na.rm=TRUE) * 1.5 )
  min <- quantile(master_data[,i],0.25, na.rm=TRUE) - (IQR(master_data[,i], na.rm=TRUE) * 1.5 )
  
  # Get the id's using which
  idx <- which(master_data[,i] < min | master_data[,i] > max)
  
  # Output the number of outliers in each variable
  print(paste(i, length(idx), sep=''))
  
  # Append the outliers list
  Outliers <- c(Outliers, idx) 
}

# Sort, I think it's always good to do this
Outliers <- sort(Outliers)

# Data without outliers
master_data2 <- master_data[-Outliers,]

#####################################################################################

######################## Handling Invalid value ###################################
#Invalid negative/zero value for age column populated for some row i.e. 0, -3
#populating median values for all these rows
### Assumption - So age value substituted with median values where invalid. 
master_data$Age[which(master_data$Age < 10)] <-median(master_data$Age,na.rm = T)

master_data2$Age[which(master_data2$Age < 10)] <-median(master_data2$Age,na.rm = T)

#populating median values for all these rows
### Assumption - So Income value substituted with 0 values where invalid. 
master_data$Income[which(master_data$Income < 0)] <-0

master_data2$Income[which(master_data2$Age < 10)] <-0

master_data_eda <- master_data

######### WEIGHT OF EVIDENCE AND IV ####

library(woeBinning)
library(scorecard)
# woe binning ------ 
bins = woebin(master_data, "Performance.Tag")

#storing in woe_data file for further analysis
woe_data = woebin_ply(master_data, bins)
#check the woe value sets
str(woe_data)

# For following columns having missing values by asssigning woe values to respective columns.
master_data$No.of.dependents <- woe_data$No.of.dependents_woe
master_data$Presence.of.open.home.loan <- woe_data$Presence.of.open.home.loan_woe
master_data$Outstanding.Balance <- woe_data$Outstanding.Balance_woe  

#Storing the same in dataframe woedata
woedata<- as.data.frame(woe_data$No.of.dependents_woe,woe_data$Presence.of.open.home.loan_woe,woe_data$Outstanding.Balance_woe)

#replace with median as only one missing value is present
master_data$No.of.trades.opened.in.last.6.months[which(is.na(master_data$No.of.trades.opened.in.last.6.months)==1)] <-median(master_data$No.of.trades.opened.in.last.6.months, na.rm = T)

# Avgas CC Utilization in last 12 months : Average utilization of credit card by customer
## Assumption: NA value in Avgas.CC.Utilization.in.last.12.months is indicating 
## no usage of CC by user. So lets assign value 0 to these avg-cc-utilization values
master_data$Avgas.CC.Utilization.in.last.12.months[which(is.na(master_data$Avgas.CC.Utilization.in.last.12.months)==1)] = 0

# Create the function for mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Replacing empty strings with values having categorical variables with maximum freq that is mode.
master_data$Gender[which(is.na(master_data$Gender)==1)]<- getmode(master_data$Gender)
master_data$Marital.Status..at.the.time.of.application.[which(is.na(master_data$Marital.Status..at.the.time.of.application.) == 1)]<- getmode(master_data$Marital.Status..at.the.time.of.application.)
master_data$Education[which(is.na(master_data$Education)==1)]<- getmode(master_data$Education)
master_data$Profession[which(is.na(master_data$Profession)==1)]<- getmode(master_data$Profession)
master_data$Type.of.residence[which(is.na(master_data$Type.of.residence)==1)]<- getmode(master_data$Type.of.residence)

#######################################################################
############### 3. EDA (Exploratory Data Analysis) ####################
#######################################################################

#################Univariate Analysis#################################

#A: Performing Analysis w.r.t. demographic variables and deriving relevant insights and conclusions

#filter data with performance.tag as 1 to analyse defaults
creditcard_default <- subset(master_data, master_data$Performance.Tag == 1)

#segmented univariate analysis 
ggplot(creditcard_default,aes(x=creditcard_default$Marital.Status..at.the.time.of.application.,fill=creditcard_default$Performance.Tag))+geom_bar()+ggtitle("Marital Status vs Defaults") + labs(x="Marital Status", y= "Defaults")

#Most defaulters are married.

ggplot(creditcard_default,aes(x=creditcard_default$Type.of.residence,fill=creditcard_default$Performance.Tag))+geom_bar()+ggtitle("Type of residence vs Defaults") + labs(x="Type of residence", y= "Defaults")

#Most defaulters live in rented residences.

ggplot(creditcard_default,aes(x=creditcard_default$Education,fill=creditcard_default$Performance.Tag))+geom_bar()+ggtitle("Education vs Defaults") + labs(x="Education", y= "Defaults")  

#Most defaulters have education level as master/professional.

ggplot(creditcard_default,aes(x=creditcard_default$Profession,fill=creditcard_default$Performance.Tag))+geom_bar()+ggtitle("Profession vs Defaults") + labs(x="Profession", y= "Defaults")  

#Most defaulters have SAL as their profession.

ggplot(creditcard_default,aes(x=creditcard_default$Gender,fill=creditcard_default$Performance.Tag))+geom_bar()+ggtitle("Gender vs Defaults") + labs(x="Gender", y= "Defaults")

#Most defaulters are male. 

ggplot(creditcard_default,aes(x=creditcard_default$No.of.dependents,fill=creditcard_default$Performance.Tag))+geom_bar()+ggtitle("Dependents vs Defaults") + labs(x="Dependents", y= "Defaults")

#Most defaulters have 3 dependents.

hist(creditcard_default$Age, xlab = "Age")
boxplot(creditcard_default$Age,horizontal = F)  

#Most defaulters lie between the ages 35-50. 

hist(creditcard_default$Income, xlab = "Income")
boxplot(creditcard_default$Income,horizontal = F) 

#Most defaulters lie between incomes 0-10 (lower income ranges are more prone to default).

hist(creditcard_default$No.of.months.in.current.company, xlab = "No.of.months.in.current.company")
boxplot(creditcard_default$No.of.months.in.current.company,horizontal = F) 

#Most defaulters spend only 0-5 months in the current company they are employed

hist(creditcard_default$No.of.months.in.current.company, xlab = "No.of.months.in.current.residence")
boxplot(creditcard_default$No.of.months.in.current.residence,horizontal = F) 

#Most defaulters spend only 0-10 months in the current residence they stay in.

## From the graphs in this section, it is clear that lower demographic factors like income, number of months in current company and residence, ## significantly increase the default rate.

#B: Performing Analysis w.r.t. credit variables and deriving relevant insights and conclusions

hist(creditcard_default$No.of.times.90.DPD.or.worse.in.last.6.months, xlab = "No.of.times.90.DPD.or.worse.in.last.6.months")
boxplot(creditcard_default$No.of.times.90.DPD.or.worse.in.last.6.months,horizontal = F)

hist(creditcard_default$No.of.times.60.DPD.or.worse.in.last.6.months, xlab = "No.of.times.60.DPD.or.worse.in.last.6.months")
boxplot(creditcard_default$No.of.times.60.DPD.or.worse.in.last.6.months,horizontal = F)

hist(creditcard_default$No.of.times.30.DPD.or.worse.in.last.6.months, xlab = "No.of.times.30.DPD.or.worse.in.last.6.months")
boxplot(creditcard_default$No.of.times.30.DPD.or.worse.in.last.6.months,horizontal = F)

hist(creditcard_default$No.of.times.90.DPD.or.worse.in.last.12.months, xlab = "No.of.times.90.DPD.or.worse.in.last.12.months")
boxplot(creditcard_default$No.of.times.90.DPD.or.worse.in.last.12.months,horizontal = F)

hist(creditcard_default$No.of.times.60.DPD.or.worse.in.last.12.months, xlab = "No.of.times.60.DPD.or.worse.in.last.12.months")
boxplot(creditcard_default$No.of.times.60.DPD.or.worse.in.last.12.months,horizontal = F)

hist(creditcard_default$No.of.times.30.DPD.or.worse.in.last.12.months, xlab = "No.of.times.30.DPD.or.worse.in.last.12.months")
boxplot(creditcard_default$No.of.times.30.DPD.or.worse.in.last.12.months,horizontal = F)

hist(creditcard_default$Avgas.CC.Utilization.in.last.12.months, xlab = "Average Credit Card utilization")
boxplot(creditcard_default$Avgas.CC.Utilization.in.last.12.months,horizontal = F)

hist(creditcard_default$No.of.trades.opened.in.last.6.months, xlab = "No.of.trades.opened.in.last.6.months")
boxplot(creditcard_default$No.of.trades.opened.in.last.6.months,horizontal = F)

hist(creditcard_default$No.of.trades.opened.in.last.12.months, xlab = "No.of.trades.opened.in.last.12.months")
boxplot(creditcard_default$No.of.trades.opened.in.last.12.months,horizontal = F)

hist(creditcard_default$No.of.PL.trades.opened.in.last.6.months, xlab = "No.of.PL.trades.opened.in.last.6.months")
boxplot(creditcard_default$No.of.PL.trades.opened.in.last.6.months,horizontal = F)

hist(creditcard_default$No.of.PL.trades.opened.in.last.12.months, xlab = "No.of.PL.trades.opened.in.last.12.months")
boxplot(creditcard_default$No.of.PL.trades.opened.in.last.12.months,horizontal = F)

hist(creditcard_default$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., xlab = "Autoloans-6mon")
boxplot(creditcard_default$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,horizontal = F)

hist(creditcard_default$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., xlab = "Autoloans-12mon")
boxplot(creditcard_default$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,horizontal = F)

hist(creditcard_default$Total.No.of.Trades, xlab = "Total Trades")
boxplot(creditcard_default$Total.No.of.Trades,horizontal = F)

hist(creditcard_default$Outstanding.Balance, xlab = "Outstanding Balance")
boxplot(creditcard_default$Outstanding.Balance,names = "Outstanding.Balance",horizontal = F)

## From the given histograms, it is clear that lower values of these credit factors lead to higher defaulters.

## From the graphs in this section, it is clear that low credit factors like outstanding balance, total number of trades and average credit ## ## card utilization significantly increase the risk of default.

############## Multivariate analysis (using correlation matrix) ####################
ggcorr(creditcard_default, label = TRUE, label_size = 3,hjust = 0.5, size = 2.5, color = "black", layout.exp = 1) + ggtitle("CORRELATION MATRIX")

## From the correlation matrix it is clear that credit factors like total number of trades and average credit card utilization have a greater ## influence on rate of defaulters as compared to demographic factors in the correlation matrix.


###################  WOE/IV Analysis ##########################
###Information Value	Predictive Power
#### < 0.02	useless for prediction, 0.02 to 0.1	Weak predictor, 0.1 to 0.3	Medium predictor
#### 0.3 to 0.5	Strong predictor, >0.5	Suspicious or too good to be true
#### It is made sure our independent categorical variables are stored as factor in R
#### Here as in Performance.Tag

#### Compute Information Value and WOE
IV <- create_infotables(data=master_data_eda, y="Performance.Tag", bins=10, parallel=FALSE)
## in IV list,  the list Summary contains IV values of all the independent variables.
IV_Value = data.frame(IV$Summary)
## To get WOE table for variable Income, we need to call Tables list from IV list.
print(IV$Tables$Income, row.names=FALSE)
## Similarly,
print(IV$Tables$Age, row.names=FALSE)
print(IV$Tables$Education, row.names=FALSE)
print(IV$Tables$Avgas.CC.Utilization.in.last.12.months, row.names=FALSE)
##### lets plot for better all round idea
plot_infotables(IV, "Avgas.CC.Utilization.in.last.12.months")
#### lets generate multiple plots in one page for better visuality
plot_infotables(IV, IV$Summary$Variable[28:20], same_scale=FALSE)
plot_infotables(IV, IV$Summary$Variable[19:11], same_scale=FALSE)
plot_infotables(IV, IV$Summary$Variable[10:2], same_scale=FALSE)
plot_infotables(IV, IV$Summary$Variable[1], same_scale=FALSE)

######lets plot them individually for getting insights while doing binning

plot_infotables(IV,"Age")
plot_infotables(IV, "Gender")
plot_infotables(IV, "Marital.Status..at.the.time.of.application.")
plot_infotables(IV,"No.of.dependents")
plot_infotables(IV, "Education")
plot_infotables(IV, "Profession")
plot_infotables(IV, "Income")
plot_infotables(IV, "Type.of.residence")

plot_infotables(IV, "No.of.months.in.current.residence")
plot_infotables(IV, "No.of.months.in.current.company")

plot_infotables(IV, "Total.No.of.Trades")
plot_infotables(IV, "Outstanding.Balance")

plot_infotables(IV, "Presence.of.open.home.loan")
plot_infotables(IV, "Presence.of.open.auto.loan")

plot_infotables(IV, "Avgas.CC.Utilization.in.last.12.months")

plot_infotables(IV, "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.")
plot_infotables(IV, "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")

plot_infotables(IV, "No.of.PL.trades.opened.in.last.6.months")
plot_infotables(IV, "No.of.PL.trades.opened.in.last.12.months")

plot_infotables(IV, "No.of.trades.opened.in.last.6.months")
plot_infotables(IV, "No.of.trades.opened.in.last.12.months")

plot_infotables(IV,"No.of.times.30.DPD.or.worse.in.last.6.months")
plot_infotables(IV, "No.of.times.60.DPD.or.worse.in.last.6.months")
plot_infotables(IV, "No.of.times.90.DPD.or.worse.in.last.6.months")

plot_infotables(IV,"No.of.times.30.DPD.or.worse.in.last.12.months")
plot_infotables(IV, "No.of.times.60.DPD.or.worse.in.last.12.months")
plot_infotables(IV, "No.of.times.90.DPD.or.worse.in.last.12.months")

## Variables' having IV value below 0.02 are not useful for prediction
IV_Useful_Variables <- IV_Value[IV_Value$IV>=0.02,]
IV_Useful_Variables
##                                                       Variable         IV
##                         Avgas.CC.Utilization.in.last.12.months 0.30992917
##                           No.of.trades.opened.in.last.12.months 0.29797230
##                        No.of.PL.trades.opened.in.last.12.months 0.29589714
## No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 0.29541759
##                                             Outstanding.Balance 0.24627959
##                    No.of.times.30.DPD.or.worse.in.last.6.months 0.24155115
##                                              Total.No.of.Trades 0.23662955
##                        No.of.PL.trades.opened.in.last.6.months 0.21972723
##                   No.of.times.90.DPD.or.worse.in.last.12.months 0.21386327
##                    No.of.times.60.DPD.or.worse.in.last.6.months 0.20582586
##  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 0.20517617
##                   No.of.times.30.DPD.or.worse.in.last.12.months 0.19824100
##                            No.of.trades.opened.in.last.6.months 0.18602708
##                   No.of.times.60.DPD.or.worse.in.last.12.months 0.18548895
##                    No.of.times.90.DPD.or.worse.in.last.6.months 0.16010599
##                               No.of.months.in.current.residence 0.07895394
##                                                          Income 0.04241078
##                                 No.of.months.in.current.company 0.02176071
#thus 18 variables are found to be useful predictors

#################  Bivariate analysis #################################

#convert performance tag variableto factor for ease of analysis.
master_data_eda$Performance.Tag <- as.factor(master_data_eda$Performance.Tag)

#Analysing w.r.t multiple factors 

ggplot(master_data_eda, aes(x = Avgas.CC.Utilization.in.last.12.months, y = No.of.PL.trades.opened.in.last.12.months, group=Performance.Tag, color=Performance.Tag)) + geom_line(stat='summary', fun.y='mean') +  geom_point(stat='summary', fun.y='mean')
## No of PL-trades opened is relatively higher for default users. 

ggplot(data=master_data_eda, aes(x=No.of.times.90.DPD.or.worse.in.last.6.months, y=Avgas.CC.Utilization.in.last.12.months, group=Performance.Tag, color=Performance.Tag)) + geom_line(stat='summary', fun.y='mean') +  geom_point(stat='summary', fun.y='mean')
## For default users Avg-CC-utilization is overall higher , Also CC-usage is going high with increasing DPD values. 

ggplot(data=master_data_eda, aes(x=Total.No.of.Trades, y=Outstanding.Balance, group=Performance.Tag, color=Performance.Tag)) + geom_line(stat='summary', fun.y='mean') + geom_point(stat='summary', fun.y='mean')
## Total no of trades is overall in higher nos for default users. 
## Also outstanding balance is relatively higher for most of default users.

ggplot(data=master_data_eda, aes(x=Income, y=Outstanding.Balance, group=Performance.Tag, color=Performance.Tag)) + geom_line(stat='summary', fun.y='mean')  
##For defaulters Outstanding balance is higher.
# No upward/downward trend for outstanding balance with increasing income.
#If outstanding is more than 12.5lakh its a matter of concern.

ggplot(master_data_eda, aes(x = Income, y = Avgas.CC.Utilization.in.last.12.months, group=Performance.Tag, color=Performance.Tag)) + geom_line(stat='summary', fun.y='mean') + geom_point(stat='summary', fun.y='mean')
##With increasing income avg-cc-usage decreases for whole population.
## If avg cc usage is >40 for a low income, >30 for middle income, >25 for higher income, it should be looked at.

ggplot(data=master_data_eda, aes(x=Income, y=No.of.times.90.DPD.or.worse.in.last.6.months, group=Performance.Tag, color=Performance.Tag)) + geom_line(stat='summary', fun.y='mean') +  geom_point(stat='summary', fun.y='mean')
##With increasing Income, DPD nos are decreasing. 
##Also for defaulting users DPD nos are way higher.
## High no of defaulters are in lower to medium income range. 

ggplot(data=master_data_eda, aes(x=Income, y=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., group=Performance.Tag, color=Performance.Tag)) + geom_line(stat='summary', fun.y='mean') +  geom_point(stat='summary', fun.y='mean')
##With increase in income no of inquiries are decreasing for non defaulters.
##With increase in income no of inquiries relatively higher for defaulters.

ggplot(data=master_data_eda, aes(x=No.of.dependents, y=Income, group=Performance.Tag, color=Performance.Tag)) + geom_line(stat='summary', fun.y='mean') +  geom_point(stat='summary', fun.y='mean') 
##Income per no of dependents is very low for defaulters comapared to non-defaulters. 

ggplot(data=master_data_eda, aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., y=Total.No.of.Trades , group=Performance.Tag, color=Performance.Tag)) + geom_line(stat='summary', fun.y='mean') +  geom_point(stat='summary', fun.y='mean')
##With increasing no of inquiries in last 12 months, total no of trades increases, then gradually it becomes constant.
## for default users total no of trades is higher.

#From the EDA and correlation matrix, it is clear that credit factors like outstanding balance, total number of trades and average #credit card utilization have a greater influence over the risk of default as compared to demographic factors.
#Warning messages obtained if any, can be ignored.

###################################################################################
################## 4. Model Building ,Evaluation and Validation ###################
###################################################################################

##################### Logistic Regression Modelling based on demographic data ####################
#Removing the duplicate entry for these application IDs
demo <- demo[which(duplicated(demo$Application.ID) == F), ]

#removing rows where Performance Tag is not applicable or not given
demo <- demo[!is.na(demo$Performance.Tag) == TRUE||(demo$Performance.Tag==' '),]

#check for missing values
sapply(demo, function(x) length(which(x == '')))
#No missing values are present

#check for NA values
sapply(demo, function(x) sum(is.na(x)))
#some NA values are present

#check for duplicate rows
sum(duplicated(demo))
#no duplicate rows present

#removing NA performance tag values
demo <- demo[!is.na(demo$Performance.Tag) == TRUE,]

#For following columns we handle NA values by asssigning median value to respective NA records.
demo$No.of.dependents[which(is.na(demo$No.of.dependents)==1)]<-median(demo$No.of.dependents, na.rm = T)

#handling invalid values for age and income
demo$Age[which(demo$Age < 10)] <-median(demo$Age,na.rm = T)
demo$Income[which(demo$Income < 0)] <-0

#Replacing NA strings with values with maximum freq
demo$Gender[which(is.na(demo$Gender)==1)]<- getmode(demo$Gender)
demo$Marital.Status..at.the.time.of.application.[which(is.na(demo$Marital.Status..at.the.time.of.application.)==1)]<- getmode(demo$Marital.Status..at.the.time.of.application.)
demo$Education[which(is.na(demo$Education)==1)]<- getmode(demo$Education)
demo$Profession[which(is.na(demo$Profession)==1)]<- getmode(demo$Profession)
demo$Type.of.residence[which(is.na(demo$Type.of.residence)==1)]<- getmode(demo$Type.of.residence)

#convert all characters types to factor by sapply (for ease of analysis)
demo[sapply(demo, is.character)] <- lapply(demo[sapply(demo, is.character)], as.factor)

#converting number of dependents into factor for ease of analysis
demo$No.of.dependents <- as.factor(demo$No.of.dependents)

target_col <- c("Performance.Tag")

fact_cols <- c("Gender","Marital.Status..at.the.time.of.application.","No.of.dependents","Education","Profession","Type.of.residence")

numeric_cols<-c('Age','Income','No.of.months.in.current.residence','No.of.months.in.current.company')
                
# We are not consideing ApplicationID 
# Feature Scaling and Dummy creation

scaled_numeric_data <-data.frame(sapply(demo[numeric_cols], scale))
factor_data <- data.frame(demo[fact_cols])
dummie_data <- data.frame(sapply(factor_data,function(x) data.frame(model.matrix(~x-1,data =factor_data))[,-1]))
target_data <- demo[target_col]

# combine all relevant columns to build final training data
finaldemo<- cbind(target_data,scaled_numeric_data,dummie_data)

#adding performance tag to final dataframe
finaldemo$Performance.Tag <- demo$Performance.Tag

#converting target variable as factor
finaldemo$Performance.Tag<-as.factor(finaldemo$Performance.Tag)

#structure of the final dataframe
str(finaldemo)

#Model building using logistic regression

#separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(finaldemo), 0.7*nrow(finaldemo))
train = finaldemo[trainindices,]
test = finaldemo[-trainindices,]

#Initial model - Build using all variables
model_1 <- glm(Performance.Tag~.,data=train,family = "binomial")
summary(model_1)

# used STEPAIC to find the best model
model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)

#use the final call of the stepAIC function to build the model
model_3 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Gender + No.of.dependents.x3 + 
    Profession.xSE + Type.of.residence.xLiving.with.Parents + 
    Type.of.residence.xOthers + Type.of.residence.xOwned + Type.of.residence.xRented, 
    family = "binomial", data = train)
summary(model_3)
vif(model_3)

#Removing Type.of.residence.xRented because of high VIF (7.178)
model_4 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Gender + No.of.dependents.x3 + 
    Profession.xSE + Type.of.residence.xLiving.with.Parents + 
    Type.of.residence.xOthers + Type.of.residence.xOwned, 
    family = "binomial", data = train)
summary(model_4)
vif(model_4)

#Now all variables have VIF<2. So, check for p-value
#Removing Type.of.residence.xLiving.with.Parents because of high p-value (0.8881)
model_5 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Gender + No.of.dependents.x3 + 
    Profession.xSE + Type.of.residence.xOthers + Type.of.residence.xOwned, 
    family = "binomial", data = train)
summary(model_5)
vif(model_5)

#Removing Type.of.residence.xLiving.with.Parents because of high p-value (0.8881)
model_6 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Gender + No.of.dependents.x3 + 
    Profession.xSE + Type.of.residence.xOthers + Type.of.residence.xOwned, 
    family = "binomial", data = train)
summary(model_6)
vif(model_6)

#Removing Type.of.residence.xOwned because of high p-value (0.6044)
model_7 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Gender + No.of.dependents.x3 + 
    Profession.xSE + Type.of.residence.xOthers, family = "binomial", data = train)
summary(model_7)
vif(model_7)

#Removing Type.of.residence.xOthers because of high p-value (0.3015)
model_8 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Gender + No.of.dependents.x3 + 
    Profession.xSE, family = "binomial", data = train)
summary(model_8)
vif(model_8)

#Removing Gender because of high p-value (0.0733)
model_9 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + No.of.dependents.x3 + Profession.xSE, 
    family = "binomial", data = train)
summary(model_9)
vif(model_9)

#Removing No.of.dependents.x3 because of high p-value (0.0505)
model_10 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Profession.xSE, family = "binomial", data = train)
summary(model_10)
vif(model_10)

##The top predictor variables present in the final model are:
## 1. Income
## 2. No.of.months.in.current.residence
## 3. No.of.months.in.current.company
## 4. Profession.xSE

################ Model Evaluation and Validation #################

#all variables have VIF<2 and p-values less than 0.05
#Hence, model_10 is deemed to be the final model.

final_model <- model_10

#predicted probabilities of default for test data
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

# Let's see the summary 

summary(test_pred)

test$prob <- test_pred

test_pred_def <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_def <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))

table(test_actual_def,test_pred_def)

test_conf <- confusionMatrix(test_pred_def, test_actual_def, positive = "Yes")
test_conf

# Let's use the probability cutoff of 50%.

test_pred_def <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_def <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))

table(test_actual_def,test_pred_def)

test_conf <- confusionMatrix(test_pred_def, test_actual_def, positive = "Yes")
test_conf

#######################################################################

perform_fn <- function(cutoff) 
{
  predicted_def <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_def, test_actual_def, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.1)]
cutoff

# Let's choose the optimal cutoff value of 0.042 (from the graph) for final model

test_cutoff_def <- factor(ifelse(test_pred >=0.042, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_def, test_actual_def, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]
conf_final

# Accuracy = 52.11%
# Sensitivity = 57.69%
# Specificity = 51.87%

### KS -statistic - Test Data ######

test_cutoff_def <- ifelse(test_cutoff_def=="Yes",1,0)
test_actual_def <- ifelse(test_actual_def=="Yes",1,0)

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_def, test_actual_def)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # 0.09554

#KS-statistic is 9.554%

### PLotting the ROC Curve ###
plot(performance_measures_test,main = "ROC curve")

### Area under ROC Curve ###
auc_ROCR <- performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 
#0.5478

#################################################################
### Plotting Gain and Lift Chart ###

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
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

Default_decile = lift(test_actual_def, test_pred, groups = 10)
Default_decile

plot(y=Default_decile$Gain,x=Default_decile$bucket,type ="l",lwd = 2,xlab="Bucket",ylab="Gain",main = "Gain Chart")

plot(y=Default_decile$Cumlift, x=Default_decile$bucket,col="red",type="l",xlab="% of total targeted",ylab = "Lift",main="Lift Chart")
######################################################################


#####
library(DMwR)

# Logistic Regression: using SMOTE analysis (to account for the data imbalance)

#Initial model - Build model 1 containing all variables
train_smote <- SMOTE(Performance.Tag ~ ., train, perc.over = 100, perc.under=200)

summary(train_smote$Performance.Tag)
## 0 is 4164 and 1 is 4164

# Tag= 1 implies default, 0 implies good
train_smote_model_1 = glm(Performance.Tag ~ ., data = train_smote, family = "binomial")
summary(train_smote_model_1)

# used STEPAIC to find the best model
train_smote_model_2 <- stepAIC(train_smote_model_1,direction = "both")
summary(train_smote_model_2)

#use the final call of the stepAIC function to build the model
train_smote_model_3 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Gender + No.of.dependents.x2 + 
    No.of.dependents.x4 + Education.xPhd + Education.xProfessional + 
    Profession.xSE + Type.of.residence.xOthers, family = "binomial", 
    data = train_smote)
summary(train_smote_model_3)
vif(train_smote_model_3)

#Now all variables have VIF<=2. So, check for p-value
#Removing No.of.dependents.x4 because of high p-value (0.1316)
train_smote_model_4 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Gender + No.of.dependents.x2 + 
    Education.xPhd + Education.xProfessional + Profession.xSE + Type.of.residence.xOthers, family = "binomial", 
    data = train_smote)
summary(train_smote_model_4)
vif(train_smote_model_4)

#Removing Profession.xSE because of high p-value (0.1061)
train_smote_model_5 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Gender + No.of.dependents.x2 + Education.xPhd + Education.xProfessional + 
    Type.of.residence.xOthers, family = "binomial", data = train_smote)
summary(train_smote_model_5)
vif(train_smote_model_5)

#Removing No.of.dependents.x2 because of high p-value (0.1018)
train_smote_model_6 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Gender + Education.xPhd + Education.xProfessional + 
    Type.of.residence.xOthers, family = "binomial", data = train_smote)
summary(train_smote_model_6)
vif(train_smote_model_6)

#Removing Gender because of high p-value (0.1014)
train_smote_model_7 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Education.xPhd + Education.xProfessional + 
    Type.of.residence.xOthers, family = "binomial", data = train_smote)
summary(train_smote_model_7)
vif(train_smote_model_7)

#Removing Type.of.residence.xOthers because of high p-value (0.0596)
train_smote_model_8 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Education.xPhd + Education.xProfessional, family = "binomial", data = train_smote)
summary(train_smote_model_8)
vif(train_smote_model_8)

#all variables have VIF<=2 and p-values less than 0.05
#Hence, train_smote_model_8 is deemed to be the final model.

##The top predictor variables present in the final model are:
## 1. Income 
## 2. No.of.months.in.current.residence
## 3. No.of.months.in.current.company
## 4. Education.xPhd
## 5. Education.xProfessional


################ Model Evaluation and Validation #################

# MODEL EVALUATION FOR SMOTE ####
train_smote_model_final <- train_smote_model_8

#predicted probabilities of default for test data
test_pred_smote = predict(train_smote_model_final, type = "response", 
                    newdata = test[,-1])


# Let's see the summary 

summary(test_pred_smote)

test$prob_smote <- test_pred_smote
View(test)
# Let's use the probability cutoff of 50%.

test_pred_def_smote <- factor(ifelse(test_pred_smote >= 0.50, "Yes", "No"))
test_actual_def_smote <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))


table(test_actual_def_smote,test_pred_def_smote)

test_conf <- confusionMatrix(test_pred_def_smote, test_actual_def_smote, positive = "Yes")
test_conf
##Accuracy :53.59% Sensitivity : 57.46% Specificity : 53.42%

perform_fn_2 <- function(cutoff_smote) 
{
  predicted_def_smote <- factor(ifelse(test_pred_smote >= cutoff_smote, "Yes", "No"))
  conf <- confusionMatrix(predicted_def_smote, test_actual_def_smote, positive = "Yes")
  acc_smote <- conf$overall[1]
  sens_smote <- conf$byClass[1]
  spec_smote <- conf$byClass[2]
  out <- t(as.matrix(c(sens_smote, spec_smote, acc_smote))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred_smote)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn_2(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_smote <- s[which(abs(OUT[,1]-OUT[,2])<0.2)]
cutoff_smote

# Let's choose the optimal cutoff value of 0.505 for final model

test_cutoff_def_smote <- factor(ifelse(test_pred_smote >=0.505 , "Yes", "No"))

conf_final_smote <- confusionMatrix(test_cutoff_def_smote, test_actual_def_smote, positive = "Yes")

acc_s_model <- conf_final_smote$overall[1]

sens_s_model <- conf_final_smote$byClass[1]

spec_s_model <- conf_final_smote$byClass[2]
conf_final_smote

# Accuracy = 56.37%
# Sensitivity = 53.06%
# Specificity = 56.51%

### KS -statistic - Test Data ######

test_cutoff_def_smote <- ifelse(test_cutoff_def_smote=="Yes",1,0)
test_actual_def_smote <- ifelse(test_actual_def_smote=="Yes",1,0)

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_def_smote, test_actual_def_smote)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # 0.0957
#KS-statistic is 9.57%

### PLotting the ROC Curve ###
plot(performance_measures_test,main = "ROC curve")

### Area under ROC Curve ###
auc_ROCR <- performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 
#0.5479

#################################################################
### Plotting Gain and Lift Chart ###

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
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

Default_decile = lift(test_actual_def_smote, test_pred_smote, groups = 10)
Default_decile

plot(y=Default_decile$Gain,x=Default_decile$bucket,type ="l",lwd = 2,xlab="Bucket",ylab="Gain",main = "Gain Chart")

plot(y=Default_decile$Cumlift, x=Default_decile$bucket,col="red",type="l",xlab="% of total targeted",ylab = "Lift",main="Lift Chart")
######################################################################

######################################################################
### Logistic Regression (Model Building) for the combined data set ###
######################################################################

target_col <- c("Performance.Tag")

fact_cols <- c("Gender","Marital.Status..at.the.time.of.application." 
               ,"Education","Profession","Type.of.residence")

# numeric_cols <- c(names(which(sapply(master_data, is.numeric))))

numeric_cols<-c('Age','Income','No.of.months.in.current.residence','No.of.months.in.current.company'
                ,'Total.No.of.Trades','Outstanding.Balance','Avgas.CC.Utilization.in.last.12.months',"No.of.dependents"
                ,'No.of.times.90.DPD.or.worse.in.last.6.months','No.of.times.60.DPD.or.worse.in.last.6.months','No.of.times.30.DPD.or.worse.in.last.6.months'
                ,'No.of.times.90.DPD.or.worse.in.last.12.months','No.of.times.60.DPD.or.worse.in.last.12.months','No.of.times.30.DPD.or.worse.in.last.12.months'
                ,'No.of.trades.opened.in.last.6.months','No.of.trades.opened.in.last.12.months'
                ,'No.of.PL.trades.opened.in.last.6.months','No.of.PL.trades.opened.in.last.6.months'
                ,'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.'
                ,'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.'
                ,'No.of.PL.trades.opened.in.last.12.months','Presence.of.open.home.loan','Presence.of.open.auto.loan')

# We are not consideing ApplicationID 
# Feature Scaling and Dummy creation

scaled_numeric_data <-data.frame(sapply(master_data[numeric_cols], scale))
dummie_data <- data.frame(sapply(master_data[fact_cols],function(x) data.frame(model.matrix(~x-1,data = master_data[fact_cols]))[,-1]))
target_data <- master_data[target_col]

# combine all relevant columns to build final training data
final_df<- cbind(target_data,scaled_numeric_data,dummie_data)
 
final_df$Performance.Tag<-as.factor(final_df$Performance.Tag)

str(final_df)

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(final_df), 0.7*nrow(final_df))
train = final_df[trainindices,]
test = final_df[-trainindices,]

#Initial model - Build using all variables
model_1 <- glm(Performance.Tag~.,data=train,family = "binomial")
summary(model_1)

# used STEPAIC to find the best model
model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)

#use the final call of the stepAIC function to build the model
model_3 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Total.No.of.Trades + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.6.months + 
    No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
    No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
    No.of.PL.trades.opened.in.last.12.months + Presence.of.open.auto.loan + 
    Profession.xSE, family = "binomial", data = train)

summary(model_3)
vif(model_3)

#Removing No.of.times.60.DPD.or.worse.in.last.6.months because of high VIF (11.354)
model_4 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Total.No.of.Trades + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +             No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
    No.of.PL.trades.opened.in.last.12.months + Presence.of.open.auto.loan + 
    Profession.xSE, family = "binomial", data = train)

summary(model_4)
vif(model_4)

#Removing No.of.PL.trades.opened.in.last.12.months because of high VIF (6.266)
model_5 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Total.No.of.Trades + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +                 No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +         No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
    Presence.of.open.auto.loan + Profession.xSE, family = "binomial", data = train)

summary(model_5)
vif(model_5)

#Removing No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. because of high VIF (6.106)   
model_6 <-glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Total.No.of.Trades + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +                No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +                  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    Presence.of.open.auto.loan + Profession.xSE, family = "binomial", data = train)

summary(model_6)
vif(model_6)

#Removing Total.No.of.Trades because of high VIF (3.656) and high p-value (0.1925)
model_7 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +                            No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +                              No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    Presence.of.open.auto.loan + Profession.xSE, family = "binomial", data = train)

summary(model_7)
vif(model_7)

#Removing No.of.times.90.DPD.or.worse.in.last.12.months because of high VIF (3.287)
model_8 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +                                 No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    Presence.of.open.auto.loan + Profession.xSE, family = "binomial", data = train)

summary(model_8)
vif(model_8)

#Now all variables have VIF<2. So, check for p-value
#Removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. because of high p-value (0.577)
model_9 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +                                 No.of.PL.trades.opened.in.last.6.months + Presence.of.open.auto.loan + Profession.xSE, 
    family = "binomial", data = train)

summary(model_9)
vif(model_9)

#Removing Income because of high p-value (0.088221)
model_10 <- glm(formula = Performance.Tag ~ No.of.months.in.current.residence + 
    No.of.months.in.current.company + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +                                 No.of.PL.trades.opened.in.last.6.months + Presence.of.open.auto.loan + Profession.xSE, 
    family = "binomial", data = train)

summary(model_10)
vif(model_10)

#Removing Profession.xSE because of high p-value (0.0766)
model_11 <- glm(formula = Performance.Tag ~ No.of.months.in.current.residence + 
    No.of.months.in.current.company + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +                                 No.of.PL.trades.opened.in.last.6.months + Presence.of.open.auto.loan, 
    family = "binomial", data = train)

summary(model_11)
vif(model_11)

#Removing No.of.months.in.current.company because of high p-value (0.059)
model_12 <- glm(formula = Performance.Tag ~ No.of.months.in.current.residence + 
    Outstanding.Balance + Avgas.CC.Utilization.in.last.12.months + 
    No.of.times.30.DPD.or.worse.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + Presence.of.open.auto.loan, 
    family = "binomial", data = train)

summary(model_12)
vif(model_12)

#all variables have VIF<2 and p-values less than 0.05
#Hence, model_12 is deemed to be the final model.

##The top predictor variables present in the final model are:
## 1. No.of.months.in.current.residence
## 2. Outstanding.Balance
## 3. Avgas.CC.Utilization.in.last.12.months
## 4. No.of.times.30.DPD.or.worse.in.last.6.months
## 5. No.of.PL.trades.opened.in.last.6.months
## 6. Presence.of.open.auto.loan

final_model <- model_12


################ Model Evaluation and Validation  #################

#predicted probabilities of default for test data
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

# Let's see the summary 

summary(test_pred)

test$prob <- test_pred

test_pred_def <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_def <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))

table(test_actual_def,test_pred_def)

test_conf <- confusionMatrix(test_pred_def, test_actual_def, positive = "Yes")
test_conf

# Let's use the probability cutoff of 50%.

test_pred_def <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_def <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))


table(test_actual_def,test_pred_def)

test_conf <- confusionMatrix(test_pred_def, test_actual_def, positive = "Yes")
test_conf
##Accuracy :95.68% Sensitivity : 0% Specificity : 100% (which is absurd as sensitivity cannot be practically 0)

#######################################################################

perform_fn <- function(cutoff) 
{
  predicted_def <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_def, test_actual_def, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.25)]
cutoff

# Let's choose a cutoff value of 0.042 for final model

test_cutoff_def <- factor(ifelse(test_pred >=0.042, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_def, test_actual_def, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]
conf_final

# Accuracy = 55.85%
# Sensitivity = 69.06%
# Specificity = 55.25%

# Let's choose a cutoff value of 0.05 for final model

test_cutoff_def <- factor(ifelse(test_pred >=0.05, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_def, test_actual_def, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]
conf_final

# Accuracy = 67.22%
# Sensitivity = 55.36%
# Specificity = 67.75%

### KS -statistic - Test Data ######

test_cutoff_def <- ifelse(test_cutoff_def=="Yes",1,0)
test_actual_def <- ifelse(test_actual_def=="Yes",1,0)

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_def, test_actual_def)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # 0.2311

#KS-statistic is 23.11%

### PLotting the ROC Curve ###
plot(performance_measures_test,main = "ROC curve")

### Area under ROC Curve ###
auc_ROCR <- performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 
#0.6155

#################################################################
### Plotting Gain and Lift Chart ###

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
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

Default_decile = lift(test_actual_def, test_pred, groups = 10)
Default_decile

plot(y=Default_decile$Gain,x=Default_decile$bucket,type ="l",lwd = 2,xlab="Bucket",ylab="Gain",main = "Gain Chart")

plot(y=Default_decile$Cumlift, x=Default_decile$bucket,col="red",type="l",xlab="% of total targeted",ylab = "Lift",main="Lift Chart")
######################################################################


###########  SMOTE(synthetic minority oversampling technique) by ROSE package
#SMOTE algorithm creates artificial data based on feature space (rather than data space) 
# similarities from minority samples. It generates a random set of minority class observations 
# to shift the classifier learning bias towards minority class.
#######################################################

#####
library(DMwR)

# Logistic Regression: using SMOTE analysis (to account for the data imbalance)

#Initial model - Build model 1 containing all variables
train_smote <- SMOTE(Performance.Tag ~ ., train, perc.over = 100, perc.under=200)

summary(train_smote$Performance.Tag)
## 0 is 4084 and 1 is 4084

# Tag= 1 implies default, 0 implies good
train_smote_model_1 = glm(Performance.Tag ~ ., data = train_smote, family = "binomial")
summary(train_smote_model_1)

# used STEPAIC to find the best model
train_smote_model_2 <- stepAIC(train_smote_model_1,direction = "both")
summary(train_smote_model_2)

#use the final call of the stepAIC function to build the model
train_smote_model_3 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Total.No.of.Trades + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.6.months + 
    No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
    No.of.trades.opened.in.last.12.months + No.of.PL.trades.opened.in.last.6.months + 
    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
    No.of.PL.trades.opened.in.last.12.months + Presence.of.open.auto.loan + 
    Marital.Status..at.the.time.of.application. + Education.xPhd + 
    Education.xProfessional + Profession.xSE + Type.of.residence.xOthers, 
    family = "binomial", data = train_smote)

summary(train_smote_model_3)
vif(train_smote_model_3)

#Removing No.of.trades.opened.in.last.12.months because of high VIF (25.999)
train_smote_model_4 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Total.No.of.Trades + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.60.DPD.or.worse.in.last.6.months + 
    No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
    No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
    No.of.PL.trades.opened.in.last.12.months + Presence.of.open.auto.loan + 
    Marital.Status..at.the.time.of.application. + Education.xPhd + 
    Education.xProfessional + Profession.xSE + Type.of.residence.xOthers, 
    family = "binomial", data = train_smote)

summary(train_smote_model_4)
vif(train_smote_model_4)

#Removing No.of.times.60.DPD.or.worse.in.last.6.months because of high VIF (11.659)
train_smote_model_5 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Total.No.of.Trades + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +         No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +         No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
    No.of.PL.trades.opened.in.last.12.months + Presence.of.open.auto.loan + 
    Marital.Status..at.the.time.of.application. + Education.xPhd + 
    Education.xProfessional + Profession.xSE + Type.of.residence.xOthers, 
    family = "binomial", data = train_smote)

summary(train_smote_model_5)
vif(train_smote_model_5)

#Removing No.of.PL.trades.opened.in.last.12.months because of high VIF (7.6693)
train_smote_model_6 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Total.No.of.Trades + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +         No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +     No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
    Presence.of.open.auto.loan + Marital.Status..at.the.time.of.application. + Education.xPhd + 
    Education.xProfessional + Profession.xSE + Type.of.residence.xOthers, 
    family = "binomial", data = train_smote)

summary(train_smote_model_6)
vif(train_smote_model_6)

#Removing No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. because of high VIF (7.0404)
train_smote_model_7 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Total.No.of.Trades + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +             No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +         No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    Presence.of.open.auto.loan + Marital.Status..at.the.time.of.application. + Education.xPhd + 
    Education.xProfessional + Profession.xSE + Type.of.residence.xOthers, 
    family = "binomial", data = train_smote)

summary(train_smote_model_7)
vif(train_smote_model_7)

#Removing Total.No.of.Trades because of high VIF (4.0585) and high p-value (0.053)
train_smote_model_8 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +                No.of.times.90.DPD.or.worse.in.last.12.months + No.of.PL.trades.opened.in.last.6.months +                   No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    Presence.of.open.auto.loan + Marital.Status..at.the.time.of.application. + Education.xPhd + 
    Education.xProfessional + Profession.xSE + Type.of.residence.xOthers, 
    family = "binomial", data = train_smote)

summary(train_smote_model_8)
vif(train_smote_model_8)

#Removing No.of.times.90.DPD.or.worse.in.last.12.months because of high VIF (3.151) and p-value (0.073)
train_smote_model_9 <- glm(formula = Performance.Tag ~ Income + No.of.months.in.current.residence + 
    No.of.months.in.current.company + Outstanding.Balance + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +                      No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    Presence.of.open.auto.loan + Marital.Status..at.the.time.of.application. + Education.xPhd + 
    Education.xProfessional + Profession.xSE + Type.of.residence.xOthers, 
    family = "binomial", data = train_smote)

summary(train_smote_model_9)
vif(train_smote_model_9)

#Removing Outstanding.Balance because of high VIF (2.13)
train_smote_model_10 <- glm(formula = Performance.Tag ~ Income + 
    No.of.months.in.current.residence + No.of.months.in.current.company + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    Presence.of.open.auto.loan + Marital.Status..at.the.time.of.application. + Education.xPhd + 
    Education.xProfessional + Profession.xSE + Type.of.residence.xOthers, 
    family = "binomial", data = train_smote)

summary(train_smote_model_10)
vif(train_smote_model_10)

#Now all variables have VIF<=2. So, check for p-value
#Removing Marital.Status..at.the.time.of.application. because of high p-value (0.1336)
train_smote_model_11 <- glm(formula = Performance.Tag ~ Income + 
    No.of.months.in.current.residence + No.of.months.in.current.company + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    Presence.of.open.auto.loan + Education.xPhd + Education.xProfessional + Profession.xSE + Type.of.residence.xOthers, 
    family = "binomial", data = train_smote)

summary(train_smote_model_11)
vif(train_smote_model_11)

#Removing Profession.xPhd because of high p-value (0.1043)
train_smote_model_12 <- glm(formula = Performance.Tag ~ Income + 
    No.of.months.in.current.residence + No.of.months.in.current.company + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    Presence.of.open.auto.loan + Education.xProfessional + Profession.xSE + Type.of.residence.xOthers, 
    family = "binomial", data = train_smote)

summary(train_smote_model_12)
vif(train_smote_model_12)

#Removing Education.xProfessional because of high p-value (0.1555)
train_smote_model_13 <- glm(formula = Performance.Tag ~ Income + 
    No.of.months.in.current.residence + No.of.months.in.current.company + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
    Presence.of.open.auto.loan + Profession.xSE + Type.of.residence.xOthers, 
    family = "binomial", data = train_smote)

summary(train_smote_model_13)
vif(train_smote_model_13)

#Removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. because of high p-value (0.1014)
train_smote_model_14 <-  glm(formula = Performance.Tag ~ Income + 
    No.of.months.in.current.residence + No.of.months.in.current.company + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + Presence.of.open.auto.loan + Profession.xSE + 
    Type.of.residence.xOthers, family = "binomial", data = train_smote)

summary(train_smote_model_14)
vif(train_smote_model_14)

#Removing Profession.xSE because of high p-value (0.0797)
train_smote_model_15 <- glm(formula = Performance.Tag ~ Income + 
    No.of.months.in.current.residence + No.of.months.in.current.company + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +No.of.PL.trades.opened.in.last.6.months + Presence.of.open.auto.loan +
    Type.of.residence.xOthers, family = "binomial", data = train_smote)

summary(train_smote_model_15)
vif(train_smote_model_15)

#Removing Type.of.residence.xOthers because of high p-value (0.0772)
train_smote_model_16 <- glm(formula = Performance.Tag ~ Income + 
    No.of.months.in.current.residence + No.of.months.in.current.company + 
    Avgas.CC.Utilization.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.6.months +                                             No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., 
    family = "binomial", data = train_smote)

summary(train_smote_model_16)
vif(train_smote_model_16)

#Removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. because of high p-value (0.0967)
train_smote_model_17 <- glm(formula = Performance.Tag ~ Income + 
    No.of.months.in.current.residence + No.of.months.in.current.company +  Avgas.CC.Utilization.in.last.12.months +     No.of.times.30.DPD.or.worse.in.last.6.months + No.of.PL.trades.opened.in.last.6.months, family = "binomial", data = train_smote)

summary(train_smote_model_17)
vif(train_smote_model_17)

#all variables have VIF<=2 and p-values less than 0.05
#Hence, train_smote_model_17 is deemed to be the final model.

##The top predictor variables present in the final model are:
## 1. Income 
## 2. No.of.months.in.current.residence
## 3. No.of.months.in.current.company
## 4. Avgas.CC.Utilization.in.last.12.months
## 5. No.of.times.30.DPD.or.worse.in.last.6.months
## 6. No.of.PL.trades.opened.in.last.6.months


################ Model Evaluation and Validation #################

# MODEL EVALUATION FOR SMOTE ####
train_smote_model_final <- train_smote_model_17

#predicted probabilities of default for test data
test_pred_smote = predict(train_smote_model_final, type = "response", 
                    newdata = test[,-1])


# Let's see the summary 

summary(test_pred_smote)

test$prob_smote <- test_pred_smote
View(test)
# Let's use the probability cutoff of 50%.

test_pred_def_smote <- factor(ifelse(test_pred_smote >= 0.50, "Yes", "No"))
test_actual_def_smote <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))


table(test_actual_def_smote,test_pred_def_smote)

test_conf <- confusionMatrix(test_pred_def_smote, test_actual_def_smote, positive = "Yes")
test_conf
##Accuracy :62.46% Sensitivity : 64.64% Specificity : 62.36%

perform_fn_2 <- function(cutoff_smote) 
{
  predicted_def_smote <- factor(ifelse(test_pred_smote >= cutoff_smote, "Yes", "No"))
  conf <- confusionMatrix(predicted_def_smote, test_actual_def_smote, positive = "Yes")
  acc_smote <- conf$overall[1]
  sens_smote <- conf$byClass[1]
  spec_smote <- conf$byClass[2]
  out <- t(as.matrix(c(sens_smote, spec_smote, acc_smote))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred_smote)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn_2(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff_smote <- s[which(abs(OUT[,1]-OUT[,2])<0.2)]

#######################################################################

perform_fn_2 <- function(cutoff_smote) 
{
  predicted_def_smote <- factor(ifelse(test_pred_smote >= cutoff_smote, "Yes", "No"))
  conf <- confusionMatrix(predicted_def_smote, test_actual_def_smote, positive = "Yes")
  acc_smote <- conf$overall[1]
  sens_smote <- conf$byClass[1]
  spec_smote <- conf$byClass[2]
  out <- t(as.matrix(c(sens_smote, spec_smote, acc_smote))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred_smote)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn_2(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff_smote <- s[which(abs(OUT[,1]-OUT[,2])<0.1)]
cutoff_smote

# Let's choose the optimal cutoff value of 0.505 for final model

test_cutoff_def_smote <- factor(ifelse(test_pred_smote >=0.505 , "Yes", "No"))

conf_final_smote <- confusionMatrix(test_cutoff_def_smote, test_actual_def_smote, positive = "Yes")

acc_s_model <- conf_final_smote$overall[1]

sens_s_model <- conf_final_smote$byClass[1]

spec_s_model <- conf_final_smote$byClass[2]
conf_final_smote

# Accuracy = 63.13%
# Sensitivity = 63.65%
# Specificity = 63.1%

### KS -statistic - Test Data ######

test_cutoff_def_smote <- ifelse(test_cutoff_def_smote=="Yes",1,0)
test_actual_def_smote <- ifelse(test_actual_def_smote=="Yes",1,0)

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_def_smote, test_actual_def_smote)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # 0.2675
#KS-statistic is 26.75%


### PLotting the ROC Curve ###
plot(performance_measures_test,main = "ROC curve")

### Area under ROC Curve ###
auc_ROCR <- performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 
#0.6337

#################################################################
### Plotting Gain and Lift Chart ###

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
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

Default_decile = lift(test_actual_def_smote, test_pred_smote, groups = 10)
Default_decile

plot(y=Default_decile$Gain,x=Default_decile$bucket,type ="l",lwd = 2,xlab="Bucket",ylab="Gain",main = "Gain Chart")

plot(y=Default_decile$Cumlift, x=Default_decile$bucket,col="red",type="l",xlab="% of total targeted",ylab = "Lift",main="Lift Chart")
######################################################################

############### Validation for Logistic Regression Models ##################

## This is done by choosing new test data and setting different 
## numeric parameters in the set.seed() function
#1. Logistic regression (without using SMOT)
#cross validation on new test data set
set.seed(1)
split_indices <- sample.split(final_df,SplitRatio=0.7)
test_cv1 <-final_df[!split_indices,]
test_pred1 <- predict(final_model, type="response",newdata=test_cv1[,-1])
#confusion matrix based on cutoff previously obtained (0.05)
test_cutoff <- factor(ifelse(test_pred1>=0.05,"Yes","No"))
test_actual <- factor(ifelse(test_cv1$Performance.Tag==1,"Yes","No"))
conf_final <- confusionMatrix(test_cutoff,test_actual,positive = "Yes")
conf_final
##Accuracy : 67.45% 
##Sensitivity : 54.4%
##Specificity : 68.03%

#cross validation on new test data set
set.seed(50)
split_indices <- sample.split(final_df,SplitRatio=0.7)
test_cv2 <-final_df[!split_indices,]
test_pred2 <- predict(final_model, type="response",newdata=test_cv2[,-1])
#confusion matrix based on cutoff previously obtained (0.05)
test_cutoff <- factor(ifelse(test_pred2>=0.05,"Yes","No"))
test_actual <- factor(ifelse(test_cv2$Performance.Tag==1,"Yes","No"))
conf_final <- confusionMatrix(test_cutoff,test_actual,positive = "Yes")
conf_final
##Accuracy :67.68%
##Sensitivity : 54.97%
##Specificity : 68.23%

#2. Logistic regression (using SMOT)
#cross validation on new test data set
set.seed(1)
split_indices <- sample.split(final_df,SplitRatio=0.7)
test_cv1 <-final_df[!split_indices,]
test_pred1 <- predict(train_smote_model_final, type="response",newdata=test_cv1[,-1])
#confusion matrix based on cutoff previously obtained (0.505)
test_cutoff <- factor(ifelse(test_pred1>=0.505,"Yes","No"))
test_actual <- factor(ifelse(test_cv1$Performance.Tag==1,"Yes","No"))
conf_final <- confusionMatrix(test_cutoff,test_actual,positive = "Yes")
conf_final
##Accuracy : 63.28%
##Sensitivity : 60.33%
##Specificity : 63.41%

#cross validation on new test data set
set.seed(50)
split_indices <- sample.split(final_df,SplitRatio=0.7)
test_cv2 <-final_df[!split_indices,]
test_pred2 <- predict(train_smote_model_final, type="response",newdata=test_cv2[,-1])
#confusion matrix based on cutoff previously obtained (0.505)
test_cutoff <- factor(ifelse(test_pred2>=0.505,"Yes","No"))
test_actual <- factor(ifelse(test_cv2$Performance.Tag==1,"Yes","No"))
conf_final <- confusionMatrix(test_cutoff,test_actual,positive = "Yes")
conf_final
##Accuracy : 63.35%
##Sensitivity : 61.03% 
##Specificity : 63.45%

#In both the cases,each model gives virtually similar results for a given cutoff

######################################################################

######################################################################
### Random Forest (Model Building) for the combined data set ###
######################################################################

############ Random Forest ###################
#Remove Application ID as it is not relevant for analysis.
master_data_tree <- master_data2
master_data_tree$Gender <- as.factor(master_data_tree$Gender)

master_data_tree$Marital.Status..at.the.time.of.application. <- as.factor(master_data_tree$Marital.Status..at.the.time.of.application.)

master_data_tree$Education <- as.factor(master_data_tree$Education)

master_data_tree$Profession <- as.factor(master_data_tree$Profession)

master_data_tree$Type.of.residence <- as.factor(master_data_tree$Type.of.residence)

master_data_tree$Performance.Tag<-as.factor(ifelse(master_data_tree$Performance.Tag==0,"no","yes"))
split_indices <- sample.split(master_data_tree, SplitRatio = 7/10)

train_tree<- master_data_tree[split_indices, ]
test_tree<- master_data_tree[!split_indices, ]

# Without SCALED data categorical data encoding [Outliers not removed]
######################################################################
#Remove Application ID as it is not relevant for analysis.
master_data_tree1 <- master_data
master_data_tree1 <-master_data_tree1[,-1]
master_data_tree1$Gender <- as.factor(master_data_tree1$Gender)

master_data_tree1$Marital.Status..at.the.time.of.application. <- as.factor(master_data_tree1$Marital.Status..at.the.time.of.application.)

master_data_tree1$Education <- as.factor(master_data_tree1$Education)

master_data_tree1$Profession <- as.factor(master_data_tree1$Profession)

master_data_tree1$Type.of.residence <- as.factor(master_data_tree1$Type.of.residence)

master_data_tree1$Performance.Tag<-as.factor(ifelse(master_data_tree1$Performance.Tag==0,"no","yes"))
split_indices <- sample.split(master_data_tree1, SplitRatio = 7/10)

train_tree1<- master_data_tree1[split_indices, ]
test_tree1<- master_data_tree1[!split_indices, ]

# train and test split with SCALED data
######################################################################

set.seed(100)

final_df$Performance.Tag<-as.factor(ifelse(final_df$Performance.Tag==0,"no","yes"))
split_indices <- sample.split(final_df, SplitRatio = 7/10)

data_for_sampling <- final_df[split_indices, ]

test<- final_df[!split_indices, ]

###################################################################
# Random Forest (model building) without SMOT

rf_norm1 <- randomForest(Performance.Tag ~., 
                        data = train_tree1, 
                        proximity = F, 
                        do.trace = T, 
                        mtry = 5,
                        ntree=1000)
summary(rf_norm1)


################ Model Evaluation and Validation #################

# make predictions on the test set
tree.predict_nrm <- predict(rf_norm1, test_tree1, type = "class")
# evaluate the results
confusionMatrix(tree.predict_nrm, test_tree1$Performance.Tag, positive = "yes") 

#In terms of probbability
rf_pred_synthetic <- predict(rf_norm1, test_tree1, type = "prob")

#Let's find out the optimal cutoff value for probalility with synthetic data
#Cutoff for randomforest to assign yes or no
perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_synthetic[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_tree1$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 


# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(1,"darkgreen",2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])==min(abs(OUT_rf[,1]-OUT_rf[,2])))]
cutoff_rf
#The plot shows that cutoff value of around 0.05 optimises sensitivity and accuracy

test_pred_optimal<- factor(ifelse(rf_pred_synthetic[, 2] >= 0.05, "yes", "no"))
conf_rf <- confusionMatrix(test_pred_optimal, test_tree1$Performance.Tag, positive = "yes")
conf_rf

# Accuracy : 59.2%
# Sensitivity : 64.59% 
# Specificity : 58.97%

# Final RF important variables
importance <- rf_norm1$importance 
importance <- data.frame(importance)
importance 

##The top predictor variables based on decreasing MeanDecreasingGini are:
##					                   MeanDecreaseGini  
##No.of.months.in.current.company			   363.4985208                                                
##Income						   345.7562181
##Avgas.CC.Utilization.in.last.12.months       		   338.8680126
##Age							   338.7036148
##No.of.months.in.current.residence 			   273.7168575
##Total.No.of.Trades					   202.6982407

####################### KS - statistic -Random Forest - Test Data ###############################################
library(ROCR)
test_actual_default<-as.factor(ifelse(test_tree1$Performance.Tag == "yes", 1,0))
pred_object_test<- prediction(as.numeric(test_pred_optimal), as.numeric(test_actual_default))

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  #0.2356

#KS-statistic is 23.56% 

### PLotting the ROC Curve ###
plot(performance_measures_test,main = "ROC curve")

### Area under ROC Curve ###
auc_ROCR <- performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 
#0.6178

#################################################################
### Plotting Gain and Lift Chart ###

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
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

Default_decile = lift(test_actual_default, rf_pred_synthetic[, 2] , groups = 10)
Default_decile

plot(y=Default_decile$Gain,x=Default_decile$bucket,type ="l",lwd = 2,xlab="Bucket",ylab="Gain",main = "Gain Chart")

plot(y=Default_decile$Cumlift, x=Default_decile$bucket,col="red",type="l",xlab="% of total targeted",ylab = "Lift",main="Lift Chart")
##########################

###################################################################
#Generate data synthetically
train <- ROSE(Performance.Tag ~ ., data = train_tree1, seed = 1)$data

table(train$Performance.Tag)

#Random Forest (model building) using SMOTE analysis (to account for the data imbalance)

rf_SMOT <- randomForest(Performance.Tag ~., 
                             data = train, 
                             proximity = F, 
                             do.trace = T, 
                             mtry = 5,
                             ntree=1000)
summary(rf_SMOT)

################ Model Evaluation and Validation #################

# make predictions on the test set
tree.predict <- predict(rf_SMOT, test_tree1, type = "class")
# evaluate the results
confusionMatrix(tree.predict, test_tree1$Performance.Tag, positive = "yes") 

#In terms of probbability
rf_pred_synthetic <- predict(rf_SMOT, test_tree1, type = "prob")

#Let's find out the optimal cutoff value for probalility with synthetic data
#Cutoff for randomforest to assign yes or no
perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_synthetic[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_tree1$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 


# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(1,"darkgreen",2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])==min(abs(OUT_rf[,1]-OUT_rf[,2])))]
cutoff_rf
#0.1584

#The plot shows that cutoff value of around 16% optimises sensitivity and accuracy

test_pred_optimal<- factor(ifelse(rf_pred_synthetic[, 2] >= 0.16, "yes", "no"))
conf_rf <- confusionMatrix(test_pred_optimal, test_tree1$Performance.Tag, positive = "yes")
conf_rf

# Accuracy : 62.86%
# Sensitivity : 61.86%  
# Specificity : 62.91%

# Final RF important variables
importance <- rf_SMOT$importance 
importance <- data.frame(importance)
importance
##The top predictor variables based on decreasing MeanDecreasingGini are:
##					                 MeanDecreaseGini  
##No.of.times.30.DPD.or.worse.in.last.6.months 		 1873.21039                                                  
##No.of.times.60.DPD.or.worse.in.last.6.months		 1732.88465
##No.of.times.30.DPD.or.worse.in.last.12.months     	 1612.29503
##No.of.times.90.DPD.or.worse.in.last.6.months		 1528.38914				   	   		
##No.of.times.90.DPD.or.worse.in.last.12.months 	 1439.55264		   
##No.of.times.60.DPD.or.worse.in.last.12.months     	 1370.20839			   
##Outstanding.Balance					 1189.51049
##Avgas.CC.Utilization.in.last.12.months                 1033.37614

####################### KS - statistic -Random Forest - Test Data ###############################################
library(ROCR)
test_actual_default<-as.factor(ifelse(test_tree1$Performance.Tag == "yes", 1,0))
pred_object_test<- prediction(as.numeric(test_pred_optimal), as.numeric(test_actual_default))

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  #0.2476

#KS-statistic is 24.76% 

### PLotting the ROC Curve ###
plot(performance_measures_test,main = "ROC curve")

### Area under ROC Curve ###
auc_ROCR <- performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 
#0.6238

#################################################################
### Plotting Gain and Lift Chart ###

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
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

Default_decile = lift(test_actual_default, rf_pred_synthetic[, 2] , groups = 10)
Default_decile

plot(y=Default_decile$Gain,x=Default_decile$bucket,type ="l",lwd = 2,xlab="Bucket",ylab="Gain",main = "Gain Chart")

plot(y=Default_decile$Cumlift, x=Default_decile$bucket,col="red",type="l",xlab="% of total targeted",ylab = "Lift",main="Lift Chart")
##########################

#From the lift gain chart, it is clear that most of the values (80%) are accurately predicted by the 6th decile for all the models (logistic regression and random forest) built.


#############################################################################
####### 5. Final model selection for prediction and further analysis  #######
#############################################################################

#Area under ROC Curve (auc) is the evaluation metric used for final model selection.
#Area under ROC Curve (0.6325) is greater for logistic regression model (using smote) 
#therefore, train_smote_model_final is taken as the final model for further analysis]
##############################################

####################################################################################
### Predicting the default rate of rejected applicants using the final model ###
#####################################################################################
#checking details of rejected applicants
str(rejected_applicants)
#remove application ID as it is not relevant for analysis
rejected_applicants <- rejected_applicants[,-1]

#analysing rejected population w.r.t. the final model
pred_missing = predict(train_smote_model_final, type = "response",newdata = rejected_applicants[,-1])
summary(pred_missing)

#NA value imputation by mean to ensure consistency in analysis
pred_missing[which(is.na(pred_missing))] <- mean(pred_missing, na.rm = T)
summary(pred_missing)

#prediction based on optimal cutoff (0.505)
rejected_applicants$Performance.Tag <- as.numeric(ifelse(pred_missing >=0.505, "1", "0"))
summary(factor(rejected_applicants$Performance.Tag))
# 0     1
#166 1259
#most of the rejected population is currently identified as defaulters using the model

###################################################################################
###### 6. Building the application score card and finding the cut-off score #######
###################################################################################

## Build an application scorecard with the good to bad odds of 10 to 1 
## at a score of 400 doubling every 20 points.  
final_df$population <- predict(train_smote_model_final, type = "response", newdata = final_df[,-1])

#Odd(good) = (1-P(bad))/P(bad)
final_df$odds <-sapply(final_df$population,function(x) x/(1-x))

#computing  ln(odd(good))
final_df$log_odds <-sapply(final_df$odds,function(x)log(x))

Offset = 400
PDO = 20
log_odds=10
Factor = PDO/log(2)
Factor  #28.8539

#cutoff<- 400 + (Factor * (log((1-model_cutoff)/model_cutoff) - log(10)))

#prdicting the scores for approved applicants
final_df$Score = ceiling(Offset + (Factor*final_df$log_odds))

str(final_df$Score)
summary(final_df$Score)
## min - 361 to max - 481
## mean score for approved customers - 396
## Higher the score, lesser the risk fo defaulting
quantile(final_df$Score,seq(0,1,0.2))  

#cutoff selected for the final model was 0.513
#finding the cutoff score
cutoff<- 400 + (Factor * (log(0.487/0.513) - log(10)))
cutoff
#cutoff score is 332

ggplot(final_df,aes(final_df$Score))+geom_histogram()+labs(x="Score", y= "Population")
#check the score distibution for approved population
boxplot(final_df$Score,main="Credit Score for Approved Applicants")

#No.of applicants above the cutoff score (332)
nrow( subset(final_df, final_df$Score>332 ) )
#69847
#No.of applicants below the cutoff score (332)
nrow( subset(final_df, final_df$Score<=332 ) )
#0

######################################################################################
### Predicting score for rejected applicants ###
######################################################################################
## Build an application scorecard with the good to bad odds of 10 to 1 
## at a score of 400 doubling every 20 points.  
rejected_applicants$population <- predict(train_smote_model_final, type = "response", newdata = rejected_applicants[,-1])

#Odd(good) = (1-P(bad))/P(bad)
rejected_applicants$odds <-sapply(rejected_applicants$population,function(x) x/(1-x))

#computing  ln(odd(good))
rejected_applicants$log_odds <-sapply(rejected_applicants$odds,function(x)log(x))

#prdicting the scores for approved applicants
rejected_applicants$Score = ceiling(Offset + (Factor*rejected_applicants$log_odds))

str(rejected_applicants$Score)
summary(rejected_applicants$Score)

#NA value imputation by mean to ensure consistency in analysis
rejected_applicants$Score[which(is.na(rejected_applicants$Score))] <- mean(rejected_applicants$Score, na.rm = T)
summary(rejected_applicants$Score)

## min - -68 to max - 1440
## mean score for approved customers - 724
## Higher the score, lesser the risk fo defaulting
quantile(rejected_applicants$Score,seq(0,1,0.2))  

#cutoff<- 400 + (Factor * (log((1-model_cutoff)/model_cutoff) - log(10)))

#cutoff selected for the final model was 0.513
#finding the cutoff score
cutoff<- 400 + (Factor * (log(0.487/0.513) - log(10)))
cutoff
#cutoff score is 332

ggplot(rejected_applicants,aes(rejected_applicants$Score))+geom_histogram()+labs(x="Score", y= "Population")
#check the score distibution for approved population
boxplot(rejected_applicants$Score,main="Credit Score for Rejected Applicants")

#No.of applicants above the cutoff score (332)
nrow( subset(rejected_applicants, rejected_applicants$Score>332 ) )
#1320
#No.of applicants below the cutoff score (332)
nrow( subset(rejected_applicants, rejected_applicants$Score<=332 ) )
#105

##Total number of rejected customers by bank : 1425
##Therefore, based on the cutoff score, 105 people would not be issued the credit card.
##Thus, the rejected population is used to assess model performance.
##Also, The application scorecard is correctly built using the final model and a suitable score cut-off is identified. 
##The scores of rejected population are compared with those of the approved ones.  

#####################################################################################
########### 7. Financial Benefits and Analysis using Results and Insights ###########
#####################################################################################
#checking percentage of rejected_applicants
nrow(rejected_applicants)/nrow(master_data)
#0.0204

#The percentage of approved population is (100 - 2.04)% i.e. 97.96%

# Financial Benefit of a model will be in terms of either
# a. decreasing the rejection the non-defaulters
# b. increasing the rejection of defaulters

#lets consider the average loss of 10000 when each non defaulters application is rejected)
#and an average loss of 100000 when each accepted applicant defaults
master_data$Performance.Tag <- as.factor(master_data$Performance.Tag)
summary(master_data$Performance.Tag)
#0 (non-defaulters) - 66920
#1 (defaulters) - 2947

Loss_without_model<- (nrow(subset(master_data,Performance.Tag==0))*10000) - (nrow(subset(master_data,Performance.Tag==1))*100000)
Loss_without_model
#374500000

#profit using the model and its cut-off (0.505)
test_pred_log_final = predict(train_smote_model_final, type = "response", newdata = master_data[,-1])
test_cutoff <- factor(ifelse(test_pred_log_final >= 0.505, "Yes", "No"))
test_actual <- factor(ifelse(master_data$Performance.Tag==1,"Yes", "No"))
conf_final <- confusionMatrix(test_cutoff, test_actual, positive = "Yes")
conf_final

#          Reference
#Prediction    No   Yes
#       No   41413  1191
#       Yes  25507  1756

#total profit due to each true positive and each true negative
#minus loss from each false positive and each false negative prediction
loss_with_model <- ((41413*10000) + (1756*100000))- ((25507*10000) + (1191*100000))
loss_with_model
#215560000

#Net financial gain calculation using the above values
Reduction_loss  <-(Loss_without_model-loss_with_model)*100/Loss_without_model
Reduction_loss 
#42.44
#the financial loss incurred is reduced by 42.44% due to the final model

####REVENUE LOSS####
#No of candidates rejected by the model who didn't default - 25507 
#Total No of candidates who actually didn't default - 66920
Customers_Causing_Revenue_loss <- (25507/66920)*100
Customers_Causing_Revenue_loss
#38.11566
#Thus, 38.12% of non-defaulting customers cause revenue loss to CredX

####CREDIT LOSS####
# Credit loss without model
prop.table(table(master_data$Performance.Tag)) 
#         0          1 
#0.95781986 0.04218014
#% of candidates approved and then defaulted = 4.218%
#4.218% cause credit loss to CredX

# Credit loss with model
nrow(master_data)
#69867
#% of candidates approved and then did not default = (1191/69867)*100 i.e. 1.705%

Credit_loss_reduction <- (1191/2957)*100
Credit_loss_reduction
#40.27731
#Thus, the credit loss incurred by CredX is reduced by 40.28% due to the final model

######################################################################################
## The following assumptions are made during the EDA and model building of this project.
## 1. The categorical and independent variables are converted into factors for EDA and dummy variable creation for ease of analysis.
## 2. The NA/missing value imputation is done for numeric variables with WOE/IV values for ease of analysis.
## 3. The missing value imputation is done using mode for categorical variables for ease of analysis.
## 4. But NA values in performance tag are considered as the rejected population. The rows containing such values are not ignored as they are used for application scorecard and financial analysis at the later stage.
## 5. Some of the outliers can be taken care of using WOE/IV values and binning of variables.
## 6. For some EDA, only performance tag 1 is considered as we want to analyse the influence of various factors on number of defaulters.
## 7. Warning messages arising from function calls, if any can be ignored.
## 8. WOE/IV binning and values are not explicitly shown in model building as they are taken care of in earlier stages of the EDA.
## 9. SVM is not used since the amount of data involved and to be modelled is very large.
## 10. Random Forests are used for modelling rather than Decision Trees since a diverse number of trees based on
## different features and parameters are constructed, thus reducing the problem of overfitting .
## 11. For random forests, cross-validation can be done immediately after model building rather than explicitly since the given problem is a classification problem.
## 12. In random forests, outliers are not ignored as decision rules at the leaves of the trees constructed are not sensitive to outliers and hence model accuracy is not affected.
## 13. Outliers are not used in logistic regression as it affects model accuracy.
######################################################################################

######################################################################################
## The following factors are taken care of during the model building of this project.
## 1. In logistic regression models, variable removal is done on the basis of VIF and then based on p-values.
## 2. The final model selected is involved to predict the default probabilities for rejected population and for 
## financial analysis and scorecard building.
## 3. The model evaluation and validation done is based on appropriate cutoff score using the graph
## of accuracy, sensitivity and specificity and also by using lift-gain charts.
## 4. SMOT (Synthetic Minority Oversampling Technique) is used to provide better model results and handle class imbalance issues.
######################################################################################

######################################################################################
## The following assumptions have been made while assessing the model in financial terms
## 1. The cost of acquisition for every customer has been considered same
## 2. The expected business/revenue/profit for every customer has been considered same
## 3. The figures (acquisition cost/revenue per person) are taken for representational purpose only. Actual
## values may vary.
######################################################################################

######################################################################################
## The following conclusions are drawn from this capstone project:
## 1. The auc (area under the ROC curve) value is used as the evaluation metric for model selection.
## 2. Thus, the logistic regression model obtained after using SMOT (train_smote_model_final) is used for 
## financial analysis, prediction and scorecard building.
## 3. Credit factors like avg credit card utilization and no of trades have greater impact on 
## the model efficiency and analysis (EDA) as compared to demographic factors.
## 4. The cutoff score obtained was 333. The population having score below that cutoff would not be issued the credit card.
## 5. Using the model, the financial, revenue and credit loss incurred by CredX was reduced substantially.
######################################################################################