setwd("D:\\PG_Diploma\\PredAnalytics")
#loading the csv file
car <- read.csv("CarPrice_Assignment.csv")
#display the file details
str(car)

#loading packages required for assignment
library(reshape2)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)

#check for missing and NA values
sapply(car, function(x) any(x == ''))
sapply(car, function(x) any(is.na(x)))

#check for duplicate car ID values 
sum(duplicated(car$CarID))

#convert CarName to lowercase
car$CarName <- tolower(car$CarName)

#splitting CarName into two seperate columns and storing them as CompanyName and ModelName
#car$CarName <- as.character(car$CarName)
Details <- str_split_fixed(car$CarName,"[ ]",2)
Details <- as.data.frame(Details)
car$CompanyName <- Details$V1
car$ModelName <- Details$V2

#rectifying incorrect CompanyName data
car$CompanyName<- as.character(car$CompanyName)
car$CompanyName[which(car$CompanyName=="maxda")]<- "mazda"
car$CompanyName[which(car$CompanyName=="porcshce")]<- "porsche"
car$CompanyName[which(car$CompanyName=="vokswagen")]<- "volkswagen"
car$CompanyName[which(car$CompanyName=="vw")]<- "volkswagen"
car$CompanyName[which(car$CompanyName=="toyouta")]<- "toyota"
car$CompanyName<- as.factor(car$CompanyName)

#Converting symboling (insurance risk rating) into factor variable 
#-2 or -1:safe
#0 or 1:low risk
#2 or 3: risky
levels(car$symboling)[1] <- "safe"
levels(car$symboling)[2] <- "low risk"
levels(car$symboling)[3] <- "risky"
car$symboling[which(car$symboling==-2|car$symboling==-1)] <- levels(car$symboling)[1]
car$symboling[which(car$symboling==0|car$symboling==1)] <- levels(car$symboling)[2]
car$symboling[which(car$symboling==2|car$symboling==3)] <- levels(car$symboling)[3]
car$symboling <- as.factor(car$symboling)

#summarize the different levels of factor type independent variables
summary(car$fueltype)
summary(car$aspiration)
summary(car$doornumber)
summary(car$enginelocation)
summary(car$carbody)
summary(car$drivewheel)
summary(car$enginetype)
summary(car$cylindernumber)
summary(car$fuelsystem)
summary(car$CompanyName)
summary(car$symboling)

#preparing data for regression analysis by creating dummy variables
#analyzing dual-level categorical variables (fuel type, aspiration, doornumber and enginelocation)
#convert them into binary levels (0,1) and convert them into numeric data type
levels(car$fueltype)<-c(0,1) #0-diesel, 1-gas
car$fueltype <- as.numeric(levels(car$fueltype))[car$fueltype]

levels(car$aspiration)<-c(0,1) #0-std, 1-turbo
car$aspiration <- as.numeric(levels(car$aspiration))[car$aspiration]

levels(car$doornumber)<-c(0,1) #0-two, 1-four
car$doornumber <- as.numeric(levels(car$doornumber))[car$doornumber]

levels(car$enginelocation)<-c(0,1) #0-front, 1-rear
car$enginelocation <- as.numeric(levels(car$enginelocation))[car$enginelocation]

#analysing multi-level categorical variables (carbody, drivewheel, enginetype, cylindernumber, fuelsystem, CompanyName and symboling)
#deriving respective dummy variables for the above multi-level categorical variables for regression analysis
#converting them into numeric data type after the above step
#remove redundant columns and replace them with dummy variables after each step
dummy_carbody <- data.frame(model.matrix(~carbody,data=car))
dummy_carbody[,-1]
car1 <- cbind(car[,-7],dummy_carbody)

dummy_drivewheel <- data.frame(model.matrix(~drivewheel,data=car))
dummy_drivewheel[,-1]
car2 <- cbind(car1[,-7],dummy_drivewheel)

dummy_enginetype <- data.frame(model.matrix(~enginetype,data=car))
dummy_enginetype[,-1]
car3 <- cbind(car2[,-13],dummy_enginetype)

dummy_cylindernumber <- data.frame(model.matrix(~cylindernumber,data=car))
dummy_cylindernumber[,-1]
car4 <- cbind(car3[,-13],dummy_cylindernumber)

dummy_fuelsystem <- data.frame(model.matrix(~fuelsystem,data=car))
dummy_fuelsystem[,-1]
car5 <- cbind(car4[,-14],dummy_fuelsystem)

dummy_company <- data.frame(model.matrix(~CompanyName,data=car))
dummy_company[,-1]
car6 <- cbind(car5[,-22],dummy_company)

dummy_symboling <- data.frame(model.matrix(~symboling,data=car))
dummy_symboling[,-1]
car7 <- cbind(car6[,-2],dummy_symboling)

#save the final dataframe into a new one (carsfinal)
carsfinal <- car7

#remove CarID, CarName and ModelName columns (one by one) as they are not required for regression model analysis
carsfinal <- carsfinal[,-1]
carsfinal <- carsfinal[,-1]
carsfinal <- carsfinal[,-19]
#CarID is nothing but a unique ID assigned to a particular car and hence, is not important for the analysis
#CarName and ModelName are redundant variables becuase CompanyName is already extracted from CarName
#As per that problem statement, CompanyName is to be considered as independent variable for model building
#(ModelName need not be considered for the same)

#deriving new metrics for regression analysis
#average overall mileage per horsepower
carsfinal$mpg <- (carsfinal$citympg+carsfinal$highwaympg)/2
carsfinal$mpghp <- (carsfinal$mpg/carsfinal$horsepower)
#mileage per peak rpm
carsfinal$mpgrpm <- (carsfinal$mpg/carsfinal$peakrpm)
#bore to stroke ratio
carsfinal$boretostroke <- (carsfinal$boreratio/carsfinal$stroke)
#hp to curbweight
carsfinal$powertoweight <- (carsfinal$horsepower/carsfinal$curbweight)

#understanding correlation of price w.r.t. mpg per hp, horsepower, peakrpm, bore to stroke ratio and curbweight
cor(carsfinal$price,carsfinal$mpghp)
cor(carsfinal$price,carsfinal$horsepower)
cor(carsfinal$price,carsfinal$peakrpm)
cor(carsfinal$price,carsfinal$boretostroke)
cor(carsfinal$price,carsfinal$curbweight)
#from the above result, it is clear that:
#1.car price is inversely related to peakrpm and mpg per hp
#2.car price is directly related to horsepower, bore to stroke ratio and curbweight

# separate training and testing data 
# 70% data for training and remaining for testing
set.seed(100)
trainindices= sample(1:nrow(carsfinal), 0.7*nrow(carsfinal))
train = carsfinal[trainindices,]
test = carsfinal[-trainindices,]

#build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)
#R-squared:  0.9851,    Adjusted R-squared:  0.9741

#perform stepwise (iterative) model selection using stepAIC functions
step <- stepAIC(model_1, direction="both")
#removes insignificant variables not required for building the model
#build new model after removing insignicant variables (having '+' symbol preceding them) (sum of sq < 1000000) using stepAIC
model_2 <- lm(formula=price ~ drivewheelrwd + enginetypeohcv + symbolingrisky + fueltype
              + CompanyNamesaab + boretostroke + horsepower + boreratio + carbodyhardtop + enginetypedohcv + CompanyNamemercury 
	      + CompanyNameisuzu + cylindernumberthree + fuelsystemmpfi + mpghp + powertoweight + CompanyNamevolvo
	      + fuelsystem2bbl + citympg + carbodysedan + CompanyNamehonda + carbodyhatchback + highwaympg + mpgrpm
	      + carbodywagon + cylindernumbersix + CompanyNamebmw + cylindernumberfour + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_2)
#R-squared:  0.9845,    Adjusted R-squared:  0.9774
#checking for multicollinearity using VIF
# If the VIF is above 2 as the business goal says, you would remove 
# the variables if they are statistically insignificant (p>0.05)
vif(model_2)

#mpgrpm has high VIF 651.245 but it is significant as its p-value is 0.0006 (< 0.05)
#highwaympg and citympg has VIF>200 but p-value<0.05

#drivewheelrwd has VIF 8.657 and p-value 0.14 (>0.05)
#hence, it is deemed to be insignificant and is removed first from the model

model_3 <- lm(formula=price ~ enginetypeohcv + symbolingrisky + fueltype
              + CompanyNamesaab + boretostroke + horsepower + boreratio + carbodyhardtop + enginetypedohcv + CompanyNamemercury 
	      + CompanyNameisuzu + cylindernumberthree + fuelsystemmpfi + mpghp + powertoweight + CompanyNamevolvo
	      + fuelsystem2bbl + citympg + carbodysedan + CompanyNamehonda + carbodyhatchback + highwaympg + mpgrpm
	      + carbodywagon + cylindernumbersix + CompanyNamebmw + cylindernumberfour + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_3)
#R-squared:  0.9842,    Adjusted R-squared:  0.9771 
#checking for multicollinearity using VIF
vif(model_3)

#CompanyNamesaab has VIF 3.03 but its p-value is 0.2(>0.05)
#hence, it is removed as it is insignificant

model_4 <- lm(formula=price ~ enginetypeohcv + symbolingrisky + fueltype
              + boretostroke + horsepower + boreratio + carbodyhardtop + enginetypedohcv + CompanyNamemercury 
	      + CompanyNameisuzu + cylindernumberthree + fuelsystemmpfi + mpghp + powertoweight + CompanyNamevolvo
	      + fuelsystem2bbl + citympg + carbodysedan + CompanyNamehonda + carbodyhatchback + highwaympg + mpgrpm
	      + carbodywagon + cylindernumbersix + CompanyNamebmw + cylindernumberfour + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_4)
#R-squared:  0.9839,    Adjusted R-squared:  0.9769
#checking for multicollinearity using VIF
vif(model_4)

#boretostroke has VIF 6.69 and p-value 0.207 (>0.05)
#hence, it is removed as it is insignificant

model_5 <- lm(formula=price ~ enginetypeohcv + symbolingrisky + fueltype
              + horsepower + boreratio + carbodyhardtop + enginetypedohcv + CompanyNamemercury 
	      + CompanyNameisuzu + cylindernumberthree + fuelsystemmpfi + mpghp + powertoweight + CompanyNamevolvo
	      + fuelsystem2bbl + citympg + carbodysedan + CompanyNamehonda + carbodyhatchback + highwaympg + mpgrpm
	      + carbodywagon + cylindernumbersix + CompanyNamebmw + cylindernumberfour + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_5)
#R-squared:  0.9837,    Adjusted R-squared:  0.9768
#checking for multicollinearity using VIF
vif(model_5)

#enginetypeohcv has VIF 2.76 but p-value 0.21 (>0.05)
#hence, it is removed as it is insignificant

model_6 <- lm(formula=price ~ symbolingrisky + fueltype
              + horsepower + boreratio + carbodyhardtop + enginetypedohcv + CompanyNamemercury 
	      + CompanyNameisuzu + cylindernumberthree + fuelsystemmpfi + mpghp + powertoweight + CompanyNamevolvo
	      + fuelsystem2bbl + citympg + carbodysedan + CompanyNamehonda + carbodyhatchback + highwaympg + mpgrpm
	      + carbodywagon + cylindernumbersix + CompanyNamebmw + cylindernumberfour + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_6)
#R-squared:  0.9834,    Adjusted R-squared:  0.9767
#checking for multicollinearity using VIF
vif(model_6)

#boreratio has VIF 8.66 and p-value 0.096 (>0.05)
#hence, it is removed as it is insignificant

model_7 <- lm(formula=price ~ symbolingrisky + fueltype
              + horsepower + carbodyhardtop + enginetypedohcv + CompanyNamemercury 
	      + CompanyNameisuzu + cylindernumberthree + fuelsystemmpfi + mpghp + powertoweight + CompanyNamevolvo
	      + fuelsystem2bbl + citympg + carbodysedan + CompanyNamehonda + carbodyhatchback + highwaympg + mpgrpm
	      + carbodywagon + cylindernumbersix + CompanyNamebmw + cylindernumberfour + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_7)
#R-squared:  0.9829,    Adjusted R-squared:  0.9762
#checking for multicollinearity using VIF
vif(model_7)

#horsepower has VIF 107 and p-value 0.107 (>0.05)
#hence, it is removed as it is insignificant

model_8 <- lm(formula=price ~ symbolingrisky + fueltype + carbodyhardtop + enginetypedohcv + CompanyNamemercury 
	      + CompanyNameisuzu + cylindernumberthree + fuelsystemmpfi + mpghp + powertoweight + CompanyNamevolvo
	      + fuelsystem2bbl + citympg + carbodysedan + CompanyNamehonda + carbodyhatchback + highwaympg + mpgrpm
	      + carbodywagon + cylindernumbersix + CompanyNamebmw + cylindernumberfour + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_8)
#R-squared:  0.9825,    Adjusted R-squared:  0.9759
#checking for multicollinearity using VIF
vif(model_8)

#fueltype has VIF 8.61 but p-value 0.06 (>0.05)
#hence, it is removed as it is insignificant

model_9 <- lm(formula=price ~ symbolingrisky + carbodyhardtop + enginetypedohcv + CompanyNamemercury 
	      + CompanyNameisuzu + cylindernumberthree + fuelsystemmpfi + mpghp + powertoweight + CompanyNamevolvo
	      + fuelsystem2bbl + citympg + carbodysedan + CompanyNamehonda + carbodyhatchback + highwaympg + mpgrpm
	      + carbodywagon + cylindernumbersix + CompanyNamebmw + cylindernumberfour + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_9)
#R-squared:  0.9819,    Adjusted R-squared:  0.9759
#checking for multicollinearity using VIF
vif(model_9)

#powertoweight has VIF 17.57 but high p-value 0.08 (>0.05)
#hence, it is removed as it is insignificant

model_10 <- lm(formula=price ~ symbolingrisky + carbodyhardtop + enginetypedohcv + CompanyNamemercury 
	      + CompanyNameisuzu + cylindernumberthree + fuelsystemmpfi + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + citympg + carbodysedan + CompanyNamehonda + carbodyhatchback + highwaympg + mpgrpm
	      + carbodywagon + cylindernumbersix + CompanyNamebmw + cylindernumberfour + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_10)
#R-squared:  0.9813,    Adjusted R-squared:  0.9747
#checking for multicollinearity using VIF
vif(model_10)

#cylindernumberthree has VIF 2.88 and p-value 0.052 (>0.05)
#hence, it is removed as it is insignificant

model_11 <- lm(formula=price ~ symbolingrisky + carbodyhardtop + enginetypedohcv 
	      + CompanyNamemercury + CompanyNameisuzu + fuelsystemmpfi + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + citympg + carbodysedan + CompanyNamehonda + carbodyhatchback + highwaympg + mpgrpm
	      + carbodywagon + cylindernumbersix + CompanyNamebmw + cylindernumberfour + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_11)
#R-squared:  0.9806,    Adjusted R-squared:  0.9741 
#checking for multicollinearity using VIF
vif(model_11)

#now all variables have p-value less than 0.05
#check for VIF
#mpgrpm has highest VIF (470.02) and hence can be removed 

model_12 <- lm(formula=price ~ symbolingrisky + carbodyhardtop + enginetypedohcv 
	      + CompanyNamemercury + CompanyNameisuzu + fuelsystemmpfi + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + citympg + carbodysedan + CompanyNamehonda + carbodyhatchback + highwaympg 
	      + carbodywagon + cylindernumbersix + CompanyNamebmw + cylindernumberfour + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_12)
#R-squared:  0.9798,    Adjusted R-squared:  0.9732
#checking for multicollinearity using VIF
vif(model_12)

#highwaympg and citympg have VIF (>46) and p-value 0.184 and 0.09, respectively (>0.05)
#hence, both are removed as both are insignificant

model_13 <- lm(formula=price ~ symbolingrisky + carbodyhardtop + enginetypedohcv 
	      + CompanyNamemercury + CompanyNameisuzu + fuelsystemmpfi + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + carbodysedan + CompanyNamehonda + carbodyhatchback
	      + carbodywagon + cylindernumbersix + CompanyNamebmw + cylindernumberfour + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_13)
#R-squared:  0.976,    Adjusted R-squared:  0.9701
#checking for multicollinearity using VIF
vif(model_13)

#cylindernumberfour and cylindernumbersix have both VIF >5 and p-values both > 0.1 (>0.05)
#hence, both are removed as both are insignificant

model_14 <- lm(formula=price ~ symbolingrisky + carbodyhardtop + enginetypedohcv 
	      + CompanyNamemercury + CompanyNameisuzu + fuelsystemmpfi + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + carbodysedan + CompanyNamehonda + carbodyhatchback
	      + carbodywagon + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_14)
#R-squared:  0.9764,    Adjusted R-squared:  0.9698
#checking for multicollinearity using VIF
vif(model_14)

#enginetypedohcv has VIF 1.36 and p-value 0.2 (>0.05)
#hence, it is removed as it is insignificant

model_15 <- lm(formula=price ~ symbolingrisky + carbodyhardtop
	      + CompanyNamemercury + CompanyNameisuzu + fuelsystemmpfi + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + carbodysedan + CompanyNamehonda + carbodyhatchback
	      + carbodywagon + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_15)
#R-squared:  0.976,    Adjusted R-squared:  0.9696
#checking for multicollinearity using VIF
vif(model_15)

#now all variables have p-value less than 0.05
#check for VIF
#carbodysedan and carbodyhatchback have the highest VIFs (both >12) and hence can be removed 

model_16 <- lm(formula=price ~ symbolingrisky + carbodyhardtop
	      + CompanyNamemercury + CompanyNameisuzu + fuelsystemmpfi + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + CompanyNamehonda 
	      + carbodywagon + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_16)
#R-squared:  0.9729,    Adjusted R-squared:  0.9663
#checking for multicollinearity using VIF
vif(model_16)

#carbodywagon has VIF 1.31 but p-value 0.54(>0.05)
#hence, it is removed as it is insignificant

model_17 <- lm(formula=price ~ symbolingrisky + carbodyhardtop
	      + CompanyNamemercury + CompanyNameisuzu + fuelsystemmpfi + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + CompanyNamehonda + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_17)
#R-squared:  0.9728,    Adjusted R-squared:  0.9664
#checking for multicollinearity using VIF
vif(model_17)

#carbodyhardtop has VIF 1.44 but p-value 0.164 (>0.05)
#hence, it is removed as it is insignificant

model_18 <- lm(formula=price ~ symbolingrisky + CompanyNamemercury + CompanyNameisuzu + fuelsystemmpfi + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + CompanyNamehonda + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_18)
#R-squared:  0.9724,    Adjusted R-squared:  0.9662
#checking for multicollinearity using VIF
vif(model_18)

#now all variables have p-value less than 0.05
#check for VIF
#fuelsystemmpfi has the highest VIF 6.98 and hence can be removed 

model_19 <- lm(formula=price ~ symbolingrisky + CompanyNamemercury + CompanyNameisuzu + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + CompanyNamehonda + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginesize + enginelocation, data = train)
summary(model_19)
#R-squared:  0.9702,    Adjusted R-squared:  0.9639
#checking for multicollinearity using VIF
vif(model_19)

#now all variables have p-value less than 0.05
#check for VIF
#enginesize has the highest VIF 6.7 and hence can be removed 

model_20 <- lm(formula=price ~ symbolingrisky + CompanyNamemercury + CompanyNameisuzu + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + CompanyNamehonda + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginelocation, data = train)
summary(model_20)
#R-squared:  0.9044,     Adjusted R-squared:  0.885
#checking for multicollinearity using VIF
vif(model_20)

#symbolingrisky has VIF 1.58 but p-value 0.687 (>0.05)
#hence, it is removed as it is insignificant

model_21 <- lm(formula=price ~ CompanyNamemercury + CompanyNameisuzu + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + CompanyNamehonda + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginelocation, data = train)
summary(model_21)
#R-squared:  0.9043,     Adjusted R-squared:  0.8858
#checking for multicollinearity using VIF
vif(model_21)

#CompanyNameisuzu has VIF 1.25 but p-value 0.487 (>0.05)
#hence, it is removed as it is insignificant

model_22 <- lm(formula=price ~ CompanyNamemercury + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + CompanyNamehonda + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + peakrpm + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginelocation, data = train)
summary(model_22)
#R-squared:  0.9039,    Adjusted R-squared:  0.8863
#checking for multicollinearity using VIF
vif(model_22)

#peakrpm has VIF>2 and p-value 0.406 (>0.05)
#hence, it is removed as it is insignificant 

model_23 <- lm(formula=price ~ CompanyNamemercury + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + CompanyNamehonda + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + aspiration + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginelocation, data = train)
summary(model_23)
#R-squared:  0.9033,    Adjusted R-squared:  0.8865
#checking for multicollinearity using VIF
vif(model_23)

#aspiration has VIF<2 but p-value 0.527 (>0.05)
#hence, it is removed as it is insignificant

model_24 <- lm(formula=price ~ CompanyNamemercury + mpghp + CompanyNamevolvo
	      + fuelsystem2bbl + CompanyNamehonda + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginelocation, data = train)
summary(model_24)
#R-squared: 0.903,    Adjusted R-squared:  0.8871
#checking for multicollinearity using VIF
vif(model_24)

#fuelsystem2bbl has VIF 2.27 and p-value 0.56 (>0.05)
#hence, it is removed as it is insignificant

model_25 <- lm(formula=price ~ CompanyNamemercury + mpghp + CompanyNamevolvo
	      + CompanyNamehonda + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + CompanyNametoyota + CompanyNamemitsubishi + enginetyperotor
	      + enginelocation, data = train)
summary(model_25)
#R-squared:  0.9027,    Adjusted R-squared:  0.8877
#checking for multicollinearity using VIF
vif(model_25)

#enginetyperotor has VIF 1.39 but p-value 0.293 (>0.05)
#hence, it is removed as it is insignificant

model_26 <- lm(formula=price ~ CompanyNamemercury + mpghp + CompanyNamevolvo
	      + CompanyNamehonda + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + CompanyNametoyota + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_26)
#R-squared:  0.9018,    Adjusted R-squared:  0.8876
#checking for multicollinearity using VIF
vif(model_26)

#CompanyNamemercury has VIF 1.06 but p-value 0.13 (>0.05)
#hence, it is removed as it is insignificant

model_27 <- lm(formula=price ~ mpghp + CompanyNamevolvo
	      + CompanyNamehonda + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + CompanyNametoyota + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_27)
#R-squared:  0.9,    Adjusted R-squared:  0.8864
#checking for multicollinearity using VIF
vif(model_27)

#CompanyNamevolvo has VIF 1.37 but p-value 0.13 (>0.05)
#hence, it is removed as it is insignificant

model_28 <- lm(formula=price ~ mpghp 
	      + CompanyNamehonda + CompanyNamebmw + carwidth + enginetypeohcf + enginetypel 
	      + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + CompanyNametoyota + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_28)
#R-squared:  0.8981,    Adjusted R-squared:  0.8852
#checking for multicollinearity using VIF
vif(model_28)

#now all variables have p-value less than 0.05
#check for VIF
#carwidth has the highest VIF (3.21) and hence can be removed 

model_29 <- lm(formula=price ~ mpghp 
	      + CompanyNamehonda + CompanyNamebmw + enginetypeohcf + enginetypel 
	      + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + CompanyNametoyota + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_29)
#R-squared:  0.8284,    Adjusted R-squared:  0.8081
#checking for multicollinearity using VIF
vif(model_29)

#enginetypel has VIF 1.31 but p-value 0.21 (>0.05)
#hence, it is removed as it is insignificant

model_30 <- lm(formula=price ~ mpghp + CompanyNamehonda + CompanyNamebmw + enginetypeohcf 
	      + CompanyNamenissan + CompanyNamevolkswagen + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + CompanyNametoyota + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_30)
#R-squared:  0.8262,    Adjusted R-squared:  0.8072
#checking for multicollinearity using VIF
vif(model_30)

#now all variables have VIF <2 and p-value < 0.05
#the number of variables, however is large
#hence, the number of variables has to be reduced till all have significance level 0.001
#CompanyNamevolkswagen has highest p-value (0.018) and can be removed

model_31 <- lm(formula=price ~ mpghp + CompanyNamehonda + CompanyNamebmw + enginetypeohcf 
	      + CompanyNamenissan + CompanyNamebuick + CompanyNamerenault + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + CompanyNametoyota + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_31)
#R-squared:  0.8185,    Adjusted R-squared:  0.8002
#checking for multicollinearity using VIF
vif(model_31)

#now all variables have VIF <2 and p-value < 0.05
#the number of variables, however is large
#hence, the number of variables has to be reduced till all have significance level 0.001
#CompanyNamerenault has highest p-value (0.03) and can be removed

model_32<- lm(formula=price ~ mpghp + CompanyNamehonda + CompanyNamebmw + enginetypeohcf 
	      + CompanyNamenissan + CompanyNamebuick + CompanyNamemazda 
	      + CompanyNameplymouth + CompanyNamedodge + CompanyNametoyota + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_32)
#R-squared:  0.8117,    Adjusted R-squared:  0.7943
#checking for multicollinearity using VIF
vif(model_32)

#now all variables have VIF <2 and p-value < 0.05
#the number of variables, however is large
#hence, the number of variables has to be reduced till all have significance level 0.001
#CompanyNamenissan has highest p-value (0.012) and can be removed

model_33<- lm(formula=price ~ mpghp + CompanyNamehonda + CompanyNamebmw + enginetypeohcf 
	      + CompanyNamebuick + CompanyNamemazda  + CompanyNameplymouth + CompanyNamedodge + CompanyNametoyota + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_33)
#R-squared:  0.8024   Adjusted R-squared:  0.7858
#checking for multicollinearity using VIF
vif(model_33)

#now all variables have VIF <2 and p-value < 0.05
#the number of variables, however is large
#hence, the number of variables has to be reduced till all have significance level 0.001
#CompanyNamedodge has highest p-value (0.01) and can be removed

model_34<- lm(formula=price ~ mpghp + CompanyNamehonda + CompanyNamebmw + enginetypeohcf 
	      + CompanyNamebuick + CompanyNamemazda  + CompanyNameplymouth + CompanyNametoyota + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_34)
#R-squared:  0.7921   Adjusted R-squared:  0.7763
#checking for multicollinearity using VIF
vif(model_34)

#now all variables have VIF <2 and p-value < 0.05
#the number of variables, however is large
#hence, the number of variables has to be reduced till all have significance level 0.001
#CompanyNamehonda has highest p-value (0.011) and can be removed

model_35<- lm(formula=price ~ mpghp + CompanyNamebmw + enginetypeohcf + CompanyNamebuick
	      + CompanyNamemazda  + CompanyNameplymouth + CompanyNametoyota + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_35)
#R-squared:  0.7817    Adjusted R-squared:  0.767
#checking for multicollinearity using VIF
vif(model_35)

#now all variables have VIF <2 and p-value < 0.05
#the number of variables, however is large
#hence, the number of variables has to be reduced till all have significance level 0.001
#CompanyNamemazda has highest p-value (0.035) and can be removed

model_36<- lm(formula=price ~ mpghp + CompanyNamebmw + enginetypeohcf + CompanyNamebuick
	      + CompanyNameplymouth + CompanyNametoyota + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_36)
#Multiple R-squared:  0.7743,    Adjusted R-squared:  0.7608
#checking for multicollinearity using VIF
vif(model_36)

#now all variables have VIF <2 and p-value < 0.05
#but, the number of variables has to be reduced till all have significance level 0.001
#CompanyNameplymouth has highest p-value (0.044) and can be removed

model_37<- lm(formula=price ~ mpghp + CompanyNamebmw + enginetypeohcf + CompanyNamebuick
	      + CompanyNametoyota + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_37)
#Multiple R-squared:  0.7673,    Adjusted R-squared:  0.7553
#checking for multicollinearity using VIF
vif(model_37)

#now all variables have VIF <2 
#CompanyNametoyota has highest p-value (0.07) and can be removed

model_38<- lm(formula=price ~ mpghp + CompanyNamebmw + enginetypeohcf + CompanyNamebuick + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_38)
#Multiple R-squared:  0.7615,    Adjusted R-squared:  0.751
#checking for multicollinearity using VIF
vif(model_38)

#now all variables have VIF <2 
#enginetypeohcf has highest p-value (0.06) and can be removed

model_39<- lm(formula=price ~ mpghp + CompanyNamebmw + CompanyNamebuick + CompanyNamemitsubishi
	      + enginelocation, data = train)
summary(model_39)
#Multiple R-squared:  0.7554,    Adjusted R-squared:  0.7465
#checking for multicollinearity using VIF
vif(model_39)

#now all variables have VIF <2 and p-value < 0.05
#however, the number of variables has to be reduced till all have significance level 0.001
#CompanyNamemitsubishi has highest p-value (0.033) and can be removed

model_40<- lm(formula=price ~ mpghp + CompanyNamebmw + CompanyNamebuick 
	      + enginelocation, data = train)
summary(model_40)
#Multiple R-squared:  0.7471,    Adjusted R-squared:  0.7398
#checking for multicollinearity using VIF
vif(model_40)

#now all variables have VIF <2 and p-value < 0.05
#now, the number of variables all have significance level less than 0.001
#hence model_40 can be considered as the final model

#predicting the results of the selected final model in test dataset
Predict_1 <- predict(model_40,test[,-18])
test$predprice <- Predict_1

# Now, checking r square between actual and predicted price
r <- cor(test$price,test$predprice)
r
rsquared <- r^2
rsquared

#The major assumption is that the VIF of the model variables has to be less than 2 and p-value of each variable in the final model is less than 0.001

#the method for eliminating variables after each model, is as follows:
#1. If the variables have higher VIF(>2) and low signifance(p-value>0.05), the variable having the highest p-value is considered if the p-value is less than 0.05
#2. If the variables all have p-values less than 0.05, the variable having the highest VIF is considered (if VIF>2)
#3. If the variables all have VIF less than 2, the variable having the highest p-value is considered if the p-value is more than 0.05
#4. Still, if variables are remaining, they are eliminated in order of p-values until all have p-value<0.001
#5. The above steps effectively mean that, irrespective of the VIF of the values, the variable elimination in models is done based on the p-value
#(highest p-value value would be eliminated at each step, regardless of the VIF)

#The reason why there is sharp reduction in adjusted R-square in some models are that high VIF variables are present even though all of them have p-values less than 0.05

#The following are the values of different parameters associated with the model
#R-squared - 0.7471
#Adjusted R-squared - 0.7398
#R - 0.8139
#Predicted R-squared - 0.6624
#The error between adjusted and predicted R-squared values is 0.074 (7.4% difference which is less than 10%)
#Based on the final model, the four variables that most accurately predict the car model price are:
#1.mpghp (mileage per horsepower)
#2.CompanyNamebmw
#3.CompanyNamebuick
#4.enginelocation