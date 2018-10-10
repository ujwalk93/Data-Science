setwd("D:\\PG_Diploma\\AssignmentUber")
getwd()
#loading and importing the csv file
uber <- read.csv("Uber Request Data.csv",stringsAsFactors=F)
#description about the data
summary(uber)
str(uber)

#loading all the required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

#checking for NA values
sum(is.na(uber$Pickup.point))
sum(is.na(uber$Status)) 
sum(is.na(uber$Request.timestamp))
sum(is.na(uber$Drop.timestamp))
sum(is.na(uber$Driver.id))
#converting all NA values to 0 for ease of analysis
uber$Driver.id[is.na(uber$Driver.id)]<-0
uber$Drop.timestamp[is.na(uber$Drop.timestamp)]<-0

#checking for duplicate Request.id values
dup <- data.frame(table(uber$Request.id))
dup[dup$Freq > 1,]

#converting data to factor type to ensure consistency in analysis
uber$Status <- as.factor(uber$Status)
uber$Pickup.point <- as.factor(uber$Pickup.point)

#converting time to standard format after data cleaning
uber$Request.timestamp <-strptime(uber$Request.timestamp,format = "%d-%m-%Y %H:%M")
uber$Drop.timestamp <-strptime(uber$Drop.timestamp,format = "%d-%m-%Y %H:%M")

#extracting and deriving requestHour metric for analysis
#converting requestHour to numeric value for ease of analysis
uber$requestHour <- format(uber$Request.timestamp, "%H")
uber$requestHour <- as.numeric(uber$requestHour)

#grouping requestHour into required timeslots for analysis using inbuilt ifelse() function (nested)
#12:00 am - 4:59 am (Early Morning)
#5:00 am - 9:59 am (Peak Morning)
#10:00 am - 4:59 pm (Normal Day Time)
#5:00 pm - 8:59 pm (Peak Evening)
#9:00 pm - 11:59 pm (Late Night)
uber$timeslots <- ifelse(uber$requestHour<5, "Early Morning",ifelse(uber$requestHour<10, "Peak Morning",ifelse(uber$requestHour<17, "Normal DayTime",ifelse(uber$requestHour<21,"Peak Evening",ifelse(uber$requestHour<24,"Late Night")))))
uber$timeslots <- as.factor(uber$timeslots)
#counting number of trips in each timeslots
summary(uber$timeslots)

#1
#Identifying the timeslots and pickup points where most cancellations and non availability of cars occur
#bar plots with position="dodge" are used in the entire problem to demarcate clearly between various points and types of data
#facet_wrap() function is used to analyse a variable based on a particular parameter(univariate analysis)
#plotting bar plots using ggplot() function for analysis to compare timeslots with respect to Status and Pickup point in each case
#analysing data for each of the timeslots for problem identification
#determining the appropriate results using bar plots  (No. of booking requests vs pickup point and No. of booking requests vs time slots (segmented univariate analysis))
#univariate and segmented univariate analysis based on timeslots, pickup point and status variables 

ggplot(uber,aes(x=timeslots,fill=Status))+geom_bar(position="dodge")+labs(x="Time slots",y="No. of booking requests")
ggplot(uber,aes(x=timeslots,fill=Status))+geom_bar(position="dodge")+labs(x="Time slots",y="No. of booking requests")+facet_wrap(~Pickup.point,nrow=2)

#2 pickup points, Airport and City
#analysing frequency of each case

table(uber$timeslots, uber$Status)
table(uber$timeslots, uber$Pickup.point)

#From the above data, it is clear that:
#1. Maximum number of cancellations happens in the peak morning time slot (5:00 am – 9:59 am).
#2. Maximum number of car non-availability happens in the peak evening timeslot (5:00 pm – 8:59 pm).

#analysing types of request for which maximum cancellations occur
#analysing based on timeslots and pickup point variable for airport and city trips which are cancelled

airporttrip <- subset(uber,uber$Pickup.point=="Airport" & uber$Status=="Cancelled")
ggplot(airporttrip,aes(x=timeslots,fill=Status))+geom_bar()+labs(x="Time slots",y="No. of cancelled requests")
citytrip <- subset(uber,uber$Pickup.point=="City" & uber$Status=="Cancelled")
ggplot(citytrip,aes(x=timeslots,fill=Status))+geom_bar()+labs(x="Time slots",y="No. of cancelled requests")

#analysing to find out when and where most non-availabilty of cars occurs
#determining the appropriate results using bar plots (No. of booking requests vs time slots)
#analysing based on timeslots variable for airport and city trips where cars are not available

airportnocars <-  subset(uber,uber$Pickup.point=="Airport"  & uber$Status=="No Cars Available")
ggplot(airportnocars,aes(x=timeslots,fill=Status))+geom_bar()+labs(x="Time slots",y="No. of 'No cars available'")
citynocars <-  subset(uber,uber$Pickup.point=="City"  & uber$Status=="No Cars Available")
ggplot(citynocars,aes(x=timeslots,fill=Status))+geom_bar()+labs(x="Time slots",y="No. of 'No cars available'")

#Observations
#1.Maximum cancellations of cars occur in the peak morning timeslot (5:00 am - 9:59 am) from the city as shown in the bar plots
#analysing frequency of each cases

peak <- subset(uber,uber$timeslots=="Peak Morning" & uber$Status=="Cancelled")
peakall <- subset(uber,uber$timeslots=="Peak Morning")
peakper <- (nrow(peak)/nrow(peakall))*100
peakper

#40.09% cancellation of cars occurs in the peak morning timeslot (5:00 am - 9:59 am) from the city

#2.Maximum non-availabilty of cars occurs in the peak evening timeslot (5:00 pm - 8:59 pm) from the airport as shown in the bar plots
#analysing frequency of each cases

evening <- subset(uber,uber$timeslots=="Peak Evening" & uber$Status=="No Cars Available")
eveningall <- subset(uber,uber$timeslots=="Peak Evening")
eveningper <- (nrow(evening)/nrow(eveningall))*100
eveningper

#59.54% non-availability of cars occurs in the peak evening timeslot (5:00 pm - 8:59 pm) from the airport

#2
#demand supply gap at airport and city (total number of trips (Demand) vs trips successfully completed (Supply))
#determining the appropriate results using bar plots (No. of booking requests vs time slots)
#univariate and segmented univariate analysis based on pickup point (city or airport)

#analysing the demand supply gap
ggplot(uber,aes(x=timeslots,fill=Status))+geom_bar()+labs(x="Time slots",y="No. of booking requests")

#analysing the demand supply gap based on pickup point for particular timeslots by doing univariate analysis
ggplot(uber,aes(x=timeslots,fill=Status))+geom_bar()+labs(x="Time slots",y="No. of booking requests")+facet_wrap(~Pickup.point,nrow=2)


#Observations
#1.Based on the graphs, we can conclude that the demand-supply gap of cars is more severe in the peak morning and peak evening timeslots
#This is due to the fact that lot of people return from the airport in the peak evening timeslot (5:00 pm - 8:59 pm) and lot of people leave the city for the airport in the peak morning timeslot (5:00 am - 9:59 am)
#performing analysis in the time slots where the gap is severe

eveningpeak <- subset(uber,uber$timeslots=="Peak Evening")
airportpeak <- subset(eveningpeak,eveningpeak$Pickup.point=="Airport")
airportcar <- subset(airportpeak,airportpeak$Status=="Trip Completed")
airportcomplete <- (nrow(airportcar)/nrow(airportpeak))*100
airportcomplete

#2.The demand-supply gap from the airport to the city is much severe in the peak evening timeslot (5:00 pm - 8:59 pm) as only 21.41% of the total trips in that particular timeslot were successfully completed

morningpeak <- subset(uber,uber$timeslots=="Peak Morning")
citypeak <- subset(morningpeak,morningpeak$Pickup.point=="City")
citycar <- subset(citypeak,citypeak$Status=="Trip Completed")
citycomplete <- (nrow(citycar)/nrow(citypeak))*100
citycomplete

#3.The demand-supply gap from the city to the airport is much severe in the peak morning timeslot (5:00 am - 9:59 am) timeslot as only 28.15% of the total trips in that particular timeslot were successfully completed
