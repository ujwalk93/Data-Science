# copying the files to directory and setting it as the  working directory 
#setwd("C:/Users/sithota/Documents/Investment_Case_study")

# verfiying the working directory
#getwd() 

#Load the Data
companies <- read.table("companies.txt", sep="\t", header=TRUE, comment.char = "", quote="")
rounds2 <- read.csv("rounds2.csv", header=TRUE, quote="")

#Cleanning the Data to merge the data frames
companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

library(dplyr)
library(stringr)

#Table - 1.1
#1. How many unique companies are present in rounds2?
nrow(distinct(companies,permalink))

#2. How many unique companies are present in the companies file?
nrow(distinct(rounds2,company_permalink))

#3. In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
# permalink (as count of distinct names is 66103 where as distinct permalinks is 66368

#4. Are there any companies in the rounds2 file which are not present in companies ? 
which(!(unique(rounds2$company_permalink) %in% unique(companies$permalink)))
# 3 permalinks with special characters even though exists in both the tables but are recognized separately

#5. Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
#   Name the merged frame master_frame.How many observations are present in master_frame ?
master_frame <- merge(rounds2,companies,by.x = "company_permalink",by.y = "permalink", all.x = TRUE )
which(is.na(master_frame$name))
#manually add for 3 permalinks with special characters


#Table - 2.1
# 1to 4
#finding avg of total investment based on funding types
fund_type_group <- group_by(master_frame, funding_round_type)
fund_type_avg <- summarise(fund_type_group, mean(raised_amount_usd, na.rm = T))
colnames(fund_type_avg)[2] <- 'avd_raised_amt_usd'
fund_type_avg <- arrange(fund_type_avg, desc(avd_raised_amt_usd))
fund_type_avg

#5. Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
#   which investment type is the most suitable for them?
filter(fund_type_avg, avd_raised_amt_usd > 5000000, avd_raised_amt_usd < 15000000)


#Table - 3.1
#filter top countries based on total investment
top9 <- filter(master_frame, funding_round_type == 'venture')
country_group <- group_by(top9, country_code)
top9 <- summarise(country_group, sum(raised_amount_usd, na.rm = T))
colnames(top9)[2] <- 'sum_raised_amt_usd'
top9 <- arrange(top9, desc(sum_raised_amt_usd))
top9 <- top9[-which(top9$country_code == ''),]
top9 <- top9[1:9,]
top9
# Exclude CHN as china is not English speaking country as per the link given

#Checkpoint 4
#Get Primary sector
master_frame$primary_sector <- str_replace_all(master_frame$category_list,'\\|.*$','')

#cleanning data
master_frame$primary_sector <- tolower(master_frame$primary_sector)
master_frame$primary_sector[which(is.na(master_frame$primary_sector))] <- ''

# Read data from mapping.csv
mapping <- read.csv("mapping.csv" , header=TRUE)

# clean data
mapping$category_list <- str_replace_all(mapping$category_list,'0','na')
mapping$category_list <- tolower(mapping$category_list)
library(tidyr)
mapping <- gather(mapping, main_sector, val, Automotive...Sports:Social..Finance..Analytics..Advertising)
mapping <- mapping[-which(mapping$val == 0),]
mapping <- mapping[,-3]

#Merge mapping and master frame
master_frame <- merge(master_frame,mapping,by.x = "primary_sector",by.y = "category_list", all.x = TRUE )
#display the master frame
str(master_frame)

#Checkpoint 5
#filtering data into dataframes d1,d2 and d3
D1 <- filter(master_frame, funding_round_type == 'venture', country_code == 'USA', raised_amount_usd > 5000000, raised_amount_usd < 15000000)
D2 <- filter(master_frame, funding_round_type == 'venture', country_code == 'GBR', raised_amount_usd > 5000000, raised_amount_usd < 15000000)
D3 <- filter(master_frame, funding_round_type == 'venture', country_code == 'IND', raised_amount_usd > 5000000, raised_amount_usd < 15000000)

#Total amount of investments in each main sector
D1_sector_group <- group_by(D1, main_sector)
D1_invest_sum <- summarise(D1_sector_group, sum(raised_amount_usd, na.rm = T))
D2_sector_group <- group_by(D2, main_sector)
D2_invest_sum <- summarise(D2_sector_group, sum(raised_amount_usd, na.rm = T))
D3_sector_group <- group_by(D3, main_sector)
D3_invest_sum <- summarise(D3_sector_group, sum(raised_amount_usd, na.rm = T))
D1_invest_sum
D2_invest_sum
D3_invest_sum

#Total count of investments in each main sector
D1_invest_count <- data.frame(table(D1$main_sector))
D2_invest_count <- data.frame(table(D2$main_sector))
D3_invest_count <- data.frame(table(D3$main_sector))
D1_invest_count
D2_invest_count
D3_invest_count

# Table 5.1
# 1. Total number of investments 
nrow(D1)
nrow(D2)
nrow(D3)

#2. Total amount of investment (USD)
sum(D1$raised_amount_usd)
sum(D2$raised_amount_usd)
sum(D3$raised_amount_usd)

# 3 to 8
#sorting in descending order
D1_invest_count <- arrange(D1_invest_count, desc(Freq))
D2_invest_count <- arrange(D2_invest_count, desc(Freq))
D3_invest_count <- arrange(D3_invest_count, desc(Freq))
D1_invest_count
D2_invest_count
D3_invest_count

# 9. For point 3 (top sector count-wise), which company received the highest investment?
D1_by_top_sector <- filter(D1, main_sector == D1_invest_count$Var1[1])
D1_by_top_sector$name[which(D1_by_top_sector$raised_amount_usd == max(D1_by_top_sector$raised_amount_usd))]

D2_by_top_sector <- filter(D2, main_sector == D2_invest_count$Var1[1])
D2_by_top_sector$name[which(D2_by_top_sector$raised_amount_usd == max(D2_by_top_sector$raised_amount_usd))]

D3_by_top_sector <- filter(D3, main_sector == D3_invest_count$Var1[1])
D3_by_top_sector$name[which(D3_by_top_sector$raised_amount_usd == max(D3_by_top_sector$raised_amount_usd))]

#10. For point 4 (second best sector count-wise), which company received the highest investment?
D1_by_2nd_sector <- filter(D1, main_sector == D1_invest_count$Var1[2])
D1_by_2nd_sector$name[which(D1_by_2nd_sector$raised_amount_usd == max(D1_by_2nd_sector$raised_amount_usd))]

D2_by_2nd_sector <- filter(D2, main_sector == D2_invest_count$Var1[2])
D2_by_2nd_sector$name[which(D2_by_2nd_sector$raised_amount_usd == max(D2_by_2nd_sector$raised_amount_usd))]

D3_by_2nd_sector <- filter(D3, main_sector == D3_invest_count$Var1[2])
D3_by_2nd_sector$name[which(D3_by_2nd_sector$raised_amount_usd == max(D3_by_2nd_sector$raised_amount_usd))]
