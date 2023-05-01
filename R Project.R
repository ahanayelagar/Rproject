#clearing the environment - 
rm(list = ls())

#loading necessary libraries - 
library(tidyverse)
library(lubridate)
library(forcats)

#reading in the csv file - 
cc <- read.csv("class_data/fraudTest.csv")

#looking at the structure of the dataset - 
str(cc)

#DATA CLEANING - 
#checking for any missing values in the entire dataframe - 
sum(is.na(cc))
#there are no missing values
#:)

#1. converting trans_date_trans_time from a character variable to a datetime variable - for this we will be 
#creating a new variable tr_datetime that will contain the datetime variable.
cc <- mutate(cc,
             tr_datetime = dmy_hm(trans_date_trans_time))
cc

#2. converting dob from a character variable to a date variable- for this we will be 
#creating a new variable DOB that will contain the date variable.
cc <- mutate(cc,
             DOB = dmy(dob))
cc

#graphing state and amount that was transacted during frauds - 
cc <- filter(cc,is_fraud==1)
fraud_by_state <- cc %>% group_by(state) %>%
  summarise(Sum=sum(amt)) %>% arrange(desc(Sum))

ggplot(data=fraud_by_state)+
  geom_col(aes(x=reorder(state,Sum), y=Sum, fill=Sum))+
  coord_flip()+
  labs(x="State",y="Amount of Fraud ($)")+
  scale_fill_gradientn(colors=c("Blue","Red")) 

################################################################################
################################################################################
################################################################################
#trying to do age vs amt graph - 
# Get current date
current_date <- as.Date(Sys.time())

#Calculate age based on birthdate - 
#convert DOB column to yyyy-mm-dd format-
cc$DOB <- as.Date(cc$DOB, format = "%Y-%m-%d")  

#creating a new variable called age that stores the ages of everyone- 
cc$age <- as.numeric(format(current_date, "%Y")) - as.numeric(format(cc$DOB, "%Y"))  

#print the resulting data frame
print(cc)

#Finding the minimum and the maximum age to see the range in the ages of the people:
min <- min(cc$age)
min

max <- max(cc$age)
max

#the min and max ages are 21 and 99 respectively. so, the breaks were made according 
#to that.
#categorizing the ages: 
cc <- cc %>% mutate(age_category = cut(age, breaks = c(20, 40, 60, 80, 100),
                                       labels = c("20-40", "40-60",
                                                  "60-80", "80-100")))

# Print the resulting dataframe
print(cc)


#graphing age vs amt graph - 
cc <- filter(cc,is_fraud==1)

fraud_by_age <- cc %>% group_by(age_category) %>%
  summarise(Sum=sum(amt)/1000)

ggplot(data=fraud_by_age)+
  geom_point(aes(x= age_category, y=Sum, fill=Sum))+
  labs(x="age",y="Amount ($) in intervals of 1000") 

#from the graph, we can conclude that the people who have fallen into a 
#fraud are from the age group of 40-60 and they have lost approx. 

