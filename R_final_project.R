

## AHANA'S CODE - 

#clearing the environment - 
rm(list = ls())

#loading necessary libraries - 
library(tidyverse)
library(lubridate)
library(forcats)

#reading in the csv file - 
fraudTest <- read.csv("class_data/fraudTest.csv")

#looking at the structure of the dataset - 
str(fraudTest)

#DATA CLEANING - 
#checking for any missing values in the entire dataframe - 
sum(is.na(fraudTest))
#there are no missing values
#:)

#1. converting trans_date_trans_time from a character variable to a datetime variable - for this we will be 
#creating a new variable tr_datetime that will contain the datetime variable.
fraudTest <- mutate(fraudTest,
                    tr_datetime = dmy_hm(trans_date_trans_time))
fraudTest

#2. converting dob from a character variable to a date variable- for this we will be 
#creating a new variable DOB that will contain the date variable.
fraudTest <- mutate(fraudTest,
                    DOB = dmy(dob))
fraudTest

################################################################################
################################################################################
################################################################################
#trying to do age vs amt graph - 
# Get current date
current_date <- as.Date(Sys.time())

#Calculate age based on birthdate - 
#convert DOB column to yyyy-mm-dd format-
fraudTest$DOB <- as.Date(fraudTest$DOB, format = "%Y-%m-%d")  

#creating a new variable called age that stores the ages of everyone- 
fraudTest$age <- as.numeric(format(current_date, "%Y")) - as.numeric(format(fraudTest$DOB, "%Y"))  

#print the resulting data frame
print(fraudTest)

#Finding the minimum and the maximum age to see the range in the ages of the people:
min <- min(fraudTest$age)
min

max <- max(fraudTest$age)
max

#the min and max ages are 21 and 99 respectively. so, the breaks were made afraudTestording 
#to that.
#categorizing the ages: 
fraudTest <- fraudTest %>% mutate(age_category = cut(age, breaks = c(20, 40, 60, 80, 100),
                                                     labels = c("20-40", "40-60",
                                                                "60-80", "80-100")))

# Print the resulting dataframe
print(fraudTest)


#graphing age vs amt graph - 
fraudTest <- filter(fraudTest,is_fraud==1)

fraud_by_age <- fraudTest %>% group_by(age_category) %>%
  summarise(Sum=sum(amt)/1000)

ggplot(data=fraud_by_age)+
  geom_point(aes(x= age_category, y=Sum, fill=Sum))+
  labs(x="age",y="Amount ($) in intervals of 1000") 
#you might have to change the fill = sum here bc it doesn't really help.

#from the graph, we can conclude that the people who have fallen into a 
#fraud are from the age group of 40-60 and they have lost approx. 

################################################################################
################################################################################
################################################################################
#***********************TRENDS IN FRAUDULENT TRANSACTIONS*********************

#cyclicity - how do fraudulent transactions distribute on the temporal spectrum?
#is there an hourly, monthly, or seasonal trend?


#HOURLY TREND:
fraudTest$hour <- factor(format(fraudTest$tr_datetime, "%H"), levels = sprintf("%02d", 0:23))

fraud_fraudTest <- fraudTest %>%
  filter(is_fraud == 1)

ggplot(data = fraud_fraudTest, aes(x = hour, fill = factor(is_fraud))) +
  geom_bar(position = "dodge", show.legend = FALSE) +
  scale_fill_manual(values = c("#E69F00"), name = "Type", labels = c("Fraud")) +
  labs(x = "Time (Hour) in a Day", y = "Count") +
  theme_minimal() +
  scale_x_discrete(limits = sprintf("%02d", seq(0, 23, 1)), labels = sprintf("%02d", seq(0, 23, 1)))

#The results from the graph are interesting! Most fraud cases happen from 10:00 p.m. 
#to 11 p.m. 

#WEEKLY TREND:

fraudTest$day <- wday(fraudTest$tr_datetime)

fraudTest$day <- factor(fraudTest$day, levels = 1:7, labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

fraud_fraudTest <- fraudTest %>% 
  filter(is_fraud == 1)

ggplot(data = fraud_fraudTest, aes(x = day)) +
  geom_bar(fill = "#E69F00", show.legend = FALSE) +
  labs(x = "Day of Week", y = "Count", title = "Day of Week vs Fraud") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Majority of the fraudulent transactions tend to happen on Mondays.

#MONTHLY TRANSACTIONS:
fraudTest$month <- month(fraudTest$tr_datetime)

fraud_fraudTest <- fraudTest %>%
  filter(is_fraud == 1)

ggplot(fraud_fraudTest, aes(x = factor(month))) +
  geom_bar(fill = "#E69F00", show.legend = FALSE) +
  labs(x = "Month", y = "Count", title = "Month vs Fraud") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#From the graph, we can tell that fraudulent transactions peak around March - May.

## LYNNETTE'S CODE 
##Analyzing how gender affects the amount of frauds
#Summarise and group by gender

fraud <- filter(fraud,is_fraud==1)

fraud_by_gender <- fraud %>% group_by(gender) %>%
  summarise(Sum=sum(amt/1000))

str(fraud_by_gender)

#column plot of gender and Sum of amt
ggplot(data = fraud_by_gender) + 
  geom_col(aes(x = gender, y = Sum, fill = Sum))

#boxplot of fraud and Sum of amt
ggplot(data = fraud) +
  geom_boxplot(aes(x = gender, y = amt)) +
  labs(x = "Gender", y = "Amount", title = "Gender and Amount")
summary(fraud$amt)

#Is there any pattern in the transaction time (hour, day of the week, month) and the transaction amount or the gender of the user?
##What are the most common product categories that are associated with fraudulent transactions?
#  Is there a correlation between the transaction amount and the number of transactions made by a user?
#  Are there any unusual patterns in the transaction amounts or the number of transactions made by a particular user?
#  How does the distribution of transaction amounts differ between genders, card types, or product categories?
#  Are there any changes in the distribution of transaction  amounts over time?

#change the date time variable to a date variable
fraud <- mutate(fraud, date = as_date(tr_datetime))
str(fraud)

fraud <- mutate(fraud, avg_amt = mean(amt, na.rm = TRUE) )
fraud$avg_amt
str(avg_amt)

#plotting amount against the date to see the correlation between the date and the amount
ggplot(data = fraud) +
  geom_line(aes(x = date, y = amt)) + 
  facet_wrap(amt) + 
  labs(x = "date", y = "Amount") 

#Which merchant has the highest amount of frauds
fraud %>% 
  group_by(merchant) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
# fraud_Lemke-Gutman has the highest amount of frauds

#A visualization of the top 10 merchants with the highest amount of frauds
fraud %>% 
  group_by(merchant) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(data = fraud)

## JJ'S CODE: 
###########################################
# Frauds vs amt sum
###########################################
frauds_20 = frauds %>% 
  group_by(job) %>% 
  summarize(amt_sum = sum(amt, na.rm = T)) %>%
  arrange(desc(amt_sum)) %>% 
  head(20)

frauds_20 = frauds_20 %>% 
  mutate(job2 = reorder(job, amt_sum, na.rm = TRUE, decreasing = FALSE))

ggplot(data = frauds_20)+
  geom_col(aes(x = job2, y = amt_sum))+
  coord_flip()+
  labs(x = "Top 20 Professions", y = "Fraudulent Amount Spent")

###########################################
# 
###########################################

## MAXWELL'S CODE

#creates the percent columns
ggplot(data=cc_by_exp)+
  geom_col(aes(x=reorder(category,Total), y=Total, fill=Total))+
  coord_flip()+
  labs(x="Expense",y="Money Spent (Thousands of Dollars)")+
  scale_fill_gradientn("Money Spent",colors =c("yellow","Red"))+
  ggtitle("Amount of CC Spending by Expense")
#plots the amount of cc spending by expense
#grocary leads this too
#shopping_pos is in close second
ggplot(data=fraud_by_exp)+
  geom_col(aes(x=reorder(category,Sum), y=Sum, fill=Sum))+
  coord_flip()+
  labs(x="Expense",y="Amount of Money (Thousands of Dollars)")+
  scale_fill_gradientn(colors =c("yellow","Red"))+
  ggtitle("Amount of Fraudulent Spending for Different Expenses")
#plot showing how much fraud each expense occurs
#Shopping_net leads them all

ggplot(data=by_exp)+
  geom_col(aes(x=reorder(category,Percent),y=Percent,fill=Percent))+
  coord_flip()+
  scale_fill_gradientn(colors =c("yellow","Red"))+
  labs(x="Expense",y="Percent")+
  ggtitle("Percent of Fraudulent Spending by Expense")
#plots how much cc spending on each expense is fraud
#shopping net is the leader of the group

ggplot(data=cc_by_exp)+
  geom_col(aes(x=reorder(category,Count), y=Count, fill=Count))+
  coord_flip()+
  labs(x="Expense",y="Frequency of Transactions")+
  scale_fill_gradientn("Times Used",colors =c("yellow","Red"))+
  ggtitle("Frequency of CC Purchases by Expense")
#plots how frequently cc is used to by something
#gas_transport leads this
#goscery_pos is in close second
ggplot(data=fraud_by_exp)+
  geom_col(aes(x=reorder(category,Freq), y=Freq, fill=Freq))+
  coord_flip()+
  labs(x="Expense",y="Frequency of Transactions")+
  scale_fill_gradientn(colors =c("yellow","Red"))+
  ggtitle("Frequency of Fraud for Expenses")
#plot showing how frequently each expense is frauded
#shopping_net leads this too, but grocery_pos is close
ggplot(data=by_exp)+
  geom_col(aes(x=reorder(category,Fraud_Percent),y=Fraud_Percent,fill=Fraud_Percent))+
  coord_flip()+
  scale_fill_gradientn("Fraud Percentage",colors =c("yellow","Red"))+
  labs(x="Expense",y="Percent")+
  ggtitle("Percent of Fraudulent Transactions by Expense")
#plots what percent of transaction are fraud
#one percent of 



ggplot(data=cc_by_state)+
  geom_col(aes(x=reorder(state,Total), y=Total, fill=Total))+
  coord_flip()+
  labs(x="State",y="Amount of Money (Thousands of Dollars)")+
  scale_fill_gradientn(colors =c("yellow","Red"))+
  ggtitle("Amount of Credit Card Spending in US States")
#Texas spends the most money on credit cards, while NY and PA are second and third
#which makes sense as they lead in fraud
ggplot(data=States2,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Total),color="black")+
  scale_fill_gradientn("CC Spending
  (1000's of $)",colors =c("Yellow","Red"))+
  ggtitle("Sum of CC Spending in the US")+
  labs(x="Longitude",y="Latitude")
#shows how much spending there is in each state
#NY, TX, PA is very high on the list
ggplot(data=fraud_by_state)+
  geom_col(aes(x=reorder(state,Sum), y=Sum, fill=Sum))+
  coord_flip()+
  labs(x="State",y="Amount of Fraud ($)")+
  scale_fill_gradientn("Sum of Fraud",colors=c("yellow","Red"))+
  ggtitle("Amount of Fraud in US States")
#creates graph for sum by state
#New York leads in that
ggplot(data=States2,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Sum),color="black")+
  scale_fill_gradientn("Sum of Fraud",colors =c("Yellow","Red"))+
  ggtitle("Sum of Fraud in the US")+
  labs(x="Longitude",y="Latitude")
#plot to show how much fraud is in each state
#New York leads in fradulent spending
ggplot(data=States2,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Percent),color="Black")+
  scale_fill_gradientn("Percent of Fraud",colors =c("Yellow","Red"))+
  ggtitle("Percent of Fraudulent Spending in the US")+
  labs(x="Longitude",y="Latitude")
#plots by state how much spending is Fraud
#Conneticut is leading the way
ggplot(data=cc_by_state)+
  geom_col(aes(x=reorder(state,Count), y=Count, fill=Count))+
  coord_flip()+
  labs(x="State",y="Fequency of Transactions")+
  scale_fill_gradientn(colors =c("yellow","Red"))+
  ggtitle("Use of Credit Cards in US States")
#Computes how often each state uses a credit card
#TX uses there credit card the most followed by NY, PA
ggplot(data=States2,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Count),color="black")+
  scale_fill_gradientn("Frequency of 
  CC Use",colors =c("Yellow","Red"))+
  ggtitle("Frequency of CC Use in the US")+
  labs(x="Longitude",y="Latitude")
#plots how frequently each state uses a cc
ggplot(data=fraud_by_state)+
  geom_col(aes(x=reorder(state,Freq), y=Freq, fill=Freq))+
  coord_flip()+
  labs(x="State",y="Frequency of Fraud")+
  scale_fill_gradientn("Frequency",colors=c("yellow","Red"))+
  ggtitle("Frequency of Fraud in US States")
#Plots how frequently a state reports fraud
#NY has the most reported frauds
ggplot(data=States2,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Freq),color="black")+
  scale_fill_gradientn("Frequency",colors =c("Yellow","Red"))+
  ggtitle("Amount of Fraudulent Transactions by State")+
  labs(x="Longitude",y="Latitude")
#plots for each state transactions are fraud
ggplot(data=States2,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Fraud_Percent),color="black")+
  scale_fill_gradientn("Transaction
  Percentage",colors =c("Yellow","Red"))+
  ggtitle("Percent of Fraudulent Transactions by State")+
  labs(x="Longitude",y="Latitude")
#plots for each state what percent of transactions are fraud

gg1+
  geom_point(data=lower_48_cc, aes(x=long,y=lat),size=.1)+
  labs(x="Longitude",y="Latitude")+
  ggtitle("CC Transaction Location in US")
#plots where all transaction occur in US