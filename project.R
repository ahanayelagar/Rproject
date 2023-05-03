#clearing the environment - 
rm(list = ls())

#loading necessary libraries - 
library(tidyverse)
library(lubridate)
library(forcats)
library(RColorBrewer)

#reading in the csv file - 
fraudTest <- read_csv("~/Desktop/Class_Data/archive/fraudTest.csv")

#looking at the structure of the dataset - 
str(fraudTest)

#DATA CLEANING - 
#checking for any missing values in the entire dataframe - 
sum(is.na(fraudTest))
#there are no missing values
#:)

#1. convert tr_datetime from a character variable to a datetime variable - for this we will be 
#creating a new variable tr_datetime that will contain the datetime variable.
#fraudTest <- mutate(fraudTest, tr_datetime = dmy_hm(trans_date_trans_time))
#fraudTest

#2. convert dob from a character variable to a date variable- for this we will be 
#creating a new variable DOB that will contain the date variable.
#fraudTest <- mutate(fraudTest, DOB = dmy(dob))
#fraudTest
#3. 
frauds <- filter(fraudTest,is_fraud==1)
#Creates a dataset that is of cases of fraud
fraud_by_state <- frauds %>% group_by(state) %>%
  summarise(Sum=sum(amt), Freq=n(), Avg=Sum/Freq) %>% arrange(state)
#creates state data, including total amount, freq, and average fraud.
ggplot(data=fraud_by_state)+
  geom_col(aes(x=reorder(state,Sum), y=Sum, fill=Sum))+
  coord_flip()+
  labs(x="State",y="Amount of Fraud ($)")+
  scale_fill_gradientn(colors=c("Blue","Red"))+
  ggtitle("Amount of Fraud in US States")
#creates graph for sum by state
ggplot(data=fraud_by_state)+
  geom_col(aes(x=reorder(state,Freq), y=Freq, fill=Freq))+
  coord_flip()+
  labs(x="State",y="Frequency of Fraud")+
  scale_fill_gradientn(colors=c("Blue","Red"))+
  ggtitle("Frequency of Fraud in US States")
#Plots how frequently a state reports fraud
#NY has the most reported frauds
cc_by_state <- fraudTest %>% group_by(state) %>%
  summarise(Total=sum(amt)/1000, Count=n()) %>% arrange(state)
#groups all transactions by state
ggplot(data=cc_by_state)+
  geom_col(aes(x=reorder(state,Total), y=Total, fill=Total))+
  coord_flip()+
  labs(x="State",y="Amount of Transactions (Thousands of Dollars)")+
  scale_fill_gradientn(colors =c("Blue","Red"))+
  ggtitle("Amount of Credit Card Spending in US States")
#Texas spends the most money on credit cards, while NY and PA are second and third
#which makes sense as they lead in fraud
ggplot(data=cc_by_state)+
  geom_col(aes(x=reorder(state,Count), y=Count, fill=Count))+
  coord_flip()+
  labs(x="State",y="Fequency of Transactions")+
  scale_fill_gradientn(colors =c("Blue","Red"))+
  ggtitle("Use of Credit Cards in US States")
#Computes how often each state uses a credit card
#TX uses there credit card the most followed by NY, PA
fraud_by_exp <- frauds %>% group_by(category) %>%
  summarise(Sum=sum(amt)/1000, Freq=n(), Avg=Sum/Freq) %>% arrange(desc(Sum))#
#group the frauds by the expense
ggplot(data=fraud_by_exp)+
  geom_col(aes(x=reorder(category,Sum), y=Sum, fill=Sum))+
  coord_flip()+
  labs(x="Expense",y="Amount of Transactions (Thousands of Dollars)")+
  scale_fill_gradientn(colors =c("Blue","Red"))+
  ggtitle("Amount of Fraud for Different Expenses")
#plot showing how much fraud each expense occurs
ggplot(data=fraud_by_exp)+
  geom_col(aes(x=reorder(category,Freq), y=Freq, fill=Freq))+
  coord_flip()+
  labs(x="Expense",y="Frequency of Transactions")+
  scale_fill_gradientn(colors =c("Blue","Red"))+
  ggtitle("Frequency of Fraud for Expenses")
#plot showing how frequently each expense is frauded
cc_by_exp <- fraudTest %>% group_by(category) %>%
  summarise(Sum=sum(amt)/1000, Freq=n(), Avg=Sum/Freq) %>% arrange(desc(Sum))
ggplot(data=cc_by_exp)+
  geom_col(aes(x=reorder(category,Sum), y=Sum, fill=Sum))+
  coord_flip()+
  labs(x="Expense",y="Money Spent (Thousands of Dollars)")+
  scale_fill_gradientn("Money Spent",colors =c("Blue","Red"))+
  ggtitle("Amount of CC Spending by Expense")
#plots the amount of cc spending by expense
ggplot(data=cc_by_exp)+
  geom_col(aes(x=reorder(category,Sum), y=Sum, fill=Sum))+
  coord_flip()+
  labs(x="Expense",y="Frequency of Transactions")+
  scale_fill_gradientn("Times Used",colors =c("Blue","Red"))+
  ggtitle("Frequency of CC Purchases by Expense")
#plots how frequently cc is used to by something
by_exp <- full_join(cc_by_exp,fraud_by_exp,join_by(category))
#joins the 2 exp datasets together
by_exp <- by_exp %>% mutate(Percent=Sum.y/Sum.x/1000)
#creates the percent column
ggplot(data=by_exp)+
  geom_col(aes(x=reorder(category,Percent),y=Percent,fill=Percent))+
  coord_flip()+
  scale_fill_gradientn(colors =c("Blue","Red"))+
  labs(x="Expense",y="Percent")+
  ggtitle("Percent of Fraud by Expense")
#plots how much cc spending on each expense is fraud
install.packages(c("maps", "mapdata","ggmap"))
library(ggmap)
library(maps)
library(mapdata)
#allows for mapping
States <- map_data("state")
#map data for states
By_state <- full_join(cc_by_state,fraud_by_state,by=join_by(state))
#joins my two state datasets together
By_state <- By_state[-c(1,8,9),]
#gets rid of AK, HI, DC as we don't have maping info
By_state <- By_state %>% mutate(Percent= Sum/Total/1000*100,
                                Fraud_Percent=(Freq/Count)*100)
#creates the percent and fraud percentage column 
By_state <- By_state %>% mutate(region=tolower(state.name[match(state,state.abb)]))
#create region to make it possible to join
States2 <- full_join(States,By_state,by=join_by(region))
#joins the two datasets together
States2["Sum"][is.na(States2["Sum"])] <- 0
States2["Percent"][is.na(States2["Percent"])] <- 0
States2["Avg"][is.na(States2["Avg"])] <- 0
States2["Freq"][is.na(States2["Freq"])] <- 0
States2["Total"][is.na(States2["Total"])] <- 0
States2["Count"][is.na(States2["Count"])] <- 0
States2["Fraud_Percent"][is.na(States2["Fraud_Percent"])] <- 0
#puts in zeros for anydata we are missing
ggplot(data=States2,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Percent),color="Black")+
  scale_fill_gradientn("Percent of Fraud",colors =c("Yellow","Red"))+
  ggtitle("Percent of Fraud in the US")+
  labs(x="Longitude",y="Latitude")
#plots by state how much spending is Fraud
ggplot(data=States2,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Sum),color="black")+
  scale_fill_gradientn("Sum of Fraud",colors =c("Yellow","Red"))+
  ggtitle("Sum of Fraud in the US")+
  labs(x="Longitude",y="Latitude")
#plot to show how much fraud is in each state
ggplot(data=States2,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Total),color="black")+
  scale_fill_gradientn("CC Spending
  (1000's of $)",colors =c("Yellow","Red"))+
  ggtitle("Sum of CC Spending in the US")+
  labs(x="Longitude",y="Latitude")
#shows how much spending there is in each state
ggplot(data=States2,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Count),color="black")+
  scale_fill_gradientn("Frequency of 
  CC Use",colors =c("Yellow","Red"))+
  ggtitle("Frequency of CC Use in the US")+
  labs(x="Longitude",y="Latitude")
#plots how frequently each state uses a cc
ggplot(data=States2,aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Fraud_Percent),color="black")+
  scale_fill_gradientn("Transaction
  Percentage",colors =c("Yellow","Red"))+
  ggtitle("Percent of Transaction are Fraud by State")+
  labs(x="Longitude",y="Latitude")
#plots for each state what percent of transactions are fraud
usa <- map_data("usa")
#download map data for USA
gg1<- ggplot()+
  geom_polygon(data = usa, aes(x=long,y=lat, group=group),fill="white",color="black")+
  coord_fixed(1.3)
#creates function to plot USA data
gg1
lower_48_fraud <- frauds %>% filter(long>=-130)
lower_48_cc <- fraudTest %>% filter(long>=-130)
#gets rid of data outside the lower 48
gg1+
  geom_point(data=lower_48_fraud, aes(x=long,y=lat), color="black", size=.5)+
  labs(x="Longitude",y="Latitude")+
  ggtitle("CC Fraud Location in US")
#plots where all fraud occur in US
gg1+
  geom_point(data=lower_48_cc, aes(x=long,y=lat), color="black", size=.1)+
  labs(x="Longitude",y="Latitude")+
  ggtitle("CC Transaction Location in US")
#plots where all transaction occur in US
  