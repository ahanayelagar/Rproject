
rm(list = ls())

library (tidyverse)

fraud <- read.csv("Class Data/fraudTest.csv")
str(fraud)

sum(is.na(fraud))

#1. convert tr_datetime from a character variable to a datetime variable - for this we will be

#creating a new variable tr_datetime that will contain the datetime variable.

#fraud <- mutate(fraud, tr_datetime = dmy_hm(trans_date_trans_time))
fraud <-  mutate(fraud, tr_datetime = as.POSIXct(trans_date_trans_time, format = "%Y-%m-%d  %H:%M:%S"))
str(fraud)

#2. convert dob from a character variable to a date variable- for this we will be

#creating a new variable DOB that will contain the date variable.

fraud <- mutate(fraud, DOB = ymd(dob))

str(fraud)

##fraud <- filter(fraudTest,is_fraud==1)
##fraud_by_state <- fraud %>% group_by(state) %>%
#  summarise(Sum=sum(amt)) %>% arrange(desc(Sum))
#str(fraud_by_state)

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


