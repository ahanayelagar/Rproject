rm(list = ls())
library(tidyverse)
library(lubridate)
library(forcats)

fraudTest <- read.csv("class_data/fraudTest.csv")

frauds <- filter(fraudTest,is_fraud==1)

not_frauds <- filter(fraudTest,is_fraud==0)

str(frauds)
# amt vs job

frauds_job_grouped = frauds %>% group_by(job) %>%
  summarise(count = n())

fraud_top_20 <- frauds_job_grouped %>% 
  arrange(desc(count)) %>% #sort in descending order by variable count
  head(20) #head takes the first n (here, 20) rows

fraud_top_20 = fraud_top_20 %>% 
  mutate(job2 = reorder(job, count, na.rm = TRUE, decreasing = FALSE))

ggplot(data = fraud_top_20)+
  geom_col(aes(x = job2, y = count))+
  coord_flip()+
  labs(x = "Profession", y = "Number of Fraudulent Cases")


ggplot(data = fraud_top_20)+
  geom_col(aes(x = job, y = amt))+
  coord_flip()
  
######################################

fraud_top_20_amt = frauds %>% 
  group_by(job) %>% 
  summarize(avg_amt = mean(amt, na.rm = T),
            avg_fraud = mean(is_fraud))
  arrange(desc(amt)) %>% 
  head(20)
  
  


