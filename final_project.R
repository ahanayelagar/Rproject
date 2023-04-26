rm(list = ls())
library(tidyverse)
library(lubridate)
library(forcats)

fraudTest <- read.csv("class_data/fraudTest.csv")

frauds <- filter(fraudTest,is_fraud==1)

not_frauds <- filter(fraudTest,is_fraud==0)

str(frauds)
str(not_frauds)

# amt vs job

# sum amt for each job, do for top 20 jobs

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

