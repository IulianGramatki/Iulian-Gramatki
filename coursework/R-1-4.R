rm(list=ls())
setwd("E:/Tech/Rcourse-1-4")
data<-read_csv("daily_SPEC_2014.csv.bz2")

library(dplyr)
library(tidyr)

#Change spaces in variable names to dots
names(data) <- gsub(" ",".",names(data))

#Question 1
data %>%
  filter(State.Name == "Wisconsin", Parameter.Name == "Bromine PM2.5 LC") %>%
  summarise(mean(Arithmetic.Mean))
  
