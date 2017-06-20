library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(lubridate)

rm(list=ls())
setwd("C:/Tech/Rcourse-1-4")
data<-read_csv("daily_SPEC_2014.csv.bz2")

#Change spaces in variable names to dots
names(data) <- gsub(" ",".",names(data))

#Question 1
data %>%
  filter(State.Name == "Wisconsin", Parameter.Name == "Bromine PM2.5 LC") %>%
  summarise(mean(Arithmetic.Mean))
  
#Question 2
data %>%
  group_by(Parameter.Name) %>%
  summarise(mean=mean(Arithmetic.Mean)) %>%
  arrange(desc(mean))

#Question 3
data %>%
  group_by(State.Code,County.Code,Site.Num) %>%
  filter(Parameter.Name=="Sulfate PM2.5 LC") %>%
  summarise(mean=mean(Arithmetic.Mean),param=first(Parameter.Name)) %>%
  arrange(desc(mean))

#Question 4
Q4 <- data %>%
  group_by(State.Name) %>%
  filter(Parameter.Name=="EC PM2.5 LC TOR",State.Name=="Arizona"|State.Name=="California") %>%
  summarise(mean=mean(Arithmetic.Mean),param=first(Parameter.Name)) %>%
  arrange(desc(mean))
Q4
abs(Q4[1,2]-Q4[2,2])

#Question 5
data %>%
  filter(Longitude<(-100), Parameter.Name == "OC PM2.5 LC TOR") %>%
  summarise(median(Arithmetic.Mean))

#Question 6
meta<-read_excel("aqs_sites.xlsx")
names(meta) <- gsub(" ",".",names(meta))
meta %>%
  filter(Land.Use=="RESIDENTIAL",Location.Setting=="SUBURBAN")

#Question 7
#join the datasets
meta <- meta %>% rename("Site.Num"="Site.Number")
data <- data %>%
  mutate(State.Code = as.double(State.Code), County.Code= as.double(County.Code), Site.Num = as.double(Site.Num))
data2 <- full_join(data,meta) #,by=c("State.Code","County.Code","Site.Num")
data2 %>% 
  filter(Land.Use=="RESIDENTIAL",Location.Setting=="SUBURBAN",Longitude>=(-100),Parameter.Name == "EC PM2.5 LC TOR") %>%
  select(State.Code,County.Code,Site.Num,Parameter.Name,Arithmetic.Mean,Land.Use,Location.Setting,Longitude) %>%
  arrange(desc(Arithmetic.Mean)) %>%

#Question 8
data2 <- data2 %>%
  mutate(Date.Local=ymd(Date.Local)) %>%
  mutate(month=month(Date.Local))
data2  %>%
  filter(Land.Use=="COMMERCIAL",Parameter.Name=="Sulfate PM2.5 LC") %>%
  group_by(month) %>%
  summarise(mean=mean(Arithmetic.Mean)) %>%
  arrange(desc(mean))

#Question 9
data2 %>%
  filter(State.Code==6,County.Code==65,Site.Num==8001,
         Parameter.Name=="Sulfate PM2.5 LC"|Parameter.Name=="Total Nitrate PM2.5 LC") %>%
  group_by(Date.Local,Parameter.Name) %>%
  summarise(mean=mean(Arithmetic.Mean)) %>%
  group_by(Date.Local) %>%
  summarise(sulfnitr=sum(mean)) %>%
  filter(sulfnitr>10)

#Question 10
csulf <- data2 %>% 
  group_by(State.Code,County.Code,Site.Num,Date.Local) %>%
  filter(Parameter.Name=="Sulfate PM2.5 LC") %>%
  summarise(sulf=mean(Arithmetic.Mean))
cnitr <- data2 %>% 
  group_by(State.Code,County.Code,Site.Num,Date.Local) %>%
  filter(Parameter.Name=="Total Nitrate PM2.5 LC") %>%
  summarise(nitr=mean(Arithmetic.Mean))
csn <- full_join(csulf,cnitr)
csn <- filter(csn,!is.na(sulf),!is.na(nitr))
csn %>%
  group_by(State.Code,County.Code,Site.Num) %>%
  summarise(corr=cor(sulf,nitr)) %>%
  arrange(desc(corr))
