setwd("C:/Users/Jt Klimek/Desktop/User Data/Documents/MSA/FALL 1/Time Series 1")
library(gdata)
library(dplyr)
library(stringr)

well_data<-read.csv("G-2866_T.csv")%>%
  select(-Well_ft, -Code)%>%
  group_by(time)%>%
  mutate(new_time=as.character(time),
         hour=str_split(new_time,":")[[1]][1],
         minute=str_split(new_time,":")[[1]][2])%>%
  ungroup(time)%>%
  group_by(date)%>%
  mutate(month=str_split(date,"/")[[1]][1],
         day=str_split(date,"/")[[1]][2],
         year=str_split(date,"/")[[1]][3])%>%
  ungroup(date)%>%
  mutate(minute=as.integer(minute),hour=as.integer(hour),day=as.integer(day),
         month=as.integer(month),year=as.integer(year))%>%
  filter(!(year==2018&month==6&day>8)&
           !(year==2018&month==6&day==8&hour>10)&
           !(year==2018&month==6&day==8&hour==9&minute>30))%>%
  arrange(year,month,day,hour)%>%
  group_by(year,month,day, hour, tz_cd)%>%
  summarise(Corrected_mean=mean(Corrected))%>%
  ungroup(year,month,day,hour)

new_well_data<-well_data%>%
  group_by(year,month)%>%
  summarize(month_mean=mean(Corrected_mean),
            numb=n())%>%
  arrange(year,month)