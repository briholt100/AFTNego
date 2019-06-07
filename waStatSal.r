df <- read.csv("AnnualEmpSalary2017.csv")
library(tidyverse)
library(dplyr)
library(lubridate)
var.name<- names(df)
var.name[5:9] <- c('2013','2014','2015','2016','2017')
names(df) <- var.name
df %>% filter(
  grepl("seattle",AgyTitle,ignore.case = T)) %>% 
  select(Agy)

df %>% filter(Agy==670,
              grepl("faculty|ftf",JobTitle,ignore.case = T)) %>%
  summarise(n())

df %>% 
  filter(grepl("seattle",
               AgyTitle,ignore.case = T)) %>%
  select(Agy)
gath.df<- df %>% 
  filter(Agy==670) %>% 
  gather(key=year,value=sal,-c(Agy:JobTitle)) %>% 
  filter(sal>0) %>% mutate(year=ymd(gath.df$year,truncated = 2L))

gath.df %>% group_by(year) %>%   summarise(mean(sal))

gath.df %>% filter(grepl("faculty|ftf",JobTitle,ignore.case = T)) %>% group_by(year) %>% summarise(n(),mean(sal),min(sal),max(sal))


gath.df %>% 
  filter(grepl("faculty|ftf|ptf",JobTitle,ignore.case = T)) %>% 
  group_by(JobTitle) %>% 
  summarise(n())  %>% slice(-4)%>% print(n = Inf)
