library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)
df <- read.csv("//S-N-NSCHOME/nschomes/U980354189/Documents/My Data Sources/Data/AnnualEmpSalary2017.csv")
var.name<- names(df)
var.name[5:9] <- c('2013','2014','2015','2016','2017')
names(df) <- var.name
df$JobTitle <- sub("SPEC, FACULTY DEV.& COMPLI","SPEC, HR DEV.& COMPLI",df$JobTitle,ignore.case=T)
df$JobTitle <- trimws(df$JobTitle)
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
  filter(sal>0)

gath.df <- mutate(gath.df,year=ymd(paste0(gath.df$year,"06-30-")))

gath.df %>% group_by(year) %>%   summarise(mean(sal))

faculty.grepl <- "faculty|ftf|ptf"
gath.df %>% filter(grepl(faculty.grepl,JobTitle,ignore.case = T)) %>% group_by(year) %>% summarise(n(),mean(sal),min(sal),max(sal))


gath.df %>% 
  mutate(yr=ifelse(year=="2013-06-30",'2013',
                                        ifelse(year=="2014-06-30",'2014',
                                           ifelse(year=="2015-06-30",'2015',
                                            ifelse(year=="2016-06-30",'2016','2017'))))) %>% 
    mutate(status=ifelse(
      grepl(faculty.grepl,JobTitle,ignore.case =T)==T,'faculty',
       ifelse(grepl('dean|director',JobTitle,ignore.case =T)==T,'dean/dir','non-fac'))) %>%
        group_by(yr,status) %>% 
  summarise(n=n(),m=mean(sal),sd=sd(sal)) %>% 
  ggplot(aes(x=yr,y=sd,group=status,color=status))+geom_point()+geom_line()+ scale_y_continuous(labels = scales::comma)+
  ggtitle("Seattle College standard deviation in Salary") +
  xlab("Year") + ylab("Standard Deviation of Sal in Dollars")

gath.df %>% 
  mutate(yr=ifelse(year=="2013-06-30",'2013',
                   ifelse(year=="2014-06-30",'2014',
                          ifelse(year=="2015-06-30",'2015',
                                 ifelse(year=="2016-06-30",'2016','2017'))))) %>% 
  mutate(status=ifelse(
    grepl(faculty.grepl,JobTitle,ignore.case =T)==T,'faculty',
    ifelse(grepl('dean|director',JobTitle,ignore.case =T)==T,'dean/dir','non-fac'))) %>%
  group_by(yr,status) %>% 
  summarise(n=n(),m=mean(sal),sd=sd(sal)) %>% 
  ggplot(aes(x=yr,y=m,group=status,color=status))+geom_point()+geom_line()+ scale_y_continuous(labels = scales::comma)+
  ggtitle("Seattle College averages in Salary") +
  xlab("Year") + ylab("Mean Salary in Dollars")
