library(ggplot2)
library(dplyr)

#campus
df<-read.csv(file="I:/My Data Sources/AFT Data/leapfrog_issue.csv",na.strings = "")
str(df)
range(df$SRV.YEARS)
df$cutYearsWorked<-cut(df$SRV.YEARS,5)
levels(df$cutYearsWorked)<-c("1-10","10 - 20","20-29","29-38","38-47")

head(df)

#the following looks just at people likely to teach lab
df %>% filter(LAB !='N') %>% select(-cutYearsWorked, -PROG.SYMBOL, -PROGRAM) %>% 
  mutate(cutYearsWorked = cut(SRV.YEARS, breaks = quantile(SRV.YEARS, probs = seq(0, 1, 0.33)))) %>%
  ggplot(aes(x=SRV.YEARS,y=BASE.SALARY,group=cutYearsWorked,color=cutYearsWorked))+
  geom_point(size=1.5)+
  geom_smooth(se = T, method = "lm")+
  labs(title = "linear model applied to 5 different lab teaching \ncohorts separated by years worked\nShaded region is Standard Error",
       x = "Service years")


Lab.df<-read.csv(file="I:/My Data Sources/AFT Data/labLeap.csv",na.strings = "",sep="\t")
Lab.df$cutYearsWorked<-cut(Lab.df$yrs,5)
levels(Lab.df$cutYearsWorked)<-c("1-7.5","7.5 - 14","14-20","20-27","27-33")

head(Lab.df)
Lab.df %>% ggplot(aes(x=yrs,y=salary,group=cutYearsWorked,color=cutYearsWorked))+geom_point(size=1.5)+
  geom_smooth(se = T, method = "lm")+
  labs(title = "linear model applied to 5 different \ncohorts separated by years worked\nShaded region is Standard Error",
       x = "Service years")
