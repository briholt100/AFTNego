library(ggplot2)
library(dplyr)

#One idea: sort by service years.  Take recent hire salary and subtract 
#from that all of the other salaries.  Iterate that through all of the hires.  
#you should see an average negative number.  But that magnitude will be large
#for recent hires and minimal for long term hires unless you get leap frogging. 


#campus
df<-read.csv(file="I:/My Data Sources/AFT Data/leapfrog_issue.csv",na.strings = "")
str(df)
range(df$SRV.YEARS)
df$cutYearsWorked<-cut(df$SRV.YEARS,5,replace=F)
levels(df$cutYearsWorked)<-c("1-10","10 - 20","20-29","29-38","38-47")
head(df)


b<-df %>% select(PROGRAM ,PROG.SYMBOL)
nrow(unique(b))


#the following looks just at people likely to teach lab
df %>% 
#  filter(LAB !='N') %>% 
  select(-cutYearsWorked, -PROG.SYMBOL, -PROGRAM) %>% 
  mutate(cutYearsWorked = 
           cut(SRV.YEARS,include.lowest=T, 
               breaks = quantile(SRV.YEARS, probs = seq(0, 1, 0.2))
               , labels = c("1 - 4","4 - 9","9 - 15","15 - 23","23 - 33")
         )) %>%
#df %>% 
    ggplot(aes(x=SRV.YEARS,y=BASE.SALARY,group=cutYearsWorked,color=cutYearsWorked))+
  geom_point(size=1.5)+
  geom_smooth(se = T, method = "lm")+
  labs(title = "linear model applied to 5 teaching \ncohorts separated by years worked\n\nGrids are weekly workload",
       x = "Service years")+
facet_wrap(~WEEKLY.LOAD)

Lab.df<-read.csv(file="I:/My Data Sources/AFT Data/labLeap.csv",na.strings = "",sep="\t")
Lab.df$cutYearsWorked<-cut(Lab.df$yrs,5)
levels(Lab.df$cutYearsWorked)<-c("1-7.5","7.5 - 14","14-20","20-27","27-33")

head(Lab.df)
Lab.df %>% ggplot(aes(x=yrs,y=salary,group=cutYearsWorked,color=cutYearsWorked))+geom_point(size=1.5)+
  geom_smooth(se = T, method = "lm")+
  labs(title = "linear model applied to 5 different \ncohorts separated by years worked\nShaded region is Standard Error",
       x = "Service years")




###concept of changing individual scores into difference scores from workers hired later


##Make concept data; 5 workers hired over a 10 year period:

df <- data.frame(subj = letters[1:5], 
                 srv.year = as.integer(c(1,1,2,4,10) ),
                 salary = c(15000.0,17000.0,18000.0,25000.0,27000.0), stringsAsFactors = FALSE)
str(df)

mean(df[1,3] - df[-1,3])
mean(df[2,3] - df[-1,3])
mean(df[3,3] - df[-(1:3),3])
mean(df[4,3] - df[-(1:4),3])
mean(df[5,3] - df[-(1:5),3])

#the above needs to be added to a new column, perhaps using apply (function, row, )
