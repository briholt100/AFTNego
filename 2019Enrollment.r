library(pdftools)
library(tidyverse)
library(stringr)
#ER<- pdf_text("U:/Documents/My Data Sources/AFT Data/2018-19 Academic Year Enrollment Report.pdf")

ER<- pdf_text("2018-19 Academic Year Enrollment Report.pdf")


ER %>% readr::read_lines()
t1 <- ER[4]
#t1<- t1 %>% str_replace_all("'15 to '19",'15-19')#manages the space
#t1<- strsplit(t1,'\r\\n')#  on windowes
t1<- strsplit(t1,'\n') # on linux
t1<- t1[[1]][4:38]
t1<- sub('([0-9])','+\\1',t1) %>%  str_replace_all(",","") %>%   str_squish()
t1 <- gsub(' ([0-9])','+\\1',t1)
t1 <- gsub(' (-[0-9])','+\\1',t1)
t1 <- gsub(' \\+','+',t1)
t1 <- gsub('%','',t1)
t1 <- t1 %>% strsplit(split='\\+')

str(t1)
t1.df <- data.frame(matrix(unlist(t1),ncol=8,byrow=T))
colnames(t1.df) <- c('college', '14-15','15-16','16-17','17-18','18-19','change 15-19','% change')

t1.df[,2:8]<- as.data.frame(apply(t1.df[,2:8],2,as.numeric))
str(t1.df)
t1.df[,1:6] %>% mutate(dif= t1.df$`18-19` - t1.df$`14-15`,dif.per = 100*dif/`14-15`)

sum(t1.df[1,2:6])

t1.df[,1:6] %>% rowwise() %>% summarise(sum=sum(`14-15`:`18-19`))

apply(t1.df[,2:6],1,function(x) mean(x))
