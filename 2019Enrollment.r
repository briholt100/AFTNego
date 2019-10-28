library(pdftools)
library(tidyverse)
ER<- pdf_text("U:/Documents/My Data Sources/AFT Data/2018-19 Academic Year Enrollment Report.pdf")
ER %>% readr::read_lines()
t1 <- ER[4]
#t1<- t1 %>% str_replace_all("'15 to '19",'15-19')#manages the space 
t1<- strsplit(t1,'\r\n')
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
t1.df[,1:6] %>% mutate(dif= t1.df$`18-19` - t1.df$`14-15`,dif.per = round(100*dif/`14-15`,1))

sum(t1.df[1,2:6])/5

t1.df[,1:6] %>% rowwise() %>% mutate(mean = mean(`14-15`:`18-19`),dif.from.mean=`18-19`-mean)

apply(t1.df[,2:6],1,function(x) mean(x))

      
      ####Using tabilizer
      
      url <- "https://nam04.safelinks.protection.outlook.com/?url=https%3A%2F%2Fus19.mailchimp.com%2Fmctx%2Fclick%3Furl%3Dhttp%253A%252F%252Fwww.seattlecolleges.edu%252Fintranet%252Fgetdocument.aspx%253FsiteID%253D57%2526docID%253D11498%2526docType%253Dpdf%26xid%3Dd119db75c7%26uid%3D100528594%26pool%3D%26subject%3D&data=02%7C01%7C%7C0bfe3908f96749839c7408d758a2f456%7C02d8ff38d7114e31a9156cb5cff788df%7C0%7C0%7C637075329817390952&sdata=8qpDN4f6w%2BDtN9EWGmt7tku9%2F3ZGcrH2ixD3vy1GoIk%3D&reserved=0"


library(pdftools)
library(tabulizer)
library(dplyr)
library(stringr)
# default call with no parameters changed
matrix_results <- extract_tables(url)

# get back the tables as data frames, keeping their headers
df_results <- extract_tables(url, output = "data.frame", header = TRUE)
df <- df_results[[1]]
colnames(df) <- df[1,]
df <- df[-1,]

apply(df,2,class)

newnames <- sub("^(.)","y\\1",colnames(df[2:6]))

colnames(df) <- c('College',newnames,"y15-19","y%15-19")
df <- as.data.frame(df)
df<- apply(df,2,function (x) gsub(",|%","",x))
df[,2:8] <- apply(df[,2:8],2,function(x) as.numeric(x))

df %>% rowwise() %>% mutate(dif = `y2018-19` - `y2014-15`  ,
                            sum1= `y2014-15`+ `y2015-16`+`y2016-17`+`y2017-18`,+`y2018-19`,
                            sum2 = sum(`y2014-15`,`y2015-16`,`y2016-17`,`y2017-18`,`y2018-19`))

apply(df[,2:6],1,function(x) df[,6]-mean(x))

                            
