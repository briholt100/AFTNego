require(RCurl)
library(ggplot2)
#note that google doc must be published to CSV
prezSearch2013<- "https://docs.google.com/spreadsheet/pub?key=0AnDE-SHhWCM4dHFmek45aGh0S2V0U2RyWUdpd1o4VkE&single=true&gid=0&output=csv"
tc <- getURL(prezSearch2013, ssl.verifypeer=FALSE)
pollData <- read.csv(textConnection(tc),header=T)
head(pollData,15)
names(pollData)

colnames(pollData) <- c("timestamp","list","explain","alt1","alt2","alt3","comment","Status")
names(pollData)
#first remove parantheses, make a title column
#Then str split, but how to make those into coloumns?

pollData$names<-pollData$list #creates new variable

#pulls out everything but names (except for "monte")
pollData$names<-gsub("Associate Vice Chancellor \\(B\\.A\\.S District coordinator\\)|sccc|-north|District Liason Gates grants \\(former |vice chancellor for finance & technology|north vpi|north vice president|Retired \\(total speculation\\)|--|, administrative services",""
     ,as.character(pollData$names),ignore.case=T) 
pollData$names<-gsub("jensen)","Jensen",as.character(pollData$names),ignore.case=T)
pollData$names<-gsub(" Ores","Ores",as.character(pollData$names),ignore.case=T)
pollData$names<-sub(" ?","",as.character(pollData$names),ignore.case=T)
pollData$names<-sub(", ",",",as.character(pollData$names),ignore.case=T)
pollData$names<-sub("pete|peter","Peter",as.character(pollData$names),ignore.case=T)
table(pollData$names)
list<-(strsplit(as.character(pollData$names),","))
table(unlist(list))
pollData[c(3,7)]  #comments