require(RCurl)
library(ggplot2)
#note that google doc must be published to CSV
prezSearch2013<- "https://docs.google.com/spreadsheet/pub?key=0AnDE-SHhWCM4dHFmek45aGh0S2V0U2RyWUdpd1o4VkE&single=true&gid=0&output=csv"
tc <- getURL(prezSearch2013, ssl.verifypeer=FALSE)
pollData <- read.csv(textConnection(tc),header=T)
str(pollData)
dim(pollData)
names(pollData)

colnames(pollData) <- c("timestamp","list","explain","alt1","alt2","alt3","comment","Status")
names(pollDat)


#first remove parantheses, make a title column
#Then str split, but how to make those into coloumns?
pollData$index<-as.numeric(as.factor(pollData$timestamp))
pollData$names<-pollData$list #creates new variable
pollData$names<-gsub("Central VPI|Associate Vice Chancellor \\(B\\.A\\.S District coordinator\\)|sccc|-north|District Liason Gates grants \\(former |vice chancellor for finance & technology|north vpi|north vice president|Retired \\(total speculation\\)|--|, administrative services",""
     ,as.character(pollData$names),ignore.case=T) 
pollData$names<-gsub("jensen)","Jensen",as.character(pollData$names),ignore.case=T)
pollData$names<-sub("pete|peter","Peter",as.character(pollData$names),ignore.case=T)
pollData$names<-sub(" Ores","Ores",as.character(pollData$names),ignore.case=T)
pollData$names<-sub("gayla shoemaker","Gayla Shoemake",as.character(pollData$names),ignore.case=T)
pollData$names<-sub("gayla|Gayla Shoemake|Gayla Shoemake Shoemake","Gayla Shoemake",as.character(pollData$names),ignore.case=T)
pollData$names<-sub(", ",",",as.character(pollData$names),ignore.case=T)
pollData$names<-sub(" ?","",as.character(pollData$names),ignore.case=T)
pollData$names<-sub(", ",",",as.character(pollData$names),ignore.case=T)
pollData$names<-sub(" Ores","Ores",as.character(pollData$names),ignore.case=T)
pollData$names<-sub("dr.","",as.character(pollData$names),ignore.case=T)
pollData$names<-sub(", ",",",as.character(pollData$names),ignore.case=T)
pollData$names<-sub(" ?","",as.character(pollData$names),ignore.case=T)
pollData$names<-sub(", ",",",as.character(pollData$names),ignore.case=T)

grep("dr.",pollData$names,ignore.case=T,value=T)
table(pollData$names)


list<-(strsplit(as.character(pollData$names),","))
table(unlist(list))

pie(table(unlist(list)),col=rainbow(nrow(table(unlist(list)))),main="Pie Chart of Potential Acting Presidents")


list.final<-unlist(list)
sans.ward<-table(list.final[list.final!="Alan Ward"])
df<-data.frame(sans.ward)
df.sort<-df[order(df$Freq),]
b<-barplot(df.sort$Freq, names.arg=unique(df.sort$Var1),horiz=F
        ,axes=T, axisnames = FALSE
#        ,legend.text=df$Var1
        ,col=rainbow(length(df.sort$Var1))
        ,main="Potential Acting Presidents"
        ,xlab="Note that Alan Ward has been removed")
#text(b,0,df.sort$Var1,cex=.5,pos=3)

legend("topleft", legend=df.sort$Var1, cex=.7, 
       bty="n", fill=rainbow(length(df.sort$Var1)))

names.tbl<-data.frame(pollData$index,pollData$names,pollData$alt1,pollData$alt2, pollData$alt3)

#
#
#
#
#
####Below attempts to start pulling out the comments
pollData[c(3,7)]  #comments

pollData[pollData[4]=="Tracy Furutani" | pollData[5]=="Tracy Furutani" , c(4:6)]
pete.tracy<-grep("tracy|pete",pollData$list,ignore.case=T,value=F)  #this should find "list" items including tracy or pete
pollData[pete.tracy & pollData[4]=="Tracy Furutani" | pollData[5]=="Tracy Furutani"  , c(9,4:6)] 
      #this should pull just those records with Tracy in both fields

no.list<-grep("not|no",pollData$explain,ignore.case=T,value=F)
mary.list<-grep("mary",pollData$explain,ignore.case=T,value=F) 

mary.list<-c(mary.list,grep("mary",pollData$comment,ignore.case=T,value=F))
pollData[mary.list,c(3,7)]

pollData[no.list & mary.list,]