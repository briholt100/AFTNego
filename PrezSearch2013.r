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
names(pollData)


#first remove parantheses, make a title column
#Then str split, but how to make those into coloumns?

pollData$names<-pollData$list #creates new variable

pollData$names<-gsub("Associate Vice Chancellor \\(B\\.A\\.S District coordinator\\)|sccc|-north|District Liason Gates grants \\(former |vice chancellor for finance & technology|north vpi|north vice president|Retired \\(total speculation\\)|--|, administrative services",""
     ,as.character(pollData$names),ignore.case=T) 
pollData$names<-gsub("jensen)","Jensen",as.character(pollData$names),ignore.case=T)
pollData$names<-sub("pete|peter","Peter",as.character(pollData$names),ignore.case=T)

pollData$names<-sub("gayla shoemaker","Gayla Shoemake",as.character(pollData$names),ignore.case=T)
pollData$names<-sub("gayla|Gayla Shoemake|Gayla Shoemake Shoemake","Gayla Shoemake",as.character(pollData$names),ignore.case=T)
pollData$names<-sub(", ",",",as.character(pollData$names),ignore.case=T)
pollData$names<-sub(" ?","",as.character(pollData$names),ignore.case=T)
pollData$names<-sub(", ",",",as.character(pollData$names),ignore.case=T)
pollData$names<-gsub(" Ores","Ores",as.character(pollData$names),ignore.case=T)
table(pollData$names)


list<-(strsplit(as.character(pollData$names),","))
table(unlist(list))

pie(table(unlist(list)),labels=lbls,col=rainbow(length(lbls)),main="Pie Chart of Potential Acting Presidents")


list.final<-unlist(list)
sans.ward<-table(list.final[list.final!="Alan Ward"])
df<-data.frame(sans.ward)
df.sort<-df[order(df$Freq),]
b<-barplot(df.sort$Freq, names.arg=unique(df.sort$Var1),horiz=F
        ,axes=T, axisnames = FALSE
#        ,legend.text=df$Var1
        ,col=rainbow(length(df.sort$Var1))
        ,main="Potential Acting Presidents"
        ,xlab="Note that Alan Ward removed")
#text(b,0,df.sort$Var1,cex=.5,pos=3)

legend("topleft", legend=df.sort$Var1, cex=1, 
       bty="n", fill=rainbow(length(df.sort$Var1)))

pollData[c(3,7)]  #comments