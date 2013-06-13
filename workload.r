library(ggplot2)
getwd()
dir()
setwd("./AFT Data")
workload<-read.csv("./WorkloadEquityOfficeHoursApril24.csv", header=T, sep="\t")
dim(workload)
summary(workload)
head(workload)
colnames(workload)<-c("response","support.office","support.Other","what.input","program","fulltime","which.campus")
str(workload)
attach(workload)
anyNorth<-grep(".*north.*",which.campus,ignore.case=T,value=F)
anyCentral<-grep(".*central.*",which.campus,ignore.case=T,value=F)
anySouth<-grep(".*south.*",which.campus,ignore.case=T,value=F)
anySVI<-grep(".*svi.*",which.campus,ignore.case=T,value=F)
noFulltime<-grep("^[[:space:]]*$",workload$fulltime,ignore.case=T,value=F)

detach(workload)
levels(workload$which.campus)<-c(levels(workload$which.campus),"NoCampus")
workload$which.campus[-c(anyNorth,anySouth,anyCentral,anySVI)]<-"NoCampus"
workload$which.campus<-factor(workload$which.campus)

fulltimeLevels<-levels(workload$fulltime)
fulltimeLevels<-c(fulltimeLevels,"NoneGiven")
levels(workload$fulltime)<-fulltimeLevels
workload$fulltime[noFulltime]<-"NoneGiven"


str(workload)
names(workload)

bts<-(grep("b&.*|abe.*|esl.*\abe|esl|bt.*",workload$program,ignore.case=T, value=F))

addmargins(t(table(workload$support.office[anySVI],workload$which.campus[anySVI])))

table(workload$support.office[bts | anySouth])

addmargins(t(table(workload$support.office[bts],workload$which.campus[bts])))

subset(workload,grepl("b&.*|abe.*|esl.*\abe|esl|bt.*",workload$program,ignore.case=T))


myPlot<-function(df,index,y) {
  plot(df[,index] ~ df$y
       #main=names(df[index],
        #          pch=16
#                  xlab="X label",
 #                 ylab=names(df)[index]
                  )
                  )
}


myPlot(workload,5,workload$support.office)


workloadBar<-ggplot(data = workload, aes(x = fulltime, y = 1, fill = support.office)) + 
  geom_bar(stat = "identity", position = position_stack(width = 0.9)) + 
  
  xlab("fulltime") + 
  ylab("number of something")

plot(workload$support.office~workload$fulltime)
