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

anyNorth<-grep(".*north.*",which.campus,ignore.case=T,value=F)
anyCentral<-grep(".*central.*",which.campus,ignore.case=T,value=F)
anySouth<-grep(".*south.*",which.campus,ignore.case=T,value=F)
anySVI<-grep(".*svi.*",which.campus,ignore.case=T,value=F)
noFulltime<-grep("^[[:space:]]*$",workload$fulltime,ignore.case=T,value=F)

pureCampus<-1:nrow(workload)
workload<-cbind(workload,pureCampus)
workload$pureCampus[anyNorth]<-"North"
workload$pureCampus[anyCentral]<-"Central"
workload$pureCampus[anySouth]<-"South"
workload$pureCampus[anySVI]<-"SVI"
workload$pureCampus[-c(anySouth,anyNorth,anyCentral,anySVI)]<-"NoCampus"
fulltimeLevels<-levels(workload$fulltime)
fulltimeLevels<-c(fulltimeLevels,"NoneGiven")
workload$fulltime[noFulltime]<-"noneGiven"
workload$fulltime<-factor(workload$fulltime,fulltimeLevels)




head(workload)

names(workload)
bts<-(grep("b&.*|abe|esl|bt.*",workload$program,ignore.case=T, value=F))
A<-(t(table(workload$support.office[-bts],workload$program[-bts])))
addmargins(A)

table(support.office,workload$pureCampus,workload$fulltime)
list(workload$pureCampus=c("North","Central"))


hist(A)
hist(table(workload$support.office[workload$which.campus=="Central"]),col=workload$support.office)

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
