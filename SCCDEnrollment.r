SccdEnrollments<-read.csv("C:\\Users\\Brian\\Desktop\\R-Data\\SCCDEnrollment.csv",header=T,sep=",")
ls()
library(psych)
attach(SccdEnrollments)
describe(SccdEnrollments)
#describeBy(list(c(Enrolled,classCap)),list(c(Campus,Type)))

#fixed below is whether it returns a logical.
cancelled<-grep("can",Instructor,ignore.case=T,fixed=F)

SccdEnrollments<-cbind(SccdEnrollments,"cancel"= c("N","Y"))
SccdEnrollments$cancel="N"
SccdEnrollments$cancel[cancelled]="Y"
table(SccdEnrollments$cancel)

SccdEnrollments[SccdEnrollments$Enrolled != 0 & SccdEnrollments$cancel=="Y",10:11]

SccdEnrollments$cancel[SccdEnrollments$Enrolled != 0 & SccdEnrollments$cancel=="Y"]="N"

detach(SccdEnrollments)
attach(SccdEnrollments)


split.by.campus.and.type<-split(SccdEnrollments,list(Campus,Type))
split.by.campus.type.cancel<-split(SccdEnrollments,list(Campus,Type,cancel))
split.by.type<-split(SccdEnrollments,list(Type))
split.by.type.and.campus<-split(SccdEnrollments,list(Type, Campus))

#t= transpose
# the following will calc means for the listed variables split by Campus and Type
credit.by.campus.type.sum<-t(round(sapply(split.by.type.and.campus, function(x) 
  colSums(x[,c(
    #"CREDIT",
    "Enrolled",
    "classCap", 
    "Wait.List"
    #,
    #"FTES.TOTAL", 
    #"FTES.STATE"
  )])),3))
#the funciton in the preceding command is created in that line.

t(round(sapply(split.by.campus.type.cancel,function(x) 
  colMeans(x[,c(
    #"CREDIT",
    "Enrolled",
    "classCap", 
    "Wait.List"
    #,
    #"FTES.TOTAL", 
    #"FTES.STATE"
  )])),3))
#the funciton in the preceding command is created in that line.




# the following will calc means for the listed variables split by Type
t(round(sapply(split.by.type,function(x) 
  colMeans(x[,c(
    #"CREDIT",
    "Enrolled",
    "classCap", 
    "Wait.List"
    #,
    #"FTES.TOTAL", 
    #"FTES.STATE"
  )])),3))
#the funciton in the preceding command is created just after.


split.by.cancel<-split(SccdEnrollments,SccdEnrollments$cancel)


#tapply(Enrolled,split.by.cancel,mean)

#plot(density(Wait.List))
#plot(density(Wait.List[cancel=="N"]))

#points(Enrolled[cancel=="N" & Campus=="North"], classCap[cancel=="N" & Campus=="North"], col="green")


library(lattice)

credit.cut<-equal.count(SccdEnrollments$CREDIT,4)
enrolled.cut<-equal.count(SccdEnrollments$Enrolled,4)
classCap.cut<-equal.count(SccdEnrollments$classCap,4)
Wait.List.cut<-equal.count(SccdEnrollments$Wait.List,4)
FTEstate.cut<-equal.count(SccdEnrollments$FTES.STATE,4)

X<-data.frame(cbind(CREDIT,Enrolled,classCap,Wait.List,FTES.TOTAL))
#splom(~X, col= rgb(0,0,0,0.2))
#histogram(~FTES.STATE|Type*enrolled.cut)

#this one is interesting
#densityplot(~Enrolled|Type*classCap.cut, type = c("percent"))
#densityplot(~FTES.STATE|Type*enrolled.cut, type = c("count"))

xyplot(FTES.STATE~Enrolled|Type*classCap.cut,
       #layout=c(1,4), 
       as.table = T, pch = 20, col= rgb(0,0,0,0.2),
       panel = function(x, y, ...){
         panel.xyplot(x, y, ...)
         fit<-lm(y~x)
         panel.abline(fit, lwd=2, col= "green")
         panel.loess(x,y, lwd=2, col="red", lty=2)
         panel.abline(v=c(25,30,36),lty=5,col=1)
       } , xlab= "Enrolled", ylab="FTES.STATE", 
       main="FTES.STate by Enrolled" )


xyplot(Enrolled~classCap|FTEstate.cut,
       prepanel = function(x, y) prepanel.loess(x, y, span = 1),
       xlab = "Enrolled", ylab = "classCap)",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.loess(x, y, span=1)
         fit<-lm(Enrolled~classCap*FTEstate.cut)
         panel.lmline(fit,lwd=2, col="red")
         
       })
X<-data.frame(cbind(Campus,Enrolled,classCap,FTES.STATE))

#how to analyze the credits taught by instructor


library(ggplot2)

qplot(Type,Enrolled,geom="violin")


ESL.classes<-SCCDEnrollment[grep("ABE|esl",Title,ignore.case=T,value=F),]
campus.split<-split(SCCDEnrollment,list(Campus,Type))
campus.quarter.split<-split(SCCDEnrollment,list(Campus, Quarter))
lapply(SCCDEnrollment,function(x) mean))
qplot(y=ESL.classes$Enrolled, x=ESL.classes$Quarter,geom="violin")
