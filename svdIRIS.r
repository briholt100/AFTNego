i=0
for (i in 1:100){i<-i+1
                print(i)}

data(iris)
##playing with flower data
str(iris)
summary(iris)
install.packages("ggplot2")
library(ggplot2)
par(mfrow = c(2,3))
i=0
for (i in 1:length(iris)){plot(iris[,i])}
gp<-ggplot(data=iris,aes(y=Sepal.Length,x=Sepal.Width,color=Species))
gp + geom_point()
distWL<-dist(iris$Sepal.Length,iris$Sepal.Width,method="euclidian")
svdWL<-svd(distWL)
str(svdWL)
