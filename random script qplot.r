
samples<-30
n<-30
residuals<-rep(NA,n)
y<-rep(NA,n)
x<-rep(NA,n)

#an easy way to replicate the sampling of 1 observation from Height, n times.
x<-replicate(1,sample(Height,n))
mean(x)

# My loop to sample n times from Height
 for(i in 1:n) {
      x[i:n]<-sample(Height,n)
      residuals[i:n]<-(x[i]-mean(Height))
        print(x[i])
     print(i)      
      y[i]<-mean(x)
     print(y[i])
 }
#y<-mean(x)
x
mean(x)
y
mean(y)

mean(Height)
median(y)
median(Height)
layout(matrix(c(1,2,3,4,5,6,7,8),2,4, byrow=T))
hist(Height)
lines(density(Height),col="blue")
hist(y)
lines(density(y),col="blue")
hist(x)
lines(density(y),col="green")
boxplot(Height, y, x, names=c("height", "y","x"),xlab= "Height cf with sample")
abline(h=mean(Height),col="green")
abline(h=mean(y),col="red")
boxplot(y, xlab="Y")
abline(h=mean(Height),col="green")
abline(h=mean(y),col="red")
plot(density(residuals), col="orange")

describe(y)
describe(x)
describe(Height)



## A largish data set
n <- 10000
x1  <- matrix(rnorm(n), ncol=2)
x2  <- matrix(rnorm(n, mean=3, sd=1.5), ncol=2)
x   <- rbind(x1,x2)

oldpar <- par(mfrow=c(2,2))
smoothScatter(x, nrpoints=0)
smoothScatter(x)


n<-10
m<-replicate(n,lm(Weight~Height,subset=sample(Height,10)))
