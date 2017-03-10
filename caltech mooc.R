trainSet<-data.frame(matrix(
  c(1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0),
  nrow=4,byrow=T))

colnames(trainSet)<-c("X0","X1","X2","Z")

x<-trainSet[,1:3]
y<-trainSet[,4]
y<-matrix(0,4,4)
y1<-c(0,0,0) #weight
y2<-c(0,0,0) #sensor
k<-0
if(k<10){
  k<-0
  for(i in 1:nrow(x))  for(j in 1:length(y1)){
    y2[j]<-y1[j]*x[i,1:3]
    y1[j]<-y1[j]+.1
    s<-sum(y1)
    k<-k+1
    
    print(cbind(y1,y2,k,s))
  }    
}
y1
x

trainSet<-data.frame(matrix(
  c(1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0),
  nrow=4,byrow=T))

colnames(trainSet)<-c("X0","X1","X2","Z")
threshold <- 0.5                #sets a threshold for comparison
w <- c(0, 0, 0)                 #sets the weights to intitial zero w[i]
sensor <- c(0, 0, 0)            #sets the sensor outcome to intitial zero c[i]
error<-1                        #sets error to non zero
final.w<-NULL


BrianPercept<-function(x,y,lr=.1){
            #lr <- 0.1                      #sets the rate of change/learning
            threshold <- 0.5                #sets a threshold for comparison
            w <- c(0, 0, 0)                 #sets the weights to intitial zero w[i]
            sensor <- matrix(
              rep(0,nrow(x)*
                    length(x)),
              nrow=nrow(x))            #sets the sensor outcome to intitial zero c[i]
            error<-1                        #sets error to non zero
            final.w<-NULL
                for(i in 1:nrow(x)) for (j in 1:length(x)){
                    sensor[i,j]<-w[i]*x[i,j]           #sensor test
                    s<-sum(sensor[i])          #sensor summation to compare to threshold
                    #s<-.2   Just a coding test
                    n<-ifelse(s<threshold, n<-0,n<-1) #outcome of comparison to threshold
                    delta<- lr*error                  #delta, change or correction of learning rate
                    final.w<-x[i,j]*delta #iterative report
                    error<-y[i]-n            #if error != zero, then we aren't done
                    print("error")
                    print(error)
                    print("final.w")
                    print(final.w)
                    print(sensor)
            }
            print(w)
            print(sensor)
    }

BrianPercept(trainSet[,1:3],trainSet[,4])


  #  y=mx+b+e

a<-rnorm(10,0,1)

b<-2*(rnorm(10,0,1))

plot(a,b, ylim=c(0,max(b)+2),xlim=c(0,max(a)+2) )


abline(coef=c(-6,3))


betaNorm <- betaCauch <- rep(NA,1000)
for (i in 1: 1000){
  x <- rnorm(50); e <- rnorm(50); e2 <- rcauchy(50); 
  b0 <-1; b1 <-2  
  y <- b0 + b1*x + e; y2 <- b0 + b1*x + e2
  betaNorm[i] <- lm(y ~ x)$coeff[2]; betaCauch[i] <- lm(y2 ~ x)$coeff[2]
}
quantile(betaNorm)

plot(y2,pch=19, col="blue")
abline(coef=c(-40,3),col="blue")
abline(coef=c(55,-2.8),col="red")

####From: http://www.stat.cmu.edu/~cshalizi/350/lectures/25/lecture-25.pdf
classify.linear = function(x,w,b) {
  distance.from.plane = function(z,w,b) { sum(z*w) + b }
  distances = apply(x, 1, distance.from.plane)
  return(ifelse(distances < 0, -1, +1))
}

perceptron = function(x, y, learning.rate=1) {
  w = vector(length = ncol(x)) # Initialize the parameters
  b = 0
  k = 0 # Keep track of how many mistakes we make
  euclidean.norm<-function(x){return(sqrt(sum(x^2)))}
  R = max(euclidean.norm(x))
  made.mistake = TRUE # Initialized so we enter the while loop
  while (made.mistake) {
    made.mistake=FALSE # Presume that everything's OK
    for (i in 1:nrow(x)) {
      if (y[i] != classify.linear(x[,i],w,b)) {
        w <- w + learning.rate * y[i]*x[,i]
        b <- b + learning.rate * y[i]*R^2
        k <- k+1
        made.mistake=TRUE # Doesn't matter if already set to TRUE previously
      }
    }
  }
  return(w=w,b=b,mistakes.made=k)
}

x<-matrix(1:12,ncol=3)
y=c(1,1,0,1)

perceptron(x,y)
###(Note that euclidean.norm isn't a built-in function, but it's easy to write. Exercise:   Write this function; to work in the code above it should take a matrix and return a vector giving the Euclidean norm of each row.)



# Rosenblatt's perceptron

data(iris)



#dd <- iris[iris$Species != "versicolor", -(3:4)]
#dd<-cbind(a[1:2],b[1:2])

#dd <- dd[-42, ] # this point breaks linear separation (at least w/o intercept)
dm <- data.matrix(dd[sample(nrow(dd),10,replace=T), 1:2])
dm <- cbind(dm, 1) # to add intercept to the separating plane
dc <- rep(1, nrow(dm))
dc[dd$Species == "virginica"] <- -1
#dc[dd[,1]]<- -1

trainPerceptron <- function(dm, dc) {
  result <- list()
  oldW <- c()
  w <- rep(0, ncol(dm))
  while (!identical(w, oldW)) {
    oldW <- w
    for (i in 1:nrow(dm)) {
      pred <- sign(dm[i,] %*% w)
      w = w + (dc[i] - pred) * dm[i, ]

    }
    # cat(w, "\n")
    result <- c(result, list(w))
  }
  result
}

plotPerceptronSteps <- function(pcResult, dm, dc, steps) {
  for (s in steps) {
    w <- pcResult[[s]]
    plot(dm[,1:2], main=paste("step", s), pch=ifelse(dc > 0, 1, 2), sub=paste(w, collapse=' '))
    abline(-w[3] / w[2], -w[1] / w[2])
  
  }
}



 pc <- trainPerceptron(dm, dc)
pc
plotPerceptronSteps(pc, dm, dc, length(pc))
 png("perceptron-biased.png", width=600, height=600)
 par(mfrow=c(2,2))
 plotPerceptronSteps(pc, dm, dc, c(20, 50, 250, length(pc)))
 dev.off()

result


#Homeowrk 1
#prob 7
x<-runif(2,-1,1)
function(m,x,b){
  m*x+b
}

y=f(.3,x,.3)
dd<-cbind(x,y)

plot(y,type="l",ylim=c(-1,1),xlim=c(-1,1))
plot(dd,ylim=c(-1,1),xlim=c(-1,1), col="blue",type="l")
abline(a=.3, b=.3,col="blue")
points(dd,col="blue",pch=19)

testDF<-cbind(a,b)
colnames(testDF)<-c("first","second")
testCX<-rep(1,10)
testDF<-data.frame(testDF)
#dc[dd$Species == "virginica"] <- -1

for (i in nrow(testDF)){
  ifelse(testDF[,i]<f(.3,x,.3), testCX[i] <- -1, testCX[i] <- 1)  
}
x

testCX[testDF$second<f(.3,x,.3)]<- -1

cbind(testDF,testCX)
plot(testDF[testCX==1,1:2],col="green",pch=19,ylim=c(-2,5),xlim=c(-1,3))
points(testDF[testCX==-1,1:2],col="red",pch=19)


a<-rep(1,10)
b<-c(rep(0,5),rep(1,5))
c<-rep(c(1,2),5)
d<-sample(b,10,replace=T)
d[d==0]<--1
trainDF<-data.frame(a,b,c,d)


#y=m*x+b
#m=3
#b=.1

a<-rep(1,10)
b<-runif(10,-1,1)
c<-rep(.1,10)
d<-3*b+c
#d[d==0]<- -1
trainDF<-data.frame(a,b,c,d)






w<-rep(0,3)
learning.rate<-.1
threshold<-.5
made.mistake<-TRUE

if(trainDF[i,4] != f(x)){
  w<-w+learn.rate*trainDF[i,4]*trainDF[i,1:3]
  b<-b+learn.rate*trainDF[i,4]*R^2
  k<-k+1
  made.mistake<-TRUE
}


perceptron = function(x, y, learning.rate=1) {
  w = vector(length = ncol(x)) # Initialize the parameters
  b = 0
  k = 0 # Keep track of how many mistakes we make
    made.mistake<-TRUE
  euclidean.norm<-function(x){return(sqrt(sum(x^2)))}
  R = max(euclidean.norm(x))
  made.mistake <- TRUE # Initialized so we enter the while loop
  while (made.mistake==T) {
    made.mistake=FALSE # Presume that everything's OK
    for (i in 1:nrow(x)) {
      if(y[i] != sum(x[i,1:3]*w)+b) {
        w<-w+learning.rate*y[i]*x[i,1:3]
        b<-b+learning.rate*y[i]*R^2
        k<-k+1
        made.mistake<-TRUE # Doesn't matter if already set to TRUE previously
        #print(w<-w)
        #print(b<-b)
        print(k)
      }
    }
  }
  print(w<-w)
  print(b<-b)
  print(k<-k)
}


perceptron(trainDF[,1:3],trainDF[,4])