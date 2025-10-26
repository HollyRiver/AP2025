## MH alg. simple example ##

## target density ##

p = function(x) {
0.5*dnorm(x,0,1) + 0.5*dnorm(x,5,1)
}

set.seed(111) # initialization 
xx = rnorm(1,0,1)
for ( j in 1:10000)
{
x = xx[length(xx)]
y = rnorm(1,x,1) # random walk proposal 
u = runif(1)       # uniform r.v.
if (u<(p(y)/p(x))) xx = c(xx,y) 
# min(1, p(y)/p(x) ) : acceptance prob. 
if ((p(y)/p(x))<u) xx= c(xx,x)
}

thin = xx[seq(5001, 10000, by=100)]
acf(thin)
# burn-in for the first half
# check the ergodicity & thinning (by = 10, 50, 100) 

hist(thin) 
ts.plot(thin)  
ts.plot(cummean(thin))

thin -> t1 ; thin -> t2 ; thin -> t3 ;
# Three series with different initial values

ts.plot(t1)  
points(t2, type='l', col='red') 
points(t3, type='l', col='blue')

#Gelman-Rubin Statistics
L = length(t1)
W = (var(t1)+var(t2)+var(t3))/3
B = L*var(c(mean(t1),mean(t2), mean(t3)))
gel = (W*(L-1)/L + B/L)/W
# \to 1 means the convergence

## Naive Bayes simple example ##

data(iris)
#install.packages('klaR')  
library(klaR)
library(e1071)
train <- sample(1:150, 100) 
nb <- NaiveBayes(Species ~., data = iris, subset = train)
# From klaR
predict(nb, iris[-train,])$class
tt <- table(iris$Species[-train], predict(nb, iris[-train,])$class)


train <- sample(1:150, 100) 
nb1 <- naiveBayes(Species ~., data = iris, subset = train, 
                  laplace=1) # Laplace smoothing
# From e1071
default_pred <- predict(nb1, iris[-train,1:4], type="class")
table(default_pred, iris[-train,]$Species,dnn=c("Prediction","Actual"))



