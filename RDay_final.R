library(dplyr)

#Read in NGHS data
nghs <- read.csv("C:/Users/Jess/Documents/R Day 2016/R Day 2016/nghs.csv")

#Check out the dataset
head(nghs)

#Make temp copy
d2<- nghs

#Round age
d2$age1<-round(d2$AGE,1)

#Subset between 9 and 19
d3<-subset(d2,d2$age1>9 & d2$age1<=19)

#Check age1 variable (should be 9.1--19)
table1<-d3 %>%
  count(age1) %>%
  mutate(prop = prop.table(n))
table1


#Select variables of interest
d4<-d3[,c(1,2,6,8,9,10,12,17)]

#test normality of BP variables

qqnorm(d4$SYSAV, main="Normal Q-Q Plot for Systolic BP");qqline(d4$SYSAV, col = 2)
qqnorm(d4$DIA5AV, main="Normal Q-Q Plot for Diastolic BP");qqline(d4$DIA5AV, col = 2)

#Box Cox transformation of BP variables
library(MASS)
library(forecast)

#Split SBP dataframe by age
d11<-with(d4,split(d4,age1))

mat2<-matrix(NA,100,11)
for(i in 1:100){
  
  d<-d11[[i]]
  # now to transform sbp vector
  SYSAV_trans = log(d$SYSAV)
  #test normality of transformed SBP
  shapiro_sys<-shapiro.test(SYSAV_trans)
  #how many p-values reject null?
  pvalue<-ifelse(shapiro_sys$p.value<0.05,1,0)
  # now to transform dbp vector
  DIA5AV_trans = d$DIA5AV^2 
  #test normality of transformed DBP
  shapiro_dia<-shapiro.test(DIA5AV_trans)
  #how many p-values reject null?
  pvalue_d<-ifelse(shapiro_dia$p.value<0.05,1,0)
  meansbp<-mean(SYSAV_trans, na.rm = TRUE)
  meandbp<-mean(DIA5AV_trans, na.rm = TRUE)
  ttest_sbp<-t.test(SYSAV_trans ~ d$RACE, d)
  pvalue_sbp<-ifelse(ttest_sbp$p.value<0.05,1,0)
  ttest_dbp<-t.test(DIA5AV_trans ~ d$RACE, d)
  pvalue_dbp<-ifelse(ttest_dbp$p.value<0.05,1,0)
  age <- d$age1[1]
  mat2[i,]<-c(age,shapiro_sys$p.value,pvalue,meansbp,ttest_sbp$p.value,pvalue_sbp,shapiro_dia$p.value,pvalue_d,meandbp,ttest_dbp$p.value,pvalue_dbp)
}
mat2

colnames(mat2)<-c("Age","Shapiro_SBP","Norm_SBP","Mean SBP","ttest_sbp","sig_sbp","Shapiro_DBP","Norm_DBP","Mean DBP","ttest_dbp","sig_dbp")
mat2<-data.frame(mat2)
colSums(mat2)


#### White ###
white<-matrix(NA,100,7)
for(i in 1:100){
  
  d<-d11[[i]][which(d11[[i]]$RACE==1),]
  # now to transform sbp vector
  SYSAV_trans = log(d$SYSAV)
  #test normality of transformed SBP
  shapiro_sys<-shapiro.test(SYSAV_trans)
  #how many p-values reject null?
  pvalue<-ifelse(shapiro_sys$p.value<0.05,1,0)
  # now to transform dbp vector
  DIA5AV_trans = d$DIA5AV^2 
  #test normality of transformed DBP
  shapiro_dia<-shapiro.test(DIA5AV_trans)
  #how many p-values reject null?
  pvalue_d<-ifelse(shapiro_dia$p.value<0.05,1,0)
  meansbp<-mean(SYSAV_trans, na.rm = TRUE)
  meandbp<-mean(DIA5AV_trans, na.rm = TRUE)
  age <- d$age1[1]
  white[i,]<-c(age,shapiro_sys$p.value,pvalue,meansbp,shapiro_dia$p.value,pvalue_d,meandbp)
}
white

white<-data.frame(white)
colSums(white)

### Black ###
black<-matrix(NA,100,7)
for(i in 1:100){
  
  d<-d11[[i]][which(d11[[i]]$RACE==2),]
  # now to transform sbp vector
  SYSAV_trans = log(d$SYSAV)
  #test normality of transformed SBP
  shapiro_sys<-shapiro.test(SYSAV_trans)
  #how many p-values reject null?
  pvalue<-ifelse(shapiro_sys$p.value<0.05,1,0)
  # now to transform dbp vector
  DIA5AV_trans = d$DIA5AV^2 
  #test normality of transformed DBP
  shapiro_dia<-shapiro.test(DIA5AV_trans)
  #how many p-values reject null?
  pvalue_d<-ifelse(shapiro_dia$p.value<0.05,1,0)
  meansbp<-mean(SYSAV_trans, na.rm = TRUE)
  meandbp<-mean(DIA5AV_trans, na.rm = TRUE)
  age <- d$age1[1]
  black[i,]<-c(age,shapiro_sys$p.value,pvalue,meansbp,shapiro_dia$p.value,pvalue_d,meandbp)
}
black

black<-data.frame(black)
colSums(black)


#save(mat2,file="C:/Users/Jess/Documents/R Day 2016/R Day 2016/BP_analysis.Rdata")


#---------------------------------------------
# My Local Polynomial Smoothers Bootstrap Code 
#---------------------------------------------
# Fit a polynomial smoothers and calculates a symmetric nonparametric bootstrap confidence intervals.
# Arguments:
#    x, y       : data values
#    nreps      : number of bootstrap replicates

require(KernSmooth)
lpsboot <- function(x,y,nreps=1000, band=5, confidence = 0.95){
  # Put input data into a data frame, sorted by x, with no missing values.
  dat <- na.omit(data.frame(x=x,y=y))
  if(nrow(dat) == 0) {
    print("Error:l No data left after dropping NAs")
    print(dat)
    return(NULL)
  }
  ndx <- order(dat$x)
  dat$x <- dat$x[ndx]
  dat$y <- dat$y[ndx]
  # Fit curve to data
  require(KernSmooth)
  len <- length(dat$x)
  f0 <- locpoly(x, y, kernel = "epanechnikov", bandwidth = band, gridsize = len)
  y.fit <- f0$y
  # Generate bootstrap replicates
  mat <- matrix(0,NROW(dat), nreps)
  for(i in seq(nreps)){
    ndx <- sample(len,replace=T)
    x.repl <- x[ndx]
    y.repl <- y[ndx]
    f <- locpoly(x.repl, y.repl, kernel = "normal", bandwidth =  5, gridsize = len)
    mat[, i] <- f$y
  }
  # calculating confidence intervals
  ci <- t(apply(mat, 1, quantile, probs = c((1-confidence)/2, (1+confidence)/2)))
  res <- cbind(as.data.frame(f0), ci)
  colnames(res) <- c('x','y', 'lwr.limit','upr.limit')
  res
}

#---------------------------------------------
# My Kernel Smoothers Bootstrap Code 
#---------------------------------------------
# Fit a kernel smoothers and calculates a symmetric nonparametric bootstrap confidence intervals.
# Arguments:
#    x, y       : data values
#    nreps      : number of bootstrap replicates
#mohammed, Nov 3, 2013
ksboot <- function(x,y,nreps=1000, band=5, confidence = 0.95){
  # Put input data into a data frame, sorted by x, with no missing values.
  dat <- na.omit(data.frame(x=x,y=y))
  if(nrow(dat) == 0) {
    print("Error: No data left after dropping NAs")
    print(dat)
    return(NULL)
  }
  ndx <- order(dat$x)
  dat$x <- dat$x[ndx]
  dat$y <- dat$y[ndx]
  # Fit curve to data
  require(KernSmooth)
  len <- length(dat$x)
  f0 <- ksmooth(x, y, kernel = "normal", bandwidth = band, n.points = len)
  y.fit <- f0$y
  # Generate bootstrap replicates
  mat <- matrix(0,NROW(dat), nreps)
  for(i in seq(nreps)){
    ndx <- sample(len,replace=T)
    x.repl <- x[ndx]
    y.repl <- y[ndx]
    f <- ksmooth(x.repl, y.repl, kernel = "normal", bandwidth =  5, n.points = len)
    mat[, i] <- f$y
  }
  # calculating confidence intervals
  ci <- t(apply(mat, 1, quantile, probs = c((1-confidence)/2, (1+confidence)/2)))
  res <- cbind(as.data.frame(f0), ci)
  colnames(res) <- c('x','y', 'lwr.limit','upr.limit')
  res
}

old.par<-par(mfrow=c(3,2))
# ----------------------------------------------------
#  local polynomial smoothing estimator for SBP - white and black
# ----------------------------------------------------
m <- with(mat2, lpsboot(mat2$Age, mat2$Mean.SBP, nreps = 5000))
with(mat2, plot(mat2$Age, mat2$Mean.SBP, las = 1, main="Local Polynomial Smoothing: White and African American", xlab="Age", ylab="Mean log SBP"))
with(m, matpoints(x, m[, - 1], type = 'l', col = c(1, 2, 2), lty = c(1, 2, 2)))
# -------------------------------------------
# kernel smoothing estimator - white and black
# -------------------------------------------
m <- with(mat2, ksboot(mat2$Age, mat2$Mean.SBP, nreps = 5000))
with(mat2, plot(mat2$Age, mat2$Mean.SBP, las = 1, main="Kernal Smoothing: White and African American", xlab="Age", ylab="Mean log SBP"))
with(m, matpoints(x, m[, - 1], type = 'l', col = c(1, 2, 2), lty = c(1, 2, 2)))
# ----------------------------------------------------
#  local polynomial smoothing estimator for SBP - white
# ----------------------------------------------------
m <- with(white, lpsboot(white[,1], white[,4], nreps = 5000))  
with(white, plot(white[,1], white[,4], las = 1, main="Local Polynomial Smoothing: White", xlab="Age", ylab="Mean log SBP")) 
with(m, matpoints(x, m[, - 1], type = 'l', col = c(1, 2, 2), lty = c(1, 2, 2))) 
# -------------------------------------------
# kernel smoothing estimator - white
# -------------------------------------------
m <- with(white, ksboot(white[,1], white[,4], nreps = 5000))
with(white, plot(white[,1], white[,4], las = 1, main="Kernal Smoothing: White", xlab="Age", ylab="Mean log SBP"))
with(m, matpoints(x, m[, - 1], type = 'l', col = c(1, 2, 2), lty = c(1, 2, 2)))
# ----------------------------------------------------
#  local polynomial smoothing estimator for SBP - black
# ----------------------------------------------------
m <- with(black, lpsboot(black[,1], black[,4], nreps = 5000))
with(black, plot(black[,1], black[,4], las = 1, main="Local Polynomial Smoothing: African American", xlab="Age", ylab="Mean log SBP"))
with(m, matpoints(x, m[, - 1], type = 'l', col = c(1, 2, 2), lty = c(1, 2, 2)))
# -------------------------------------------
# kernel smoothing estimator - black
# -------------------------------------------
m <- with(black, ksboot(black[,1], black[,4], nreps = 5000))
with(black, plot(black[,1], black[,4], las = 1, main="Kernal Smoothing: African American", xlab="Age", ylab="Mean log SBP"))
with(m, matpoints(x, m[, - 1], type = 'l', col = c(1, 2, 2), lty = c(1, 2, 2)))
par(old.par)

dbp.par<-par(mfrow=c(3,2))
# ----------------------------------------------------
#  local polynomial smoothing estimator for DBP - white and black
# ----------------------------------------------------
m <- with(mat2, lpsboot(mat2$Age, mat2$Mean.DBP, nreps = 5000))
with(mat2, plot(mat2$Age, mat2$Mean.DBP, las = 1, main="Local Polynomial Smoothing: White and African American", xlab="Age", ylab="Mean Squared DBP"))
with(m, matpoints(x, m[, - 1], type = 'l', col = c(1, 2, 2), lty = c(1, 2, 2)))
# -------------------------------------------
# kernel smoothing estimator for DBP - white and black
# -------------------------------------------
m <- with(mat2, ksboot(mat2$Age, mat2$Mean.DBP, nreps = 5000))
with(mat2, plot(mat2$Age, mat2$Mean.DBP, las = 1, main="Kernal Smoothing: White and African American", xlab="Age", ylab="Mean Squared DBP"))
with(m, matpoints(x, m[, - 1], type = 'l', col = c(1, 2, 2), lty = c(1, 2, 2)))
# ----------------------------------------------------
#  local polynomial smoothing estimator for DBP - white
# ----------------------------------------------------
m <- with(white, lpsboot(white[,1], white[,7], nreps = 5000))  
with(white, plot(white[,1], white[,7], las = 1, main="Local Polynomial Smoothing: White", xlab="Age", ylab="Mean Squared DBP")) 
with(m, matpoints(x, m[, - 1], type = 'l', col = c(1, 2, 2), lty = c(1, 2, 2))) 
# -------------------------------------------
# kernel smoothing estimator for DBP - white
# -------------------------------------------
m <- with(white, ksboot(white[,1], white[,7], nreps = 5000))
with(white, plot(white[,1], white[,7], las = 1, main="Kernal Smoothing: White", xlab="Age", ylab="Mean Squared DBP"))
with(m, matpoints(x, m[, - 1], type = 'l', col = c(1, 2, 2), lty = c(1, 2, 2)))
# ----------------------------------------------------
#  local polynomial smoothing estimator for SBP - black
# ----------------------------------------------------
m <- with(black, lpsboot(black[,1], black[,7], nreps = 5000))
with(black, plot(black[,1], black[,7], las = 1, main="Local Polynomial Smoothing: African American", xlab="Age", ylab="Mean Squared DBP"))
with(m, matpoints(x, m[, - 1], type = 'l', col = c(1, 2, 2), lty = c(1, 2, 2)))
# -------------------------------------------
# kernel smoothing estimator for DBP black
# -------------------------------------------
m <- with(black, ksboot(black[,1], black[,7], nreps = 5000))
with(black, plot(black[,1], black[,7], las = 1, main="Kernal Smoothing: African American", xlab="Age", ylab="Mean Squared DBP"))
with(m, matpoints(x, m[, - 1], type = 'l', col = c(1, 2, 2), lty = c(1, 2, 2)))
par(dbp.par)

# i is the subject  (columns)
# j is the time    (rows)
# Yij <- 21.5 + 0.7*(tij - 5) - 0.05*(tij - 5)^2 + a0i + e1ij
# a0i ~ N(0, 2.5^2), e1ij ~ N(0, 0.5^2)
#n <- length(ui)
#a0i <- rnorm(1, 0, 2.5)
SIM <- function(n){
  m <-10
  j <- seq(1, 10, by = 1)
  u <-replicate(n, sapply(j, function(j) runif(1, j - 1, j)))
  ui <- round(c(u))
  a0i <- rep(rnorm(n, 0, 2.5),each=10)
  e1ij <- rnorm(n*m, 0, 0.5)
  y <- 21.5 + 0.7*(ui - 5) - 0.05*(ui - 5)^2 + a0i + e1ij
  d1 <- data.frame(ID = 1:length(y), age = ui, y1 = y)
  with(d1, split(d1, age))
}
#lapply(SIM(100),function(x) summary(x[,3]))
foo <- function(n){
  X <- SIM(n)
  k <- length(X)
  res <- sapply(1:k, function(i){
    d <- X[[i]]
    y1 <- d[, "y1"]
    m <- mean(y1)
    sd<- sd(y1)
    c(m,sd)
  })  
  #rownames(res) <- c('mean')
  list(res)
}
n<-100
# similuating B probabilities
B <- 500  # change as appropriate
a<-replicate(B, foo(n))
a1<-sapply(a, function(x) list(x))
a1
# Smoothing Estimate by kernel smoothing
foo1 <- function(l){
  l <- a1
  k <- length(l)
  res <- sapply(1:k, function(i){
    d <- l[[i]]
    age <-seq(0, 10, by = 1)
    sm<-ksmooth(age,d,kernel="normal",bandwidth=5,x.points=1:11)$y
    sd<-sd(sm)
    c(sm)
  })
  list(res)
}
foo1(a1)
n1<-data.frame(foo1(a1))
#----------------------------------------
# computing bias, MSE and coverage
#----------------------------------------
#RM.k<-rowMeans(data.frame(foo1(a1)))
#ui<-seq(0, 100, by = 1)
#AM<- 21.5 + 0.7*(ui - 5) - 0.05*(ui - 5)^2
#bias.k<-RM.k-AM
#MSE.k=apply(data.frame(foo1(a1)),1, function(x) mean((x-AM)^2))
#bias.k
#MSE.k
#---------------------]=----------------------------------
# Smoothing Estimate by local polynomial smoothing
#-------------------------------------------------------
library(KernSmooth)
foo2 <- function(l){
  l <- a1
  k <- length(l)
  res <- sapply(1:k, function(i){
    d <- l[[i]]
    age <-seq(0, 10, by = 1)
    sm<-locpoly(age,d,kernel="normal",bandwidth=5,gridsize=11)$y
    c(sm)
  })
  list(res)
}
foo2(a1)
n2<-data.frame(foo2(a1))
#------------------------------------
# LP - computing bias, MSE, Coverage
#------------------------------------
library(matrixStats)
P<-ui<-seq(0, 10, by = 1)
Q<- 21.5 + 0.7*(P - 5) - 0.05*(P - 5)^2
vec<-matrix(NA,11,7)
for(i in 0:10){
  bias.k<-rowMeans(n1[i+1,])
  MSE.k<-sqrt(mean((n1[i+1,]-Q[i+1])^2))
  cover.k<-(ifelse((2*rowSds(as.matrix(n1[i+1,])))<=Q,1,0))
  bias.l<-rowMeans(n2[i+1,])
  MSE.l<-sqrt(mean((n2[i+1,]-Q[i+1])^2))
  cover.l<-(ifelse((2*rowSds(as.matrix(n2[i+1,])))<=Q,1,0))
  one<-sqrt(mean((n2[i+1,]-Q[i+1])^2))/sqrt(mean((n1[i+1,]-Q[i+1])^2))
  vec[i,]<-c(bias.k,MSE.k,cover.k,bias.l,MSE.l,cover.l,one)
}
vec


