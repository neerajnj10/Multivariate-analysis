Assignment 1

Use the state-crime data for this assignment.

The state-crime data contain (among others) the name and the following variables for 50 states:  
> Abbr (descriptor)  
> Murder, Rape, Robbery, Assault, Burglary, Larceny, Auto (numeric $Y$ variables)

Use R to complete the following:

#### 1. Create the $50 \times 7$ data matrix $\mathbf{Y}$ for the numeric crime variables using the above order for columns. Compute the mean vector for the $Y$ variables.

```{r}
library(mult)
library(matrixStats)
library(e1071)
library(car)
crime <- read.csv("state-crime.dat")
head(crime)
Y <- as.matrix(crime[,5:11])
head(Y)
crime_mean <- apply(Y, 2, function(x) mean(x))
crime_mean

#Or this way

cr_mean <- round(colMeans(Y),2)
cr_mean
```

#### 2. Compute:  $\frac{1}{\sqrt{n-1}}(\mathbf{Y}-\mathbf{\bar{Y}})$. 

```{r}
n<- nrow(Y)
crime_sqrt <- sweep(Y, 2, cr_mean, FUN="-")/sqrt(n - 1)
head(crime_sqrt)
```

#### 3. Compute the covariance matrix of the $Y$ variables (do not use a built-in function).  

```{r}
crime_svd <- svd(crime_sqrt)
A <- crime_svd$v[,]
dimnames(A) <- list(names(Y), c("V1", "V2", "V3", "V4","V5","V6", "V7"))
round(crime_cov <- t(A) %*% cov(Y) %*% A, 4)
```

#### 4. Compute the robust mean vector and covariance matrix. Are there multivariate outliers? Discuss.  

```{r}
Mest <- huber.estimates(Y)

round(Mest$huber.mean, 2)
# with traditional method, not extremely different
round(cr_mean,2)

round(Mest$huber.cov, 1)
crime_cov <- round(cov(Y), 1)
crime_cov
#some of the data in covariance matrices are different.


## To find the outliers we use following procedure.
round(Mest$weights, 3)
#data suggests we do not have an expensive outlier to sub.
```

#### 5. Compute:  $\frac{1}{\sqrt{n-1}}(\mathbf{Y}-\mathbf{\bar{Y}})\mathbf{D}_{\frac{1}{s_j}}$.  

```{r}
D <- diag(1/sqrt(diag(crime_cov)))
crime_D <- crime_sqrt %*% D
head(crime_D)
```

#### 6. Compute the correlation matrix of the \(Y\) variables (do not use a built-in function).  

```{r}
R <- D %*% crime_cov %*% D
dimnames(R) <- list(names(Y), names(Y))
round(R, 3)

#visualize correlation
pairs(Y,
  panel = function (x, y, ...) {
    points(x, y, ...)
    abline(lm(y ~ x), col = "grey")
  }, pch = ".", cex = 2)
```

#### 7. Give univariate summaries for each of the numeric crime variables.  

```{r}
univariate_summary<- apply(Y,2,function(x) summary(x))
univariate_summary

#experimenting with new package (mentioned above) I discovered recently.
fun1<-function(x){
summ<-
rbind(colMins(x),colQuantiles(x)[,2],colMedians(x),colMeans(x),colSds(x),colQuantiles(x)[,4],colIQRs(x),colMaxs(x))
    
row.names(summ)<-c("Min.","1st Qu.","Median","Mean","sd","3rd Qu.","IQR","Max.")
summ
}
set.seed(125)
fun1(Y)
```




#### 8. Make normal probability plots for these variables and assess normality.  


$shapiro.test$ works well for data below the sample size that ranges in hundreds.

```{r}
normalQQ(Y[,], nr=2, nc=4)
```

- `Murder` = **platykurtic**
- `Rape` = **positive skewed** 
- `Robbery` = **slightly positive skewed**
- `Assualt` = **normally assumed**
- `Burglary` = **slightly positive skewed, normality assumed**
- `Larceny` = **leptokurtic**
- `Auto` = **slightly negative skewed**


```{r}
#theoratical assessment based on shapiro (very popular) test.
shapiro.test(Y[,"Murder"]) #p- value <=0.05 rejects the Null hypothesis that data comes from normal distribution
shapiro.test(Y[,"Rape"])
shapiro.test(Y[,"Robbery"])
shapiro.test(Y[,"Assault"])
shapiro.test(Y[,"Burglary"])
shapiro.test(Y[,"Larceny"])
shapiro.test(Y[,"Auto"])
```


*In the end it would be good to know this. Someone mentioned-  Normality tests don't do what most think they do. Shapiro's test, Anderson Darling, and others are null hypothesis tests AGAINST the the assumption of normality. These should not be used to determine whether to use normal theory statistical procedures. In fact they are of virtually no value to the data analyst. Under what conditions are we interested in rejecting the null hypothesis that the data are normally distributed? I have never come across a situation where a normal test is the right thing to do. When the sample size is small, even big departures from normality are not detected, and when your sample size is large, even the smallest deviation from normality will lead to a rejected null.*



#### 9. Transform as appropriate and confirm normality by examining the transformed normal probability plots.  

```{r}
#only selecting the following variable based on shapior test performed above on individual variables and assessing their results.
p1 <- log(Y[,"Murder"])
p2 <- log(Y[,"Rape"])
p3 <- log(Y[,"Robbery"])
p7 <- log(Y[,"Auto"])


#std=function(x){
#  if(length(which(is.na(x)))==0)
#  (x-mean(x))/sd(x)
#  else(x-mean(x,na.rm=T))/sd(x,na.rm=T)
#}
#s <-std(Y[,"Murder"])
#s <- scale(Y[,"Murder"])
#shapiro.test(s)
#Shapiro-Wilk normality test

#data:  s
#W = 0.9524, p-value = 0.04286

#above mentioned standardization methodologies were tried apart from log transformation for Murder and Robbery variable as saphire test did not change the p value drastically for these two. However it was found the scaling as well as standardization around meand and standard deviation remained the same irrespective of the transformation applied for Murder, while for Robbery, the log transformation improved the p value but not drastically enough. We would therefore consider the log transform and contine with the results.


shapiro.test(p1) #did not change
shapiro.test(p2) #good
shapiro.test(p3)#slightly better
shapiro.test(p7) #good

#plotting only for above mentioned variables.
qqnorm(p1)
q1 <- quantile(p1, probs = seq(0.25, 0.75, 0.25))
abline(h=q1)

qqnorm(p2)
q2 <- quantile(p2, probs = seq(0.25, 0.75, 0.25))
abline(h=q2)

qqnorm(p3)
q3 <- quantile(p3, probs = seq(0.25, 0.75, 0.25))
abline(h=q3)

qqnorm(p7)
q7 <- quantile(p7, probs = seq(0.25, 0.75, 0.25))
abline(h=q7)
Y1 <- cbind(p1, p2, p3, Y[,"Assault"], Y[,"Burglary"], Y[,"Larceny"], p7 )
colnames(Y1) <- c("Murder_trans", "Rape_trans", "Robbery_trans", "Assault_trans",
                  "Burglary_trans", "Larceny_trans", "Auto_trans")
```






#### 10. Assess multivariate normality of the original and transformed variables using a gamma probability plot.  

```{r}
gammaQQ(Y)
gammaQQ(Y1)
```
