## STAT 541: Assignment 3

The state-crime data contain the name and the following variables for 50 states:  
> Abbr (descriptor)  
> Unemploy, Police, InSchool (numeric \(X\) variables)   
> Murder, Rape, Robbery, Assault, Burglary, Larceny, Auto (numeric \(Y\) variables)  

```{r}
library(mult)
data(crime)
dim(crime)
head(crime)
```

Use R to complete the following:
            
#### 1. Compute the principal variables of the numeric \(Y\) variables based on the covariance matrix. Determine how many principal variables are meaningful.
```{r}
Y <- crime[,5:11]
X <- crime[, 12:14]
####
Y.prcomp.cov <- prcomp(Y)
summary(Y.prcomp.cov)
```


- From our results, we can infer that first two component variables are meaningful to us based on the proportion of variation explained by them which we will further interpret using plots next.


#### 2. Interpret these variables using plots (e.g., a biplot, $V_1$ vs. $V_2$, etc) and correlatons with the $Y$ variables as appropriate. Discuss problems related to the scaling of the variables.
```{r}
biplot(Y.prcomp.cov)
barplot(Y.prcomp.cov$sdev/Y.prcomp.cov$sdev[1])
screeplot(Y.prcomp.cov, type="lines")
#correlation of component variables with Y variable.
loadings <- Y.prcomp.cov$rotation
loadings
#further we can see:
sd1 <- Y.prcomp.cov$sdev
sd1
```

- Barplot and biplot further shows that after the first 2 components there is steep drop in standard deviation after the two. 

- From *loadings* , we find following: This means that the first principal component is a linear combination of the variables: `-0.0018*Murder`  `-0.0097*Rape` `-0.0679*Robbery` `-0.1295*Assaul`t `-0.3711*Burglary` `-0.9010*Larceny` , where each variable are the standardised versions of the variables originally in data (that each have mean of 0 and variance of 1)

- PC1 has all negative values of Y variables, while PC2 has all the variables with postive values for absolute loadings, except larceny. Hence PC2 represents a contrast between the V1, V2, V3, V4, V5, V7, and of V6. V1...7 are respectively the Y variables, while there is no contarst in PC1.

- PC1 is highly correlated with `Larceny`, while PC2 is highly correlated with `Auot` and `Assault`.


#### 3. Compute the principal variables of the numeric \(Y\) variables based on the correlation matrix. Determine how many principal variables are meaningful.
```{r}
Y.prcomp.cor <- prcomp(Y, scale=TRUE)
summary(Y.prcomp.cor)
```

- From our result first 4 components look meaningful, looking at the cumulative porportion.


#### 4. Interpret these variables using plots and correlatoins with the $Y$ variables. Compare these results with those in 2.
```{r}
biplot(Y.prcomp.cor)
barplot(Y.prcomp.cor$sdev/Y.prcomp.cor$sdev[1])
screeplot(Y.prcomp.cor, type="lines")
#correlation of component variables with Y variable.
loading.cor <- Y.prcomp.cor$rotation
loading.cor
#further we can see:
sd2 <- Y.prcomp.cor$sdev
sd2
sd2^2
```

- Usually we take the components that show variance equal to more than 1. In this case, graphically it is first 2 components.

- Results from 2 show that PC1 has higher most of no. of data points and variables exlained by it, while PC2 has a single variable with its axis, while the results obtained here shows `almost` fairly equal distribution of data points and variables between the two components.

- Here `Assault`, `Burglary` are *somewhat* correlated to PC1 , while PC is correlated highly with `larceny`, and somewhat with `rape` and `robbery` as compared to results from question 2 above.


#### 5. Compute the $X$ and $Y$ variables based on the correlation matrix. Determine how many principal variables are meaningful.
```{r}
xycor<-prcomp(cbind(Y,X), scale= TRUE)
summary(xycor) 
```

- we can assume first 6 components are meaningful from the result obtained as cumulative variance explained by 6 components is over 90% and is sufficient for explanation.


#### 6. Interpret these variables using plots and correlations with the $X$ and $Y$ variables.
as.
```{r}
biplot(xycor)
barplot(xycor$sdev/xycor$sdev[1])
screeplot(xycor, type="lines")
#correlation of component variables with Y variable.
loading.xycor <- xycor$rotation
loading.xycor
#further we can see:
sd3 <- xycor$sdev
sd3
```

- Screenplot indicates the prospect of using 3 principal components as meaningful as they describe the variance of over and about 1.

- biplot and loading values indicate the correlation between the components and variables, where PC1 has *somewhat* correlation with `Robbery`, `Assault`, while PC2 has high correlation with `Larceny`, `InSchool`.

#### 7. Compute the ``constrained'' principal variables using the numeric \(X\) variables to explain the variation in the numeric \(Y\) variable. Interpret these variables using plots (e.g., a biplot)
```{r}
constrained.can <- pca.can(Y,X, alpha = 0.5)
constrained.can$U.var
constrained.can$U.p
constrained.can$U.coeff
constrained.can$U.corr

```

- First canonical `U1` variables explain almost 80% of the variance in Y variable. 
- U1 component is highly coorelated with `Police` and somewhat with `Inschool`, U2 is not highly correlated with any variable.
