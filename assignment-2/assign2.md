## STAT 541: Assignment 2


Use the state-crime data for this assignment.

The state-crime dataset contains the name and the following variables for the 50 states:
> Abbr (descriptor)  
> Unemploy, Police, InSchool (numeric \(X\) variables)  
> Murder, Rape, Robbery, Assault, Burglary, Larceny, Auto (numeric \(Y\) variables)  

Use R to complete the following:

#### 1. Compute $S_{X}, S_{XY}, \mbox{and} \  S_{Y}$.
		
```{r}
crime <- read.csv("state-crime.dat",header=TRUE,sep=",")
attach(crime)
head(crime)
Y <- crime[,5:11]
X <- crime[,12:14]
round(Xcov <- cov(X),2)
round(YCov <- cov(Y),2)
round(XYcov <- cov(X,Y),2)
```
         
#### 2. Compute the sample correlation matrix ($q \times p$) between the $X$ and $Y$ variables. Test individual correlations for significance. Discuss your results.
		
```{r}
round(cor(X,Y),3)
```

Rape is not correlated to any X variables. 
                  
#### 3. Compute the multiple correlation between each $Y$ variable and the $X$ variables. Test for significance. What do you conclude?
		
```{r}
library(mult)
Y1 <- Murder
mult.corr(X,Y1)
Y2 <- Rape
mult.corr(X,Y2)
Y3 <- Robbery
mult.corr(X,Y3)
Y4 <- Assault
mult.corr(X,Y4)
Y5 <- Burglary
mult.corr(X,Y5)
Y6 <- Larceny
mult.corr(X,Y6)
Y7 <- Auto
mult.corr(X,Y7)
```

The multiple correlation coefficient of Rape and the X variables is not significant. We can remove the variable Rape for regression analysis. 
         
#### 4. Compute the partial regression coefficients, i.e., partial correlations, between each $Y$ variable and each $X$ variable adjusting for the other $X$ variables. Test the significance of each. What do you conclude? How do you conclusions differ from those in quesiton 2?
         
```{r}
library(mult)
crime.pcor <- par.corr(X,Y)
crime.pcor$partial.corr
lm1 <- lm(Murder ~ Unemploy+Police+InSchool)
(sum1 <- summary(lm1))

lm2 <- lm(Rape ~ Unemploy+Police+InSchool)
(sum2 <- summary(lm2))
lm3 <- lm(Robbery ~ Unemploy+Police+InSchool)
(sum3 <- summary(lm3))
lm4 <- lm(Assault ~ Unemploy+Police+InSchool)
(sum4 <- summary(lm4))
lm5 <- lm(Burglary ~ Unemploy+Police+InSchool)
(sum5 <- summary(lm5))
lm6 <- lm(Larceny ~ Unemploy+Police+InSchool)
(sum6 <- summary(lm6))
lm7 <- lm(Auto ~ Unemploy+Police+InSchool)
(sum7 <- summary(lm7))

```

The partial regression coefficient for Murder and InSchool is not significant. Rape and X varialbes are not significant correlated. Assault and InSchool are not significant.  
Burglary and unemploy, Burglary and InSchool are not significant.
         
#### 5. Compute the canonical correlations between the $X$ variables and the $Y$ variables. Sequentially test the canonical correlations for significance. What do you conclude?
         
```{r}
can.corr(X,Y)
```

Depends on Chi-square, 2 pairs of the 3 canonical correlations are significant.  
         
#### 6. Compute the canonical variable scores based on the centered data matrices.
         
```{r}
cancor(X,Y)
(U <- cancor(X,Y)$xcenter %*% can.corr(X,Y)$B)
(V <- cancor(X,Y)$ycenter %*% can.corr(X,Y)$A)
```

         
#### 7. Plot the canonical variables, i.e., $U_i$ versus $V_i$, corresponding to the significant canonical correlations. Comment on the canonical relationships.
         
```{r}
par(mfrow=c(1,2))
can.plot(X,Y)
```
The plots show the first pair and second pair of canonical scores are reasonably normal in distribution. But the third pair of canonical scores doesn't show strong realtionship between each other. 
         
#### 8. Approximate $S_{XY}$ by a biplot. Discuss the relationships between the $X$ and $Y$ variables in terms of this biplot.
         
```{r}

can.biplot(X,Y)

```

covariances between the variables are approximated by the angles between gi and hj. 
One dimension for Unemploy associate to Larceny.
The other Y variables are in the direction associate to variable InSchool and Police. 
