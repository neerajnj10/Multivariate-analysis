## STAT 541: Assignment 4

The diabetes data from the Reaven and Miller study is available in the mult package.  The following variables were measures on 145 patients:  
> RelWt - relative weight  
> GluFast - blood sugar level prior to the glucose tolerance test  
> GluTest - average blood sugar level during the test  
> InsTest - average insulin level during the test  
> SSPG - a measure of how glucose and insulin interact  
> EClass -  
> CClass - clinical diagnosis (3=Normal, 2=Chemical Diabetic, 1=Overt Diabetic)  
            
#### 1. Examine the relationship among the numeric variables given above (also include GluDiff=GluTest-GluFast) for each of the three groups defined by CClass. This should include graphical techniques and PCAs (e.g., `princomp`) as well as multivariate normality plots. Characterize the differences among the three groups.  

```{r}
library(mult)
data(diabetes)
head(diabetes)
attach(diabetes)

dia1 <- subset(diabetes,CClass=="1") #subset diabetes data for CClass is 1(overt diabetic)
dia2 <- subset(diabetes,CClass=="2") #subset diabetes data for CClass is 2(chemical diabetic)
dia3 <- subset(diabetes,CClass=="3") #subset diabetes data for CClass is 3(normal)
X1 <- dia1[,1:6] 
X2 <- dia2[,1:6]
X3 <- dia3[,1:6]
round(X1cor <- cor(X1),2) #correlation among numeric variables for CClass = overt diabetic
round(X2cor <- cor(X2),2)  #correlation among numeric variables for CClass =chemical diabetic
round(X3cor <- cor(X3),2) #correlaiton among numeric variables for CClass=normal
pairs(diabetes[1:6],pch=21,bg=c("red","green","blue")[unclass(CClass)])
X1.pca <- pca(X1) #pca for numeric variables when cclass is 1 
X2.pca <- pca(X2) #pca for numeric variables when cclass is 2
X3.pca <- pca(X3) #pca for numeric variables when cclass is 3

normalQQ(X1[,], nr=2, nc=4)
normalQQ(X2[,], nr=2, nc=4)
normalQQ(X3[,], nr=2, nc=4)
```


Variables GluDiff and GluTest are highly related, there is a collinear problem. We might want to only useing GluDiff for analysis.  
From the scatterplot, variable RelWeight might not be a useful variable to predict the daibetic groups. 



#### 2. Discriminate among the groups using graphical and modeling techniques. Give the basic summaries of the `lda` model. Consider which variables are the best discriminators. Interpret the results by plotting in discriminate space and examining the canonical correlations, i.e.,	the correlations between the discriminant and original variables, etc. 

```{r}
library(MASS)
diab.vars <- cbind(RelWeight, GluFast, GluTest, GluDiff, InsTest, SSPG)
(diab.lda <- lda(diab.vars, CClass))
diab.pred <- predict(diab.lda, dimen=2)$x
cor(diab.pred, diab.vars)
eqscplot(diab.pred, type="n", xlab = "Disc1", ylab = "Disc2")
text(diab.pred, labels=as.character(CClass), col = 3 + 2 * unclass(CClass), cex = 0.6)
```


The first discriminant variable is most important and most strongly related to GluFast, GluTest, GluDiff, SSPG. The second discriminant variable is strongly related to InsTest. 
From the plots, we can see Overt Diabetic is a lot different from groups Normal and Chemical Diabetic. There is not much discrimination between groups Normal and Chemical Diabetic. 



#### 3. Perform classifications using `lda` and `qda` on the entire data set. Discuss the results in terms of the confusion matrices. Also, assess using LOOCV for both 'lda` and `qda`. Discuss.


```{r}

# learning from correlation above, we will make some changes in selection of variables, 
#that is, remove `gluTest` from our classification models below now.

diab.var <- cbind(RelWeight, GluFast, GluDiff, InsTest, SSPG)

#classification using lda.

#lda deals okay with collinear variables, so we may choose to keep or remove them.

library(MASS)
dia.lda <- lda(diab.var, CClass)
dia.lda$means
dia.lda$scaling
dia.pred <- predict(dia.lda)$class
table(CClass, dia.pred)

#further more.
train <- sample(1:145, 100)
table(diabetes$CClass[train])
dia.lda.train <- lda(CClass ~ ., diabetes, prior = c(1,1,1)/3, subset = train)
diabe.predict <- predict(dia.lda.train, diabetes[-train, ])$class
table(diabetes$CClass[-train], diabe.predict)

#no error in 1st class, only 3 are wrongly classified in 2nd, and 4 again in 3rd class, 
#hence very high accuracy.


#Using leave-one-out cross validation (LOOCV) now.

diab.lda.cv <- lda(CClass ~ ., diabetes, prior = c(1,1,1)/3, CV=TRUE)
table(diabetes$CClass, diab.lda.cv$class)

#no error in first class, in second class 9 are wrongly classified, in 3rd class 8 are 
#wrongly classified.


# Now using only variables that are not correlated.

training <- diabetes[train,]
training <- training[-c(3,7)] #remove 3rd variable and 7th variable from consideration as 
                               #they are not required.
test <- diabetes[-train,]
test <- test[-c(3,7)] #same as above, remove redundant variables.



dia.lda.train1 <- lda(CClass ~ RelWeight +GluFast +GluDiff + InsTest +SSPG, training)
diabe.predict1 <- predict(dia.lda.train1, test)$class
table(diabetes$CClass[-train], diabe.predict)
# 0 error in 1st, 3 in 2nd, 4 in 3rd.




lda.cv <- lda(CClass ~ RelWeight +GluFast +GluDiff + InsTest +SSPG, diabetes,  prior = c(1,1,1)/3, CV=TRUE)
table(diabetes$CClass, lda.cv$class)
#0 error in 1st, 11 in 2nd, and 3 misclassified in 3rd class for LOOCV.

  
```



```{r}  
# classification using qda.


#for qda, we will have to deal with collinear variables as they are not handled well in the model, 
#so we will remove #it, like we did in diab.var.

qdiab <- qda(diab.var, CClass)
qpred <- predict(qdiab)$class
table(CClass, qpred)

#furthermore.

qdia.train <- qda(CClass ~ RelWeight +GluFast +GluDiff + InsTest +SSPG, training)
qdiabpred <- predict(qdia.train, test)$class
table(test$CClass, qdiabpred)

# O misclassified in 1st class, 2 in 2nd class, 2 in 3rd class.


#Using leave-one-out cross validation (LOOCV) now.

diab.qda.cv <- qda(CClass ~ RelWeight +GluFast +GluDiff + InsTest +SSPG, training,  
                   prior = c(1,1,1)/3, CV=TRUE)
table(training$CClass, diab.qda.cv$class)
#for training set, 2 error in 1sst class, 6 in 2nd, and 2 in 3rd class, high accuracy.

qda.cv <- qda(CClass ~ RelWeight +GluFast +GluDiff + InsTest +SSPG, diabetes,
              prior = c(1,1,1)/3, CV=TRUE)
table(diabetes$CClass, qda.cv$class)

#on entire dataset, 3 error in 1st class, 9 in 2nd, and 1 in 3rd class. Pretty good acuracy.
```
            
#### 4. Combine chemical and overt diabetics into a single diabetic group. Based on the results of item 2, justify why this is or is not a good idea. Discriminate between the normal and diabetic groups using appropriate variables and interpret the results.  

```{r}
CClass[CClass== 2] <- 1   # change the display of level 2 to "1" (same as level 1) 
CClass[CClass== 3] <- 0   
CClass <- factor(CClass) 

(diab.lda2 <- lda(diab.var, CClass))
diab.pred2 <- predict(diab.lda2, dimen=2)
cor(diab.pred2$x, diab.var)
plot(diab.pred2$x, type="n", xlab = "Disc1", ylab = "Disc2")
text(diab.pred2$x, labels=as.character(CClass), col = 3 + 1 * unclass(CClass), cex = 0.6)

```



The combine process did better discrimination analysis for the diabetes dataset. From question 2, we cannot see the major differences between normal and chemical diabetic groups. After we combine the two diabetics groups, we get better result shows that the discrimination between the Normal and Diabetic groups. 
The discriminant variable is most strongly related to GluTest, GluDiff, and SSPG. 
