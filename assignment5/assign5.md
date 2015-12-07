## Assignment 5

In this assignment, clustering and MDS will be done on the air pollution data (airpoll.dat in Chapter 2 of Everitt).  The following variables were measured on 60 metropolitan areas:  
> Rainfall  
> Education  
> Popden  
> Nonwhite  
> NOX  
> SO2  
> Mortality  

The full dataset is too large to do this assignment. To limit the number of observations, use the following metropolitan areas:
```{r}
source("airpoll.dat")
airpoll1 <- airpoll[c(59, 12, 55, 5, 19, 35, 27, 34),]
airpoll1
```  
            
#### 1. Perform single, complete, and average linkage cluster analyses on the rows (metropolitan areas) of the standardized data set. Present the dendrograms for these analyses.
```{r}
summary(airpoll1)
air.sc <- scale(airpoll1)
(D <- dist(air.sc))


# single linkage
h.single <- hclust(D, method="single")
plot(h.single, labels = rownames(airpoll1), ann = F)
rect.hclust(h.single, k=3)
h.single.history <- cbind(h.single$merge, h.single$height)
colnames(h.single.history) <- c("leader", "joiner",  "distance")
rownames(h.single.history) <- paste("C",1:(nrow(airpoll1)-1))
h.single.history



#complete linkage
h.complete <- hclust(D, method="complete")
plot(h.complete, labels = rownames(airpoll1), ann = F)
rect.hclust(h.complete, k=3)
h.complete.history <- cbind(h.complete$merge, h.complete$height)
colnames(h.complete.history) <- c("leader", "joiner",  "distance")
rownames(h.complete.history) <- paste("C",1:(nrow(airpoll1)-1))
h.complete.history


#average linkage
h.average <- hclust(D, method="average")
plot(h.average, labels = rownames(airpoll1), ann = F)
rect.hclust(h.average, k=3)
h.average.history <- cbind(h.average$merge, h.average$height)
colnames(h.average.history) <- c("leader", "joiner",  "distance")
rownames(h.average.history) <- paste("C",1:(nrow(airpoll1)-1))
h.average.history
```



#### 2. Compute the derived dissimilarity matrix for each of the analyses. Compare the clustering approaches using a Euclidean distance measure on the "residuals" and compute the cophenetic correlations. Based on these assessment measures, which method do you recommend? Why?
```{r}
#single
(D.single <- cophenetic(h.single))
sqrt(sum((as.vector(D) - D.single)^2))
cor(as.vector(D), D.single)

#complete
(D.complete <- cophenetic(h.complete))
sqrt(sum((as.vector(D) - D.complete)^2))
cor(as.vector(D), D.complete)


#average
(D.average <- cophenetic(h.average))
sqrt(sum((as.vector(D) - D.average)^2))
cor(as.vector(D), D.average)

```

- I would recommend `average linkage` analysis, because the correlation between D???D^, the residuals of the clustering "model," is good and the euclidean distance is least in this case, hence a better fit.


#### 3. Perform a non-hierarchical cluster analysis using $k$-means. Use the results of your best hierarchical cluster analysis and its dendrogram to determine the expected number of clusters $k$ and to get the initial seed points. How does the non-hierarchical clustering compare to your best hierarchical clustering?

```{r}
cutree(h.single, 3)
initial.points1 <- tapply(air.sc, list(rep(cutree(h.single, 3), ncol(air.sc)),
                                       col(air.sc)), mean)
dimnames(initial.points1) <- list(NULL, dimnames(air.sc)[[2]])
initial.points1
k.means1 <- kmeans(air.sc, initial.points1)
k.means1$cluster
k.means1$centers

cutree(h.complete, 3)
initial.points2 <- tapply(air.sc, list(rep(cutree(h.complete, 3), ncol(air.sc)),
                                       col(air.sc)), mean)
dimnames(initial.points2) <- list(NULL, dimnames(air.sc)[[2]])
initial.points2
k.means2 <- kmeans(air.sc, initial.points2)
k.means2$cluster
k.means2$centers


#on average linkage-best one.
cutree(h.average, 3)
initial.points <- tapply(air.sc, list(rep(cutree(h.average, 3), ncol(air.sc)),
                                       col(air.sc)), mean)
dimnames(initial.points) <- list(NULL, dimnames(air.sc)[[2]])
initial.points
k.means <- kmeans(air.sc, initial.points)
k.means$cluster
k.means$centers
```

- single-linkage produced little confusion in clsutering with that k-means. `chicagoIL` was wrongly clustered.

- cutting the complete-linkage produced identical clusters as found by k-means.

- Not surprisingly, the clusters found by k-means are identical to those found by cutting the average-linkage tree, (which is aso our best hierarchical clustering found previously) into three groups. 


#### 4. Perform metric, Sammom, and nonmetric MDS analyses on the rows of the data set. Determine the dimensionality for each method. Plot the low-dimensional representation for each method and interpret as appropriate. Contrast the three methods.

```{r}
#metric MDS

library(MASS)
dist.c <- dist(airpoll1)
air.cmds1 <- cmdscale(dist.c, k=1, eig=TRUE, x.ret=TRUE)
air.cmds2 <- cmdscale(dist.c, k=2, eig=TRUE, x.ret=TRUE)
air.cmds3 <- cmdscale(dist.c, k=3, eig=TRUE, x.ret=TRUE)
air.cmds2$eig[1:4]

air.cmds2$points[,1] <- -air.cmds2$points[,1]
eqscplot(air.cmds2$points, type = "n", xlab="", ylab="")
text(air.cmds2$points, labels = as.character(substring(row(airpoll1), 1, 2)), col = 3 + 2*unclass(row(airpoll1)), 
     cex =1.5)


#sammon MDS

dist.s <- dist(airpoll1)
air.smds1 <- sammon(dist.s, k=1)
air.smds2 <- sammon(dist.s, k=2)
air.smds3 <- sammon(dist.s, k=3)
air.smds2$points[,1] <- -air.smds2$points[,1]
eqscplot(air.smds2$points, type = "n", xlab="", ylab="")
text(air.smds2$points, labels = as.character(substring(row(airpoll1), 1, 2)), col = 3 + 2*unclass(row(airpoll1)), cex = 1.5)


#non metric MDS

dist.n <- dist(airpoll1)
air.nmds1 <- isoMDS(dist.n, k=1)
air.nmds2 <- isoMDS(dist.n, k=2)
air.nmds3 <- isoMDS(dist.n, k=3)
air.nmds2$points[,1] <- -air.nmds2$points[,1]
eqscplot(air.nmds2$points, type = "n", xlab="", ylab="")
text(air.nmds2$points, labels = as.character(substring(row(airpoll1), 1, 2)),
     col = 3 + 2*unclass(row(airpoll1)), cex = 1.5)



#determing dimensionality

#metric MDS

dist.f1 <- dist(air.cmds1$points)
dist.f2 <- dist(air.cmds2$points)
dist.f3 <- dist(air.cmds3$points)
S.cl1 <- sum(dist.c^2 - dist.f1^2)/sum(dist.c^2)
S.cl2 <- sum(dist.c^2 - dist.f2^2)/sum(dist.c^2)
S.cl3 <- sum(dist.c^2 - dist.f3^2)/sum(dist.c^2)
plot(1:3, c(S.cl1, S.cl2, S.cl3), xlab="Dimension = k", ylab="Classical Stress")
lines(1:3, c(S.cl1, S.cl2, S.cl3))



#sammom
air1.smds1 <- sammon(dist.s, k=1)
air1.smds2 <- sammon(dist.s, k=2)
air1.smds3 <- sammon(dist.s, k=3)
dist.sf1 <- dist(air1.smds1$points)
dist.sf2 <- dist(air1.smds2$points)
dist.sf3 <- dist(air1.smds3$points)
S.s1 <- sum((dist.s - dist.sf1)^2)/sum(dist.s)
S.s2 <- sum((dist.s - dist.sf2)^2)/sum(dist.s)
S.s3 <- sum((dist.s - dist.sf3)^2)/sum(dist.s)
plot(1:3, c(S.s1, S.s2, S.s3), xlab="Dimension = k", ylab="Sammon Stress")
lines(1:3, c(S.s1, S.s2, S.s3))


#nonmetric MDS
S.nm1 <- air.nmds1$stress/100
S.nm2 <- air.nmds2$stress/100
S.nm3 <- air.nmds3$stress/100
plot(1:3, c(S.nm1, S.nm2, S.nm3), xlab="Dimension = k", ylab="Nonmetric Stress")
lines(1:3, c(S.nm1, S.nm2, S.nm3))


```


- The 2-dimensional representation shows the same structure as the discriminant fit in two dimensions.
- The 2-dimensional representation shows the same structure as the discriminant fit in two dimensions and a nearly identical representation as classical MDS.
- The 2-dimensional representation shows the same structure as the discriminant fit in two dimensions and the nonmetric MDS representation is nearly identical to the classical and Sammon MDS representations as you can see from 2-dimensional (low dimension) representation.

-  The dimensionality is chosen at the elbow point. 
- The screen plot for classical scaling shows that a 2-dimensional representation provides a good fit, but k=3 shows `some` improvement.
- The scree plot for Sammom scaling shows that a 2-dimensional representation provides a good fit, but k=3 shows `little` improvement.
- The screen plot for nonmmetric scaling shows that a 2-dimensional representation provides a good fit, but k=3 shows improvement. (more than previous two).
