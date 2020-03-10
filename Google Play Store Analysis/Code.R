### Installing libraries

source("DataAnalyticsFunctions.R")
library(ggplot2)
library(glmnet)
library(arulesViz)
library(tidyverse)
library(highcharter) 
library(lubridate)
library(stringr)
library(xts)
install.packages("tree")
library(tree)
install.packages("partykit")
library(partykit)
install.packages("randomForest")
library(randomForest)

### Reading the dataset

googleplaystore <- read.csv("googleplaystore.csv")
head(googleplaystore)

### Cleaning the dataset

clean_google <- googleplaystore %>%
  mutate(
    # Change Installs into numeric values
    Installs = gsub("\\+", "", as.character(Installs)),
    Installs = as.numeric(gsub(",", "", Installs)),
    ## If we change Installs into numeric values, we should get rid of "+" signs. Do you think it's a good idea?
    # Convert reviews to numeric
    Reviews = as.numeric(Reviews),
    # Eliminate "M" in Size column and change in to numeric value
    Size = gsub("M", "", Size),
    # Eliminate "k" and substitute with 0
    Size = ifelse(grepl("k",Size), as.numeric(gsub("k", "", Size)), as.numeric(Size)*1024),
    # Remove currency symbol from price and change it into numeric value
    Price = as.numeric(gsub("\\$", "", as.character(Price))),
    # Change Last updated column into date format
    Last.Updated = mdy(Last.Updated)
  )
clean_google <- clean_google %>%
  mutate(logInstall = log(Installs))

## Create days column in data frame
clean_google$days <- as.Date(as.character("2018-09-08"), format="%Y-%m-%d") - as.Date(as.character(clean_google$Last.Updated), format="%Y-%m-%d")
drop <- c("App", "Genres", "Current.Ver", "Installs", "Last.Updated", "Android.Ver")
clean_google = clean_google[,!(names(clean_google) %in% drop)]

clean_google_nona <- na.omit(clean_google)
head(clean_google_nona,5)

### Data Visualization

### Scatter plot of Rating vs log(no of installs)
ggplot(clean_google,aes(x=log(Installs),y=Rating))+
  geom_point()+
  scale_y_continuous(limits=c(1,5))+
  theme(axis.text.x = element_text(angle = 90)) +
  geom_smooth(method='lm')

### Density plot of Ratings, we see that average rating is quite high
ggplot(clean_google,aes(x=Rating))+
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=mean(na.omit(Rating))),
             color="red", linetype="dashed", size=1)+
  scale_x_continuous(limits=c(1,5))

### Count of apps for each category
ggplot(clean_google,aes(x=fct_infreq(factor(Category)),fill=Category))+
  geom_histogram(stat='count')+
  theme(axis.text.x = element_text(angle = 90),legend.position = "none")+
  xlab('Category')

### Boxplots for different categories. 
### Based on this we decided to divide our data into different groups where there was a significant drop in median installations.
### The reason for this was that some categories might not have a high number of installations in general.
ggplot(data=clean_google_nona,aes(x=fct_reorder(Category,logInstall),y=logInstall))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Data exploration

### Create Category based on installment numbers
clean_apps <- clean_google_nona %>% 
  group_by(Category) %>% 
  mutate(Install_M = median(logInstall))

cut1<-median(clean_apps$Install_M[clean_apps$Category == "EVENTS"])
cut2<-median(clean_apps$Install_M[clean_apps$Category=="TRAVEL_AND_LOCAL"])

CategoryLevel <- rep(0,length(clean_apps[,2])) 
CategoryLevel[clean_apps$Install_M<=cut1] <- 1        
CategoryLevel[clean_apps$Install_M>cut1] <- 2  
CategoryLevel[clean_apps$Install_M>cut2] <- 3 
### Read as factors (and not numbers)
clean_apps$CategoryLevel <- factor(CategoryLevel, levels=c(1,2,3))

### Category 1 has low median installs
Category1 <- clean_apps %>%
  filter(CategoryLevel == "1")

### Category 2 has medium median installs
Category2 <- clean_apps %>%
  filter(CategoryLevel == "2")

### Category 3 has high median installs
Category3 <- clean_apps %>%
  filter(CategoryLevel == "3")

ggplot(data = clean_apps, aes(Content.Rating, Rating, col = Type)) +
  geom_boxplot() +
  facet_wrap(~CategoryLevel) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Variation of installations with the number of days since last installed
ggplot(clean_apps, aes(days, logInstall)) +
  geom_jitter(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~CategoryLevel)

## Get rid of useless columns after creating each categories
drop2 <- c("Install_M", "CategoryLevel")
Category1 = Category1[,!(names(Category1) %in% drop2)]
Category2 = Category2[,!(names(Category2) %in% drop2)]
Category3 = Category3[,!(names(Category3) %in% drop2)]




###################################################
#### Clustering 
#### kmeans cluster based on rating and install for entire dataset.
simple <- clean_google_nona[,c(2,8)]
### colleting the relevant columns
Ssimple <- scale(simple)
### Standardizing our columns
### Computing kmeans
Ssimple_kmeans <- kmeans(Ssimple,4,nstart=10)
Ssimple_kmeans
colorcluster <- 1+Ssimple_kmeans$cluster
plot(Ssimple, col = 1, xlab="Rating (Standardized)", ylab="Install (Standardized)")
plot(Ssimple, col = colorcluster, xlab="Rating (Standardized)", ylab="Install (Standardized)")
points(Ssimple_kmeans$centers, col = 1, pch = 24, cex = 1.5, lwd=1, bg = 2:5)
### The command
Ssimple_kmeans$centers
## displays the k centers (good to interpret)
Ssimple_kmeans$size
### We can compute the average rating of each cluster and average installs
tapply(clean_google_nona[,2],Ssimple_kmeans$cluster,mean)
tapply(clean_google_nona[,8],Ssimple_kmeans$cluster,mean)
### and contrast with the overall average rating and installs
mean(clean_google_nona[,2])
mean(clean_google_nona[,8])

### We performed similar clustering analysis for the three groups of categories we created to understand our target clients.
### Category 1
simple <- Category1[,c(2,8)]
### colleting the relevant columns
Ssimple <- scale(simple)
### Standardizing our columns
### Computing kmeans
Ssimple_kmeans <- kmeans(Ssimple,4,nstart=10)
Ssimple_kmeans
colorcluster <- 1+Ssimple_kmeans$cluster
plot(Ssimple, col = 1, xlab="Rating (Standardized)", ylab="Install (Standardized)")
plot(Ssimple, col = colorcluster, xlab="Rating (Standardized)", ylab="Install (Standardized)")
points(Ssimple_kmeans$centers, col = 1, pch = 24, cex = 1.5, lwd=1, bg = 2:5)
### Category 2
simple <- Category2[,c(2,8)]
### colleting the relevant columns
Ssimple <- scale(simple)
### Standardizing our columns
### Computing kmeans
Ssimple_kmeans <- kmeans(Ssimple,4,nstart=10)
Ssimple_kmeans
colorcluster <- 1+Ssimple_kmeans$cluster
plot(Ssimple, col = 1, xlab="Rating (Standardized)", ylab="Install (Standardized)")
plot(Ssimple, col = colorcluster, xlab="Rating (Standardized)", ylab="Install (Standardized)")
points(Ssimple_kmeans$centers, col = 1, pch = 24, cex = 1.5, lwd=1, bg = 2:5)
### Category 3
simple <- Category3[,c(2,8)]
### colleting the relevant columns
Ssimple <- scale(simple)
### Standardizing our columns
### Computing kmeans
Ssimple_kmeans <- kmeans(Ssimple,4,nstart=10)
Ssimple_kmeans
colorcluster <- 1+Ssimple_kmeans$cluster
plot(Ssimple, col = 1, xlab="Rating (Standardized)", ylab="Install (Standardized)")
plot(Ssimple, col = colorcluster, xlab="Rating (Standardized)", ylab="Install (Standardized)")
points(Ssimple_kmeans$centers, col = 1, pch = 24, cex = 1.5, lwd=1, bg = 2:5)


### Data Mining

### We will be running a regression model to predict which factors drive installations.
### Running various regression models to figure out the best model.

### We will be running k-fold cross validation simultaneously to figure out our best model.
### lasso
df <- Category1
Mx<- model.matrix(logInstall ~.^2,data=df)[,-1]
My<- df$logInstall
head(df)
lasso <- glmnet(Mx,My)
lassoCV <- cv.glmnet(Mx,My)
summary(lasso)
par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))
plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))

features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) 
data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)
### k-fold cross validation for Category 1
n <- nrow(df)
nfold <- 10
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
OOS <- data.frame(regression = rep(NA, nfold),null=rep(NA,nfold),PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), L.min=rep(NA,nfold), L.1se=rep(NA,nfold),forest=rep(NA,nfold),tree=rep(NA,nfold)) 

for(k in 1:nfold){
  train <- which(foldid!=k) # train on all but fold `k'
  rmin <- glm(My~., data=data.min, subset=train)
  if ( length(features.1se) == 0){  r1se <- glm(logInstall~1, data=data.1se, subset=train) 
  } else {r1se <- glm(My~., data=data.1se, subset=train)
  }
  regression <- glm(formula = logInstall~., data = df,subset=train)
  null <- glm(logInstall~1, data = df, subset = train)
  tree <- tree(logInstall~ Rating+Reviews+Size+Type+Price+Content.Rating+days, data=df, subset = train)
  forest <- randomForest(formula = logInstall ~ Rating + Reviews + Size + Type + Price + Content.Rating + days, data = df,subset = train,nodesize = 5, ntree = 500, mtry = 4)
  lassomin  <- glmnet(Mx[train,],My[train],lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train],lambda = lassoCV$lambda.1se)
  
  ## fit the models
  
  pred.regression <- predict(regression, newdata = df[-train,], type= "response")
  pred.null <- predict(null, newdata=df[-train,], type="response")
  pred.tree <- predict(tree, newdata=df[-train,], type="vector")
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  pred1se  <- predict(r1se, newdata=data.1se[-train,], type="response")
  pred.forest <- predict(forest,newdata=df[-train,], type="response")
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  
  # predict on the test
  
  OOS$regression[k] <- R2(y=df$logInstall[-train], pred = pred.regression)
  OOS$regression[k]
  OOS$null[k] <- R2(y=df$logInstall[-train], pred=pred.null)
  OOS$null[k]
  OOS$tree[k] <- R2(y=df$logInstall[-train], pred=pred.tree)
  OOS$tree[k]
  OOS$PL.min[k] <- R2(y=My[-train], pred=predmin)
  OOS$PL.1se[k] <- R2(y=My[-train], pred=pred1se)
  OOS$forest[k] <- R2(y=df$logInstall[-train], pred=pred.forest)
  OOS$forest[k]
  OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin)
  OOS$L.1se[k] <- R2(y=My[-train], pred=predlasso1se)
  
  # Calculating the R2
  }
### Lets list the mean of the results stored in the dataframe OOS
### we have nfold values in OOS for each model, this computes the mean of them)
colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=1, yjust=1.5),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))
if (nfold >= 10){
  ### This plots a box plot with the performance of the three models
  names(OOS)[1] <-"Full model"
  ### Lets zoom in  to see better the performance of 
  ### the small and the null model
  boxplot(OOS, col="plum", las = 2, ylab=expression(paste("OOS ",R^2)), xlab="", main="10-fold Cross Validation-Group 1")
}

### lasso for category 2
df <- Category2
Mx<- model.matrix(logInstall ~.^2,data=df)[,-1]
My<- df$logInstall
head(df)
lasso <- glmnet(Mx,My)
lassoCV <- cv.glmnet(Mx,My)
summary(lasso)
par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))
plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))

features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) 
data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)
### k-fold cross validation for Category 2
n <- nrow(df)
nfold <- 10
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
OOS <- data.frame(regression = rep(NA, nfold),null=rep(NA,nfold),PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), L.min=rep(NA,nfold), L.1se=rep(NA,nfold),forest=rep(NA,nfold),tree=rep(NA,nfold)) 

for(k in 1:nfold){
  train <- which(foldid!=k) # train on all but fold `k'
  rmin <- glm(My~., data=data.min, subset=train)
  if ( length(features.1se) == 0){  r1se <- glm(logInstall~1, data=data.1se, subset=train) 
  } else {r1se <- glm(My~., data=data.1se, subset=train)
  }
  regression <- glm(formula = logInstall~., data = df,subset=train)
  null <- glm(logInstall~1, data = df, subset = train)
  tree <- tree(logInstall~ Rating+Reviews+Size+Type+Price+Content.Rating+days, data=df, subset = train)
  forest <- randomForest(formula = logInstall ~ Rating + Reviews + Size + Type + Price + Content.Rating + days, data = df,subset = train,nodesize = 5, ntree = 500, mtry = 4)
  lassomin  <- glmnet(Mx[train,],My[train],lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train],lambda = lassoCV$lambda.1se)
  
  ## fit the models
  
  pred.regression <- predict(regression, newdata = df[-train,], type= "response")
  pred.null <- predict(null, newdata=df[-train,], type="response")
  pred.tree <- predict(tree, newdata=df[-train,], type="vector")
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  pred1se  <- predict(r1se, newdata=data.1se[-train,], type="response")
  pred.forest <- predict(forest,newdata=df[-train,], type="response")
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  
  # predict on the test
  
  OOS$regression[k] <- R2(y=df$logInstall[-train], pred = pred.regression)
  OOS$regression[k]
  OOS$null[k] <- R2(y=df$logInstall[-train], pred=pred.null)
  OOS$null[k]
  OOS$tree[k] <- R2(y=df$logInstall[-train], pred=pred.tree)
  OOS$tree[k]
  OOS$PL.min[k] <- R2(y=My[-train], pred=predmin)
  OOS$PL.1se[k] <- R2(y=My[-train], pred=pred1se)
  OOS$forest[k] <- R2(y=df$logInstall[-train], pred=pred.forest)
  OOS$forest[k]
  OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin)
  OOS$L.1se[k] <- R2(y=My[-train], pred=predlasso1se)
  
  # Calculating the R2
}
### Lets list the mean of the results stored in the dataframe OOS
### we have nfold values in OOS for each model, this computes the mean of them)
colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=1, yjust=1.5),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))
if (nfold >= 10){
  ### This plots a box plot with the performance of the three models
  names(OOS)[1] <-"Full model"
  ### Lets zoom in  to see better the performance of 
  ### the small and the null model
  boxplot(OOS, col="plum", las = 2, ylab=expression(paste("OOS ",R^2)), xlab="", main="10-fold Cross Validation")
}
### lasso for category 3
df <- Category3
Mx<- model.matrix(logInstall ~.^2,data=df)[,-1]
My<- df$logInstall
head(df)
lasso <- glmnet(Mx,My)
lassoCV <- cv.glmnet(Mx,My)
summary(lasso)
par(mar=c(1.5,1.5,2,1.5))
par(mai=c(1.5,1.5,2,1.5))
plot(lassoCV, main="Fitting Graph for CV Lasso \n \n # of non-zero coefficients  ", xlab = expression(paste("log(",lambda,")")))

features.min <- support(lasso$beta[,which.min(lassoCV$cvm)])
length(features.min)
features.1se <- support(lasso$beta[,which.min( (lassoCV$lambda-lassoCV$lambda.1se)^2)])
length(features.1se) 
data.min <- data.frame(Mx[,features.min],My)
data.1se <- data.frame(Mx[,features.1se],My)
### k-fold cross validation for Category 3
n <- nrow(df)
nfold <- 10
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
OOS <- data.frame(regression = rep(NA, nfold),null=rep(NA,nfold),PL.min=rep(NA,nfold), PL.1se=rep(NA,nfold), L.min=rep(NA,nfold), L.1se=rep(NA,nfold),forest=rep(NA,nfold),tree=rep(NA,nfold)) 

for(k in 1:nfold){
  train <- which(foldid!=k) # train on all but fold `k'
  rmin <- glm(My~., data=data.min, subset=train)
  if ( length(features.1se) == 0){  r1se <- glm(logInstall~1, data=data.1se, subset=train) 
  } else {r1se <- glm(My~., data=data.1se, subset=train)
  }
  regression <- glm(formula = logInstall~., data = df,subset=train)
  null <- glm(logInstall~1, data = df, subset = train)
  tree <- tree(logInstall~ Rating+Reviews+Size+Type+Price+Content.Rating+days, data=df, subset = train)
  forest <- randomForest(formula = logInstall ~ Rating + Reviews + Size + Type + Price + Content.Rating + days, data = df,subset = train,nodesize = 5, ntree = 500, mtry = 4)
  lassomin  <- glmnet(Mx[train,],My[train],lambda = lassoCV$lambda.min)
  lasso1se  <- glmnet(Mx[train,],My[train],lambda = lassoCV$lambda.1se)
  
  ## fit the models
  
  pred.regression <- predict(regression, newdata = df[-train,], type= "response")
  pred.null <- predict(null, newdata=df[-train,], type="response")
  pred.tree <- predict(tree, newdata=df[-train,], type="vector")
  predmin <- predict(rmin, newdata=data.min[-train,], type="response")
  pred1se  <- predict(r1se, newdata=data.1se[-train,], type="response")
  pred.forest <- predict(forest,newdata=df[-train,], type="response")
  predlassomin <- predict(lassomin, newx=Mx[-train,], type="response")
  predlasso1se  <- predict(lasso1se, newx=Mx[-train,], type="response")
  
  # predict on the test
  
  OOS$regression[k] <- R2(y=df$logInstall[-train], pred = pred.regression)
  OOS$regression[k]
  OOS$null[k] <- R2(y=df$logInstall[-train], pred=pred.null)
  OOS$null[k]
  OOS$tree[k] <- R2(y=df$logInstall[-train], pred=pred.tree)
  OOS$tree[k]
  OOS$PL.min[k] <- R2(y=My[-train], pred=predmin)
  OOS$PL.1se[k] <- R2(y=My[-train], pred=pred1se)
  OOS$forest[k] <- R2(y=df$logInstall[-train], pred=pred.forest)
  OOS$forest[k]
  OOS$L.min[k] <- R2(y=My[-train], pred=predlassomin)
  OOS$L.1se[k] <- R2(y=My[-train], pred=predlasso1se)
  
  # Calculating the R2
}
### Lets list the mean of the results stored in the dataframe OOS
### we have nfold values in OOS for each model, this computes the mean of them)
colMeans(OOS)
OOS
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=1, yjust=1.5),
        ylab= bquote( "Out of Sample " ~ R^2), xlab="Fold", names.arg = c(1:10))
if (nfold >= 10){
  ### This plots a box plot with the performance of the three models
  names(OOS)[1] <-"Full model"
  ### Lets zoom in  to see better the performance of 
  ### the small and the null model
  boxplot(OOS, col="plum", las = 2, ylab=expression(paste("OOS ",R^2)), xlab="", main="10-fold Cross Validation")
}
### We chose to proceed with two models after looking at the results. Random forest has the highest R2 but Classification tree is easily interpretable.
### random forest

###### Random Forest for all groups with the variable importances

completedata <- df[complete.cases(df), ]
model1 <- randomForest(logInstall~Rating+Reviews+Size+Type+Price+Content.Rating+days, data=completedata, nodesize=5, ntree = 500, mtry = 4)
model1
importance(model1)
varImpPlot(model1,cex=0.8,main = 'Variable Importance in Group 1')

df <- Category2
completedata <- df[complete.cases(df), ]
model1 <- randomForest(logInstall~Rating+Reviews+Size+Type+Price+Content.Rating+days, data=completedata, nodesize=5, ntree = 500, mtry = 4)
model1
importance(model1)
varImpPlot(model1,cex=0.8,main = 'Variable Importance in Group 2')

df <- Category3
completedata <- df[complete.cases(df), ]
model1 <- randomForest(logInstall~Rating+Reviews+Size+Type+Price+Content.Rating+days, data=completedata, nodesize=5, ntree = 500, mtry = 4)
model1
importance(model1)
varImpPlot(model1,cex=0.8,main = 'Variable Importance in Group 3')

### We can see that Rating is the most important variable followed by number of days since last update and Size of the app.

par(mar=c(2,2,0.5,2))
### Regression Tree
library(rpart)
install.packages('rpart.plot')
library(rpart.plot)
# grow tree
fit <- rpart(logInstall~Rating+Reviews+Size+Type+Price+Content.Rating+days,method="anova", data=df)
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits
# create additional plots

rsq.rpart(fit) # visualize cross-validation results  
# plot tree
par(mar=c(2,2,0,2))
plot(fit, uniform=TRUE)
text(fit, use.n=TRUE, all=TRUE, cex=.8)
par(mfrow=c(1,1))
# prune the tree
pfit<- prune(fit, cp=0.00116) # from cptable   
# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Regression Tree for Group1 ")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
par(mfrow=c(1,1)) # two plots on one page
rsq.rpart(pfit) # visualize cross-validation results  
tree

