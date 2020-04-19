rm(list = ls())

# exploratory analysis
library(cluster)
library (leaps)
library (ggplot2)
library (ggplotAssist)

insurance <- read.csv('insurance.csv',header=T)
names(insurance)
head(insurance)
attach(insurance)
str(insurance)

summary(insurance)

insurance$sex <- as.numeric(insurance$sex, 
                            levels = c("female","male"),
                            labels = c(1,2))
insurance$smoker <- as.numeric(factor(insurance$smoker, 
                                      levels = c("yes","no"),
                                      labels = c(1,2)))
insurance$region <- as.numeric(factor(insurance$region, 
                                      levels = c("southwest","southeast","northeast","northwest"),
                                      labels = c(1,2,3,4)))
insurance$charges <- as.numeric(insurance$charges)

hist(insurance$charges, main="Histogram of All Insurance Charges", xlab="Insurance Charges", xlim=c(0,70000))

plot(insurance$smoker, insurance$charges, main="Boxplot of Insurance Charges and Smoking Status", xlab="Smoking Status", ylab="Insurance Charges", ylim=c(0,70000), col="red")

plot(insurance$age, insurance$charges)

plot(insurance$bmi, insurance$charges)

plot(insurance$smoker, insurance$charges)

plot(insurance$sex, insurance$charges)

plot(insurance$region, insurance$charges)

plot(insurance$children, insurance$charges)

pairs( ~ charges + children + bmi + age + region, insurance)

#model building
lm.fit1=lm(charges ~ . , data=insurance)
summary(lm.fit1)
AIC(lm.fit1)
BIC(lm.fit1)
plot(lm.fit1)

lm.fit2=lm(charges ~ age + bmi + children + smoker , data=insurance)
summary(lm.fit2)
AIC(lm.fit2)
BIC(lm.fit2)
plot(lm.fit2)

lm.fit3=lm(charges ~ age + region + bmi + children + smoker , data=insurance)
summary(lm.fit3)
AIC(lm.fit3)
BIC(lm.fit3)
plot(lm.fit3)

lm.fit4=lm(charges ~ age +smoker , data=insurance)
summary(lm.fit4)
AIC(lm.fit4)
BIC(lm.fit4)
plot(lm.fit4)

#logistic regression
glm(charges~.,data=insurance)
summary(m1)
plot(m1)

set.seed(1)
train = sample(nrow(insurance),nrow(insurance)*0.8)
insurance.train = insurance[train,]  #training data set
insurance.test = insurance[-train,]  #test data set

m1train = glm(charges~.,data=insurance.train)
AIC(m1train)
BIC(m1train)
summary(m1train)

#statistic significant
m2 = glm(charges~age+bmi+children+smoker,data=insurance)
summary(m2)
plot(m2)

m2train = glm(charges~age+bmi+children+smoker,data=insurance.train)
AIC(m2train)
BIC(m2train)
summary(m2train)

#cross validation
k=10

M1CVMSE=rep(0,k)
M2CVMSE=rep(0,k)
M3CVMSE=rep(0,k)

folds=sample(1:k,nrow(insurance),replace=TRUE)

for(j in 1:k)
{
  M1CV=lm(charges~.,data=insurance[folds!=j,])
  M1CVMSE [j]=mean((charges-predict(M1CV,insurance))[folds==j]^2)
}
for(j in 1:k)
{
  M2CV=lm(charges~age+bmi+children+smoker,data=insurance[folds!=j,])
  M2CVMSE [j]=mean((charges-predict(M2CV,insurance))[folds==j]^2)
}
for(j in 1:k)
{
  M3CV=lm(charges~age+region+bmi+children+smoker,data=insurance[folds!=j,])
  M3CVMSE [j]=mean((charges-predict(M3CV,insurance))[folds==j]^2)
}

MeanM1MSE=mean(M1CVMSE)
MeanM2MSE=mean(M2CVMSE)
MeanM3MSE=mean(M3CVMSE)

#stepwise selection
regfit.fwd=regsubsets(charges~.,data=insurance,method="forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(charges~.,data=insurance,method="backward")
summary(regfit.bwd)

regfit.hybrid=regsubsets(charges~.,data=insurance,method="seqrep")
summary(regfit.hybrid)

#best subset selection
reg.best=regsubsets(charges~.,data=insurance)
summary(reg.best)

reg.summary=summary(reg.best)
names(reg.summary)
reg.summary$rsq  

#unadjusted R2
which.max(reg.summary$rsq)
reg.summary$rss  

#residual sum of squares
which.min(reg.summary$rss)

par(mfrow=c(1,2))

#adjusted R2
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="b",pch=8)
which.max(reg.summary$adjr2)
points(2,reg.summary$adjr2[2], col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type="b",pch=8)
which.min(reg.summary$bic)
points(2,reg.summary$bic[2],col="red",cex=2,pch=20)

#coefficient of the model selected from best selection
coef(reg.best,2)
coef(reg.best,3)
coef(reg.best,4)
coef(reg.best,5)



#After analysing the dataset, I´ve decided to perform a cluster analysis
#between the data variables 'age' and 'charges'.
# Hierarchical Clustering method calculating
#the cluster groups by Euclidean distance.

#Input dataset

#Refine Dataset
insurance_Ref <- insurance [c(1,7)]

#Using the elbow method to find the optimal number of clusters (n)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(insurance_Ref, i)$withinss)

#Elbow method plot
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS') +
  rect(2.85,15014969101,3.1,28014969101, col= rgb(1,0,1.0,alpha=0.5)) +
  text(3,40014969101, 'n=3')

#Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(insurance_Ref, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 3)

#possible clustering chart
insurance.labs=insurance[,7]

insurance.data=insurance[,1:6]

dim(insurance.data)

table(insurance.labs)

data.dist=dist(insurance.data)

hc1=hclust(data.dist)

hc2=hclust(data.dist, method="average")

hc3=hclust(data.dist, method="single")

par(mfrow=c(1,3))

plot(hc1, main="Complete Linkage", xlab="", sub="",ylab="")

plot(hc2, main="Average Linkage", xlab="", sub="",ylab="")

plot(hc3, main="Single Linkage", xlab="", sub="",ylab="")

hc.clusters1=cutree(hc1,3)

hc.clusters2=cutree(hc2,3)

hc.clusters3=cutree(hc3,3) 

table(hc.clusters1,insurance.labs)

table(hc.clusters2,insurance.labs)

table(hc.clusters3,insurance.labs) 

#visualising the clusters
clusplot(insurance_Ref,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 0,
         plotchar = FALSE,
         span = FALSE,
         main = paste('Cluster Analysis'),
         xlab = 'Charges',
         ylab = 'Age')

#Plot of the cluster groups real values
y_hc <- as.factor(y_hc)
levels(y_hc) <- c('Medium Cost', 'Low Cost', 'High Cost')

ggplot(data = insurance_Ref) +
  geom_point(aes(x = age, y = charges, color = y_hc)) +
  labs(x = 'Age',
       y = 'Medical Costs',
       colour = 'Cluster Groups',
       title = 'Cluster Groups Values') +
  scale_color_manual(values = c('green','blue','red')) 
