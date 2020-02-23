(x=c(1,2,4,5))
sum(x)

#(x2=c(1,2,,4,,5))
(x2=c(1,2,NA,4,NA,5))
sum(x2)
sum(x2,na.rm=T)
?sum
length(x2)
is.na(x2)
sum(is.na(x2))
sum(is.na(x2))/length(is.na(x2))    #%perc of missing value
x2
mean(x2,na.rm=T)

x2[is.na(x2)]
x2[c(F,F,T,F,T,T)]
x2=is.na.POSIXlt(
  
  
  
  
)#pinstall<-c('rpart','rpart.plot','caret','arules','arulesViz')
#
# library(VIM)
library(VIM)
?data
data(sleep)
sleep
?sleep
head(sleep)
tail(sleep)
str(sleep)
?str
dim(sleep)
length(sleep)
summary(sleep)
##





##
(x=200:300)
quantile(x)
quantile(x, seq(0,1,.1))
quantile(x, seq(0,1,.01))


##

head(sleep)
is.na(sleep)
sum(is.na(sleep))
colSums(is.na(sleep))
rowSums(is.na(sleep))
complete.cases(sleep)
sum(complete.cases(sleep))
sleep[complete.cases(sleep),]
sleep[!complete.cases(sleep),]
xy
xy[xy>0]
library(dplyr)
(c1<-names(xy[xy>0]))
sleep[,c1]
sleep %>% select(c1) %>%length()
sleep %>% select(-c1) %>%length()
sleep %>% select(-c1)
'%notin%'<-Negate('%in%')
c2<- names(sleep)  %notin% c1
sleep[,c2]




#data partitioning
(x=1:100)
s1<-sample(x,size = 70)
length(s1)
sum(s1)


#
set.seed(134)
s2<-sample(x,size = 70)
length(s2)
sum(s2)


s3<-sample(x, size=.7* length(x))
length(s3)
x



(x=trunc(rnorm(100,mean = 60,sd=15)))
s4<-sample(x,size = 70)
length(s4)
sum(s4)



###



mtcars
mtcars %>% sample_n(24)
mtcars %>% sample_frac(.7)
dim(mtcars) ;nrow(mtcars)
(index=sample(1:nrow(mtcars),size=.7*nrow(mtcars)))
mtcars[index,]
dim(mtcars[index,])
mtcars[-index,]


pinstall<-c('rpart','rpart.plot','caret','arules','arulesViz','factoextra','dendextend','fpc','Nbclust','cluster','caTools','amap','animation','gsheet','readxl','rJava','xlsx','wordcloud','wordclound2','modeest','fdth','e1071')
install.packages(pinstall)


#pinstall <- c('rpart','rpart.plot', 'caTools', 'caret','arules','arulesViz', 'factoextra', 'dendextend','NbClust', 'cluster','fpc', 'amap','animation', 'gsheet', 'readxl', 'rJava', 'xlsx','wordcloud', 'wordcloud2', 'modeest','fdth','e1071' )
tspackages <- c('timeseries','xts','zoo','forecast','TTR','quantmod', 'lubridate','smooth','Mcomp')
tmpackages <- c('rtweet',"curl", 'twitterR', 'ROAuth', 'syuzhet')
lppackages <- c('lpSolve', 'linprog', 'lpSolveAPI')

install.packages(pinstall)

#Multiple Install
list.of.packages <- plist #substitute plist with name of list of packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load if avl, install if not avl---
#Load if available, install packages if not available in the system & then load
if (!require('quantmod')) {
  install.packages('quantmod')
  library(quantmod)
}









#-------------------------------------------------
#data partition


library(caTools)
sample= sample.split(Y = mtcars$am, SplitRatio = .7)
sample
table(sample)
prop.table(table(sample))
(y1=mtcars[sample==T, ])   #True set
(y2=mtcars[sample==F, ] )   #false set
table(y1$am);prop.table(table(y1$am))
table(y2$am);prop.table(table(y2$am))



library(ggplot2)
library(lattice)
library(caret)
(intrain<- createDataPartition(y=mtcars$am,p=.7, list = F))   #taking a sample out of data for training
train<-mtcars[intrain,]
test<- mtcars[-intrain,]
prop.table(table(train$am)); prop.table(table(test$am))





#linear regression



women
head(women)
model= lm(weight ~ height, data=women)
summary(model)
#y =mx +c
# weight =3.45 * height + - 87.51
plot(x=women$height, y=women$weight)
abline(model, col=2)
residuals(model)
women$weight
predwt<-predict(model, newdata = women,type = 'response')
head(women)
3.45*58-87
cbind(women,predwt, res = women$weight-predwt, res2=residuals(model))

summary(model)
sqrt(sum(residuals(model)^2))    #SSE(sum of square of error)
range(women$height)
ndata=data.frame(height=c(66.5, 75.8))
predict(model, newdata = ndata, type='response')
confint(model)


#Hypotesis testing
#case2: LR------------------------------------
link1='https://docs.google.com/spreadsheets/d/1h7HU0X_Q4T5h5D1Q36qoK40Tplz94x_HZYHOJJC_edU/edit#gid=2061602817'
library(gsheet)
df=as.data.frame(gsheet2tbl(link1))
head(df)
link='https://docs.google.com/spreadsheets/d/1h7HU0X_Q4T5h5D1Q36qoK40Tplz94x_HZYHOJJC_edU/edit#gid=2023826519'
df1=as.data.frame(gsheet2tbl(link))
head(df1)
model2= lm(Y ~ X, data=df1)
summary(model2)
plot(df1$X,df1$Y)
abline(model2)
resid(model2)
range(df1$X)
(ndata2=data.frame(X=3:4))







#----------------------------------------------------------------------------------------------------------------------------------#


# Decision Tree - Classification
#we want predict for combination of input variables, is a person likely to survive or not

#import data from online site
path = 'https://raw.githubusercontent.com/DUanalytics/datasets/master/csv/titanic_train.csv'
titanic <- read.csv(path)
head(titanic)
names(titanic)
data = titanic[,c(2,3,5,6,7)]  #select few columns only
head(data)
dim(data)
#load libraries
library(rpart)
library(rpart.plot)
str(data)
#Decision Tree
names(data)
table(data$Survived)
prop.table(table(data$Survived))
str(data)
data$Pclass = factor(data$Pclass)
fit <- rpart(Survived ~ ., data = data, method = 'class')
fit
rpart.plot(fit, extra = 104, cex=.8,nn=T)  #plot
head(data)
printcp(fit) #select complexity parameter
prunetree2 = prune(fit, cp=.017544)# pruned the tree,   Cp=complexity  parameter
rpart.plot(prunetree2, cex=.8,nn=T, extra=104)
prunetree2
nrow(data)
table(data$Survived)
# predict for Female, pclass=3, siblings=2, what is the chance of survival

#Predict class category or probabilities
library(dplyr)
(testdata = sample_n(data,2))
predict(prunetree2, newdata=testdata, type='class')
predict(prunetree2, newdata=testdata, type='prob')
str(data)
testdata2 = data.frame(Pclass=factor(2), Sex=factor('male'), Age=15, SibSp=2)
testdata2
predict(prunetree2, newdata = testdata2, type='class')
predict(prunetree2, newdata = testdata2, type='prob')

#Use decision trees for predicting
#customer is likely to buy a product or not with probabilities
#customer is likely to default on payment or not with probabilities
#Student is likely to get selected, cricket team likely to win etc

#Imp steps
#select columns for prediction
#load libraries, create model y ~ x1 + x2 
#prune the tree with cp value
#plot the graph
#predict for new cases

#rpart, CART, classification model
#regression decision = predict numerical value eg sales




















































































#write inferences
#R2=0.942,
#sales=1.669*Area+0.9645






#multiple linear regression


link2='https://docs.google.com/spreadsheets/d/1h7HU0X_Q4T5h5D1Q36qoK40Tplz94x_HZYHOJJC_edU/edit#gid=1595306231'
ml1=as.data.frame(gsheet2tbl(link2))
ml1
model3<-lm(sqty ~ price + promotion, data = ml1)
summary(model3)
head(model3)
plot(ml1$price, ml1$sqty)
plot(ml1$promotion, ml1$sqty)
range(ml1$price)
range(ml1$promotion)
range(ml1$sqty)
plot(model3)
plot(ml1$promotion, ml1$sqty)
abline(model3, col=2)
plot(model3)

# Decision Tree - Classification
#we want predict for combination of input variables, is a person likely to survive or not

#import data from online site
path = 'https://raw.githubusercontent.com/DUanalytics/datasets/master/csv/titanic_train.csv'
titanic <- read.csv(path)
head(titanic)
names(titanic)
data = titanic[,c(2,3,5,6,7)]  #select few columns only
head(data)
dim(data)
#load libraries
library(rpart)
library(rpart.plot)
str(data)
#Decision Tree
names(data)
table(data$Survived)
str(data)
data$Pclass = factor(data$Pclass)
fit <- rpart(Survived ~ ., data = data, method = 'class')
fit
rpart.plot(fit, extra = 104, cex=.8,nn=T)  #plot
head(data)
printcp(fit) #select complexity parameter
prunetree2 = prune(fit, cp=.018)
rpart.plot(prunetree2, cex=.8,nn=T, extra=104)
prunetree2
nrow(data)
table(data$Survived)
# predict for Female, pclass=3, siblings=2, what is the chance of survival

#Predict class category or probabilities
(testdata = sample_n(data,2))
predict(prunetree2, newdata=testdata, type='class')
predict(prunetree2, newdata=testdata, type='prob')
str(data)
testdata2 = data.frame(Pclass=factor(2), Sex=factor('male'), Age=15, SibSp=2)
testdata2
predict(prunetree2, newdata = testdata2, type='class')
predict(prunetree2, newdata = testdata2, type='prob')

#Use decision trees for predicting
#customer is likely to buy a product or not with probabilities
#customer is likely to default on payment or not with probabilities
#Student is likely to get selected, cricket team likely to win etc

#Imp steps
#select columns for prediction
#load libraries, create model y ~ x1 + x2 
#prune the tree with cp value
#plot the graph
#predict for new cases

#rpart, CART, classification model
#regression decision = predict numerical value eg sales