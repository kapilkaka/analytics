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
predict(model, newdata = women,type = 'response')
head(women)
3.45*58-87
