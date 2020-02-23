#clustering
set.seed(1234)
(marks1=trunc(rnorm(n=30,mean = 70,sd=8)))
sum(marks1)
df5<- data.frame(marks1=marks1)
head(df5)


#

km3<-kmeans(df5, centers = 3); ?kmeans()
attributes(km3)
km3$cluster   #
km3$centers   # average of three cluster i made
km3$size    
sort(df5$marks1)
cbind(df5,km3$cluster)    #which row which cluster
df5$cluster= km3$cluster
head(df5)
df5  %>% arrange(cluster)
dist(df5)
dist(df5[1:5,])





#0-----------------------------------------------------------------------------------




#clustering
set.seed(1234)
(marks1=trunc(rnorm(n=30,mean = 70,sd=8)))
(marks2=trunc(rnorm(n=30,mean = 120,sd=10)))
sum(marks1)
df6<- data.frame(marks1,marks2)
head(df6)
df7<-scale(df6)
head(df7)

#

km3B<-kmeans(df7, centers = 5); #kmeans to calculate cluster means
attributes(km3B)
km3B$cluster   #
km3B$centers   # average of three cluster i made
km3B$size    
sort(df5$marks1)
cbind(df5,km3$cluster)    #which row which cluster
df6$cluster= km3B$cluster
head(df6)
df6  %>% arrange(cluster)
dist(df6)
dist(df6[1:5,])
plot(df6$marks1,df6$marks2, col=df6$cluster)



#--------------------------------------------------------------------------



set.seed(1234)
(marks1=trunc(rnorm(n=30,mean = 70,sd=8)))
(marks2=trunc(rnorm(n=30,mean = 120,sd=10)))
sum(marks1)
df6<- data.frame(marks1,marks2)
head(df6)
(df7<-scale(df6[,c('marks1','marks2')]))
head(df7)

#

km4<-kmeans(df7, centers = 5); #kmeans to calculate cluster means
attributes(km4)
km4$cluster   #
km4$centers   # average of three cluster i made
km4$size    

cbind(df7,km4$cluster)    #which row which cluster
head(df7)
df7$newcluster= km4$cluster
head(df7)
library(dplyr)
df7  %>% arrange(cluster)
dist(df7)
dist(df7[1:5,])
plot(df7$marks1,df7$marks2, col=df7$newcluster)