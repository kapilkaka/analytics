#iimlnc
#day1

?mtcars    #help on mtcars
class(mtcars)     # tell you type of data
x=1:5
class(x)
x
y=c(1,3.8,6)
str(y)
str(mtcars)
?str
class(as.integer(y))
summary(mtcars)
z=c(1L, 4L, 8L)
class(z)
str(z)




#vector----
#single dimension of one data type
x=1:100
x
x-1
x-1:10
x1=c(1,2,3,4,5)
x1
(x3=c(1,3,4,5,6))
print(x3)
x3[2]

(x4=rnorm(n=100,mean=60,sd=10))
plot(x4)
plot(density(x4))            #to plot normal distribution for continous value
hist(x4)                     #to plot histogram of continous value
hist(x4,breaks=10, col=1:15)
range(x4)                    #show minimum and maximum
min(x4);max(x4)
mean(x4);median(x4)
boxplot(x4)
sort(x4)
sort(x4,decreasing=TRUE)
plot(sort(x4), type='b')       #line and point both
plot(x4,type='l')                       #line only
x4[x4>65]                      #"[]" used to give condition value let we want value more than 65 only 
mean(x4[x4>65])         #mean of value more than 65
x4[91:100]
x4[-c(1:10)]
x4[x4>65]
length(x4[x4>65])
sum(x4>65)
sum(x4[x4>65])
x4[x4>60 & x4<70]
rev(x4)
?rev



#








#matrics----
#matrix has multiple data type rows and colums will be present
(data=c(10,30, 44,65,69,88))

(m1=matrix(data=data, nrow=2))
(m2= matrix(data=data, nrow=2, byrow=T))
rownames(m1)=c('R1','R2')
colnames(m1)= c('c1','c2','c3')
m1
m1[1,]    #to get one row
m1[1,3]     #to get particular value
m1[,3]          #to get one colume
m1[,2:3]           #to get multiple colume
colSums(m1)            #to get colume sum
colMeans(m1)           #to get columemean value
rowSums(m1)            #to get row sum
rowMeans(m1)           #to get row means
apply(m1, 1, FUN =min)
apply(m1, 1, FUN =max)
max(m1)
apply(m1[,2], 1, FUN =min)
apply(m1[,c2,drop=F] ,2,FUN=max)


#










#dataframe----


(rollno=paste('IIMLNC',1:13, sep='-'))
(name=paste('student',1:13, sep=' & '))
(age=runif(13, min=24, max=32))
(age= round(runif(13, min=24, max=32),2))
(marks= trunc(rnorm(13, mean=60, sd=10)))
(gender= sample( c('M','F'), size=13, replace=T))
(gender= sample( c('M','F'), size=13, replace=T))        #unmatch opening of bracket
(marks= trunc(rnorm(13, mean=60, sd=10)))
set.seed(34)
(gender= sample( c('M','F'), size=13, replace=T, prob=c(.7, .3)))   #to get 70% male and 30% female
set.seed(1234)      #probability concept forcertain pattern
table(gender)
set.seed(55)
(grade= sample( c('Ex','good','Sat'), size=13, replace=T, prob=c(.6, .3, .1)))
table(grade)
prop.table(table(grade))
?prop.table
sapply(list(rollno, name, age, marks, gender, grade), length)
(students= data.frame(rollno, name, age, marks, gender, grade
                      ))
?sapply                #simply apply give the length of each vector we combine


write.csv(students, 'data1/students.csv')             #execl like formate
write.csv(students, 'data1/students.csv', row.names = F)

df1 = read.csv('data1/students.csv')
df2 = read.csv('https://raw.githubusercontent.com/DUanalytics/rAnalytics/master/data/students.csv')
df2tudents
df3 = read.csv(file.choose())
df3

(x=c(-14.35, 14.35,-14.55,15.35))
floor(x); ceiling(x); trunc(x)



students


#install.packages("dplyr")
library(dplyr)

(class(students))
summary(students)
str(students)
students$gender= factor(students$gender)
str(students)
students$grade= factor(students$grade, ordered = T, levels = c('Ex', 'sat', 'Good'))
str(students)
table(students$gender)
table(students$grade)


students
library(dplyr)    #loading library
students %>% group_by(gender)    %>% tally()
students %>% group_by(gender)     %>% summarise(mean(age), n(),min(marks),max(marks))
students %>% group_by(gender, grade)    %>% summarise(mean(age))
students %>% group_by(name)       %>% tally()



              