5#day2 : 21 feb 2020

library(dplyr)
mtcars
table(mtcars$cyl)
summary(mtcars$cyl)
mtcar5s %>% group_by(cyl)     %>% tally()
mtcars %>% group_by(cyl)     %>% summarise(COUNT =n())
xtabs(~cyl, data=mtcars)
?ftable
ftable(mtcars$cyl)
#gear & cyl
table(mtcars$cyl, mtcars$gear, dnn=c('cylinder','gear'))

mtcars %>% group_by(cyl,gear)  %>% tally()
mtcars %>% group_by(cyl, gear)     %>% summarise(COUNT =n())
xtabs(~cyl + gear, data=mtcars)
ftable(mtcars$cyl, mtcars$gear)
table(mtcars$cyl, mtcars$gear, mtcars$am, dnn=c('cyl', 'gear','automanual'))

df=mtcars
head(df)
tail(df)
df$am= ifelse(df$am==0,'AUTO', 'MANual')
df
mtcars %>% mutate(TxType = ifelse(am==1, 'AUTO','Manual'))  %>% group_by(TxType) %>%summarise(COUNT= n())
mtcars
df
head(df)
df[,'mpg']
df

head(df)
df=df%>% mutate(TxType =ifelse(am==0, 'AUTO', 'manual'))
head(df)

#increASE mileage by  10%
df=mtcars
#add mpg + wt new column MGPWT
head(df)
head(df)
df$mpg*1.1
df$mpg + df$wt
df$MPGWT =df$mpg*1.1 +df$wt
head(df)
df$hpx =df$hp+ 10
df
#top 2 cars from each gear type :use group_by & top_n
df %>% group_by(gear) %>% top_n(n=2, wt=mpg) %>% select(gear,mpg)
df %>% group_by(gear) %>% arrange(~mpg) %>% select(gear,mpg)
df %>% group_by(gear) %>% top_n(n=2, wt=-mpg) %>% select(gear,mpg)
?top_n
#list out details any 2 cars picked randomly : then do 25% of the cars
df%>% sample_n(2)
df%>% sample_frac(.25)
top_frac(x, n, wt=)
df %>% sample_frac(.20) %>% arrange(mpg)
#ascending gear desending mpg
df %>% sample_frac(.20) %>% arrange(gear,desc(mpg)) 


sort(df$mpg)
df[order(df$mpg), ]
df%>%slice(1)
df%>%slice(10:15)
#find mean of mpg
?average
mean(df$mpg)
median(df$mpg)
mode(df$mpg)
?
summarise(mean(df$mpg,df$gear,df$cyl))
?summarise
df %>% group_by(gear)  %>% summarise_at(c('mpg','wt','hp','disp'),c(mean))      
df %>% group_by(gear)  %>% summarise_at(c('mpg','wt','hp','disp'),c(mean,median))      
df %>% select(gear,mpg,wt,hp,disp) %>% group_by(gear) %>% summarise_all((mean))
#find min and max of wt for each gear type
df %>% select(gear,mpg,wt) %>% group_by(gear) %>% summarise_each(c(min,max))







#graph
hist(df$mpg)
barplot(table(df$gear),col=1:3)                                                                 
pie(table(df$gear))
plot(df$wt,df$mpg)
?pie
#presentage calculation
L1<- paste(round(table(df$gear)/nrow(df)*100),'%')
pie(table(df$gear), labels =L1,col=1:3)

install.packages('ggplot2')
install.packages('reshape2')
install.packages("dplyr")
(rollno = paste ('IIM',1:10,sep='_'))
(name = paste ('SName',1:10,sep=' '))
(gender= sample(c('M','F'), size=10, replac=T))
(program= sample(c('BBA','MBA'), size=10, replac=T))

(marketing = trunc (rnorm(10,mean=60, sd=10)))
(finance= trunc (rnorm(10,mean=55, sd=12)))
students<- data.frame(rollno , name , gender , program , marketing , finance , stringsAsFactors = F)
students
head(students)

(meltsum1<-melt(students,id.vars = c('rollno','name','gender','program'),variable.name = 'subject',value.name = 'marks'))
(dcastSum1 <- dcast)
barplot(table(students$program))
hist(students$marketing)
(meltsum1<-melt(students,id.vars = c('rollno','name','gender','program'),variable.name = 'subject',value.name = 'marks'))
meltsum1
meltsum1 %<%group_by(program,gender) %<% summarise(MeanMarks =mean(marks))

#creating a data set
library(dplyr)
library(reshape2)
library(ggplot2)
(rollno = paste ('IIM',1:10,sep='_'))
(name = paste ('SName',1:10,sep=' '))
(gender= sample(c('M','F'), size=10, replac=T))
(program= sample(c('BBA','MBA'), size=10, replac=T))
(operation = trunc (rnorm(10,mean=70, sd=5)))

(marketing = trunc (rnorm(10,mean=60, sd=10)))
(finance= trunc (rnorm(10,mean=55, sd=12)))
students<- data.frame(rollno , name , gender , program , operation , marketing , finance , stringsAsFactors = F)
students
head(students)

(meltsum1<-melt(students,id.vars = c('rollno','name','gender','program'),variable.name = 'subject',value.name = 'marks'))
(dcastSum1 <- dcast)
barplot(table(students$program))
hist(students$marketing)
(meltsum1<-melt(students,id.vars = c('rollno','name','gender','program'),variable.name = 'subject',value.name = 'marks'))
meltsum1
meltsum1 %>%group_by(program,gender) %>% summarise(MeanMarks =mean(marks))

(meltsum1<-melt(students,id.vars = c('rollno','name')))
(dcastSum1 <- dcast(meltsum1, rollno + name ~ variable, value.var = 'value'))
?recast
recast(data = students, gender~variable, fun.aggregate=mean)




students %>% group_by(gender, program) %>% summarise((mean(marketing),mean(finance),mean(operation)))
meltsum1 %>%group_by(program,gender) %>% summarise(MeanMarks =mean(marks))
sum2 <- meltsum1 %>%
ggplot()



students
head(students, n=2)
students %>% group_by(gender)   %>% summarise(COUNT = n())
ggplot(students %>% group_by(gender)   %>% summarise(COUNT = n()), aes(x=gender, y=COUNT, fill=gender)
) + geom_bar(stat = 'identity') + geom_text(aes(label=COUNT)) + labs(title ='Gender Wise count')
ggplot(students, aes(x=gender, y=..count..)) + geom_bar(stat = 'count')
?aes
#stacked
ggplot(students %>% group_by(program, gender)   %>% summarise(COUNT = n()), aes(x=gender, y=COUNT, fill=program)
) + geom_bar(stat = 'identity') + geom_text(aes(label=COUNT)) + labs(title ='Gender Wise -program count')

#side by side
ggplot(students %>% group_by(program, gender)   %>% summarise(COUNT = n()), aes(x=gender, y=COUNT, fill=program)
) + geom_bar(stat = 'identity' ,position = position_dodge2(.7)) + geom_text(aes(label=COUNT)) + labs(title ='Gender Wise -program count')



names(students)
names(meltsum1)


ggplot(meltsum1 %>% group_by(program, gender, subject)   %>% summarise(meanMarks =round(mean(marks))), aes(x=gender, y=meanMarks, fill=program)
) + geom_bar(stat = 'identity' ,position = position_dodge2(.7)) + geom_text(aes(label=meanMarks),position = position_dodge2(.7)) + labs(title ='Subject -program -Gender - Mean Marks')+facet_grid(~subject)
?facet_grid








#   story telling with data

ggplot(mtcars,aes(x=wt,y= mpg, size= hp, color=factor(gear),shape=factor(am))) +geom_point()
