sotes=readxl::read_xlsx('sote-data-part.xlsx')
library(DescTools)
library(ggplot2)

#*************** Reset rating, grade, e.grade, student.level
reset = function(){
rating = sotes$Q13
# Reversing the original order: 1 = Very effective; 2 = Effective; 3 = Somewhat effective; 4 = Ineffective; 5 = Very Ineffective
mask.rating1 = rating == 1
mask.rating2 = rating == 2
mask.rating4 = rating == 4
mask.rating5 = rating == 5
rating[mask.rating1] = 5
rating[mask.rating2] = 4
rating[mask.rating4] = 2
rating[mask.rating5] = 1
assign('rating',rating,envir = .GlobalEnv)

grade = sotes$grade
grade = gsub('\\-','',gsub('\\+','',grade))
grade[grade=='D' | grade=='F'] = 'D/F'
assign('grade',grade,envir=.GlobalEnv)

e.grade = sotes$Q14
e.grade[e.grade == '1'] = 'A'
e.grade[e.grade == '2'] = 'B'
e.grade[e.grade == '3'] = 'C'
e.grade[e.grade == '4'] = 'D/F'
assign('e.grade',e.grade,envir=.GlobalEnv)

student.level = sotes$student.level
student.level[student.level=='fr'] = 1
student.level[student.level=='so'] = 2
student.level[student.level=='jr'] = 3
student.level[student.level=='sr'] = 4
student.level[student.level=='gr'] = 5
assign('student.level',student.level,envir=.GlobalEnv)

college = sotes$college
assign('college',college,envir=.GlobalEnv)

class.size = sotes$enroll


# Remove these in each test, not in advance.
mask.grade = grade %in% c('A','B','C','D/F')
assign('mask.grade',mask.grade,envir=.GlobalEnv)
mask.e.grade = e.grade %in% c('A','B','C','D/F')
assign('mask.e.grade',mask.e.grade,envir=.GlobalEnv)
mask.rating = rating != 'NA' & rating <= 5
assign('mask.rating',mask.rating,envir = .GlobalEnv)
mask.student.level = sotes$student.level %in% c('fr','so','jr','sr','gr')
assign('mask.student.level',mask.student.level,envir=.GlobalEnv)
}

#****************************** Summary of data **************************************
reset()
sem = sotes$semester
sem= sem[mask.student.level]
student.level=student.level[mask.student.level]
table(sem,student.level)/rowSums(table(sem,student.level))

reset()
rating= as.numeric(rating[mask.rating])
mask.rating1 = rating == 1
mask.rating2 = rating == 2
mask.rating4 = rating == 4
mask.rating5 = rating == 5
rating[mask.rating1] = 5
rating[mask.rating2] = 4
rating[mask.rating4] = 2
rating[mask.rating5] = 1
table(rating)
length(rating)
summary(rating)
length(rating[rating==5])/length(rating)
pie(table(rating)[5:1])
pie(table(rating))

par(mfrow=c(1,3),oma = c(0,0,0,0) + 0.1)
bar=boxplot(rating,horizontal = T,xlab='Rating',cex.lab=1,cex.axis=1)
points(bar$out, jitter(rep(1, length(bar$out)),factor = 3),pch=1)
length(rating[rating<=3])/length(rating)

e.grade = e.grade[mask.e.grade]
e.grade[e.grade=='A']=4
e.grade[e.grade=='B']=3
e.grade[e.grade=='C']=2
e.grade[e.grade=='D/F']=1
e.grade=as.numeric(e.grade)
bar=boxplot(e.grade,horizontal = T,xlab='Expected Grade',cex.lab=1,cex.axis=1,xaxt='n')
pie(table(e.grade))
axis(1,at=1:4,labels = c('D/F','C','B','A'))
points(bar$out, jitter(rep(1, length(bar$out)),factor = 3),pch=1)
length(e.grade[e.grade==5])/length(e.grade)
summary(e.grade)
length(e.grade[e.grade>=3])/length(e.grade)

grade=grade[mask.grade]
grade[grade=='A']=4
grade[grade=='B']=3
grade[grade=='C']=2
grade[grade=='D/F']=1
grade=as.numeric(grade)
bar=boxplot(grade,horizontal = T,xlab='Actual Grade',cex.lab=1,cex.axis=1,xaxt='n')
axis(1,at=1:4,labels = c('D/F','C','B','A'))
points(bar$out, jitter(rep(1, length(bar$out)),factor = 3),pch=1)
length(grade[grade<=2])/length(grade)
pie(table(grade))
summary(grade)

pie(table(student.level[mask.student.level]),labels = c('Freshman','Sophomore','Junior','Senior','Graduate'))
round(table(student.level)/length(student.level),2)

enroll = sotes$enroll
enroll[enroll<=15]=15
enroll[enroll>=16 & enroll <=25]=25
enroll[enroll>=26 & enroll <= 55]=55
enroll[enroll>=56 & enroll <= 85]=85
enroll[enroll>=86]=130
table(enroll)/length(enroll)
hist(enroll,breaks=c(0,15,25,55,85,130),xaxt='n',right=T,yaxt='n',ylim=c(0,0.02),ylab='Relative Frequency',xlab='Class Size',main='',cex.lab=1)
axis(1,at=c(0,15,25,55,85,130),lab=c(0,15,25,55,85,460),cex.axis=1.2)
axis(2,at=seq(0,0.02,0.01),labels = seq(0,0.6,0.3),cex.axis=1.5)

course.level=sotes$course.level[sotes$course.level != 'NA']
table(course.level)/length(course.level)
pie(table(course.level),labels=c('Graduate','Lower Division','Upper Division'))

dept = sotes$dept
pie(sort(table(dept),decreasing = T))
sort(round(table(dept)/length(dept),3),decreasing = T)*100
length(dept[dept %in% c('CMPE','MATH','PSYC','COMM')])/length(dept)
dept[dept %in% c('BUS1','BUS2','BUS3','BUS4','BUS5')] = 'BUS'
dept[dept =='STAT'] = 'MATH&STAT'
dept[dept=='MATH'] = 'MATH&STAT'
tab = sort(table(dept),decreasing=T)
round(tab/length(dept),3)*100
length(unique(dept))
plot(round(tab/length(dept),3)*100,type='h',xaxt='n',ylab='% of size',xlab='')
title(xlab = "Departments", line = .5, cex.lab = 1)

plot(round(tab[1:5]/length(dept),3)*100,type='h',xaxt='n',yaxt='n',ylab='% of size',xlab='',lwd=35,lend=1,col='grey',ylim=c(0,12))
axis(1,at=1:5,labels=names(tab)[1:5])
axis(2,at=seq(0,12,2),labels=seq(0,12,2))

pie(table(sotes$college))
table(sotes$college)/sum(table(sotes$college))
