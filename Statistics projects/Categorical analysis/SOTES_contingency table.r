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
  assign('class.size',class.size,envir=.GlobalEnv)
  
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

#****************************** Association between expected grade and overall rating of the instructor
reset()
mask = mask.e.grade & mask.rating 
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade, rating), ncol=5, dimnames = list(c('A','B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
round(e.rating.table/sum(e.rating.table),2)
# Chisq indep test ('rCodeC3.R')
## Example
chisq.indep(e.rating.table, digits = 2)
e.rating.test <- chisq.indep(e.rating.table, verb = FALSE)
round(e.rating.test$std.res, digits = 1)

#1. Does the association change depending on how accurately predicted?
#*************** Compare expected grade and actual grade
reset()
mask = mask.e.grade & mask.grade
grade = grade[mask]
e.grade = e.grade[mask]
grade.table = matrix(table(e.grade,grade),ncol=4, dimnames = list(c('A','B','C','D/F'), c('A','B','C','D/F')))
round(grade.table/sum(grade.table),2)*100

#***************** Those who predicted higher grade
reset()
mask = mask.grade & mask.e.grade & mask.rating & (e.grade < grade)# 'A' is numerically lower than 'B'
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade, rating), ncol=5, dimnames = list(c('A','B','C'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
# round(e.rating.table/sum(e.rating.table),2)
tab=cbind(e.rating.table[,1],apply(e.rating.table[,2:5],1,sum))
colnames(tab)=c('VeryEff','EfforLower')
tab
round(tab/sum(grade.table),2)
gk.gamma(tab)

#***************** Those who accurately predicted their grade 
reset()
mask = mask.grade & mask.e.grade & mask.rating & (e.grade == grade)
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade, rating), ncol=5, dimnames = list(c('A','B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
# round(e.rating.table/sum(e.rating.table),2)
tab=cbind(e.rating.table[,1],apply(e.rating.table[,2:5],1,sum))
colnames(tab)=c('VeryEff','EfforLower')
tab
round(tab/sum(grade.table),2)
gk.gamma(tab)

#***************** Those who predicted lower grade
reset()
mask = mask.grade & mask.e.grade & mask.rating & (e.grade > grade)# 'A' is numerically lower than 'B'
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade, rating), ncol=5, dimnames = list(c('B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
# round(e.rating.table/sum(e.rating.table),2)
tab=cbind(e.rating.table[,1],apply(e.rating.table[,2:5],1,sum))
colnames(tab)=c('VeryEff','EfforLower')
tab
round(tab/sum(grade.table),2)
gk.gamma(tab)
# Those who predicted higher grade are slightly more likely to vote for Very Effective.

# 1-1.Which type of students are dillusional?
# Now I wonder what type of students predicted higher than they deserve.
# By student levels,
reset()
mask.accuracy = mask.grade & mask.e.grade & mask.rating & mask.student.level
accuracy = numeric(length(mask.accuracy))
mask.higher = (e.grade < grade)& mask.accuracy
mask.acc = (e.grade == grade)& mask.accuracy
mask.lower =  (e.grade > grade)& mask.accuracy
accuracy[mask.higher]=1
accuracy[mask.acc]=2
accuracy[mask.lower]=3
accuracy=accuracy[accuracy!=0]
student.level = student.level[mask.accuracy]
accuracy.table = matrix(table(student.level,accuracy),ncol=3,dimnames=list(c('fr','so','jr','sr','gr'),c('Higher','Exact','Lower')))
accuracy.table = accuracy.table[5:1,]
chisq.indep(accuracy.table, digits = 2)
accuracy.test <- chisq.indep(accuracy.table, verb = FALSE)
round(accuracy.test$std.res, digits = 1)

# Added on 11/24/19
reset()
mask = mask.grade & mask.e.grade & mask.student.level
over.exp = numeric(length(mask))
over.exp[(e.grade=='A' & grade!='A')&mask] = 1
over.exp[(e.grade=='B')&(grade=='C'|grade=='D/F')&mask] = 2
over.exp[(e.grade=='C')&(grade=='D/F')&mask] = 3
mask = mask&(over.exp != 0)
student.level = student.level[mask]
over.exp = over.exp[mask] # Same rownum as student.level
over.exp = table(student.level,over.exp)
rownames(over.exp) = c('gr','sr','jr','so','fr')
colnames(over.exp) = c('A','B','C')
chisq.indep(over.exp, digits = 2)
over.exp.test <- chisq.indep(over.exp, verb = FALSE)
round(over.exp.test$std.res, digits = 1)
over.exp
methods.lst = c('katz.log','adj.log','koopman')
result = mapply(BinomRatioCI, method=methods.lst, x1=1968,n1=2166,x2=7351,n2=13459)
row.names(result) = c('est','lwr.ci','upr.ci')
round(result, digits=3)

# I don't think there's much difference among different types of students.
# Thus I narrow down by expecting 'A' which they don't deserve.
reset()
mask.accuracy = mask.grade & mask.e.grade & mask.rating & mask.student.level
accuracyA = numeric(length(mask.accuracy))
mask.A = (e.grade=='A' & grade !='A')&mask.accuracy
mask.acc = (e.grade == grade)& mask.accuracy
mask.lower =  (e.grade > grade)& mask.accuracy
accuracyA[mask.A]=1
accuracyA[mask.acc]=2
accuracyA[mask.lower]=3
accuracyA=accuracyA[accuracyA!=0]
student.level = student.level[mask.A|mask.acc|mask.lower]
accuracyA.table = matrix(table(student.level,accuracyA),ncol=3,dimnames=list(c('fr','so','jr','sr','gr'),c('Expect A(higher)','Exact','Lower')))
round(accuracyA.table/sum(accuracyA.table),2)
# Graduate students are more likely to expect un-deserving A than undergraduate students.
# Investigate whether graduate students are significantly more likely to expect A when they don't deserve.




reset()
mask = mask.grade & mask.e.grade & (e.grade < grade) & mask.student.level
e.grade = e.grade[mask]
student.level = student.level[mask]
tab=table(student.level,e.grade)
grad.table=matrix(c(sum(tab[5,1]),sum(tab[5,2:3]),c(sum(tab[1:4,1]),sum(tab[1:4,2:3]))),nrow=2,byrow=T,dimnames=list(c('Grad','UGrad'),c('A','B/C')))

mask = mask.grade & mask.e.grade & mask.student.level
sum(student.level[mask]=='5' & e.grade[mask]<grade[mask])/sum(student.level[mask] == '5')
sum(student.level[mask]!='5' & e.grade[mask]<grade[mask])/sum(student.level[mask] != '5')
# 20% of grad students are dillusional and 22% of undergraduate students are dillusional.
# And out of dillusional graduate students,
# 90% of them expect 'A' whereas 47% of dillusional undergraduates expect 'A'.
# This means 18% of total graduate students expect 'A' when they don't deserve it.
# Only 10% of total undergraduate students expect 'A' when they don't deserve it.
# Odds ratio of Grad against UGrad is 10.26999
# RR = 1.9
# Out of dillusional students, Grad students is twice more likely to expect 'A' than undergraduates.
reset()
mask.accuracy = mask.grade & mask.e.grade & mask.rating & mask.student.level
mask.A = (e.grade=='A' & grade !='A')&mask.accuracy
mask.acc = (e.grade == grade)& mask.accuracy
mask.lower =  (e.grade > grade)& mask.accuracy
advanced = numeric(sum(mask.A|mask.acc|mask.lower))
course.level = sotes$course.level[mask.A|mask.acc|mask.lower]
student.level = student.level[mask.A|mask.acc|mask.lower]
accuracyA = numeric(length(mask.accuracy))
accuracyA[mask.A]=1
accuracyA[mask.acc]=2
accuracyA[mask.lower]=3
accuracyA=accuracyA[accuracyA!=0]
mask.lower = (student.level == '5' & course.level != 'GR')
table(accuracyA[mask.lower])# 97/(97+752+154) = 0.1
# When graduate students took undergraduate course, they are not as dillusional as in graduate courses.
# Only 10% of them expect un-deserving 'A' just like undergraduates do.

# ==> This tells me that many graduate students put more effort into studying the graduate courses than 
# they did for undergraduate courses, but it wasn't sufficient to get an 'A'.
# But they do not realize the insufficient amount of work and they rather expect A even more because they put more effort into it
# compared to what they did during their undergraduate program.


#2. Does the association change depending on course level?
#***************** Course Level is Lowerdivision
reset()
mask = mask.e.grade & mask.rating & (sotes$course.level == 'LD') & (e.grade < grade)
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade, rating), ncol=5, dimnames = list(c('A','B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
round(e.rating.table/sum(e.rating.table),2)
round(table(student.level[mask&mask.student.level])/sum(table(student.level[mask&mask.student.level])),2)
chisq.indep(e.rating.table, digits = 2)
e.rating.table.test <- chisq.indep(e.rating.table, verb = FALSE)
round(e.rating.table.test$std.res, digits = 1)

#***************** Course Level is Upperdivision
reset()
mask = mask.e.grade & mask.rating & (sotes$course.level == 'UD')
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade, rating), ncol=5, dimnames = list(c('A','B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
round(e.rating.table/sum(e.rating.table),2)
round(table(student.level[mask&mask.student.level])/sum(table(student.level[mask&mask.student.level])),2)
chisq.indep(e.rating.table, digits = 2)
e.rating.table.test <- chisq.indep(e.rating.table, verb = FALSE)
round(e.rating.table.test$std.res, digits = 1)

#***************** Course Level is GRAD
reset()
mask = mask.e.grade & mask.rating & (sotes$course.level == 'GR')
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade, rating), ncol=5, dimnames = list(c('A','B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
round(e.rating.table/sum(e.rating.table),2)
round(table(student.level[mask&mask.student.level])/sum(table(student.level[mask&mask.student.level])),2)
chisq.indep(e.rating.table, digits = 2)
e.rating.table.test <- chisq.indep(e.rating.table, verb = FALSE)
round(e.rating.table.test$std.res, digits = 1)

# There isn't much difference in rating highly between LD and UD, BUT GRD courses are much more likely to rate highly.
# In Graduate courses, 37% of A-expectators rate VeryEff and 20% of A-expectators rate Eff.
# This is very high compared to 28% of A-expectators rate VeryEff in LD and 26% of A-expectators rate VeryEff in UD.
# 71% of the students enrolled in LD courses is fr and so. Almost zero graduate students took LD course.
# 92% of the students enrolled in UD courses is jr and sr. Only 3% of graduate students took UD course.
# 99% of the students in graduate courses is Graduate students and only 1% is senior students.
# This tells us that A-expectating graduate students are more likely to rate VeryEff than undergraduate students.


#3. Does the association change depending on college?
#***************** College is BUS
reset()
mask = mask.e.grade & mask.rating & (college == 'bus')
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade,rating),ncol=5,dimnames = list(c('A','B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
round(e.rating.table/sum(e.rating.table),2)

#***************** College is ENG
reset()
mask = mask.e.grade & mask.rating & (college == 'eng')
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade,rating),ncol=5,dimnames = list(c('A','B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
round(e.rating.table/sum(e.rating.table),2)

#***************** College is SOC
reset()
mask = mask.e.grade & mask.rating & (college == 'soc')
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade,rating),ncol=5,dimnames = list(c('A','B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
round(e.rating.table/sum(e.rating.table),2)

#***************** College is SCI
reset()
mask = mask.e.grade & mask.rating & (college == 'sci')
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade,rating),ncol=5,dimnames = list(c('A','B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
round(e.rating.table/sum(e.rating.table),2)
# ==>
# The association is consistent in each college and
# In College of Social studies, rating is most strongly associated with the expected grade.
# If a person expects to get an A, s/he is 70% likely to rate Very Effective.
# In other colleges, expecting an A is also strongly associated with rating Very effective.

#4. How does Class size affect rating?
# Classes 15~25, 26~35, 36~45,...
par(mfrow=c(3,3))
reset()
mask = mask.e.grade & mask.rating & (class.size >= 15 & class.size <= 25)
rating = rating[mask]
pie(table(rating))
reset()
mask = mask.e.grade & mask.rating & (class.size >= 26 & class.size <= 35)
rating = rating[mask]
pie(table(rating))
reset()
mask = mask.e.grade & mask.rating & (class.size >= 36 & class.size <= 45)
rating = rating[mask]
pie(table(rating))
reset()
mask = mask.e.grade & mask.rating & (class.size >= 46 & class.size <= 55)
rating = rating[mask]
pie(table(rating))
reset()
mask = mask.e.grade & mask.rating & (class.size >= 56 & class.size <= 65)
rating = rating[mask]
pie(table(rating))
reset()
mask = mask.e.grade & mask.rating & (class.size >= 66 & class.size <= 75)
rating = rating[mask]
pie(table(rating))
reset()
mask = mask.e.grade & mask.rating & (class.size >= 76 & class.size <= 85)
rating = rating[mask]
pie(table(rating))
reset()
mask = mask.e.grade & mask.rating & (class.size >= 86 & class.size <= 95)
rating = rating[mask]
pie(table(rating))
reset()
mask = mask.e.grade & mask.rating & (class.size >= 96)
rating = rating[mask]
pie(table(rating))

# We can see that the proportion of rating 'Very Effective' decreases as the class size increases while
# the proportions among the lower ratings are consistent.
# We can probably group them into 15~25, 26~55, 56~85, and >=86 which have similar proportions of ratings within.
# And call them small, medium, large, huge

#5. Does the association change depending on class size?
#***************** Class is small(15~25)
reset()
mask = mask.e.grade & mask.rating & (class.size >= 15 & class.size <= 25)
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade,rating),ncol=5,dimnames = list(c('A','B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
round(e.rating.table/sum(e.rating.table),2)
chisq.indep(e.rating.table, digits = 2)
e.rating.table.test <- chisq.indep(e.rating.table, verb = FALSE)
round(e.rating.table.test$std.res, digits = 1)

#***************** Class is medium(26~55)
reset()
mask = mask.e.grade & mask.rating & (class.size >= 26 & class.size <= 55)
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade,rating),ncol=5,dimnames = list(c('A','B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
round(e.rating.table/sum(e.rating.table),2)
chisq.indep(e.rating.table, digits = 2)
e.rating.table.test <- chisq.indep(e.rating.table, verb = FALSE)
round(e.rating.table.test$std.res, digits = 1)

#***************** Class is large(56~85)
reset()
mask = mask.e.grade & mask.rating & (class.size >= 56 & class.size <= 85)
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade,rating),ncol=5,dimnames = list(c('A','B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
round(e.rating.table/sum(e.rating.table),2)
chisq.indep(e.rating.table, digits = 2)
e.rating.table.test <- chisq.indep(e.rating.table, verb = FALSE)
round(e.rating.table.test$std.res, digits = 1)

#***************** Class is huge(>=86)
reset()
mask = mask.e.grade & mask.rating & (class.size >= 86)
e.grade = e.grade[mask]
rating = rating[mask]
e.rating.table = matrix(table(e.grade,rating),ncol=5,dimnames = list(c('A','B','C','D/F'), c('VeryEff','Eff','Somewhat','Ineff','VeryIneff')))
round(e.rating.table/sum(e.rating.table),2)
chisq.indep(e.rating.table, digits = 2)
e.rating.table.test <- chisq.indep(e.rating.table, verb = FALSE)
round(e.rating.table.test$std.res, digits = 1)

# As the class size increase, the number of A-expectators decreases from 48% to 36%.
# And they are more likely to rate "Very Effective" in smaller classes.
# Those who expect B or lower grades are not significantly affected by the class size.
# Thus, professors who teach in large class has lower chance of receiving "Very Effective", because
# we know A-expectators are more likely to rate "Very Effective" but fewer people expect to get A 
# in large classes and they are also less likely to rate "Very Effective" in larger classes.

# Added on 11/25
reset()
class.size = as.numeric(class.size)
mask1=class.size <= 15
mask2=class.size >= 16 & class.size <= 25
mask3=class.size >= 26 & class.size <= 55
mask4=class.size >= 56 & class.size <= 85
mask5=class.size >= 86
class.size[mask1] = 'small'
class.size[mask2] = 'small-med'
class.size[mask3] = 'med-large'
class.size[mask4] = 'large'
class.size[mask5] = 'huge'
tab=table(class.size[mask.rating],rating[mask.rating])[c(1,2,3,5,4),5:1]
chisq.indep(tab, digits = 2)
tab.test <- chisq.indep(tab, verb = FALSE)
round(tab.test$std.res, digits = 1)

# Added on 11/27
reset()
mask = mask.e.grade & mask.rating & mask.grade
e.grade=e.grade[mask]
rating=rating[mask]
grade=grade[mask]
length(rating[rating=='1'&e.grade=='A'&grade=='A'])
length(rating[rating!='1'&e.grade=='A'&grade=='A'])
length(rating[rating=='1'&e.grade=='B'&grade=='A'])
length(rating[rating!='1'&e.grade=='B'&grade=='A'])

length(rating[rating=='1'&e.grade=='A'&grade!='A'])
length(rating[rating!='1'&e.grade=='A'&grade!='A'])
length(rating[rating=='1'&e.grade=='A'&grade=='A'])
length(rating[rating!='1'&e.grade=='A'&grade=='A'])

table(grade[rating%in%c('3','4','5')])/sum(table(grade[rating%in%c('3','4','5')]))
