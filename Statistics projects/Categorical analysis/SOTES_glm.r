library(DescTools)
library(ggplot2)

sotes=readxl::read_xlsx('sote-data-part.xlsx')

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
  assign('student.level',student.level,envir=.GlobalEnv)
  
  college = sotes$college
  assign('college',college,envir=.GlobalEnv)
  
  class.size = sotes$enroll
  assign('class.size',class.size,envir=.GlobalEnv)
  
  course.level = sotes$course.level
  assign('course.level', course.level, envir=.GlobalEnv)
  
  # Remove these in each test, not in advance.
  mask.grade = grade %in% c('A','B','C','D/F')
  assign('mask.grade',mask.grade,envir=.GlobalEnv)
  mask.e.grade = e.grade %in% c('A','B','C','D/F')
  assign('mask.e.grade',mask.e.grade,envir=.GlobalEnv)
  mask.rating = rating != 'NA' & rating <= 5
  assign('mask.rating',mask.rating,envir = .GlobalEnv)
  mask.student.level = sotes$student.level %in% c('fr','so','jr','sr','gr')
  assign('mask.student.level',mask.student.level,envir=.GlobalEnv)
  mask.course.level = course.level != 'NA' 
  assign('mask.course.level',mask.course.level,envir=.GlobalEnv)
}

reset()
dat = sotes[mask.rating & mask.course.level & mask.student.level & mask.e.grade,c(1,2,4,5,6,7,8,11,12)]
colnames(dat) = c('semester','grade','course.level','dept','college','student.level','class.size','rating','e.grade')
dat$rating = ifelse(dat$rating == 5, 1, 0)
dat$dept[dat$dept %in% c('BUS1','BUS2','BUS3','BUS4','BUS5')] = 'BUS'
dat$dept[dat$dept =='STAT'] = 'MATH&STAT'
dat$dept[dat$dept=='MATH'] = 'MATH&STAT'

dat$e.grade[dat$e.grade == '1'] = 'A'
dat$e.grade[dat$e.grade == '2'] = 'B'
dat$e.grade[dat$e.grade == '3'] = 'C'
dat$e.grade[dat$e.grade == '4'] = 'D/F'

dat$semester = relevel(as.factor(dat$semester), ref='2164')
dat$course.level = relevel(as.factor(dat$course.level),ref='LD')
dat$dept = relevel(as.factor(dat$dept),ref='BUS')
dat$student.level = relevel(as.factor(dat$student.level),ref='fr')
dat$e.grade = relevel(as.factor(dat$e.grade),ref='A')

# Test Interaction
# round(coef(glm(rating ~ course.level, family=binomial('logit'),data=dat)),3)-
# round(coef(glm(rating ~ course.level+semester, family=binomial('logit'),data=dat)),3)

sotes.model = glm(rating ~ semester + course.level + dept + student.level + class.size + e.grade +
                  course.level * dept + course.level * student.level + dept * student.level, family=binomial('logit'),data=dat)
summary(sotes.model)

# save(sotes.model,file='sotes.model.RData')
# load('sotes.model.RData')

length(coef(sotes.model)) # 342 terms 68 not defined because of singularities
a=alias(sotes.model)
mat=as.matrix(a$Complete)
table(mat)
which(mat=='1',arr.ind=T)
which(mat=='-1',arr.ind=T)
colnames(mat[,c(15,79,96,155,174,194)])
### Removing multicollinear dummy variables
mm <- model.matrix(sotes.model)[, !is.na(coef(sotes.model)) ]# Remove the multicollinear predictors
df0 <- data.frame(rating = dat$rating, mm[, -1])# New data matrix without multicollinear predictors
sotes.model = glm(rating ~ ., family=binomial('logit'),data=df0)# Re-fit GLM
summary(sotes.model)
# save(sotes.model,file='sotes.model2.RData')
load('sotes.model2.RData') # Overwrites sotes.model
load('df0.RData')
# college * dept has perfect multicollinearity
# course.level*dept has 37 multicollinear variables
# dept*student.level has 31 multicollinear variables
# dept*class.size has 3 multicollinear variables
# dept*e.grade has 11 multicollinear variables
# semester*dept has 8 multicollinear variables

##-------------------------------------------------------
## logistic regression
##-------------------------------------------------------
# Our model:
# semester + course.level + dept + student.level + class.size + e.grade +
# course.level * dept + course.level * student.level + dept * student.level

# predicted probabilities
# df = data.frame(semester = dat$semester, 
#                 course.level = dat$course.level, 
#                 dept = dat$dept, 
#                 student.level = dat$student.level,
#                 class.size = dat$class.size,
#                 e.grade = dat$e.grade)
probs = predict(sotes.model, newdata = df0, type='response')
summary(probs)

beta = sotes.model$coefficients
assign('beta',beta,envir = .GlobalEnv)
assign('df0',df0,envir = .GlobalEnv)

pred.prob <- function(beta) {
  Xb = as.matrix(df0)%*%as.matrix(beta)
  return(exp(Xb) / (1 + exp(Xb)))
}

## plot the fitted curves for each color
par(mar=c(4,4,1,1))

gr = df0$student.levelgr
gr = ifelse(gr == 1, 5, 0)
sr = df0$student.levelsr
sr = ifelse(sr == 1, 4, 0)
jr = df0$student.leveljr
jr = ifelse(jr == 1, 3, 0)
so = df0$student.levelso
so = ifelse(so == 1, 2, 0)
fr = gr + sr + jr + so
fr = ifelse(fr == 0, 1, 0)
st = gr + sr + jr + so + fr

grd = df0$course.levelGR
grd = ifelse(grd == 1, 3, 0)
ud = df0$course.levelUD
ud = ifelse(ud == 1, 2, 0)
ld = grd + ud
ld = ifelse(ld == 0 , 1, 0)
div = grd + ud + ld


mean.prob1 = c(mean(pred.prob(beta)[st == 1 & div == 3]),mean(pred.prob(beta)[st == 2 & div == 3]),mean(pred.prob(beta)[st == 3 & div == 3]),mean(pred.prob(beta)[st == 4 & div == 3]),mean(pred.prob(beta)[st == 5 & div == 3]))
mean.prob2 = c(mean(pred.prob(beta)[st == 1 & div == 2]),mean(pred.prob(beta)[st == 2 & div == 2]),mean(pred.prob(beta)[st == 3 & div == 2]),mean(pred.prob(beta)[st == 4 & div == 2]),mean(pred.prob(beta)[st == 5 & div == 2]))
mean.prob3 = c(mean(pred.prob(beta)[st == 1 & div == 1]),mean(pred.prob(beta)[st == 2 & div == 1]),mean(pred.prob(beta)[st == 3 & div == 1]),mean(pred.prob(beta)[st == 4 & div == 1]),mean(pred.prob(beta)[st == 5 & div == 1]))
med.prob1 = c(median(pred.prob(beta)[st == 1 & div == 3]),median(pred.prob(beta)[st == 2 & div == 3]),median(pred.prob(beta)[st == 3 & div == 3]),median(pred.prob(beta)[st == 4 & div == 3]),median(pred.prob(beta)[st == 5 & div == 3]))
med.prob2 = c(median(pred.prob(beta)[st == 1 & div == 2]),median(pred.prob(beta)[st == 2 & div == 2]),median(pred.prob(beta)[st == 3 & div == 2]),median(pred.prob(beta)[st == 4 & div == 2]),median(pred.prob(beta)[st == 5 & div == 2]))
med.prob3 = c(median(pred.prob(beta)[st == 1 & div == 1]),median(pred.prob(beta)[st == 2 & div == 1]),median(pred.prob(beta)[st == 3 & div == 1]),median(pred.prob(beta)[st == 4 & div == 1]),median(pred.prob(beta)[st == 5 & div == 1]))


par(mar=c(5,6,1,1))
plot(x = 1:5, y = mean.prob1, xaxt='n', col='red',pch=5, ylim=c(0.3,0.7),ylab = 'Sample mean of \nestimated probabilities', xlab= 'Student Level')# Graduate Div.
axis(1, at = 1:5, labels= c('Fresh','Soph','Jr','Sr','Grad'))
points(x = 1:5, y = mean.prob2,col='blue',pch=3) # Upper Div
points(x = 1:5, y = jitter(mean.prob3),col='black',pch=4) # Lower Div
legend('topleft', c(" Graduate Div", ''," Upper Div",'', " Lower Div"),
       col=c('Red', 'White','Blue','White', 'Black'), pch=c(5,4,3),box.col = 'white',cex = 0.7)

plot(x = 1:5, y = med.prob1, xaxt='n', col='red',pch=5, ylim=c(0.3,0.7), ylab = 'Sample median of \nestimated probabilities', xlab= 'Student Level')# Graduate Div.
axis(1, at = 1:5, labels= c('Fresh','Soph','Jr','Sr','Grad'))
points(x = 1:5, y = med.prob2,col='blue',pch=3) # Upper Div
points(x = 1:5, y = jitter(med.prob3),col='black',pch=4) # Lower Div
legend('topleft', c(" Graduate Div", ''," Upper Div",'', " Lower Div"),
       col=c('Red', 'White','Blue','White', 'Black'), pch=c(5,4,3),box.col = 'white',cex = 0.7)

sum(table(sotes[sotes$student.level == 'sr'& sotes$course.level == 'GR', 'grade']))/nrow(dat)
table(sotes[sotes$student.level == 'gr' & sotes$course.level == 'GR', 'Q14'])/sum(table(sotes[sotes$student.level == 'gr' & sotes$course.level == 'GR', 'Q14']))
table(sotes[sotes$student.level == 'gr' & sotes$course.level == 'UD', 'Q14'])/sum(table(sotes[sotes$student.level == 'gr' & sotes$course.level == 'UD', 'Q14']))
table(sotes[sotes$student.level == 'gr' & sotes$course.level == 'LD', 'Q14'])/sum(table(sotes[sotes$student.level == 'gr' & sotes$course.level == 'LD', 'Q14']))
table(sotes[sotes$student.level == 'fr' & sotes$course.level == 'GR', 'Q14'])

# Mostly un-skewed
par(mfrow=c(3,5))
# Skewed to left Grad in Grad div
# Skewed to left Senior in Upper div
for(i in 1:3){
  for(j in 1:5){
    tryCatch({plot(density(pred.prob(beta)[div == i & st == j]), main='')},
             error = function(cond){
               message(cond)
               plot.new()
             })
  }
}

# Most common 
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

sort(table(dat[dat$student.level == 'gr','semester']),decreasing = T)[1] # Fall
sort(table(dat[dat$student.level == 'gr','course.level']),decreasing = T)[1] # GR
sort(table(dat[dat$student.level == 'gr','dept']),decreasing = T)[c(1,2)] # CMPE
sort(table(dat[dat$student.level == 'gr'& dat$dept=='BUS','class.size']),decreasing = T) # 83
sort(table(dat[dat$student.level == 'gr','e.grade']),decreasing = T)[1] # 'A'

soph1 = df0[which(apply(df0[,5:50],1,sum)==0),]
soph1 = soph1[soph1$student.levelso == 1 & soph1$semester2172==0 & soph1$course.levelUD ==1 & soph1$e.gradeB == 0 & soph1$e.gradeC == 0 &soph1$e.gradeD.F == 0 &soph1$class.size == 47, ]

fr1 = df0[which(apply(df0[,5:50],1,sum)==0),]
fr1 = fr1[fr1$student.levelso == 0 &fr1$student.leveljr == 0&fr1$student.levelsr == 0&fr1$student.levelgr == 0& fr1$semester2172==0 & fr1$course.levelUD ==1 & fr1$e.gradeB == 0 & fr1$e.gradeC == 0 & fr1$e.gradeD.F == 0 & fr1$class.size == 47, ]

predict(sotes.model, newdata = fr1, type='response',se.fit=T)
predict(sotes.model, newdata = soph1, type='response',se.fit=T)

grad1 = df0[df0$student.levelgr == 1 & df0$semester2172 == 0 & df0$course.levelUD == 1 & df0$deptMATH.STAT == 1 & df0$class.size == 31 & df0$e.gradeB == 0 & df0$e.gradeC == 0 & df0$e.gradeD.F == 0,]
grad = predict(sotes.model, newdata = grad1, type='response',se.fit=T)
upper1 = df0[df0$student.levelgr == 1 & df0$semester2172 == 0 & df0$course.levelGR == 1 & df0$deptMATH.STAT == 1 & df0$class.size == 31 & df0$e.gradeB == 0 & df0$e.gradeC == 0 & df0$e.gradeD.F == 0,]
upper = predict(sotes.model, newdata = upper1, type='link',se.fit = T)


table(sotes[sotes$student.level == 'sr','Q13'])[5]/sum(table(sotes[sotes$student.level == 'sr','Q13']))
table(sotes[sotes$student.level == 'jr','Q13'])[5]/sum(table(sotes[sotes$student.level == 'jr','Q13']))
table(sotes[sotes$student.level == 'so','Q13'])[5]/sum(table(sotes[sotes$student.level == 'so','Q13']))
table(sotes[sotes$student.level == 'fr','Q13'])[5]/sum(table(sotes[sotes$student.level == 'fr','Q13']))
table(sotes[sotes$student.level == 'gr','Q13'])[5]/sum(table(sotes[sotes$student.level == 'gr','Q13']))
sort(table(sotes[sotes$Q13 == 1,'student.level'])/sum(table(sotes[sotes$Q13 == 1,'student.level'])))


# cov.mat = vcov(sotes.model)
# var.log.OR = cov.mat[51,51] + cov.mat[54,54] + cov.mat[118,118] + cov.mat[123,123] -
#   cov.mat[51,54] + cov.mat[51,118] - cov.mat[51,123] - cov.mat[54,118] + cov.mat[54,123] - cov.mat[118,123]
# se.log.OR = sqrt(var.log.OR)
var.log.OR=
1/nrow(dat[dat$student.level == 'sr' & dat$course.level == 'GR' & dat$rating == 1,])+
1/nrow(dat[dat$student.level == 'sr' & dat$course.level == 'GR' & dat$rating == 0,])+
1/nrow(dat[dat$student.level == 'gr' & dat$course.level == 'GR' & dat$rating == 1,])+
1/nrow(dat[dat$student.level == 'gr' & dat$course.level == 'GR' & dat$rating == 0,])

se.log.OR = sqrt(var.log.OR)
log.OR = beta[54] + beta[123] - (beta[51] + beta[118]) # gr - sr
lwr.log.OR = log.OR - 1.96*se.log.OR
upr.log.OR = log.OR + 1.96*se.log.OR
exp(lwr.log.OR)
exp(upr.log.OR)
exp(log.OR)


GR.OR = c(1, exp(-.39236), exp(-.88639), exp(.47366), exp(-.31754))
UD.OR = c(1, exp(-.06808), exp(.24453), exp(.48378), exp(.62097))
LD.OR = c(1, exp(.09246), exp(.23406), exp(.46080), exp(.86457))

plot(GR.OR,col='red',pch=2, ylab='Odds Ratio with freshmen', xlab='Student level', xaxt='n', ylim=c(0.4, 2.4))
axis(1, at=1:5, labels = c('FR','SO','JR','SR','GR'))
points(UD.OR, col='blue',pch=1)
points(LD.OR, col='black',pch=4)
legend('topleft', c(" Graduate Div"," Upper Div", " Lower Div"),
       col=c('red','blue','black'), pch=c(2,1,4),box.col = 'white',cex = 0.7)


nrow(sotes[sotes$student.level=='gr' & sotes$course.level == 'UD' & sotes$Q13 == 5,])
nrow(sotes[sotes$student.level=='gr' & sotes$course.level == 'UD' & sotes$Q13 <= 5,])
nrow(sotes[sotes$student.level=='gr' & sotes$course.level =='GR' & sotes$Q13 == 5,])
nrow(sotes[sotes$student.level=='gr' & sotes$course.level =='GR' & sotes$Q13 <= 5,])

var.log.OR=
  1/nrow(sotes[sotes$student.level=='gr' & sotes$course.level == 'UD' & sotes$Q13 == 5,])+
  1/nrow(sotes[sotes$student.level=='gr' & sotes$course.level == 'UD' & sotes$Q13 <= 5,])+
  1/nrow(sotes[sotes$student.level=='gr' & sotes$course.level =='GR' & sotes$Q13 == 5,])+
  1/nrow(sotes[sotes$student.level=='gr' & sotes$course.level =='GR' & sotes$Q13 <= 5,])

se.log.OR = sqrt(var.log.OR)
log.OR = beta[4] + beta[119]-(beta[3] + beta[118])
lwr.log.OR = log.OR - 1.96*se.log.OR
upr.log.OR = log.OR + 1.96*se.log.OR
exp(lwr.log.OR)
exp(upr.log.OR)
exp(log.OR)


var.log.OR=
  1/nrow(sotes[sotes$student.level=='so' & sotes$course.level == 'UD' & sotes$Q13 == 5,])+
  1/nrow(sotes[sotes$student.level=='so' & sotes$course.level == 'UD' & sotes$Q13 <= 5,])+
  1/nrow(sotes[sotes$student.level=='so' & sotes$course.level =='LD' & sotes$Q13 == 5,])+
  1/nrow(sotes[sotes$student.level=='so' & sotes$course.level =='LD' & sotes$Q13 <= 5,])

se.log.OR = sqrt(var.log.OR)
log.OR = (0 + 0) - (beta[4] + beta[122])
lwr.log.OR = log.OR - 1.96*se.log.OR
upr.log.OR = log.OR + 1.96*se.log.OR
exp(lwr.log.OR)
exp(upr.log.OR)
exp(log.OR)

###############################################################################

tab=table(dat$student.level,dat$rating)[c(1,4,3,5,2),2:1]
chisq.indep(tab, digits = 5)
stlevel.test <- chisq.indep(tab, verb = FALSE)
round(stlevel.test$std.res, digits = 1)
tab/sum(tab)

chisq.indep(matrix(c(4727,7035,5011,6760),nrow=2,byrow=T))
1/OddsRatio(matrix(c(4727,7035,5011,6760),nrow=2,byrow=T))
matrix(c(4727,7035,5011,6760),nrow=2,byrow=T)/sum(matrix(c(4727,7035,5011,6760),nrow=2,byrow=T))
chisq.indep(matrix(c(9738,13795,8606,10092),nrow=2,byrow=T))
1/OddsRatio(matrix(c(9738,13795,8606,10092),nrow=2,byrow=T))
matrix(c(9738,13795,8606,10092),nrow=2,byrow=T)/sum(matrix(c(9738,13795,8606,10092),nrow=2,byrow=T))
chisq.indep(matrix(c(18344,23887,14019,15380),nrow=2,byrow=T))
1/OddsRatio(matrix(c(18344,23887,14019,15380),nrow=2,byrow=T))
matrix(c(18344,23887,14019,15380),nrow=2,byrow=T)/sum(matrix(c(18344,23887,14019,15380),nrow=2,byrow=T))
chisq.indep(matrix(c(32363,39267,5251,5785),nrow=2,byrow=T))
1/OddsRatio(matrix(c(32363,39267,5251,5785),nrow=2,byrow=T))
matrix(c(32363,39267,5251,5785),nrow=2,byrow=T)/sum(matrix(c(32363,39267,5251,5785),nrow=2,byrow=T))


tab=table(dat$course.level,dat$rating)[c(1,3,2),2:1]
chisq.indep(matrix(c(12354,17349,20627,22457),nrow=2,byrow=T))
1/OddsRatio(matrix(c(12354,17349,20627,22457),nrow=2,byrow=T))
matrix(c(12354,17349,20627,22457),nrow=2,byrow=T)/sum(matrix(c(12354,17349,20627,22457),nrow=2,byrow=T))
chisq.indep(matrix(c(32981,39806,4633,5246),nrow=2,byrow=T))
1/OddsRatio(matrix(c(32981,39806,4633,5246),nrow=2,byrow=T))
matrix(c(32981,39806,4633,5246),nrow=2,byrow=T)/sum(matrix(c(32981,39806,4633,5246),nrow=2,byrow=T))

###############################################################################
x = df0[1,] # copying skeleton 
x[1:length(x)] = 0# and init to zero
x[1] = 1 #intercept
x[which(names(beta) == 'semester2172')] = 0 # Fall
x[which(names(beta) == 'course.levelGR')] = 1 # Upper Div
x[which(names(beta) == 'deptCOMM')] = 1 #
x[which(names(beta) == 'student.levelsr')] = 1 #
x[which(names(beta) == 'class.size')] = 25 # Class size
# e.gradeA ref
x[which(names(beta) == 'course.levelGR.student.levelsr')] = 1
x[which(names(beta) == 'course.levelGR.deptCOMM')] = 1
x[which(names(beta) == 'deptCOMM.student.levelsr')] = 1
# preds=predict(sotes.model, newdata = x,type = 'response', se.fit=T)
# c(preds$fit - 1.96*preds$se.fit, preds$fit + 1.96*preds$se.fit)
(udCOMM = predict(sotes.model, newdata = x, type='response'))

# (grdBUS = predict(sotes.model, newdata = x,type = 'response'))

comm = c(frCOMM,soCOMM,jrCOMM,srCOMM,grCOMM)
psyc = c(frPSYC,soPSYC,jrPSYC,srPSYC,grPSYC)
cmpe = c(frCMPE,soCMPE,jrCMPE,srCMPE,grCMPE)
math.stat = c(frMATH.STAT,soMATH.STAT, jrMATH.STAT, srMATH.STAT, grMATH.STAT)
bus = c(frBUS,soBUS,jrBUS,srBUS,grBUS)

comm1 = c(ldCOMM,udCOMM,grdCOMM)
psyc1 = c(ldPSYC,udPSYC,grdPSYC)
cmpe1 = c(ldCMPE,udCMPE,grdCMPE)
math.stat1 = c(ldMATH.STAT,udMATH.STAT,grdMATH.STAT)
bus1 = c(ldBUS,udBUS,grdBUS)


load('comm.RData')
load('psyc.RData')
load('cmpe.RData')
load('math.stat.RData')
load('bus.RData')

load('comm1.RData')
load('psyc1.RData')
load('cmpe1.RData')
load('math.stat1.RData')
load('bus1.RData')

plot(bus,col=1,ylim=c(0.4,1),ylab='Predicted Probability',xaxt='n',xlab='Student Level',pch=1)
axis(1,1:5,c('FR','SO','JR','SR','GR'))
points(math.stat,col=4,pch=4)
points(cmpe,col=2,pch=2)
points(psyc,col=3,pch=3)
points(comm,col=6,pch=5)
legend('topleft', c('BUS','MATH&STAT','CMPE','PSYC','COMM'),
       col=c(1,4,2,3,6), pch=c(1,4,2,3,5),box.col = 'white',cex = 0.7)

plot(bus1,col=1,ylim=c(0.4,1),ylab='Predicted Probability',xaxt='n',xlab='Course Level',pch=1)
axis(1,1:3,c('LD','UD','GR'))
points(math.stat1,col=4,pch=4)
points(cmpe1,col=2,pch=2)
points(psyc1,col=3,pch=3)
points(comm1,col=6,pch=5)
legend('topleft', c('BUS','MATH&STAT','CMPE','PSYC','COMM'),
       col=c(1,4,2,3,6), pch=c(1,4,2,3,5),box.col = 'white',cex = 0.7)

plot(predictions1, ylim=c(0.5,1), col='blue', ylab='Estimated probabilities',xaxt='n',xlab='Departments')
axis(1, at=1:5, labels = c('ANTH','COMM','CS','ENGR','PSYC'))
points(predictions2,pch=2)

#==========================================================================================================|
x = df0[1,] # copying skeleton 
x[1:length(x)] = 0# and init to zero
x[1] = 1 #intercept
x[which(names(beta) == 'semester2172')] = 0 # Fall
x[which(names(beta) == 'course.levelUD')] = 1 # Upper Div
x[which(names(beta) == 'deptCOMM')] = 1 #
x[which(names(beta) == 'student.levelsr')] = 1 #
x[which(names(beta) == 'class.size')] = 25 # Class size
# e.gradeA ref
x[which(names(beta) == 'course.levelUD.student.levelsr')] = 1
x[which(names(beta) == 'course.levelUD.deptCOMM')] = 1
x[which(names(beta) == 'deptCOMM.student.levelsr')] = 1
preds=predict(sotes.model, newdata = x,type = 'response', se.fit=T)
c(lwr=preds$fit - 1.96*preds$se.fit, preds$fit, upr=preds$fit + 1.96*preds$se.fit)



x = df0[1,] # copying skeleton 
x[1:length(x)] = 0# and init to zero
x[1] = 1 #intercept
x[which(names(beta) == 'semester2172')] = 0 # Fall
x[which(names(beta) == 'course.levelUD')] = 1 # Upper Div
x[which(names(beta) == 'deptMATH.STAT')] = 1 #
x[which(names(beta) == 'student.levelsr')] = 1 # Senior
x[which(names(beta) == 'class.size')] = 25 # Class size
# e.gradeA ref
x[which(names(beta) == 'course.levelUD.student.levelsr')] = 1
x[which(names(beta) == 'course.levelUD.deptMATH.STAT')] = 1
x[which(names(beta) == 'deptMATH.STAT.student.levelsr')] = 1
preds=predict(sotes.model, newdata = x,type = 'response', se.fit=T)
c(preds$fit - 1.96*preds$se.fit, preds$fit + 1.96*preds$se.fit)

###################################11/29/2019#####################################
par(mfrow=c(2,3))
# E.grade vs Rating 5
tab = table(dat$e.grade,dat$rating)[,2:1]
chisq.indep(tab)
tab.prop = tab/rowSums(tab)
plot(tab.prop[,1], ylab='Sample proportion of rating 5', xlab='Expected grade',xaxt='n', yaxt='n', ylim=c(0,.8),cex.lab=1.7)
axis(1,1:length(tab[,1]),rownames(tab),cex.axis=1.7)
axis(2,seq(0,0.8,0.4),cex.axis=1.7)

# Department vs Rating 5
tab = table(dat$dept,dat$rating)[,2:1]
chisq.indep(tab)
tab.prop = tab/rowSums(tab)
ind = order(tab.prop[,1],decreasing = T)
tab.prop[ind,1]
plot(tab.prop[ind,1], ylab='Sample proportion of rating 5', xlab='Departments',xaxt='n', yaxt='n', ylim=c(0,.8),cex.lab=1.7)
axis(1,1:47,rownames(tab[ind,]),cex.axis=1.7)
axis(2,seq(0,0.8,0.4),cex.axis=1.7)

# tab = table(dat$dept)
# ind = order(tab)
# x = as.numeric(tab[ind])
# y = as.numeric(tab.prop[ind,1])
# plot(x,y,xlab='Department size',xaxt='n',ylab='Sample proportion of rating 5')
# axis(1,at=seq(0,10085,1000),seq(0,10085,1000))


# tab = table(dat$college,dat$rating)[,2:1]
# chisq.indep(tab)
# tab.prop = tab/rowSums(tab)
# ind = order(tab.prop[,1],decreasing = T)
# tab.prop[ind,1]
# plot(tab.prop[ind,1], ylab='Sample proportion of rating 5', xlab='College',xaxt='n', yaxt='n', ylim=c(0,.8),cex.lab=1.7)
# axis(1,1:4,rownames(tab[ind,]),cex.axis=1.7)
# axis(2,seq(0,0.8,0.4),cex.axis=1.7)

# Class size vs Rating 5
tab = table(dat$class.size,dat$rating)[,2:1]
chisq.indep(tab)
tab.prop = tab/rowSums(tab)
plot(tab.prop[,1], ylab='Sample proportion of rating 5', xlab='Class size',xaxt='n', yaxt='n', ylim=c(0,.8),cex.lab=1.7)
axis(1,1:length(tab[,1]),rownames(tab),cex.axis=1.7)
axis(2,seq(0,0.8,0.4),cex.axis=1.7)

# tab = table(dat[dat$class.size >= 120, 'dept'])
# nam = names(sort(round(tab[tab != 0]/sum(tab),4),decreasing = T))
# plot(sort(round(tab[tab != 0]/sum(tab),4),decreasing = T),type='h',lwd=10,col='grey',xaxt='n',ylab='Proportion',xlab='Department of size >= 120',yaxt='n',cex.lab=1.6)
# axis(1,1:16,nam,cex.axis=1.2)
# axis(2,seq(0.0,0.21,0.05),cex.axis=1.5)


# Student level vs Rating 5
tab = table(dat$student.level,dat$rating)[,2:1]
chisq.indep(tab)
tab.prop = tab/rowSums(tab)
ind = order(tab.prop[,1],decreasing = T)
tab.prop[ind,1]
plot(tab.prop[ind,1][c(5,4,3,1,2)], ylab='Sample proportion of rating 5', xlab='Student level',xaxt='n', yaxt='n', ylim=c(0,.8),cex.lab=1.7)
axis(1,1:5,c('Fr','So','Jr','Sr','Gr'),cex.axis=1.7)
axis(2,seq(0,0.8,0.4),cex.axis=1.7)

# Course level vs Rating 5
tab = table(dat$course.level,dat$rating)[,2:1]
chisq.indep(tab)
tab.prop = tab/rowSums(tab)
ind = order(tab.prop[,1],decreasing = T)
tab.prop[ind,1]
plot(tab.prop[ind,1][c(3,1,2)], ylab='Sample proportion of rating 5', xlab='Course level',xaxt='n', yaxt='n', ylim=c(0,.8),cex.lab=1.7)
axis(1,1:3,c('Lower','Upper','Grad'),cex.axis=1.7)
axis(2,seq(0,0.8,0.4),cex.axis=1.7)

# Semester vs Rating 5
tab = table(dat$semester,dat$rating)[,2:1]
chisq.indep(tab)
tab.prop = tab/rowSums(tab)
ind = order(tab.prop[,1],decreasing = T)
tab.prop[ind,1]
plot(tab.prop[ind,1], ylab='Sample proportion of rating 5', xlab='Semester',xaxt='n', yaxt='n', ylim=c(0,.8),cex.lab=1.7)
axis(1,1:2,c('Spring','Fall'),cex.axis=1.7)
axis(2,seq(0,0.8,0.4),cex.axis=1.7)


##----------------------
## Standardized Deviance Residual Plot
##----------------------

rdev.s = rstandard(sotes.model,type='deviance')
summary(rdev.s)
par(mar = c(4, 4, 1, 1))
index = c(53388,12762,20654,38855)# outliers
plot(rdev.s, pch = 20, cex = 0.8, xlab='Index',ylab='Standardized deviance residuals with extreme outliers', cex.axis=2)
cols = rep("transparent",length(rdev.s))
cols[index] = "red"
text(1:length(rdev.s),rdev.s,labels=1:length(rdev.s),cex=1,col=cols,font=2)

preds=predict(sotes.model, newdata = df0[index,],type = 'response', se.fit=T)
dat[index,]


# which(is.na(rdev.s))
# predict(sotes.model,newdata = df0[c(1893,7071,75731,index),],type='response',se.fit=T)


# Half-normal plot is too slow for this data takes more than 30 mins
# set.seed(126)
# sotes.hnp <- hnp(sotes.model, resid.type = "standard", sim = 99, conf = 0.95, 
#                 how.many.out = TRUE, print.out = TRUE, paint.out = TRUE)
# 
# ## plot against linear predictor
# par(mar=c(5,5,4,1))
# plot(sotes.model$linear.predictors, rdev.s, pch = 20, cex = 0.7, 
#      col = "maroon", xlim = c(-3, 4), ylim = c(-3, 3),xlab='Linear predictor', ylab='Standardized deviance residual')
# abline(h=0, lty = 3)

##----------------------
## Cook's distance Plot
##----------------------

par(mar = c(5, 5, 1, 1))
cooks.dist = cooks.distance(sotes.model)
summary(cooks.dist)
plot(cooks.dist, type = "h", col = "gray20", ylab = "Cook's Distance",cex.lab=2,cex.axis=2)
text(cooks.dist[index] ~ index, labels = index, cex = 2, pos = 4, offset = 0.4)# same outliers from rstandard
points(index, cooks.dist[index], pch = 21)

##----------------------
## Partial residual plot
##----------------------

r.part = residuals(sotes.model,type = 'partial')
plot(df0$class.size, r.part[,54], pch = 20,col='grey',ylab='Partial residuals',xlab='Class size')
lines(lowess(df0$class.size, r.part[,54]), col = "turquoise3", lwd = 1.3)

##----------------------
## Added variable plot
##----------------------

library(car)
which(names(beta) == 'deptMATH.STAT')
avPlot(sotes.model,names(beta)[33],col.lines='blue',pch=20)

##----------------------
## Constructed variable plot
##----------------------

crPlot(sotes.model,names(beta)[55],col.lines='blue',pch=20, lty=1)

##----------------------
## ROC curve
##----------------------
sum(sotes.model$y)/length(sotes.model$y)# proportion of rating 5

library(ROCit)
load('sotes.roc.RData')
sotes.roc <- rocit(score = sotes.model$fitted.values, class = sotes.model$y)
sotes.roc$AUC#0.7051949
plot(sotes.roc, YIndex = T, legend = F)
points(sotes.roc$FPR[43385],sotes.roc$TPR[43385],col='red')# manually find the youden index point?
sotes.roc$Cutoff[43385]# Optimal cutoff = 0.4334038
1-sotes.roc$FPR[43385]# Specificity(TNR) = 0.6147119
sotes.roc$TPR[43385]# Sensitivity(TPR) = 0.6919232
sum(sotes.model$y == (sotes.model$fitted.values > 0.4334038))/length(sotes.model$y)# Accuracy 0.65
# The baseline is a random classifier that predicts the rating without
# looking at the data, so it has 50% chance of classifying correctly.
# We want the model to perform better than this random classifier
# and have high true positive rate while achieving low false positive rate at the same time.
# A perfect model would show 100% TPR and 0% FPR, which would locate the optimal point
# at the top left corner which is the furthest away from the baseline.
# Youden's J statistic = sensitivity + specifity - 1 which is the vertical distance from the baseline
# can be used to find the optimal cutoff that has the maximum sensitivity + specificity.
sotes.roc$Cutoff[length(sotes.roc$Cutoff)]
