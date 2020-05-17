#student = cbind(student,G=student$G1+student$G2+student$G3)
#fac = factor(student$higher,level=c('no','yes'),labels = c(0,1))
#student$higher = 1
#student$higher = fac
##score function
score<-function(x){
  if(x>=18&x<=20){grade<-"A"}
  else if (x>=16&x<18){grade<-"B"}
  else if (x>=14&x<16){grade<-"C"}
  else if (x>=12&x<14){grade<-"D"}
  else {grade<-"F"}
  return(grade)
}
gradescaling<-function(x){
  gradescaling=character(length(x))
  for (i in 1:length(x)){gradescaling[i]<-score(x[i])}
  return(gradescaling)
}
#L<-gradescaling(student$avgG)
#student=cbind(student,L)

percentData = student %>% group_by(sex,higher) %>% count(paid) %>%
  mutate(ratio=scales::percent(n/sum(n)))
# barplot with count and percentage #
ggplot(student,aes(x=higher,fill=paid))+facet_wrap(~sex)+geom_bar(position='dodge')+
geom_text(data=percentData,aes(y=n,label=paste(n,'\n(',ratio,')',sep='')),position=position_dodge(0.9),vjust=0.5,size=3.5)


full.model = glm(higher~.,family=binomial(link='logit'),data=student)
summary(full.model)
# response variable = log odds of taking higher education
# As getting 1 year older, the log odds decrease by 2
anova(full.model,test="Chisq")
null.model = glm(higher ~ 1,data=student,family = binomial(link="logit"))
# AIC = -2(log-likelihood) + 2K    Low AIC is good
step(null.model,scope=list(upper=full.model),direction = "both",test="Chisq",data=student)
reduced.model = glm(higher~Mjob+absences+Medu+school+studytime+romantic+G1+age+paid+sex,family=binomial(link = 'logit'),data=student)
summary(reduced.model)
anova(reduced.model,test="Chisq")
# Removed school p-value = 0.9
reduced.model = glm(higher~Mjob+absences+Medu+studytime+romantic+G1+age+paid+sex,family=binomial(link = 'logit'),data=student)
anova(reduced.model,test="Chisq")
# Removed absences p-value = 0.28
reduced.model = glm(higher~Mjob+Medu+studytime+romantic+G1+age+paid+sex,family=binomial(link = 'logit'),data=student)
anova(reduced.model,test="Chisq")
summary(reduced.model)

fitted.results = predict(reduced.model,newdata = student)# they're > 1 which doesn't make sense
tail(exp(fitted.results))

########## k-fold cross validation
library(caret)
k = 10
n = nrow(student)
set.seed(1)
flds = createFolds(y=1:n,k=k,list=TRUE,returnTrain = FALSE)

R2.pred = numeric()
MSP <- numeric()

for (i in 1:k){
  fld = unlist(flds[i])
  sample.val = youtube.transf[fld,]
  sample.est = youtube.transf[-fld,]
  fit = lm(views~subscribers+dislikes+like.ratio+comments+engagement.ratio+title.length+description.length+trend.pub.diff+trend_tag_highest+trend_tag_total+weekdayMon+weekdayTue+weekdayWed+weekdayThu+weekdayFri+weekdaySat,data=sample.est)
  y.hat = predict(fit,sample.val[,c('subscribers','dislikes','like.ratio','comments','engagement.ratio','title.length','description.length','trend.pub.diff','trend_tag_highest','trend_tag_total','weekdayMon','weekdayTue','weekdayWed','weekdayThu','weekdayFri','weekdaySat')])
  y.act = youtube.transf[fld,'views']
  pred_err = y.act - y.hat
  R2.pred[i] = 1 - sum((y.act - y.hat)^2,na.rm=T)/sum((y.act-mean(y.act))^2,na.rm=T)
  intercept[i] = coef(fit)[1]
  subscribers[i] = coef(fit)[2]
  dislikes[i] = coef(fit)[3]
  like.ratio[i] = coef(fit)[4]
  comments[i] = coef(fit)[5]
  engagement.ratio[i] = coef(fit)[6]
  title.length[i] = coef(fit)[7]
  description.length[i] = coef(fit)[8]
  trend.pub.diff[i] = coef(fit)[9]
  trend_tag_highest[i] = coef(fit)[10]
  trend_tag_total[i] = coef(fit)[11]
  weekdayMon[i] = coef(fit)[12]
  weekdayTue[i] = coef(fit)[13]
  weekdayWed[i] = coef(fit)[14]
  weekdayThu[i] = coef(fit)[15]
  weekdayFri[i] = coef(fit)[16]
  weekdaySat[i] = coef(fit)[17]
  MSP[i] <- sum(pred_err^2,na.rm = T)/length(fld)
}

# Deviance column


# Ideas:
# mean of G1, G2, G3 respectively.
# sd of G1, G2, G3
# histogram G1,G2,G3

library('gridExtra')
means = apply(student[,c('G1','G2','G3')],2,mean)
means = as.data.frame(means)
p1 = ggplot(data=student,aes(y=G1))+geom_boxplot()
#+stat_summary(fun.y='mean',geom='point',colour='red',size=2)
p2 = ggplot(data=student,aes(y=G2))+geom_boxplot()
p3 = ggplot(data=student,aes(y=G3))+geom_boxplot()
grid.arrange(p1,p2,p3,nrow=1)


# stripchart paid~higher
# dot plot(ggplot) (G1+G2+G3)/3 against absences
# facet_wrap(~higher)  x= absences y = G3 + stat_smooth(method='lm')
# boxplot x = paid, y = G3
# geom_bar x = goout, fill = absences
# geom_bar x = goout, fill = sex
