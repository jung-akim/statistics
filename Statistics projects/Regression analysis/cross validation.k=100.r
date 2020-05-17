setwd('261A Project')
load('youtube.transf.RData')
model = lm(views~subscribers+dislikes+like.ratio+comments+engagement.ratio+title.length+description.length+trend.pub.diff+trend_tag_highest+trend_tag_total+weekday,data=youtube.transf)
############ Transform the predictors and the response
# Dummay variables for weekday
weekdays = model.matrix(object=~weekday, data=youtube.transf)[,-1]# Changing weekdays to dummy vars(remove 'Intercept' column)
# replace 'weekday' with 'weekdays(6 variables, base = Sun)'
youtube.transf = youtube.transf[,-which(names(youtube.transf)=='weekday')]
youtube.transf = cbind(youtube.transf, weekdays)
# Reason for dummyvariables: When using 'predict' fcn,
# Error: variable 'weekday' was fitted with type "factor" but type "numeric" was supplied

library(caret)
k = 100
n = nrow(youtube.transf)
set.seed(10)
flds = createFolds(y=1:n,k=k,list=TRUE,returnTrain = FALSE)

R2.pred = numeric()
intercept = numeric()
subscribers <- numeric()
dislikes <- numeric()
like.ratio <- numeric()
comments = numeric()
engagement.ratio = numeric()
title.length = numeric()
description.length = numeric()
trend.pub.diff = numeric()
trend_tag_highest = numeric()
trend_tag_total = numeric()
weekdayMon = numeric()
weekdayTue = numeric()
weekdayWed = numeric()
weekdayThu = numeric()
weekdayFri = numeric()
weekdaySat = numeric()
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
output = cbind(R2.pred,MSP,intercept,subscribers,dislikes,like.ratio,comments,engagement.ratio,title.length,
               description.length,trend.pub.diff,trend_tag_highest,trend_tag_total,weekdayMon,weekdayTue,
               weekdayWed,weekdayThu,weekdayFri,weekdaySat)

write.csv(output,'cross validation.csv')

# MSRes of our model = 0.3578
hist(output[,1],main=colnames(output)[1],breaks=k)# mean(output[,'R2.pred'])#0.8908056
hist(output[,2],main=colnames(output)[2],breaks=k)# mean(output[,'MSP'])#0.3552043
hist(output[,3],main=colnames(output)[3],breaks=k)# mean(output[,'intercept'])#8.446399
hist(output[,4],main=colnames(output)[4],breaks=k)# mean(output[,'subscribers'])#5.768313e-05
hist(output[,5],main=colnames(output)[5],breaks=k)# mean(output[,'dislikes'])#0.4412504
hist(output[,6],main=colnames(output)[6],breaks=k)# mean(output[,'like.ratio'])#0.6233443
hist(output[,7],main=colnames(output)[7],breaks=k)# mean(output[,'comments'])#0.4863348
hist(output[,8],main=colnames(output)[8],breaks=k)# mean(output[,'engagement.ratio'])#-7.575853
hist(output[,9],main=colnames(output)[9],breaks=k)# mean(output[,'title.length'])#-0.001977105
hist(output[,10],main=colnames(output)[10],breaks=k)# mean(output[,'description.length'])#-0.001545037
hist(output[,11],main=colnames(output)[11],breaks=k)# mean(output[,'trend.pub.diff'])#0.0001480395
hist(output[,12],main=colnames(output)[12],breaks=k)# mean(output[,'trend_tag_highest'])#0.0002382167
hist(output[,13],main=colnames(output)[13],breaks=k)# mean(output[,'trend_tag_total'])#-0.02539381
hist(output[,14],main=colnames(output)[14],breaks=k)# mean(output[,'weekdayMon'])#0.06657803
hist(output[,15],main=colnames(output)[15],breaks=k)# mean(output[,'weekdayTue'])#0.09144876
hist(output[,16],main=colnames(output)[16],breaks=k)# mean(output[,'weekdayWed'])#0.1095406
hist(output[,17],main=colnames(output)[17],breaks=k)# mean(output[,'weekdayThu'])#0.1882769
hist(output[,18],main=colnames(output)[18],breaks=k)# mean(output[,'weekdayFri'])#0.1480456
hist(output[,19],main=colnames(output)[19],breaks=k)# mean(output[,'weekdaySat'])#-0.002173855
# Intercept = 8.4463727149
# subscribers = 0.0000576849
# dislikes = 0.4412407674
# like.ratio = 0.6232764860
# comments = 0.4863417959
# engagement.ratio = -7.5757811317
# title.length = -0.0019772728
# description.length = -0.0015448595
# trend.pub.diff = 0.0001480458
# trend_tag_highest = 0.0002382206
# trend_tag_total = -0.0253938670
# weekdayMon = 0.0665774643
# weekdayTue = 0.0914534685
# weekdayWed = 0.1095482690
# weekdayThu = 0.1882895300
# weekdayFri = 0.1480527272
# weekdaySat = -0.0021731333


# quote: https://machinelearningmastery.com/k-fold-cross-validation/
# The choice of k is usually 5 or 10, but there is no formal rule. 
# As k gets larger, the difference in size between the training set and the resampling subsets 
# gets smaller. As this difference decreases, the bias of the technique becomes smaller

# from hw 11
#table = cbind(1:1000,flavor,R2.prediction)
#table[table[which(table[,3]<0)],]
#colnames(table) = c('Iteration','Flavor','R2.prediction')
#hist(table[,3],xlim=c(-3,1),ylim=c(0,300),breaks = 100,xaxt='n',main='R2.prediction',xlab='R2.prediction')
#axis(1,at=seq(-3,1,.1))
#mean.R2.prediction = mean(table[,3])  #  mean.R2.prediction = 0.5457136
