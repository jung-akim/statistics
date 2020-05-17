setwd('261A Project')
load('youtube.transf.RData')
############ Transform the predictors and the response
# Dummay variables for weekday
weekdays = model.matrix(object=~weekday, data=youtube.transf)[,-1]# Changing weekdays to dummy vars(remove 'Intercept' column)
# replace 'weekday' with 'weekdays(6 variables, base = Sun)'
youtube.transf = youtube.transf[,-which(names(youtube.transf)=='weekday')]
youtube.transf = cbind(youtube.transf, weekdays)
# Reason for dummyvariables: When using 'predict' fcn,
# Error: variable 'weekday' was fitted with type "factor" but type "numeric" was supplied

library(caret)
k = 10
n = nrow(youtube.transf)
set.seed(10)
flds = createFolds(y=1:n,k=k,list=TRUE,returnTrain = FALSE)

R2.pred = numeric()
intercept = numeric()
subscribers <- numeric()
like.ratio <- numeric()
total.engagement = numeric()
engagement.ratio = numeric()
title.length = numeric()
description.length = numeric()
trend.pub.diff = numeric()
caps = numeric()
trend_tag_highest = numeric()
trend_tag_total = numeric()
MSP <- numeric()

for (i in 1:k){
  fld = unlist(flds[i])
  sample.val = youtube.transf[fld,]
  sample.est = youtube.transf[-fld,]
  fit = lm(views~subscribers+like.ratio+total.engagement+engagement.ratio+title.length+
             description.length+trend.pub.diff+caps+trend_tag_highest+
             trend_tag_total,data=sample.est)
  y.hat = predict(fit,sample.val[,c('subscribers','like.ratio','total.engagement','engagement.ratio',
                                    'title.length','description.length','trend.pub.diff',
                                    'caps','trend_tag_highest','trend_tag_total')])
  y.act = youtube.transf[fld,'views']
  pred_err = y.act - y.hat
  R2.pred[i] = 1 - sum((y.act - y.hat)^2,na.rm=T)/sum((y.act-mean(y.act))^2,na.rm=T)
  intercept[i] = coef(fit)[1]
  subscribers[i] = coef(fit)[2]
  like.ratio[i] = coef(fit)[3]
  total.engagement[i] = coef(fit)[4]
  engagement.ratio[i] = coef(fit)[5]
  title.length[i] = coef(fit)[6]
  description.length[i] = coef(fit)[7]
  trend.pub.diff[i] = coef(fit)[8]
  caps[i] = coef(fit)[9]
  trend_tag_highest[i] = coef(fit)[10]
  trend_tag_total[i] = coef(fit)[11]
  MSP[i] <- sum(pred_err^2,na.rm = T)/length(fld)
}
output = cbind(R2.pred,MSP,intercept,subscribers,like.ratio,total.engagement,engagement.ratio,title.length,
               description.length,trend.pub.diff,caps,trend_tag_highest,trend_tag_total)
rownames(output)=1:10
colnames(output)[1]='R^2 predict.'
write.csv(output,'cross validation k = 10.csv')

hist(output[,1],main='Rsquared Prediction',breaks=k,xlab='')# mean(output[,'R2.pred'])
hist(output[,2],main=colnames(output)[2],breaks=k,xlab='')# mean(output[,'MSP'])
hist(output[,3],main=colnames(output)[3],breaks=k,xlab='')# mean(output[,'intercept'])
hist(output[,4],main=colnames(output)[4],breaks=k,xlab='')# mean(output[,'subscribers'])
hist(output[,5],main=colnames(output)[5],breaks=k,xlab='')# mean(output[,'like.ratio'])
hist(output[,6],main=colnames(output)[6],breaks=k,xlab='')# mean(output[,'total.engagement'])
hist(output[,7],main=colnames(output)[7],breaks=k,xlab='')# mean(output[,'engagement.ratio'])
hist(output[,8],main=colnames(output)[8],breaks=k,xlab='')# mean(output[,'title.length'])
hist(output[,9],main=colnames(output)[9],breaks=k,xlab='')# mean(output[,'description.length'])
hist(output[,10],main=colnames(output)[10],breaks=k,xlab='')# mean(output[,'trend.pub.diff'])
hist(output[,11],main=colnames(output)[11],breaks=k,xlab='')# mean(output[,'caps'])
hist(output[,12],main=colnames(output)[12],breaks=k,xlab='')# mean(output[,'trend_tag_highest'])
hist(output[,13],main=colnames(output)[13],breaks=k,xlab='')# mean(output[,'trend_tag_total'])

# Our model

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
