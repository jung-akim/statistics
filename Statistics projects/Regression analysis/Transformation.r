setwd('261A Project')
load('youtube.RData')
first.model = lm(views~.,data=youtube)
full.model.log<-lm(I(log(views))~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+weekday+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total,data=youtube)
num.youtube = apply(youtube[,c(1:10,12:24)],2,as.numeric)
attach(youtube)
pairs(views~title.length+title.word.count+description.length+description.word.count,data=you)# scatterplot matrix
alias(lm(views~subscribers+likes+dislikes+comments+comments.disabled+publish.hour+tags.count+trend.day.count+trend.pub.diff+like.ratio+total.engagement+engagement.ratio+title.length+title.word.count+description.length+description.word.count,data=youtube))
vif(lm(views~subscribers+likes+dislikes+comments+publish.hour+tags.count+trend.day.count+trend.pub.diff+like.ratio+engagement.ratio+title.length+title.word.count+description.length+description.word.count,data=you))
you.predictors = you
you.predictors$views = NULL
cor(you.predictors)
class(you$subscribers)
cor(you$subscribers,you$views)
you$subscribers
cor(you)

fit.subscribers= lm(views~subscribers,data=youtube)#skip
fit.likes = lm(views~likes,data=youtube)#okay
fit.dislikes = lm(views~dislikes,data=youtube)#okay
fit.like.ratio = lm(views~like.ratio, data=youtube)#skip

fit = lm(views~.,data=youtube)
plot(fitted(fit),rstudent(fit))
#Mine; total.engagement, engagement.ratio, tile.length, description.length

fit.total.engagement = lm(views~total.engagement,data=youtube)#constant variance issue(Not Non-linear)

graphics=function(){graphics.off()
  par(mfrow=c(2,2),oma=c(1,1,3,1))}
# total.engagement
graphics()
fit.total.engagement = lm(I(log(views))~total.engagement,data=youtube)#constant variance issue(Not Non-linear)
plot(fitted(fit.total.engagement),rstudent(fit.total.engagement),main='Residual plot')
qqnorm(rstandard(fit.total.engagement),ylim=c(-3,3))
abline(0,1,col='red')
plot(total.engagement,log(views),main=paste('Scatterplot r = ',cor(total.engagement,log(views))))
abline(fit.total.engagement,col='red')
title("log(views)~total.engagement", outer=TRUE)
graphics()
fit.total.engagement = lm(I(log(views))~I(log(total.engagement+1)),data=youtube)
plot(fitted(fit.total.engagement),rstudent(fit.total.engagement),main='Residual Plot')
qqnorm(rstandard(fit.total.engagement),ylim=c(-3,3))
abline(0,1,col='red')
plot(log(total.engagement+1),log(views),main=paste('Scatterplot r = ',cor(log(total.engagement+1),log(views))))
abline(fit.total.engagement,col='red')
title('log(views)~log(total.engagement+1)',outer=T)

# engagement.ratio
graphics()
fit.engagement.ratio = lm(I(log(views))~engagement.ratio, data = youtube)
plot(fitted(fit.engagement.ratio),rstudent(fit.engagement.ratio),main='Residual Plot')
qqnorm(rstandard(fit.engagement.ratio),ylim=c(-3,3))
abline(0,1,col='red')
plot(engagement.ratio,log(views),main=paste('Scatterplot r = ',cor(engagement.ratio,log(views))))# 0.1162487
abline(fit.engagement.ratio,col='red')
title('log(views)~engagement.ratio',outer=T)
#triangular pattern with pointy on the right.
#normal assumption fits well
graphics()
fit.engagement.ratio = lm(I(log(views))~I(sqrt(engagement.ratio)),data=youtube)
plot(fitted(fit.engagement.ratio),rstudent(fit.engagement.ratio),main='Residual Plot')
qqnorm(rstandard(fit.engagement.ratio),ylim=c(-3,3))
abline(0,1,col='red')
plot(sqrt(engagement.ratio),log(views),main=paste('Scatterplot r = ',cor(sqrt(engagement.ratio),log(views))))# 0.1468135
abline(fit.engagement.ratio,col='red')
title('log(views)~sqrt(engagement.ratio)',outer=T)

# title.length
graphics()
fit.title.length = lm(I(log(views))~title.length, data = youtube)
plot(fitted(fit.title.length),rstudent(fit.title.length),main='Residual Plot')
qqnorm(rstandard(fit.title.length),ylim=c(-3,3))
abline(0,1,col='red')
plot(title.length,log(views),main=paste('Scatterplot r = ',cor(title.length,log(views))))# -0.04983594
abline(fit.title.length,col='red')
title('log(views)~title.length',outer=T)
# It looks great! constant variance, Normal Assumption great.

# description.length
graphics()
fit.description.length = lm(I(log(views))~description.length, data=youtube)
plot(fitted(fit.description.length),rstudent(fit.description.length),main='Residual Plot')
qqnorm(rstandard(fit.description.length),ylim=c(-3,3))
abline(0,1,col='red')
plot(description.length,log(views),main=paste('Scatterplot r = ',cor(description.length,log(views))))# 0.1096136
abline(fit.description.length,col='red')# 0.1096136
title('log(views)~description.length',outer=T)
#triangular pattern with pointy on the right.
# normal assumption fits well.
#(both plots look very similar to engagement ratio)
graphics()
fit.description.length = lm(I(log(views))~I(sqrt(description.length)), data=youtube)
plot(fitted(fit.description.length),rstudent(fit.description.length),main='Residual Plot')
qqnorm(rstandard(fit.description.length),ylim=c(-3,3))
abline(0,1,col='red')
plot(sqrt(description.length),log(views),main=paste('Scatterplot r = ',cor(sqrt(description.length),log(views))))# 0.1432769
abline(fit.description.length,col='red')
title('log(views)~sqrt(description.length)',outer=T)

# Tranformed model
graphics.off()
par(mfrow=c(1,1),oma=c(1,1,1,1))
full.model.log<-lm(I(log(views))~I(sqrt(subscribers))+I(log(likes+1))+I(log(dislikes+1))+like.ratio+I(log(comments+1))+I(log(total.engagement+1))+engagement.ratio+comments.disabled+ratings.disabled+weekday+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total,data=youtube)
plot(fitted(full.model.log),rstudent(full.model.log))

#---------------------------------- Scatterplot for 16 predictors -------------------------------------------------
na.likes = which(is.na(youtube$likes))
na.dislikes = which(is.na(youtube$dislikes))
na.comments = which(is.na(youtube$comments))
na.like.ratio = which(is.na(youtube$like.ratio))
na.rows = union(union(na.likes,na.dislikes),union(na.comments,na.like.ratio))
no.na.youtube = youtube[-na.rows,]
graphics()
plot(no.na.youtube$likes,log(no.na.youtube$views))
plot(no.na.youtube$dislikes,log(no.na.youtube$views))
plot(no.na.youtube$comments,log(no.na.youtube$views))
plot(no.na.youtube$publish.hour,log(no.na.youtube$views))
title('Scatterplot: likes,dislikes,comments,publish.hour',outer=T)
graphics()
plot(tag_appeared_in_title_count,log(views))
plot(trend_tag_highest,log(views))
plot(trend_tag_total,log(views))
plot(subscribers,log(views))
title('Scatterplot: tag.app.title.ct,trend.tag.highest,trend.tag.total,subscribers',outer=T)
graphics()
plot(like.ratio,log(youtube$views))
plot(tags.count,log(youtube$views))
plot(trend.day.count,log(youtube$views))
plot(trend.pub.diff,log(youtube$views))
title('Scatterplot: like.ratio,tags.ct,trend.day.ct,trend.pub.diff',outer=T)
graphics()
plot(total.engagement,log(youtube$views))
plot(engagement.ratio,log(youtube$views))
plot(title.length,log(youtube$views))
plot(description.length,log(youtube$views))
title('Scatterplot: total.eng,eng.ratio,title.len,descrp.len',outer=T)
#----------------------------------------------------------------------------------
attach(youtube)
graphics.off()
plot(likes,views)
fit= lm(I(log(views))~.,data=youtube)
plot(fitted(fit),rstudent(fit))
plot(fitted(fit),likes[which()])
#----------------------------------------------------------------------------------
graphics.off()
par(mfrow=c(2,2))
fit.trend.pub.diff = lm(I(log(views))~trend.pub.diff,data=youtube)
plot(fitted(fit.trend.pub.diff),rstudent(fit.trend.pub.diff),main='log(views)~trend.pub.diff')
qqnorm(rstandard(fit.trend.pub.diff),ylim=c(-3,3))
abline(0,1,col='red')
fit.trend.pub.diff = lm(I(log(views))~I(sqrt(trend.pub.diff)),data=youtube)
plot(fitted(fit.trend.pub.diff),rstudent(fit.trend.pub.diff),main='log(views)~sqrt(trend.pub.diff)')
qqnorm(rstandard(fit.trend.pub.diff),ylim=c(-3,3))
abline(0,1,col='red')

fit.trend.pub.diff = lm(I(log(views))~I(log(trend.pub.diff)),data=youtube)
plot(fitted(fit.trend.pub.diff),rstudent(fit.trend.pub.diff),main='log(views)~log(trend.pub.diff)')
qqnorm(rstandard(fit.trend.pub.diff),ylim=c(-3,3))
abline(0,1,col='red')
fit.trend.pub.diff = lm(I(log(views))~I(1/sqrt(trend.pub.diff)),data=youtube)
plot(fitted(fit.trend.pub.diff),rstudent(fit.trend.pub.diff),main='log(views)~1/sqrt(trend.pub.diff)')
qqnorm(rstandard(fit.trend.pub.diff),ylim=c(-3,3))
abline(0,1,col='red')
#--------------------------------------------------------------------------------------------------------
setwd('../../Desktop/261A Project')
load('youtube.RData')
graphics.off()
fit = lm(I(log(views))~.-tag_appeared_in_title_count+I(exp(tag_appeared_in_title_count)),data=youtube)
plot(exp(no.na.youtube$tag_appeared_in_title_count),rstudent(fit),xlab='exp(Tag Appeared In Title Count)',ylab='Studentized Residuals',xlim=c(0,500))
plot(no.na.youtube$trend_tag_total,rstudent(fit),xlab='Tag Appeared In Title Count',ylab='Studentized Residuals')
plot(no.na.youtube$description.length,rstudent(fit),xlab='Tag Appeared In Title Count',ylab='Studentized Residuals')
plot(no.na.youtube$engagement.ratio,rstudent(fit),xlab='Tag Appeared In Title Count',ylab='Studentized Residuals')
plot(no.na.youtube$like.ratio,rstudent(fit),xlab='Tag Appeared In Title Count',ylab='Studentized Residuals')

#--------------------------------------------------------------------------------------------------------
resplot<-function(linear.model){
  plot(predict(linear.model),rstudent(linear.model),
       main="Residual Plot",xlab="Predicted Values",ylab="Studentized Residuals",ylim=c(-3,3))
  abline(0,0)
  abline(1,0,lty=2)
  abline(-1,0,lty=2)
  abline(3,0,lty=3)
  abline(-3,0,lty=3)
  print(mean(residuals(linear.model)^2))
}

qq<-function(linear.model){
  qqnorm(rstandard(linear.model),xlim=c(-3,3),ylim=c(-3,3))
  abline(0,1)
}

look<-function(x,y){
  #par(mfrow=c(2,2))
  #plot(x,y)
  #abline(lm(y~x))
  #qq(lm(y~x))
  resplot(lm(y~x))
}

#-------------------------------------------------------------------------------------------------------
colID<-function(df){
  mat=data.frame(row.names = 1:ncol(df))
  mat[,1]=colnames(df)
  mat[,2]=sapply(df,class)
  colnames(mat)=c("colname","class")
  return(mat)
}