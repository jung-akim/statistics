load('youtube.transf.RData')
nvmax = length(youtube.transf)
############ Transform the predictors and the response
# Dummay variables for weekday
weekdays = model.matrix(object=~weekday, data=youtube.transf)[,-1]# Changing weekdays to dummy vars(remove 'Intercept' column)
# replace 'weekday' with 'weekdays(6 variables, base = Sun)'
youtube.transf = youtube.transf[,-which(names(youtube.transf)=='weekday')]
youtube.transf = cbind(youtube.transf, weekdays)
############ Deleting NA rows to implement regsubsets(It doesn't allow different lengths.)
# all.f = regsubsets(x=youtube.transf[,2:length(youtube.transf)],y=youtube.transf$views,method='forward')
# Error in leaps.setup(x, y, wt = weights, nbest = nbest, nvmax = nvmax,  : 
# NA/NaN/Inf in foreign function call (arg 4)
na.likes = which(is.na(youtube.transf$likes))
na.dislikes = which(is.na(youtube.transf$dislikes))
na.comments = which(is.na(youtube.transf$comments))
na.like.ratio = which(is.na(youtube.transf$like.ratio))
na.rows = union(union(na.likes,na.dislikes),union(na.comments,na.like.ratio)) # 86 NA rows
youtube.transf = youtube.transf[-na.rows,]# Exclude NA rows to do exhaustive search
#=========================================================================================================================================

library('leaps')
all = regsubsets(x=youtube.transf[,-1],y=youtube.transf$views,method='exhaustive',all.best=F,nbest=1,nvmax=nvmax)
# Warning message:
# In leaps.setup(x, y, wt = weights, nbest = nbest, nvmax = nvmax,  :
#                 2  linear dependencies found
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which
p <- apply(Matrix,1, sum)
MSRes <- SSRes/(nrow(youtube.transf)-p)
# Make a nice table
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSRes, Cp)
output
write.csv(output,'exhaustive search3.csv')

k = p - 1
graphics.off()
par(mfrow=c(2,2),mar=c(3,5,3,1))
plot(p,AdjR2, main="AdjR2 plot",xaxt='n',ylim=c(.973,.976),pch='')# views~total.engagement has very low AdjR2 = 0.8215037
text(p,AdjR2,labels=k)
plot(p,Cp,main="Cp plot",xaxt='n',ylim=c(0,60),pch='')# views~total.engagement has extremely large Cp = 27626.16610
text(p,Cp,labels=k)
#!Note: We want to see obs under Cp=p line. So I changed ylim close to xlim
abline(0,1)
plot(p,MSRes,main="MSRes plot",xaxt='n',ylim=c(0.083,0.092),pch='')# views~total.engagement has extremely large MSRes = 0.60693796
text(p,MSRes,labels=k)
plot(p,R2, main="R2 plot",xaxt='n',ylim=c(.973,.976),pch='')# views~total.engagement has extremely large MSRes = 0.8215439
text(p,R2,labels=k)
# Jung-a: I think the 4 graph suggests using "17" predictors.
# But since we have 23 predictors, that seems too many.
# Overlooking the knee a little bit, 11 seems to be okay to me.

