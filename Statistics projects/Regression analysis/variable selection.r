setwd('C:/Users/Jung A Kim/Desktop/261A Project')
load('youtube.transf.RData')
final_model<-lm(views~subscribers+like.ratio+total.engagement+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
library('leaps')
attach(youtube.transf)
youtube.transf = youtube.transf[,-c(9,10)]
graphics=function(){graphics.off()
  par(mfrow=c(2,2),oma=c(1,1,3,1))}

############ Transform the predictors and the response
# Dummay variables for weekday
weekdays = model.matrix(object=~weekday, data=youtube.transf)[,-1]# Changing weekdays to dummy vars(remove 'Intercept' column)
# replace 'weekday' with 'weekdays(6 variables, base = Sun)'
youtube.transf = youtube.transf[,-which(names(youtube.transf)=='weekday')]
youtube.transf = cbind(youtube.transf, weekdays)
#youtube.transf$weekday = as.factor(youtube.transf$weekday) # R aborts the session...
#=========================================================================================================================================

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

############ Exhaustive search: 2  linear dependencies found
nvmax = length(youtube.transf)
all = regsubsets(x=youtube.transf[,-1],y=youtube.transf$views,method='exhaustive',all.best=F,nbest=3,nvmax=nvmax)
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which
p <- apply(Matrix,1, sum)
MSRes <- SSRes/(nrow(youtube.transf)-p)
# Make a nice table
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSRes, Cp)
write.csv(output,'exhaustive search.csv')
#=========================================================================================================================================


############ ORDER of Forward selection, Backward selection (without 86 NA rows)
all.f = regsubsets(x=youtube.transf[,-1],y=youtube.transf$views,method='forward',nvmax=nvmax)
output = summary(all.f)#3  linear dependencies found
write.csv(output$which,'forward selection.csv')
all.b = regsubsets(x=youtube.transf[,-1],y=youtube.transf$views,method='backward',nvmax=nvmax)
output = summary(all.b)#3  linear dependencies found
write.csv(output$which,'backward selection.csv')
#=========================================================================================================================================

############ Include the NAs again into youtube. Re-Run code line1~16
#=========================================================================================================================================

############ exhaustive models with knee = 12,13,14 (At first included 6 dummy variables)
# model 1
fit.12 = lm(views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+title.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
vif(fit.12)# total.engagement = 892.823909, likes = 706.230706
# Excluding total.engagement raises Cp from 31.74689 to 162.8576
# Excluding likes raises Cp from 31.74689 to 58.0535
fit.11 = lm(views~subscribers+total.engagement+dislikes+like.ratio+comments+engagement.ratio+title.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
vif(fit.11)# total.engagement = 17.650560 ==> removing total.engagement raises Cp from 58.0535 to 14341.03
# Remove comments vif = 11.539450 instead which raises Cp from 58.0535 to 206.0251
fit.10 = lm(views~subscribers+total.engagement+dislikes+like.ratio+engagement.ratio+title.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
vif(fit.10)# Don't remove total.engagement vif = 12.352094 which will increase Cp dramatically.
# Remove the second highest vif(dislikes) = 9.948087 
fit.9 = lm(views~subscribers+total.engagement+like.ratio+engagement.ratio+title.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
vif(fit.9)# Under 2.2
summary(fit.9)# Under 0.02
model1 = fit.9# Res Err = 0.2993     Adj Rsquared = 0.9739



# model 2
fit.13 = lm(views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
vif(fit.13)# total.engagement =  899.237616, likes = 711.159656(exclude)
fit.12 = lm(views~subscribers+dislikes+like.ratio+comments+total.engagement+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
vif(fit.12)# total.engagement = 17.654265, but remove comments vif = 11.541984
fit.11 = lm(views~subscribers+dislikes+like.ratio+total.engagement+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
vif(fit.11)# total.engagement = 12.352440, but remove the second highest dislikes = 9.955007
# since removing total.engagement raises Cp much more dramatically than dislikes
fit.10 = lm(views~subscribers+like.ratio+total.engagement+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
summary(fit.10)# Under 0.04
vif(fit.10)# Under 2.4
model2 = fit.10
final_model = model2############################### Final Model ##########################


# model 3
fit.14 = lm(views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total+weekday,data=youtube.transf)
vif(fit.14)# total.engagement = 901.412878, likes = 713.366512(exclude)
fit.13 = lm(views~subscribers+dislikes+like.ratio+comments+total.engagement+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total+weekday,data=youtube.transf)
vif(fit.13)# total.engagement (vif = 17.846009) but removal of total.engagement --> Gigantic increase in Cp 
# Remove comments( vif =  11.636775)
fit.12 = lm(views~subscribers+dislikes+like.ratio+total.engagement+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total+weekday,data=youtube.transf)
vif(fit.12)# total.engagement vif = 12.429081, dislikes vif = 10.036317
# Remove dislikes
fit.11 = lm(views~subscribers+like.ratio+total.engagement+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total+weekday,data=youtube.transf)
vif(fit.11)# Under 2.4
summary(fit.11)
# Get rid of weekday with p-value > 0.1
fit.10 = lm(views~subscribers+like.ratio+total.engagement+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
summary(fit.10) # Under 0.04
model3 = fit.10 # same as model2

exp(predict(model3,newdata=youtube.transf[1,],interval='confidence'))
#     fit     lwr     upr
#1 5563622 5121507 6043903
########## Gigantic Mallows's Cp creates problem: example) observation 1
# > exp(youtube.transf[1,'views'])-1
# [1] 2564902
# > exp(fitted(model3)[1])-1
# 1
# 5563621
# The predicted value is 2.17 times bigger than the actual value

############ Alternative suggestion: Get rid of "dislikes" instead of "likes" at fit.12
############ which is not surprising since likes has the highest correlation with views with
############ r = 0.8945.
############ If we do that, Mallows's Cp goes down from 14321.67('removing likes') to 188.5391('removing dislikes')
############ This makes vif of 'likes' = 9.295087 
############ Then get rid of 'comments' which is the next largest vif = 7.835508
# > ols_mallows_cp(fit.12,full.model.log)
# [1] 992.6494
############ Then every predictor's vif is under 2.5 except for likes
############ Then get rid of trend_tag_highest (with p-value = 0.780147)
############ Now 9 significant predictors
############ Mallows's Cp goes up to 990.7443, but it's much less than 14326.94(from model 4)
# > model3 = fit.9
# > vif(model3)
# subscribers              likes 
# 1.562384           1.920326 
# like.ratio   engagement.ratio 
# 1.101466           1.526310 
# title.length description.length 
# 1.103081           1.224789 
# trend.pub.diff               caps 
# 1.072733           1.090508 
# trend_tag_total 
# 1.292666 
# > ols_mallows_cp(model3,full.model.log)
# [1] 989.6172
# > vif(model3)
# subscribers              likes         like.ratio   engagement.ratio       title.length 
# 1.562384           1.920326           1.101466           1.526310           1.103081 
# description.length     trend.pub.diff               caps    trend_tag_total 
# 1.224789           1.072733           1.090508           1.292666 
# > ols_mallows_cp(model3,full.model.log)
# [1] 989.6172
# > exp(fitted(model3.modif)[1])-1 - exp(youtube.transf[1,'views'])-1
# 1 
# 43294.33 
# > 43294.33/(exp(youtube.transf[1,'views'])-1)
# [1] 0.01687953



########## Forward selection
fit.0 = lm(views~1, data=youtube.transf)
add1(fit.0,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add total.engagement with the largest F-stat = 20822.1756
fit.1 = lm(views~total.engagement,data=youtube.transf)
add1(fit.1,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add engagement.ratio with the largest F-stat = 25505.0284
fit.2 = lm(views~total.engagement+engagement.ratio,data=youtube.transf)
add1(fit.2,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add trend.pub.diff with the largest F-stat = 66.9088
fit.3 = lm(views~total.engagement+engagement.ratio+trend.pub.diff,data=youtube.transf)
add1(fit.3,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add title.length with the largest F-stat = 48.5267
fit.4 = lm(views~total.engagement+engagement.ratio+trend.pub.diff+title.length,data=youtube.transf)
add1(fit.4,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add subscribers with the largest F-stat = 47.3045
fit.5 = lm(views~total.engagement+engagement.ratio+trend.pub.diff+title.length+subscribers,data=youtube.transf)
add1(fit.5,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add caps with the largest F-stat = 39.1320
fit.6 = lm(views~total.engagement+engagement.ratio+trend.pub.diff+title.length+subscribers+caps,data=youtube.transf)
add1(fit.6,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add description.length with F = 19.7865
fit.7 = lm(views~total.engagement+engagement.ratio+trend.pub.diff+title.length+subscribers+caps+description.length,data=youtube.transf)
add1(fit.7,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add dislikes with F = 7.3918
fit.8 = lm(views~total.engagement+engagement.ratio+trend.pub.diff+title.length+subscribers+caps+description.length+dislikes,data=youtube.transf)
add1(fit.8,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add like.ratio
fit.9 = lm(views~total.engagement+engagement.ratio+trend.pub.diff+title.length+subscribers+caps+description.length+dislikes+like.ratio,data=youtube.transf)
add1(fit.9,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add trend_tag_total
fit.10 = lm(views~total.engagement+engagement.ratio+trend.pub.diff+title.length+subscribers+caps+description.length+dislikes+like.ratio+trend_tag_total,data=youtube.transf)
add1(fit.10,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add trend_tag_highest
fit.11 = lm(views~total.engagement+engagement.ratio+trend.pub.diff+title.length+subscribers+caps+description.length+dislikes+like.ratio+trend_tag_total+trend_tag_highest,data=youtube.transf)
add1(fit.11,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add comments
fit.12 = lm(views~total.engagement+engagement.ratio+trend.pub.diff+title.length+subscribers+caps+description.length+dislikes+like.ratio+trend_tag_total+trend_tag_highest+comments,data=youtube.transf)
add1(fit.12,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add likes
fit.13 = lm(views~total.engagement+engagement.ratio+trend.pub.diff+title.length+subscribers+caps+description.length+dislikes+like.ratio+trend_tag_total+trend_tag_highest+comments+likes,data=youtube.transf)
add1(fit.13,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Stop with the threshold = 0.05
# > ols_mallows_cp(fit.13,full.model.log) # Cp goes down as the # of variables go up.
# [1] 22.56215
library(car)
vif(fit.13) # total.engagement = 899.237616, likes = 711.159656(exclude)
fit.12 = lm(views~engagement.ratio+trend.pub.diff+title.length+subscribers+caps+description.length+dislikes+like.ratio+trend_tag_total+trend_tag_highest+comments+total.engagement,data=youtube.transf)
vif(fit.12) # total.engagement = 17.654265, comments = 11.541984
# Remove comments instead
fit.11 = lm(views~engagement.ratio+trend.pub.diff+title.length+subscribers+caps+description.length+dislikes+like.ratio+trend_tag_total+trend_tag_highest++total.engagement,data=youtube.transf)
vif(fit.11) # Remove dislikes instead of total.engagement
fit.10 = lm(views~engagement.ratio+trend.pub.diff+title.length+subscribers+caps+description.length+like.ratio+trend_tag_total+trend_tag_highest++total.engagement,data=youtube.transf)
vif(fit.10)# Under 2.4
summary(fit.10)# Under 0.04
model4 = fit.10# Same as model 2,3


############ Backward selection step by step. Threshold = 0.1
# (Full model doesn't show comments.disabled, ratings.diabled, & weekdaySun estimates due to singularities)
fit.23 = lm(views~.,data=youtube.transf)
drop1(fit.23,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Drop tag_appeared_in_title
fit.22 = lm(views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+trend_tag_highest+trend_tag_total+weekday,data=youtube.transf)
drop1(fit.22,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+trend_tag_highest+trend_tag_total+weekday,test="F")
# Drop tags.count
fit.21 = lm(views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+trend_tag_highest+trend_tag_total+weekday,data=youtube.transf)
drop1(fit.21,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+trend_tag_highest+trend_tag_total+weekday,test="F")
# Drop trend.day.count
fit.20 = lm(views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+trend_tag_highest+trend_tag_total+weekday,data=youtube.transf)
drop1(fit.20,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+trend_tag_highest+trend_tag_total+weekday,test="F")
# Drop exclamation
fit.19 = lm(views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+trend.pub.diff+caps+question+tag_appeared_in_title_count+trend_tag_highest+trend_tag_total+weekday,data=youtube.transf)
drop1(fit.19,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+trend.pub.diff+caps+question+tag_appeared_in_title_count+trend_tag_highest+trend_tag_total+weekday,test="F")
# Drop weekday
fit.18 = lm(views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+trend.pub.diff+caps+question+tag_appeared_in_title_count+trend_tag_highest+trend_tag_total,data=youtube.transf)
drop1(fit.18,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+trend.pub.diff+caps+question+tag_appeared_in_title_count+trend_tag_highest+trend_tag_total,test="F")
# Drop tag_appeared_in_title_count
fit.17 = lm(views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+trend.pub.diff+caps+question+trend_tag_highest+trend_tag_total,data=youtube.transf)
drop1(fit.17,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+trend.pub.diff+caps+question+trend_tag_highest+trend_tag_total,test="F")
# Drop question
fit.16 = lm(views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
drop1(fit.16,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,test="F")
# Stop with the threshold = 0.05
fit.14 = lm(views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+publish.hour+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
# aliased coefficients in the model == perfect collinearity (ratings.disabled == comments.disabled(without NA's they're the same. They're all-True columns.))
vif(fit.14) # total.engagement = 899.902179, likes = 713.366512
fit.13 = lm(views~subscribers+dislikes+like.ratio+comments+total.engagement+engagement.ratio+publish.hour+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
vif(fit.13) # total.engagement = 17.674798, remove comments vif = 11.542748
fit.12 = lm(views~subscribers+dislikes+like.ratio+total.engagement+engagement.ratio+publish.hour+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
vif(fit.12) # remove dislikes = 9.966046, not total.engagement
fit.11 = lm(views~subscribers+like.ratio+total.engagement+engagement.ratio+publish.hour+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
summary(fit.11)# Remove publish.hour with p-value = 0.190748
fit.10 = lm(views~subscribers+like.ratio+total.engagement+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
summary(fit.10)# Under 0.04
vif(fit.10) # Under 2.4
model5 = fit.10# Same as model 2,3,4
############ Alternative suggestion: Get rid of "dislikes" instead of "likes" at fit.12
############ If we do that, Mallows's Cp goes down from 14317.58('removing likes') to 190.634('removing dislikes')
############ This makes vif of likes = 9.279007
############ Then get rid of comments which is the next largest vif = 7.818349
############ Then every predictor's vif is under 2.5
############ Then get rid of trend_tag_highest (with p-value = 0.690616)
############ Now 10 significant predictors(publish.hour has p-value = 0.08417)
############ Mallows's Cp goes up to 987.9839, but it's much less than 14326.94(from model 5)
# > boxcox(final_model)
# > bc = boxcox(final_model)
# > bc$x[bc$y==max(bc$y)]
# [1] 0.9494949



# Drop publish.hour(without Outliers)
fit.15 = lm(views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
drop1(fit.15,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,test="F")
# aliased coefficients in the model == perfect collinearity (ratings.disabled == comments.disabled(without NA's they're the same. They're all-True columns.))
fit.13 = lm(views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
vif(fit.13)# total.engagement = 900.573794
fit.12 = lm(views~subscribers+likes+dislikes+like.ratio+comments+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
vif(fit.12)# likes = 13.896771
fit.11 = lm(views~subscribers+dislikes+like.ratio+comments+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
vif(fit.11) # comments = 8.146891
summary(fit.11)# exclude caps
model6 = lm(views~subscribers+dislikes+like.ratio+comments+engagement.ratio+title.length+description.length+trend.pub.diff+trend_tag_highest+trend_tag_total,data=youtube.transf)




  
########### Stepwise selection with threshold = 0.05, 0.2(drop)
fit.0 = lm(views~1, data=youtube.transf)
add1(fit.0,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add total.engagement with F = 20822.1756
fit.1 = lm(views~total.engagement, data=youtube.transf)
drop1(fit.1,views~total.engagement,test="F")
# Don't drop
add1(fit.1,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add engagement.ratio
fit.2 = lm(views~total.engagement+engagement.ratio, data=youtube.transf)
drop1(fit.2,views~total.engagement+engagement.ratio, data=youtube.transf,test="F")
# Don't drop
add1(fit.2,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add trend.pub.diff
fit.3 = lm(views~total.engagement+engagement.ratio+trend.pub.diff, data=youtube.transf)
drop1(fit.3,views~total.engagement+engagement.ratio+trend.pub.diff, data=youtube.transf,test="F")
# Don't drop
add1(fit.3,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add title.length
fit.4 = lm(views~total.engagement+engagement.ratio+trend.pub.diff+title.length, data=youtube.transf)
drop1(fit.4,views~total.engagement+engagement.ratio+trend.pub.diff+title.length, data=youtube.transf,test="F")
# Don't drop
add1(fit.4,views~subscribers+likes+dislikes+like.ratio+comments+total.engagement+engagement.ratio+comments.disabled+ratings.disabled+publish.hour+title.length+description.length+tags.count+trend.day.count+trend.pub.diff+caps+exclamation+question+tag_appeared_in_title_count+tag_appeared_in_title+trend_tag_highest+trend_tag_total+weekday,test="F")
# Add subscribers
fit.5 = lm(views~total.engagement+engagement.ratio+trend.pub.diff+title.length+subscribers, data=youtube.transf)
drop1(fit.5,views~total.engagement+engagement.ratio+trend.pub.diff+title.length+subscribers, data=youtube.transf,test="F")
# Drop subscribers...(loops!)




# Check correlation matrix
corr = cor(youtube.transf)
write.csv(corr,'correlation matrix.csv')
