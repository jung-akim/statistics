load('youtube.RData')
full.model = lm(views~.,data=youtube)
# reduced.model = lm(views~subscribers+dislikes+like.ratio+comments+engagement.ratio+title.length+description.length+trend.pub.diff+trend_tag_highest+trend_tag_total+weekday,data=youtube.transf)

plot.outliers = function(fit,index,rownums,title){
  len.rownums = length(rownums)
  cols = rep("grey60",len.rownums)
  #cols[rownames(df) %in% outlier.names] = "transparent"
  cols[index] = "transparent"
  plot(fitted(fit),rstudent(fit),pch='*',col=cols, main=title, ylab='Studentized residuals',xlab='Fitted values')
  
  cols = rep("transparent",len.rownums)
  cols[index] = "red" #These numbers are from the "original" youtube row numbers
  text(fitted(fit),rstudent(fit),labels=rownums,cex=.7,col=cols,font=2)
  #legend('topright',legend='influential points',pch='#',col='red')
}
# rownums from the original 4525 observations with 86 omitted(where 'like.ratio' is na)
# apply(is.na(youtube),2,sum) # number of na's in each column
na.likes = which(is.na(youtube$likes))
na.dislikes = which(is.na(youtube$dislikes))
na.comments = which(is.na(youtube$comments))
na.like.ratio = which(is.na(youtube$like.ratio))
na.rows = union(union(na.likes,na.dislikes),union(na.comments,na.like.ratio))
rownums = (1:nrow(youtube))[-na.rows] # length is 4439
#rownums = which(!is.na(youtube$like.ratio))

# rstandard outliers > 3
rstrd = rstandard(full.model)
rstrd.outliers = sort(abs(rstrd[abs(rstrd)>3]),decreasing=T)# 36 outliers
rstrd.index = which(abs(rstrd)>=tail(rstrd.outliers,1))# index
graphics.off()
plot.outliers(fit=full.model, index=rstrd.index,rownums=rownums,'rstandard outliers > 3')

# leverage points
hii = hatvalues(full.model)# leverage values hii
threshold = 2*(length(variable.names(full.model)))/nrow(youtube)#2*(k+1)/n=0.01060773
leverage.pt = sort(hii[hii>threshold],decreasing=T)#too many: 99
leverage.pt.reduced = head(leverage.pt,10) 
lev.index = which(hii>=tail(leverage.pt.reduced,1))# index for reduced
plot.outliers(fit=full.model,index=lev.index,rownums=rownums,'top 10 leverage points')
full.lev.index = which(hii>=tail(leverage.pt,1))# index for full 99 pts
plot.outliers(fit=full.model,index=full.lev.index,rownums=rownums,'leverage points > 2*(k+1)/n')

# Cook's distance
# Note: We don't have cook>1 Yay!
cook = cooks.distance(full.model)# max(cook) = 0.2909703
influential = sort(cook[cook>1],decreasing=T)
cook.index = which(cook>=tail(influential,1))# index
plot.outliers(fit=full.model,index=cook.index,rownums=rownums,'Cook\'s distance > 1')
#328  850 2193 4462 
#index: 323  835 2154 4378 

# Influential, leverage outliers
lev.rstrd.outliers = intersect(names(leverage.pt),names(rstrd.outliers))# lev.pt + rstrd outliers
lev.rstrd.outliers
infl.outliers = intersect(names(influential),lev.rstrd.outliers)# Leverage + rstandard + Influential
infl.outliers# None

# COVRATIO
covratio = covratio(full.model)
threshold = 1 - 3*length(variable.names(full.model))/nrow(youtube)
infl.cov = sort(covratio[covratio<threshold],decreasing=T)
indices = which(covratio <= head(infl.cov,1))
covratio[indices]
covratio.lev.rstrd.outliers = intersect(lev.rstrd.outliers,names(infl.cov))
covratio.lev.rstrd.index = intersect(indices,intersect(rstrd.index,full.lev.index))
outliers = as.numeric(union(covratio.lev.rstrd.outliers,names(influential)))
outliers.index = union(covratio.lev.rstrd.index,cook.index)
plot.outliers(fit=full.model,index=outliers.index,rownums=rownums,'Leverage points with rstandard >3 that influence betas')

#DFBETAS; how much the observation influences beta
dfbetas = dfbetas(full.model)
max.betas = apply(abs(dfbetas),1,max)
threshold = 2/sqrt(length(fitted(full.model)))# 0.03001839
mean(max.betas)# mean = 0.02702749
infl.betas = sort(max.betas[max.betas>threshold],decreasing=T)
indices = which(max.betas >= tail(infl.betas,1))
dfbetas[indices,]
dfbetas.lev.rstrd.outliers = intersect(lev.rstrd.outliers,names(infl.betas))# "1741" "1532" "3575" "295"  "3296" "1749" "4139" "3379" "3358" "262" 
dfbetas.lev.rstrd.index = intersect(indices, intersect(rstrd.index,full.lev.index))# index for dfbetas.lev.rstrd.outliers
plot.outliers(fit=full.model,index=dfbetas.lev.rstrd.index,rownums=rownums,'Leverage points with rstandard >3 that influence betas')

outliers = as.numeric(union(dfbetas.lev.rstrd.outliers,infl.outliers))# ROW numbers
#[1] 4462 1143  677 2193 3578  850  328 2484   71 1741 4058
#[12] 1355 4062 2351  949 2086  957 3652  332 1275 2027 1537
#[23] 1746 1353 2553 1026 4366 2323 1429  150 1849 1803 2001 4385
lev.rstrd.index = intersect(full.lev.index,rstrd.index)
outliers.index = union(dfbetas.lev.rstrd.index,union(cook.index,lev.rstrd.index))
plot.outliers(fit=full.model,index=outliers.index,rownum=rownums,'Influential points')

# youtube.transf.rm = youtube.transf[-dfbetas.lev.rstrd.index,]

