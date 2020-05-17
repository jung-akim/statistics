na.like.ratio = which(is.na(youtube.transf$like.ratio))
youtube.transf = youtube.transf[-na.like.ratio,]

graphics.off()
plot(fitted(final_model),youtube.transf$views,ylab='Log(views)',xlab='Fitted Values of \'views\'')
abline(0,1)

load('youtube.RData')
full.model = lm(views~.,data=youtube)

na.likes = which(is.na(youtube$likes))
na.dislikes = which(is.na(youtube$dislikes))
na.comments = which(is.na(youtube$comments))
na.like.ratio = which(is.na(youtube$like.ratio))
na.rows = union(union(na.likes,na.dislikes),union(na.comments,na.like.ratio)) # 86 NA rows
youtube = youtube[-na.rows,]# Exclude NA rows for equal lengths for the plot

par(mfrow=c(1,2))
plot(fitted.full.model,youtube$views,ylab='Actual Views',xlab='Fitted Views(untransformed full model)')
abline(0,1)

load('youtube.RData')
na.like.ratio = which(is.na(youtube$like.ratio))
youtube.rm = youtube[-na.like.ratio,] # 25 NA like.ratio gone
plot(exp(fitted(final_model)),youtube.rm$views,ylab='Actual Views',xlab='Fitted Views(our final model)')
abline(0,1)
