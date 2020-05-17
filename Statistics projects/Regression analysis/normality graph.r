setwd('C:/Users/Jung A Kim/Desktop/261A Project')
load('youtube.transf.RData')
fit.10 = lm(views~subscribers+like.ratio+total.engagement+engagement.ratio+title.length+description.length+trend.pub.diff+caps+trend_tag_highest+trend_tag_total,data=youtube.transf)
summary(fit.10)# Under 0.04
model2 = fit.10
final_model = model2

sum(abs(rstudent(final_model))>3)
par(mfrow=c(1,2),mar=c(5,4,2,1))
plot(fitted(final_model),rstudent(final_model),xlab='Fitted Values',ylab='Studentized Residuals')
qqnorm(rstandard(final_model))
abline(0,1)
graphics.off()
v = fitted(final_model)
curve(expr=dnorm,from = -4,to=4,lty=3,ylim=c(0,0.5),ylab='Density of x',xlab='Quantiles')
lines(density((v-mean(v))/sqrt(var(v))))
legend(x = 'topright',legend=c('Standard\nNormal','Standardized\nfitted log(views)'),lty=c(3,1),cex=1)
