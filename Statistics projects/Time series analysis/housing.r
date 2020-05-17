library(tseries)
library(forecast)
library(MASS)
housing = as.data.frame(readxl::read_xlsx('CSUSHPINSA.xlsx'))# 393 observations. 157th is baseline 2000-01-01
plot(density(housing[,2]), main='')
plot(housing,xlab='Years',ylab='Avg Housing Price')
points(housing[100:303,1],housing[100:303,2],col='green')
plot(1:393,housing[,2])
abline(v=302)
housing[302,1]# "2012-02-01 UTC"
# The bubble started to form around 2000-01-01 as the housing price start to increase unusually
# and reached its peak during the second quarter of the year 2006 and then started to fall until 2012-02-01.
# From the time series plot, before 2000-01-01 when the bubble started to form,
# the housing price was gradually increasing.
# After the bubble popped and started to plummet until 2012-02-01, the housing price started to increase
# very quickly compared to the days before 2000-01-01.
# It leaves us a suspicion that the after the bubble burst, there is after-effects that still drive the housing price up until now.
# To answer this question, we can train the data before the bubble began in 2000-01-01 to fit a model
# And see how the prices would have increased had it not existed a bubble afterwards.
train.pts = housing[1:156,2]
test.pts = housing[157:393,2]
arima.diagnostic = function(train.pts, test.pts= NULL, p, d=1, q=0, method='CSS'){
  my.fit = arima(train.pts, order = c(p,d,q), method = method, xreg=1:length(train.pts))
  tsdiag(my.fit)
  PSSE = ''
  if(!is.null(test.pts)){
    preds = predict(my.fit, n.ahead = length(test.pts), se.fit = FALSE, newxreg = (length(train.pts)+1):(length(train.pts)+length(test.pts)))
    PSSE = sum((test.pts - preds)^2)
  }
  cat('AIC:',my.fit$aic,' PSSE:',PSSE,' Sigma2:',my.fit$sigma2,'\n')
  t.test.stat = my.fit$coef/sqrt(diag(my.fit$var.coef))
  cat('t-test stat:',t.test.stat[-(p+q+1)],'\ncritical t-value:',qt(p = 0.975, df = length(train.pts)-1-(p+q)))
  cat('\nExceed crit.val?', (abs(t.test.stat) > qt(p = 0.975, df = length(train.pts)-1-(p+q)))[-(p+q+1)])
  return(my.fit)
}
my.spectrum <- function(phi.of.b, theta.of.b, variance=1){
  p <- length(phi.of.b)
  q <- length(theta.of.b)
  omega <- seq(from=0, to=pi, by=.001)
  phi.of.e.minus.i.omega <- 1
  phi.of.e.i.omega <- 1
  if(p>1){
    for(i in 2:p){
      phi.of.e.minus.i.omega <-  phi.of.e.minus.i.omega + phi.of.b[i]*exp(complex(imaginary = -(i-1))*omega)
      phi.of.e.i.omega <-  phi.of.e.i.omega + phi.of.b[i]*exp(complex(imaginary = (i-1))*omega)}}
  theta.of.e.minus.i.omega <- 1
  theta.of.e.i.omega <- 1
  if(q>1){
    for(i in 2:q){
      theta.of.e.minus.i.omega <-  theta.of.e.minus.i.omega + theta.of.b[i]*exp(complex(imaginary = -(i-1))*omega)
      theta.of.e.i.omega <-  theta.of.e.i.omega + theta.of.b[i]*exp(complex(imaginary = (i-1))*omega)}}
  my.spectrum <- (variance/(2*pi))*Re(theta.of.e.minus.i.omega*theta.of.e.i.omega/(phi.of.e.minus.i.omega*phi.of.e.i.omega))
  plot(omega, 10*log10(my.spectrum), ylab="spectrum (in decibels)", type="l")
}
plot(train.pts,type='b')
# Since there is an increasing trend in the housing price, the data needs to be differenced.
plot(diff(train.pts),type='b',ylab='Differenced price',xlab='Years',xaxt='n')
axis(1,at=seq(1,156,12),labels = 1987:1999)
# After differencing there seems to be a regular period of about 10
par(mar=c(5,5,4,1))
acf(diff(train.pts), type = 'correlation',lag.max=100, xaxt='n', main='ACF plot',cex.axis=2, cex.lab=2)
axis(1, at = seq(0,100,5),cex.axis=2)
# There seems to be a sinusoidal decay in ACF.
acf(diff(train.pts), type = 'partial',lag.max=50, xaxt='n', main='P.ACF plot',cex.axis=2, cex.lab=2)
axis(1, at = seq(1,100,2),cex.axis=2)
# PACF could cut off after lag 2 or lag 3.
spec.pgram(diff(train.pts),xaxt='n', main='', cex.main =2, cex.lab = 2, ylab='Periodogram')
axis(1, seq(0,0.5,0.01),cex.axis=2)
abline(v=0,lty=2)
abline(v=0.08,lty=2)
abline(v=0.39,lty=2,col=3)
# Periodogram shows two possible peaks around theta = 0 and 0.08 and possible dip around theta = 0.39

# Based on ACF, PACF, and periodogram, the candidate models are ARIMA(2,1,0), ARIMA(3,1,0), ARIMA(2,1,2), and ARIMA(3,1,2)

# ARIMA(2,1,0)
(fit = arima.diagnostic(train.pts,test.pts,2,1,0)) # PSSE: 299580.7  Sigma2: 0.009471152
# Residuals do not satisfy the assumptions of white noise
# All coefficients are significant.

# ARIMA(3,1,0)
(fit = arima.diagnostic(train.pts,test.pts,3,1,0)) # PSSE: 315847.4  Sigma2: 0.008431653 
# Residuals satisfy the assumptions of white noise
# The second coefficient of AR is insignificant, but the third coef of AR is significant.
par(mfrow=c(2,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
my.spectrum(phi.of.b=c(1,-fit$coef[1:3]),theta.of.b = 1)
Arg(polyroot(c(1,-fit$coef[1:3])))# 0.3892784 and pi for AR poly which correspond to periods of 16 and 2
abline(v=0.3892784,lty=2)
abline(v=pi,lty=2,col=3)

# ARIMA(2,1,2) <=============|
(fit = arima.diagnostic(train.pts,test.pts,2,1,2))# PSSE: 290168.4  Sigma2: 0.008179938 
# Residuals satisfy the assumptions of white noise
# The first coefficient of MA is insignificant, but the second coef of MA is significant.
par(mfrow=c(2,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
axis(1, seq(0,0.5,0.01),cex.axis=2)
my.spectrum(phi.of.b=c(1,-fit$coef[1:2]),theta.of.b = c(1,fit$coef[3:4]))
Arg(polyroot(c(1,-fit$coef[1:2])))# 0.4343223 for AR poly which corresponds to period of 14.5
Arg(polyroot(c(1,fit$coef[3:4])))# 1.433505 for MA poly which corresponds to period of 4.4
abline(v=0.4343223,lty=2)
abline(v=1.433505,lty=2,col=3)

# ARIMA(3,1,2)
(fit = arima.diagnostic(train.pts,test.pts,3,1,2))# PSSE: 309255.8  Sigma2: 0.007861593
# Residuals satisfy the assumptions of white noise
# The third coef of AR is insignificant * May not be proper model
par(mfrow=c(2,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
my.spectrum(phi.of.b=c(1,-fit$coef[1:3]),theta.of.b = c(1,fit$coef[4:5]))
Arg(polyroot(c(1,-fit$coef[1:3])))# 4.811076e-01 and zero for AR poly which corresponds to period of 13.1 and None
Arg(polyroot(c(1,fit$coef[4:5])))# 1.26973 for MA poly which corresponds to period of 4.95
abline(v=4.811076e-01,lty=2)
abline(v=1.26973,lty=2,col=3)

# ==> ARIMA(3,1,0) and ARIMA(2,1,2) have significant coefficients and meet the residual assumptions.
# Comparing the PSSE and MSE, "ARIMA(2,1,2)" seems to be the proper model for prediction assuming no bubble.
train.pts = housing[1:156,2]
test.pts = housing[157:393,2]
fit = arima.diagnostic(train.pts,test.pts,2,1,2)
## Prediction on validation set
predicts = predict(fit, n.ahead = length(test.pts), se.fit = TRUE, newxreg = (length(train.pts)+1):(length(train.pts)+length(test.pts)))
preds <- predicts$pred
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se
years=c(rep(1987:2018,each=12),rep(2019,9))
par(mfrow=c(1,1))
plot(xaxt='n',1:156,train.pts,xlim=c(1,393),ylim=c(50,220),type='b',main='ARIMA(2,1,2) predictions after the beginning of the bubble',ylab='Avg Housing Price', xlab='Years',cex.lab=1.5, cex.axis = 1.5, cex.main= 1.5)
axis(1,at=seq(1,393,60),labels = seq(1987,2019,5))
lines(157:393, preds, type='b', col = 2)
lines(157:393, lower.bound, type='l', col=2)
lines(157:393, upper.bound, type='l', col =2)
points(157:393, test.pts,col='blue')
points(300:303,housing[300:303,2],col='blue',bg='red',pch=21)
# Not only are all point estimates lower than the actual prices, most of the actual prices are far above the upper bound of prediction interval.
# It seems like the bubble itself officially ended when the prices reached the lowest points between 2011-12-01 and 2012-03-01
# and these were actually very close to the estimated prices they would have been had there been no bubble.
# If there wasn't any after-effect of the bubble burst, the prices after the burst should be close to the prices estimated by the model.
# However, the prices after 2012-03-01 jump back up very quickly at the almost same rate as the rate of the bubble formation between 2000 and 2006.
# It may be natural that the after-effects remain after the plummet and it could have been expected to some extent.
# If a model fit to the data until 2012-03-01 shows that the actual prices are close to the expected after-effects,
# the predictions after September 2019 could be used to measure the effect remaining after the bubble burst.

train.pts = housing[1:303,2]
test.pts = housing[304:393,2]
plot(train.pts,type='b')# There is a polynomial trend in the data. 
bc = boxcox(train.pts~1)# Find the MLE power of Boxcox transformation
bc$x[which.max(bc$y)]# -0.7070707
lam = -0.7
plot(diff((train.pts^lam-1)/lam),type='b',ylab='Differenced Box-Cox with lambda = -0.7')# Differencing the BoxCox transformation is better than only differencing
adf.test(diff((train.pts^lam-1)/lam))# p-value = 0.1446
acf(diff((train.pts^lam-1)/lam),type='correlation',lag.max=300)# ACF sinusoidally decreasing
acf(diff((train.pts^lam-1)/lam),type='partial',lag.max=100)# PACF cut off after lag 1,2,7, or 9
spec.pgram(diff((train.pts^lam-1)/lam),xaxt='n', main='Periodogram', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
axis(1, seq(0,0.5,0.01),cex.axis=2)
abline(v=0.08,lty=2)
abline(v=0.16,lty=2)
abline(v=0,lty=2)
# Periodogram looks like there are three peaks and one dip
# Candidate models are ARI(1,1),ARI(2,1),ARI(7,1), and ARI(9,1)

# ARI(1,1) and ARI(2,1) have a sinusoidal decay in residuals
### ARI(7,1)
(fit = arima((train.pts^lam-1)/lam, order = c(7,1,0),method='CSS',xreg = 1:length(train.pts)))
### Residuals
par(mfrow=c(2,1))
acf(fit$residuals,type='correlation',lag.max = 100)# Residuals seem uncorrelated
plot(fit$residuals,type='h')# residuals seem to have constant mean although the variance increases a little at the end
abline(h=0)
par(mfrow=c(1,1))
### Coefficients test statistics
round(abs(fit$coef/sqrt(diag(fit$var.coef))),1)
abs(fit$coef/sqrt(diag(fit$var.coef)))>2 # Coefficients are significant
predicts = predict(fit, n.ahead = length(test.pts), se.fit = FALSE, newxreg = (length(train.pts)+1):(length(train.pts)+length(test.pts)))
predicts = (1+predicts*lam)^(1/lam)
sum((test.pts - predicts)^2)# PSSE = 6890.627


### ARI(9,1)
(fit = arima((train.pts^lam-1)/lam, order = c(9,1,0),method='CSS',xreg = 1:length(train.pts)))
### Residuals
par(mfrow=c(2,1))
acf(fit$residuals,type='correlation',lag.max = 100)# Residuals seem uncorrelated
plot(fit$residuals,type='h')# residuals seem to have constant mean although the variance increases a little at the end
abline(h=0)
par(mfrow=c(1,1))
### Coefficients test statistics
round(abs(fit$coef/sqrt(diag(fit$var.coef))),1)
abs(fit$coef/sqrt(diag(fit$var.coef)))>2 # Coefficients are significant
predicts = predict(fit, n.ahead = length(test.pts), se.fit = FALSE, newxreg = (length(train.pts)+1):(length(train.pts)+length(test.pts)))
sum((test.pts - predicts)^2)# PSSE = 2827786
### ARI(7,1) and ARI(9,1) approximately satisfy the residual assumptions of white noise.
### There seems to be no difference between ARI(7,1) and ARI(9,1) in terms of PSSE and MSE.
### Thus, ARI(7,1) may make more sense.
train.pts = housing[1:303,2]
test.pts = housing[304:393,2]
(fit = arima((train.pts^lam-1)/lam, order = c(7,1,0),method='CSS',xreg = 1:length(train.pts)))
par(mfrow=c(2,1))
spec.pgram(diff((train.pts^lam-1)/lam),xaxt='n', main='Periodogram', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
axis(1, seq(0,0.5,0.01),cex.axis=2)
# abline(v=c(0.085,0.16,0),lty=2)
my.spectrum(c(1,-fit$coef[1:7]),1,fit$sigma2)
abline(v=c(0.5639076,  2.719498,  1.422192,  0),lty=2)


predicts = predict(fit, n.ahead = length(test.pts)+120, se.fit = TRUE, newxreg = (length(train.pts)+1):(length(train.pts)+length(test.pts)+120))
preds = predicts$pred
lower.bound = preds - 2*predicts$se
upper.bound = preds + 2*predicts$se
preds = (preds*lam + 1)^(1/lam)
lower.bound = (lower.bound*lam + 1)^(1/lam)
upper.bound = (upper.bound*lam + 1)^(1/lam)

plot(xaxt='n',1:303,train.pts,xlim=c(1,513),ylim=c(0,500),type='b',main='ARI(7,1) predictions after the bubble burst',xlab='Years',ylab='Avg Housing price')
axis(1,at=seq(1,513,24),labels = seq(1987,2029,2))
lines(304:393, preds[1:length(test.pts)], type='b', col = 2)
lines(304:393, lower.bound[1:length(test.pts)], type='l', col=2)
lines(304:393, upper.bound[1:length(test.pts)], type='l', col =2)
points(304:393, test.pts,col='blue')
lines(394:513, lower.bound[(length(test.pts)+1):(length(test.pts)+120)], col='#ff61b6')
lines(394:513, upper.bound[(length(test.pts)+1):(length(test.pts)+120)], col='#ff61b6')
points(394:513, preds[(length(test.pts)+1):(length(test.pts)+120)], col='#ff61b6')

# The extremely wide 95% prediction intervals of the model indicates high uncertainty in the fluctuation of the prices.
# The model actually predicted the true trend of the prices after 2012-03-01 quite well.
# which indicates the after-effects of the bubble are quite what could have been expected.
# The pink dots and lines are the predicted prices of this model for 10 years after Sep 2019 which shows that
# the prices are likely to increase even faster in the next 10 years or so with high uncertainty.


plot(housing[,2],type='b')# There is a polynomial trend in the data. 
bc = boxcox(housing[,2]~1)# Find the MLE power of Boxcox transformation
bc$x[which.max(bc$y)]# 0.22222
lam = -0.2
plot(diff((housing[,2]^lam-1)/lam),type='b')# Differencing the BoxCox transformation is better than only differencing
plot(diff(housing[,2]),type='b')# Differencing the BoxCox transformation is better than only differencing
adf.test(diff((housing[,2]^lam-1)/lam))# p-value = 0.1589
acf(diff((housing[,2]^lam-1)/lam),type='correlation',lag.max=300)# ACF sinusoidally decreasing
acf(diff((housing[,2]^lam-1)/lam),type='partial',lag.max=100)# PACF cut off after lag 1,2,4,7,9,10 or 13
spec.pgram(diff((housing[,2]^lam-1)/lam),xaxt='n', main='Periodogram', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
axis(1, seq(0,0.5,0.01),cex.axis=2)
abline(v=c(0.08,0.17,0.25,0),lty=2)

# Candidate models are ARI(1,1), ARI(2,1), ARI(4,1), ARI(7,1), ARI(9,1), ARI(10,1), ARI(13,1)
# ARI with p <= 9 do not have the uncorrelated residuals.
### ARI(10,1) sigma^2 estimated as 4.728e-07
(fit = arima((housing[,2]^lam-1)/lam, order = c(10,1,0),method='CSS',xreg = 1:length(housing[,2])))
### Residuals
par(mfrow=c(2,1))
acf(fit$residuals,type='correlation',lag.max = 100)# Residuals seem uncorrelated
plot(fit$residuals,type='h')# residuals seem to have constant mean although the variance increases a little at the end
abline(h=0)
par(mfrow=c(1,1))
### Coefficients test statistics
round(abs(fit$coef/sqrt(diag(fit$var.coef))),1)
abs(fit$coef/sqrt(diag(fit$var.coef)))>2 # Coefficients are significant (ar5, ar6, ar8, ar9 are insignificant)

### ARI(11,1) sigma^2 estimated as 4.695e-07
(fit = arima((housing[,2]^lam-1)/lam, order = c(11,1,0),method='CSS',xreg = 1:length(housing[,2])))
### Residuals
par(mfrow=c(2,1))
acf(fit$residuals,type='correlation',lag.max = 100)# Residuals seem uncorrelated
plot(fit$residuals,type='h')# residuals seem to have constant mean although the variance increases a little at the end
abline(h=0)
par(mfrow=c(1,1))
### Coefficients test statistics
round(abs(fit$coef/sqrt(diag(fit$var.coef))),1)
abs(fit$coef/sqrt(diag(fit$var.coef)))>2 # Coefficients are insignificant (6 are insign including the 11th one)

### ARI(12,1) sigma^2 estimated as 4.452e-07 
(fit = arima((housing[,2]^lam-1)/lam, order = c(12,1,0),method='CSS',xreg = 1:length(housing[,2])))
### Residuals
par(mfrow=c(2,1))
acf(fit$residuals,type='correlation',lag.max = 100)# Residuals seem uncorrelated
plot(fit$residuals,type='h')# residuals seem to have constant mean although the variance increases a little at the end
abline(h=0)
par(mfrow=c(1,1))
### Coefficients test statistics
round(abs(fit$coef/sqrt(diag(fit$var.coef))),1)
abs(fit$coef/sqrt(diag(fit$var.coef)))>2 # Coefficients are significant (ar5, ar8, ar9, ar10 are insignificant)

### ARI(13,1) sigma^2 estimated as 3.85e-07
(fit = arima((housing[,2]^lam-1)/lam, order = c(13,1,0),method='CSS',xreg = 1:length(housing[,2])))
### Residuals
par(mfrow=c(2,1))
acf(fit$residuals,type='correlation',lag.max = 100)# Residuals seem uncorrelated
plot(fit$residuals,type='h')# residuals seem to have constant mean although the variance increases a little at the end
abline(h=0)
par(mfrow=c(1,1))
### Coefficients test statistics
round(abs(fit$coef/sqrt(diag(fit$var.coef))),1)
abs(fit$coef/sqrt(diag(fit$var.coef)))>2 # Coefficients are significant (ar5, ar6, ar7, ar8, ar9, ar10 are insignificant)


### Final model is ARI(10,1) which has the fewer coefficients and satisfy the residuals assumptions.
(fit = arima((housing[,2]^lam-1)/lam, order = c(10,1,0),method='CSS',xreg = 1:length(housing[,2])))
predicts = predict(fit, n.ahead = 120, se.fit = TRUE, newxreg = (length(housing[,2])+1):(length(housing[,2])+120))
preds = predicts$pred
lower.bound = preds - 2*predicts$se
upper.bound = preds + 2*predicts$se
preds = (preds*lam + 1)^(1/lam)
lower.bound = (lower.bound*lam + 1)^(1/lam)
upper.bound = (upper.bound*lam + 1)^(1/lam)

par(mfrow=c(2,1))
spec.pgram(diff((housing[,2]^lam-1)/lam),xaxt='n', main='Periodogram', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
axis(1, seq(0,0.5,0.01),cex.axis=1)
my.spectrum(c(1,-fit$coef[1:10]),1,fit$sigma2)
# Arg(polyroot(c(1,-fit$coef[1:10])))# .5204782, 2.572721, 1.689505, 1.084469, pi, 0 
2*pi/c(.5204782, 2.572721, 1.689505, 1.084469, pi, 0 )# Period:  12.071947  2.442233  3.718950  5.793790  2.000000       Inf
abline(v=c(.5204782, 2.572721, 1.689505, 1.084469, pi, 0),lty=2)

plot(xaxt='n',1:393,housing[,2],xlim=c(1,513),ylim=c(0,500),type='b',main='ARI(10,1) predictions after Sep 2019',xlab='Years',ylab='Avg Housing price')
axis(1,at=seq(1,513,24),labels = seq(1987,2029,2))
lines(394:513, preds, type='b', col = 2)
lines(394:513, lower.bound, type='l', col=2)
lines(394:513, upper.bound, type='l', col =2)

# Fitting ARI(10,1) to the whole data shows the similar conclusion as the model fit to the data before 2012-03-01
# in a sense that the estimated prices are increasing with high uncertainty.
# But this model tells us that the increase in the next 10 years will happen at the same pace as in the previous 10 years.
# And both models tell us that while there's a chance that the price will increase exponentially,
# there's also a chance that it will drop to the price in 2012-03-01 in Sep 2029.

# The conclusion from this analysis is that
# There was clearly an after-effect of the housing price bubble in mid 2000's which made the prices increase
# faster than they used to in the pre-bubble era of 90's and 80's.
# The current housing prices are still in effect of the bubble and it may be possible that there will be 
# another bubble in the next 10 years although the estimated prices of the model only go up for the next 10 years.
# The model fit to the data before 2012-03-01 was quite reliable since it predicted prices very close to the actual prices
# between 2012-04-01 and 2019-09-01.
# In that sense, this model could be trustworthy for people who plan to buy their house in a couple of years.
# While the lower bound of the prediction interval is constant as the price in 2012, the upper bound of the interval
# is increasing dramatically, so if a person buys the house right now, the maximum money that s/he can lose
# is about 100 unit dollars. If s/he waits another 10 years, s/he might have to pay at least 200 more than now along with
# the risk of losing up to 250 if the price plunges inferring from the last prediction interval in the plot.
# One might hesitate in fear of bubble again, but the after-effects of the bubble is so strong
# that it makes the prices increase much more rapidly, not letting the prices return to the estimated level when no bubble existed.
# This means that even if one purchases a house at a bubble's peak, it is likely that 
# the price will eventually return to the price in a few years due to the after-effects as well as
# the increasing trend in housing prices.
# 


