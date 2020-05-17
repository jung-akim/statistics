setwd('Math265/Forecast Project')
library(lmtest)
# If two candidates are similar and one is subset of the other, compare them and choose simpler one.
lrtest(fit1, fit2)  
#################################  Proj 1 584 observations  2/sqrt(584) = 0.08276059
# Hold out the last 58 observations
proj1 = read.table('proj1.txt')
train.pts = proj1[1:526,]
test.pts = proj1[527:584,]
plot(train.pts, type='b', ylab='Xt values', xlab='Time units',cex.axis=2, cex.lab = 2, main='Training data')
spec.pgram(train.pts,xaxt='n', main='Raw Periodogram of training data', cex.main =2, cex.lab = 2)
axis(1, seq(0,0.5,0.01),cex.axis=2)
abline(v=0.065, lty=2)
abline(v=0.1, lty =2)
1/0.065
1/0.1
par(mar=c(5,5,4,1))
acf(train.pts, type = 'correlation',lag.max=100, xaxt='n', main='ACF plot',cex.axis=2, cex.lab=2)
axis(1, at = 0:100,cex.axis=2)
acf(train.pts, type = 'partial',lag.max=100, xaxt='n', main='P.ACF plot',cex.axis=2, cex.lab=2)
axis(1, at = 1:100,cex.axis=2)
acf(train.pts, type = 'partial',lag.max=100, plot=F)
##### Step 1. Look at ts.plot, ACF, PACF
# ts.plot looks like it is a stationary process with more than one periods. There are short peaks appearing
# between tall peaks. 584 / 48 short peaks = 12 and 584 / 35 tall peaks = 16 ?
# There is a clear sinusoidal decay in auto correlation function starting from lag 0 which means there does not
# seem to be a MA component in the model.
# The true partial autocorrelation may be zero after lag 3,4,5,6,7, or 8, but assuming that out of 100 partial acf's,
# 5 can be out of the confidence bounds, p could be 3, 4, 5, 6, 7, or 8

##### Step 2. Parameter Estimation for the 6 candidate models:
ar.diagnostic = function(train.pts, test.pts= NULL, p, d=0, q=0, method='CSS-ML'){
  my.fit = arima(train.pts, order = c(p,d,q), method = method, include.mean = TRUE)
  tsdiag(my.fit)
  PSSE = ''
  if(!is.null(test.pts)){
    preds = predict(my.fit, n.ahead = length(test.pts), se.fit = FALSE)
    PSSE = sum((test.pts - preds)^2)
  }
  cat('AIC:',my.fit$aic,' PSSE:',PSSE,' Sigma2:',my.fit$sigma2,'\n')
  t.test.stat = my.fit$coef/sqrt(diag(my.fit$var.coef))
  cat('t-test stat:',t.test.stat,'\ncritical t-value:',qt(p = 0.975, df = length(train.pts)-1-(p+q)))
  cat('\nExceed crit.val?', (abs(t.test.stat) > qt(p = 0.975, df = length(train.pts)-1-(p+q)))[1:(p+q)])
  return(my.fit)
}
qq.plot = function(fit,pts){# Because we're using MLE method, Residuals are assumed to be normal
  qqnorm(fit$residuals/sqrt(var(fit$residuals)))
  abline(0,1, col='blue')
  # If it doesn't fit straight line,
  # qqnorm(rnorm(length(pts)))
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
# Candidate 1: AR(4)
(fit = ar.diagnostic(train.pts, test.pts, 4))# correlation at lag 1 and 2 are way off from boundaries.
qq.plot(fit,train.pts)
#==============================> AR(4) failed residual check
# Candidate 2: AR(5) * AIC: 3929.608 PSSE: 4680050 sigma^2 estimated as 97.01
(fit = ar.diagnostic(train.pts, test.pts, 5))
par(mfrow=c(2,1),mar=c(5,5,4,1))
spec.pgram(train.pts,xaxt='n', main='Periodogram of training data',ylab='Periodogram',cex.main =2, cex.lab = 2)
axis(1, seq(0,0.5,0.01),cex.axis=2)
my.spectrum(c(1,-fit$coef[1:5]),1,variance = fit$sigma2)
# Residuals look like white noise. ACF cuts off at lag 0 and p-values are high at all 10 lags
# We could retain null hypotheses and say that they are white noise.
# Candidate 3: ARMA(4,1) AIC: 3999.83  PSSE: 5628607  Sigma2: 111.0145 
(fit = ar.diagnostic(train.pts,test.pts ,p = 4,q=1))
par(mfrow=c(2,1))
spec.pgram(train.pts,xaxt='n', main='Periodogram of training data',ylab='Periodogram',cex.main =2, cex.lab = 2)
my.spectrum(c(1,-fit$coef[1:4]),fit$coef[5],variance = fit$sigma2)
# Candidate 4: ARMA(5,1) AIC: 3931.47  PSSE: 4703760  Sigma2: 96.98751 
(fit = ar.diagnostic(train.pts,test.pts ,p = 5,q=1))
par(mfrow=c(2,1))
spec.pgram(train.pts,xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 2)
my.spectrum(c(1,-fit$coef[1:5]),fit$coef[6],variance = fit$sigma2)
# Candidate 3: ARMA(4,2) * AIC: 3959.421 PSSE: 4388160 sigma^2 estimated as 102.3
(fit = ar.diagnostic(train.pts,test.pts ,p = 4,q=2))
par(mfrow=c(2,1))
spec.pgram(train.pts,xaxt='n', main='Periodogram of training data',ylab='Periodogram',cex.main =2, cex.lab = 2)
my.spectrum(c(1,-fit$coef[1:4]),fit$coef[5:6],variance = fit$sigma2)
# Candidate 4: ARMA(5,2) * AIC: 3933.361  PSSE: 4749959 sigma^2 estimated as 96.97
(fit = ar.diagnostic(train.pts,test.pts ,p = 5,q=2))
par(mfrow=c(2,1))
spec.pgram(train.pts,xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 2)
my.spectrum(c(1,-fit$coef[1:5]),fit$coef[6:7],variance = fit$sigma2)


# Choose AR(5)
# Residual plots look similar and they all look normal, AIC and PSSE are a little lower.
# Comparing the periodogram to the spectrum of the fitted model, AR(5) looks more like the periodogram than ARMA(4,2), AR(6).


##### Step 3. Fit the model to the whole dataset and check the residuals again
final.fit = arima(proj1[,1], order = c(5,0,0),include.mean = TRUE)
ts.plot(proj1[,1],type='b')# Looks the same as before without the hold-out data; stationary with 2 periods
final.fit = ar.diagnostic(proj1[,1],p=5)# Residuals look like white noise in acf plot along with the high p-values
# The standardized residuals do not show particular pattern such as sinusoidal decay 
# AIC is higher (4355.308 > 3929.608) with the whole data and there is only one insignificant coefficient
# when there were two insigniciant coefficients without the hold-out data
qq.plot(final.fit,proj1[,1])# Just as before, the residuals follow straight line
#==============================> AR(5) passed residual check!
predicts = predict(final.fit, n.ahead = 30, se.fit = TRUE)

preds <- predicts$pred
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se

predictions = round(cbind(lower.bound, preds, upper.bound, upper.bound - lower.bound),2) 

plot(554:584, proj1[554:584,],xlim=c(553,615),ylim=c(-1100,700), type="b", ylab='Xt values', xlab='The last 30 values of data and 30 predictions')
lines(585:614, preds, type="b", col=2)
lines(585:614, lower.bound, type="l", col=2)
lines(585:614, upper.bound, type="l", col=2)


#**************************************** Trying the steps 2 and 3 using 'CSS' method
# Candidate 1: AR(3)
fit = ar.diagnostic(train.pts, test.pts, 3, method='CSS')# most correlations are out of the bounds with sinusoidal decay pattern
# Candidate 2: AR(4)
fit = ar.diagnostic(train.pts, test.pts, 4, method='CSS')# correlation at lag 1 and 2 are way off from boundaries.
#==============================> AR(3) and AR(4) failed residual check
# Candidate 3: AR(5)
fit = ar.diagnostic(train.pts, test.pts, 5, method='CSS')
# Residuals look like white noise. ACF cuts off at lag 1 and p-values are high at all 10 lags
# We could retain null hypotheses and say that they are white noise.
# Candidate 4: AR(6) PSSE = 4633052
fit = ar.diagnostic(train.pts, test.pts, 6, method='CSS')# Residuals look like white noise for the same reason as AR(5)
# Candidate 5: AR(7)
fit = ar.diagnostic(train.pts, test.pts, 7, method='CSS')# Residuals look like white noise for the same reason as AR(5) and AR(6)
# Candidate 6: AR(8)
fit = ar.diagnostic(train.pts, test.pts, 8, method='CSS')# Residuals look like white noise for the same reason as AR(5) and AR(6) and AR(7)

# Choose AR(6)
# Residual plots look similar,
# For p >= 6, about half of the coefficients are considered insignificant
# Most of all, AR(6) has the lowest errors


##### Step 3. Fit the model to the whole dataset and check the residuals again
final.fit = arima(zero.mean.proj1, order = c(6,0,0))
ts.plot(zero.mean.proj1,type='b')# Looks the same as before without the hold-out data; stationary with 2 periods
# Residuals look like white noise in acf plot along with the high p-values
# The standardized residuals do not show particular pattern such as sinusoidal decay 
#==============================> AR(6) passed residual check!
predicts = predict(final.fit, n.ahead = 30, se.fit = TRUE)

preds <- predicts$pred + sample.mean
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se

predictions = round(cbind(lower.bound, preds, upper.bound, upper.bound-lower.bound),2) # Print prediction intervals

plot(554:584, proj1[554:584,],xlim=c(553,615),ylim=c(-1000,700), type="b", ylab='Xt values', xlab='The last 30 values of data and 30 predictions')
lines(585:614, preds, type="b", col=2)
lines(585:614, lower.bound, type="l", col=2)
lines(585:614, upper.bound, type="l", col=2)


#**************************************** Trying the steps 2 and 3 using 'Burg' method
burg.diagnostic = function(train.pts, test.pts= NULL, p){
  my.fit = ar.burg(train.pts, aic=FALSE, order.max = p, method=c("burg"))
  std.resid = my.fit$resid/sqrt(var(my.fit$resid,na.rm = T))
  na.ind = which(is.na(std.resid))
  par(mfrow=c(3,1))
  plot(std.resid,type='h', main='Standardized Residuals')
  abline(h=0)
  acf(as.vector(std.resid[-na.ind]),type = 'correlation',main='ACF')
  pvals = numeric()
  for( i in 1:10) pvals = c(pvals, Box.test(std.resid,type='Ljung-Box',lag=i)$p.value)
  plot(pvals,ylim=c(0,1),main='Ljung-Box test')
  abline(0.05,0,lty=2,col=4)
  t.test.stat = my.fit$ar/sqrt(diag(my.fit$asy.var.coef))
  cat('t-test stat:',t.test.stat, '\ncritical t-value:',qt(p = 0.975, df = length(train.pts)-p))
  cat('\nExceed crit.val?', abs(t.test.stat) > qt(p = 0.975, df = length(train.pts)-p))
  if(!is.null(test.pts)){
    preds = predict(my.fit, n.ahead = length(test.pts), se.fit = FALSE)
    cat('\nPSSE:',sum((test.pts - preds)^2))
  }
  return(my.fit)
}
fit = burg.diagnostic(train.pts, test.pts, 3) # Shows clear sinusoidal decay in residual acf plot
fit = burg.diagnostic(train.pts, test.pts, 4) # At lag 1 and 2, they are out of boundary
#==============================> AR(3) and AR(4) failed residual check
fit = burg.diagnostic(train.pts, test.pts, 5) # PSSE = 4630247
# Residuals look like white noise, cutting off straight after lag 0. Ljung-Box test also suggests that all acf's at
# lag 1 through 10 are zero.
fit = burg.diagnostic(train.pts, test.pts, 6) # PSSE = 4654849
fit = burg.diagnostic(train.pts, test.pts, 7) # PSSE = 4693199
fit = burg.diagnostic(train.pts, test.pts, 8) # PSSE = 4783107

# # Choose AR(5)
# Residual plots look similar,
# Among AR(5)~AR(8), only AR(5) has all significant coefficients
# Most of all, AR(5) has the lowest errors

#################################  Compare AR(5) and AR(6) using prediction widths???
par(mfrow=c(3,1), mar=c(5,6,2,1))
#==============================> AR(5) with CSS-ML passed residual check!
final.fit = arima(proj1[,1], order = c(5,0,0),include.mean = TRUE)# PSSE: 4680050
predicts = predict(final.fit, n.ahead = 30, se.fit = TRUE)

preds <- predicts$pred + sample.mean
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se

predictions1 = cbind(lower.bound, preds, upper.bound, upper.bound - lower.bound) # Print prediction intervals
summary(predictions1[,4])

plot(554:584, proj1[554:584,],xlim=c(553,615),ylim=c(-1000,700), type="b", main='MLE-AR(5)',ylab='Xt values', xlab='The last 30 values of data and 30 predictions', cex.lab = 2, cex.axis = 2, cex.main= 2)
lines(585:614, preds, type="b", col=2)
lines(585:614, lower.bound, type="l", col=2)
lines(585:614, upper.bound, type="l", col=2)

#==============================> AR(6) with CSS passed residual check!
final.fit = arima(proj1[,1], order = c(6,0,0), method='CSS',include.mean = TRUE)# PSSE = 4633052
###### Same as ####### final.fit = arima(proj1[,1],order=c(5,0,0),include.mean = T)
###################### predict(final.fit,n.ahead=30, se.fit=TRUE)

predicts = predict(final.fit, n.ahead = 30, se.fit = TRUE)

preds <- predicts$pred + sample.mean
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se

predictions3 = cbind(lower.bound, preds, upper.bound, upper.bound-lower.bound) # Print prediction intervals
summary(predictions3[,4])

plot(554:584, proj1[554:584,],xlim=c(553,615),ylim=c(-1000,700), type="b", main='OLS-AR(6)',ylab='Xt values', xlab='The last 30 values of data and 30 predictions', cex.lab = 2, cex.axis = 2, cex.main= 2)
lines(585:614, preds, type="b", col=2)
lines(585:614, lower.bound, type="l", col=2)
lines(585:614, upper.bound, type="l", col=2)

#==============================> AR(5) with burg passed residual check!
# Prediction on validation plot
fit = burg.diagnostic(train.pts, test.pts, 5)
predicts = predict(fit, n.ahead = length(test.pts), se.fit = TRUE)
preds <- predicts$pred
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se

plot(469:526,train.pts[469:526],xlim=c(469,584),ylim=c(-1000,1000),type='b',main='AR(5) predictions on validation set',ylab='Xt values', xlab='The last 58 values of training data and 58 predictions on validation set',cex.lab=1.5, cex.axis = 1.5, cex.main= 1.5)
lines(527:584, preds, type='b', col = 2)
lines(527:584, lower.bound, type='l', col=2)
lines(527:584, upper.bound, type='l', col =2)
points(527:584, test.pts,col='blue')

final.fit = ar.burg(proj1[,1], aic=FALSE, order.max = 5, method=c("burg"))# PSSE = 4630247
predicts = predict(final.fit, n.ahead = 30, se.fit = TRUE)

preds <- predicts$pred
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se

predictions2 = cbind(lower.bound, preds, upper.bound, upper.bound - lower.bound) # Print prediction intervals
summary(predictions2[,4])

plot(554:584, proj1[554:584,],xlim=c(553,615),ylim=c(-1000,700), type="b", main='Burg-AR(5)',ylab='Xt values', xlab='The last 30 values of data and 30 predictions', cex.lab = 2, cex.axis = 2, cex.main= 2)
lines(585:614, preds, type="b", col=2)
lines(585:614, lower.bound, type="l", col=2)
lines(585:614, upper.bound, type="l", col=2)

mean(abs(predictions1[,4]-predictions2[,4]))
mean(abs(predictions1[,4]-predictions3[,4]))
mean(abs(predictions3[,4]-predictions2[,4]))
# Interval widths and predictions
# Interval widths are almost the same. AR(6) and AR(5) are very similar as their mean absolute difference is only 0.39
# when the data ranges from -1240 to 1016 and symmetrically distributed.
# Thus, My final choice would be AR(5) to reduce any potential error by estimating one less parameter than AR(6).
burg.diagnostic(proj1[,1],p=5)
par(mar=c(5,5,4,1))
plot(proj1[,1], type='b', ylab='Xt values', xlab='Time units',cex.axis=2, cex.lab = 2, main='Entire data')
acf(proj1[,1],type='correlation',lag.max = 100, main='ACF of the whole data', cex.axis=2,cex.lab=2)

plot(density(proj1[,1]), main='Density of dataset 1', cex.axis=2, cex.main=2)
summary(proj1)
