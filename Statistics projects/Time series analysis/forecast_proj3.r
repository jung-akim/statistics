setwd('Math265/Forecast Project')
library(lmtest)
# If two candidates are similar and one is subset of the other, compare them and choose simpler one.
lrtest(fit1, fit2) 
#################################  Proj 3 350 observations  2/sqrt(350) = 0.1069045
# Hold out the last 35 observations
proj3 = read.table('proj3.txt')
train.pts = proj3[1:315,]
test.pts = proj3[316:350,]
plot(density(proj3[,1]), main='Density of dataset 3', cex.axis=2, cex.main=2) # data looks normal => Can use MLE method to fit the data
plot(train.pts, type='b', ylab='Xt values', xlab='Time units',cex.axis=2, cex.lab = 2, main='Training data')
axis(1,at=seq(1,315,2))
plot(train.pts[3:315]-train.pts[1:313],type='b')
# There is clearly one period of 2.
# The amplitude changes irregularly throughout the time which could indicate that the data
# has seasonality. At certain time interval, the amplitude increases and at certain intervals they decrease.
spec.pgram(train.pts,plot=F)$freq
spec.pgram(train.pts,xaxt='n', main='Raw Periodogram of training data', cex.main =2, cex.lab = 2, ylab='Periodogram')
axis(1, seq(0,0.5,0.01),cex.axis=2)
abline(v=0.5, lty=2)
abline(v=0.13, lty=2)
abline(v=0.25, lty=2, col=3)
# The periodogram seems to have one peak at 0.5 and another low local peak at 0.13
# The frequency of 0.25 could be where the periodogram plunges or it could be the effect of noise from
# the frequency of 0.13.
par(mar=c(5,5,4,1))
acf(train.pts, type = 'correlation',lag.max=100, xaxt='n', main='ACF plot',cex.axis=2, cex.lab=2)
axis(1, at = 0:100,cex.axis=2)
# ACF is decreasing sinusoidally.
par(mar=c(5,5,4,1))
acf(train.pts, type = 'partial',lag.max=100, xaxt='n', main='P.ACF plot',cex.axis=2, cex.lab=2)
axis(1, at = 1:100,cex.axis=2)
acf(train.pts, type = 'partial',lag.max=100, plot=F)
# One could say the p.acf cuts off after lag 2 or lag 5.
spec.pgram(train.pts,plot=F)$freq
spec.pgram(train.pts,xaxt='n', main='Raw Periodogram of training data', cex.main =2, cex.lab = 2,ylab='Periodogram')
axis(1, seq(0,0.5,0.01),cex.axis=2)
abline(v=0.5, lty = 2)# the expected Period of 2
abline(v=0.13, lty = 2)
abline(v=0.003,lty=2, col='grey')# This maybe a positive real root that contributes noise 
abline(v=0.25, lty=2, col=3)# This is a possible dip but based on acf plot, there doesn't seem to be an MA component.
# So it can be considered as the effect of going down from the peak
# Based on p.acf plot and periodogram, we could think of AR(5) that has one positive real root, one negative real root, 
# and one conjugate pair of roots
# Or it could be AR(2) with one positive and one negative real roots.
# Although it was unlikely that MA component exists from the sinusoidally decaying acf plot,
# we could consider that there is a dip at 0.25 and say it could be ARMA(2,2) or ARMA(5,2)

# The candidate models are: AR(5), AR(2), ARMA(2,2), and ARMA(5,2)

##### Step 2. Parameter Estimation for the 4 candidate models:
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
  cat('t-test stat:',t.test.stat[-(p+q+1)],'\ncritical t-value:',qt(p = 0.975, df = length(train.pts)-1-(p+q)))
  cat('\nExceed crit.val?', (abs(t.test.stat) > qt(p = 0.975, df = length(train.pts)-1-(p+q)))[-(p+q+1)])
  return(my.fit)
}
qq.plot = function(fit,pts){# Because we're using MLE method, Residuals are assumed to be normal
  qqnorm(fit$residuals/sqrt(var(fit$residuals)))
  abline(0,1, col='blue')
  # If it doesn't fit straight line,
  # qqnorm(rnorm(length(pts)))
}

# Candidate 1: AR(5) AIC: 2489.798  PSSE: 161874.3  Sigma2: 148.8173 
fit = ar.diagnostic(train.pts, test.pts, p=5)
par(mfrow=c(2,1))
spec.pgram(train.pts,xaxt='n', main='Raw Periodogram of training data', cex.main =2, cex.lab = 2)
my.spectrum(phi.of.b=c(1, -fit$coef[1:5]), theta.of.b=1, variance=fit$sigma2)
# ACF plot tells me that the residuals are uncorrelated, but the p-values testing H0: rho1 = rho2 = rho3 = rho4 = 0
# is below 0.05. the fifth coefficient is significant.
# Assuming AR(5) is the true model, spectrum plot does not quite look like the periodogram of the data.
# The supposed spectrum function has a bump near theta = 2.7, but the periodogram doesn't look like it has a bump.
# This could indicate the model is overfitting.

# Candidate 2: AR(2) AIC: 2526.567  PSSE: 399814.3  Sigma2: 170.8009 
fit = ar.diagnostic(train.pts, test.pts, p=2)
# ACF plot tells me that acf at lag 4 may be nonzero this violates the assumption.
# The coefficients are all significant.

# Candidate 3: ARMA(2,2)
fit = ar.diagnostic(train.pts, test.pts, 2,0,2)
# ACF plot tells me that acf at lag 2 may be nonzero.
# All coefficients are significant.

# Candidate 4: ARMA(5,2)# AIC: 2460.197  PSSE: 190543  Sigma2: 133.2549 
fit1 = ar.diagnostic(train.pts, test.pts, 5,0,2)
par(mfrow=c(2,1))
spec.pgram(train.pts,xaxt='n', main='Raw Periodogram of training data', cex.main =2, cex.lab = 2,ylab='Periodogram')
my.spectrum(phi.of.b=c(1, -fit1$coef[1:5]), theta.of.b=c(1,fit1$coef[6:7]), variance=fit1$sigma2)
# ACF plot tells me that the residuals are uncorrelated, but pvalue is below 0.05 testing H0: rho1 = ... = rho6 = 0
# Assuming ARMA(5,2) is true, spectrum seems to say the model is overfitting the data.
# The second coefficient of MA is insignificant.

# AR(5) and ARMA(5,2) seem to overfit and AR(2) and ARMA(2,2) seem to underfit.
fit2 = ar.diagnostic(train.pts, test.pts, 5,0,1) # AIC: 2459.529  PSSE: 207164  Sigma2: 133.8496 
par(mfrow=c(2,1))
spec.pgram(train.pts,xaxt='n', main='Raw Periodogram of training data', cex.main =2, cex.lab = 2)
my.spectrum(phi.of.b=c(1, -fit2$coef[1:5]), theta.of.b=c(1,fit2$coef[6]), variance=fit2$sigma2)
# order of AR with 3 and 4 underfit the data
# ACF plot tells me the residuals are uncorrelated.
# Spectrum looks almost the same as ARMA(5,2)
# All coefficients are significant.
# This is better than the four candidate models.
# From the spectrum, it could be that the roots of AR polynomial are one negative real root, two conjugate pair of roots
# with frequency around 0.5 and 2.7. The one root of MA polynomial may be a positive real root.
fit3 = ar.diagnostic(train.pts, test.pts, 5,0,1,method='CSS')
par(mfrow=c(2,1))
spec.pgram(train.pts,xaxt='n', main='Raw Periodogram of training data', cex.main =2, cex.lab = 2)
my.spectrum(phi.of.b=c(1, -fit3$coef[1:5]), theta.of.b=c(1,fit3$coef[6]), variance=fit2$sigma2)

# ======> Final model: ARMA(5,1)
# Prediction on validation plot
fit = ar.diagnostic(train.pts, test.pts, p=5)
predicts = predict(fit, n.ahead = length(test.pts), se.fit = TRUE)
preds <- predicts$pred
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se

plot(281:315,train.pts[281:315],xlim=c(281,350),ylim=c(1000,2000),type='b',main='ARMA(5,1) predictions on validation set',ylab='Xt values', xlab='The last 35 values of training data and 35 predictions on validation set',cex.lab=1.5, cex.axis = 1.5, cex.main= 1.5)
lines(316:350, preds, type='b', col = 2)
lines(316:350, lower.bound, type='l', col=2)
lines(316:350, upper.bound, type='l', col =2)
points(316:350, test.pts,col='blue')



##### Step 3. Fit the model to the whole dataset after differencing and check the residuals again
data = proj3[,1]
ts.plot(data,type='b')# Looks the same as before without the hold-out data; stationary with 2 periods where one is of 2.
final.fit = ar.diagnostic(data,p=5,d=0,q=1) # The acf plot seems to satisfy.
qq.plot(final.fit,data)# Residuals follow normal distribution.

#==============================> ARMA(5,1) passed residual check!

predicts = predict(final.fit, n.ahead = 30, se.fit = TRUE)
preds <- predicts$pred
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se
predictions = round(cbind(lower.bound, preds, upper.bound, upper.bound - lower.bound),2) 
predictions
par(mar=c(5,5,4,1),mfrow=c(2,1))
plot(321:350, proj3[321:350,],xlim=c(321,380), ylim=c(1000,2000),type="b", ylab='Xt values', xlab='The last 30 values of data and 30 predictions',cex.lab=1.5,cex.axis=2)
lines(351:380, preds, type="b", col=2)
lines(351:380, lower.bound, type="l", col=3)
lines(351:380, upper.bound, type="l", col=3)

predicts = predict(final.fit, n.ahead = 350, se.fit = TRUE)
preds <- predicts$pred
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se

plot(301:350, proj3[301:350,],xlim=c(301,700), ylim=c(1000,2000),type="b", ylab='Xt values', xlab='The last 50 values of data and 350 predictions',cex.lab=1.5,cex.axis=2)
lines(351:700, preds, type="b", col=2)
lines(351:700, lower.bound, type="l", col=3)
lines(351:700, upper.bound, type="l", col=3)
