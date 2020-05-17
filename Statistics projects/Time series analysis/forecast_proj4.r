proj4 = read.table('proj4.txt')
train.pts = proj4[1:225,]
test.pts = proj4[226:250,]
par(mfrow=c(1,1))
plot(density(proj4[,1]), main='Density of dataset 4', cex.axis=2, cex.main=2) # data looks bimodal
plot(train.pts, type='b', ylab='Xt values', xlab='Time units',cex.axis=2, cex.lab = 2, main='Training data')
# Data is non stationary with the increasing trend
# Data needs to be differenced.
plot(diff(train.pts),type='b',ylab='Differenced training points',cex.lab=1.5)
# After differencing by 1, the data looks stationary.
# There seems to be small peaks and tall peaks with regular periods,
# The total number of peaks seem to be 19 so one period could be about 13
# And there may be 10 tall peaks which could indicate period of 25
spec.pgram(diff(train.pts),plot=F)$freq
par(mar=c(5,5,4,1),mfrow=c(3,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 2, ylab='Periodogram')
axis(1, seq(0,0.5,0.01),cex.axis=2)
abline(v=0.07, lty=2)
abline(v=0.10, lty=2)
abline(v=0.4, lty=2,col=3)
# The differenced data seems to have a peak around 0.07 and 0.1 and a possible dip around 0.4 or going down from the peak.
par(mar=c(5,5,4,1))
acf(diff(train.pts), type = 'correlation',lag.max=100, xaxt='n', main='ACF plot',cex.axis=2, cex.lab=2)
axis(1, at = 0:100,cex.axis=2)
# ACF is decreasing sinusoidally.
par(mar=c(5,5,4,1))
acf(diff(train.pts), type = 'partial',lag.max=100, xaxt='n', main='P.ACF plot',cex.axis=2, cex.lab=2)
axis(1, at = 1:100,cex.axis=2)
acf(diff(train.pts), type = 'partial',lag.max=100, plot=F)
# One could say the p.acf cuts off after lag 2 then starts sinusoidal decay, there may be an MA component.
# Possible candidate models are ARIMA(4,1,0), ARIMA(4,1,1), ARIMA(4,1,2)

##### Step 2. Parameter Estimation for the 4 candidate models:
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

# ARIMA(4,1,0)
(fit = arima.diagnostic(train.pts,test.pts,p=4,d=1,q=0))
par(mfrow=c(2,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
my.spectrum(phi.of.b=c(1,-fit$coef[1:4]),theta.of.b = 1)
# The coefficients are all significant.
# ACF shows that residuals are correlated at lag 0, 1, and 2.
# spectrum of the fitted model does not seem to reflect the periodgram in that one peak is much sharper than the other
# while the periodogram shows two mild peaks around 0.5

# ARIMA(4,1,1)
(fit = arima.diagnostic(train.pts,test.pts,p=4,d=1,q=1))
par(mfrow=c(2,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
my.spectrum(phi.of.b=c(1,-fit$coef[1:4]),theta.of.b = c(1,fit$coef[5]))
# The residual assumptions are not met. and the peaks in the spectrum are too sharp.
# All coefficients are significant.

# ARIMA(4,1,2)
(fit = arima.diagnostic(train.pts,test.pts,p=4,d=1,q=2))
par(mfrow=c(2,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
my.spectrum(phi.of.b=c(1,-fit$coef[1:4]),theta.of.b = c(1,fit$coef[5:6]))
# All coefficients are significant. 
# The residual assumptions are approximately met, but not quite.

###### the three ARIMA models do not satisfy the residual assumptions and the spectrum of these models
###### has much sharper peak compared to periodogram. The coefficients were all significant.
###### So we could try higher ordered models with p = 5 or 6.

(fit = arima.diagnostic(train.pts,test.pts,5,1,0))
# Residual assumptions are met with ACF cutting off after lag zero and the p-values for Ljung-Box test are high until lag 10.
# The third coefficient is insignificant, but we might as well include it following the principle of hierarchy
par(mfrow=c(2,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
my.spectrum(phi.of.b=c(1,-fit$coef[1:5]),theta.of.b = 1)
# spectrum has sharper peaks than the periodogram.

(fit = arima.diagnostic(train.pts,test.pts,5,1,1))
# Residual assumptions are met with ACF cutting off after lag zero and the p-values for Ljung-Box test are high until lag 10.
# The third coefficient of AR and the MA coefficient are insignificant. There may not be an MA component.
par(mfrow=c(2,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
my.spectrum(phi.of.b=c(1,-fit$coef[1:5]),theta.of.b = c(1,fit$coef[6]))
# spectrum has sharper peaks than the periodogram.

(fit = arima.diagnostic(train.pts,test.pts,5,1,2))
# Residual assumptions are met with ACF cutting off after lag zero and the p-values for Ljung-Box test are high until lag 10.
# The MA coefficients are insignificant. There may not be an MA component.
par(mfrow=c(2,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
my.spectrum(phi.of.b=c(1,-fit$coef[1:5]),theta.of.b = c(1,fit$coef[6:7]))
# spectrum has sharper peaks than the periodogram.

### Trying AR polynomial with p = 6, the sixth coefficient was insignificant.
### ARIMA with p = 5 satisfy the residual assumptions.
### The three models indicate that there is no MA component.
### Therefore, the final model chosen is ARIMA(5,1,0)
(fit = arima.diagnostic(train.pts, test.pts, 5,1,0))

par(mfrow=c(2,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
my.spectrum(phi.of.b=c(1,-fit$coef[1:5]),theta.of.b = 1)
Arg(polyroot(c(1,-fit$coef[1:5])))
abline(v=.4221688,lty=2)
abline(v=.6366798,lty=2)
abline(v=3.1415927,lty=2)

# Prediction on validation set
predicts = predict(fit, n.ahead = length(test.pts), se.fit = TRUE, newxreg = (length(train.pts)+1):(length(train.pts)+length(test.pts)))
preds <- predicts$pred
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se

plot(201:225,train.pts[201:225],xlim=c(201,250),ylim=c(29000,38000),type='b',main='ARIMA(5,1,0) predictions on validation set',ylab='Xt values', xlab='The last 25 values of training data and 25 predictions on validation set',cex.lab=1.5, cex.axis = 1.5, cex.main= 1.5)
lines(226:250, preds, type='b', col = 2)
lines(226:250, lower.bound, type='l', col=2)
lines(226:250, upper.bound, type='l', col =2)
points(226:250, test.pts,col='blue')

PSSE = sum((test.pts - preds)^2)
### The roots of AR polynomial are two complex conjugate pairs and one negative real root

final.fit = arima(proj4[,1], order = c(5,1,0), xreg=1:length(proj4[,1]), method='CSS')
predicts = predict(final.fit, n.ahead = 100, se.fit = TRUE, newxreg = (length(proj4[,1])+1):(length(proj4[,1])+100))

preds <- predicts$pred
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se

predictions1 = cbind(lower.bound, preds, upper.bound, upper.bound - lower.bound) # Print prediction intervals
summary(predictions1[,4])

plot(151:250, proj4[151:250,],xlim=c(151,350),ylim=c(24000,51000),type="b", main='LS-ARIMA(5,1,0)',ylab='Xt values', xlab='The last 100 values of data and 100 predictions', cex.lab = 1.5, cex.axis = 1.5, cex.main= 1.5)
lines(251:350, preds, type="b", col=2)
lines(251:350, lower.bound, type="l", col=2)
lines(251:350, upper.bound, type="l", col=2)
# The model is
# Xt = X_{t-1} + sample.mean*(1-sum of AR coefficients) + a1*(X_{t-1}-X_{t-2}) + ... + a5*(X_{t-5}-X_{t-6})
# The first prediction(251st) is
# proj4[250,1]+
# 126.8805*(1-sum(final.fit$coef[1:5]))+
#   2.7164*(proj4[250,1]-proj4[249,1])-
#   2.5299*(proj4[249,1]-proj4[248,1])+
#   0.0772*(proj4[248,1]-proj4[247,1])+
#   1.2576*(proj4[247,1]-proj4[246,1])-
#   0.6336*(proj4[246,1]-proj4[245,1])