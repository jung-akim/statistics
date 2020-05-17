deposits = read.table('deposits.txt')
nrow(deposits)# 624 points
train.pts = deposits[1:562,]
test.pts = deposits[563:624,]
par(mfrow=c(1,1),mar=c(5,5,4,1))
plot(density(deposits[,1]), main='Density of deposits', cex.axis=2, cex.main=2) # data looks skewed to the right
# The data have several outliers
plot(train.pts, type='b', ylab='Xt values', xlab='Time units',cex.axis=2, cex.lab = 2, main='Training data')
plot(train.pts[1:100], type='b', ylab='Xt values', xlab='Time units',cex.axis=2, cex.lab = 2, main='Training data')
# The data is noisy and has a sinusoidal shape with occasionally high deposits in every 10 to 20 years
# and there also seems to be a short period of a repeated pattern in every 5 to 7 years
# There is a noticeable outlier which is 357th observation which seems to cause inflated variance in deposits for the next century.


par(mar=c(5,5,4,1),mfrow=c(3,1))
acf(train.pts, type = 'correlation',lag.max=100, xaxt='n', main='ACF plot',cex.axis=2, cex.lab=2)
axis(1, at = 0:100,cex.axis=2)
# Looks like ACF is decreasing too slowly.
acf(diff(train.pts), type = 'correlation',lag.max=100, xaxt='n', main='ACF plot',cex.axis=2, cex.lab=2)
axis(1, at = 0:100,cex.axis=2)
# After differencing by 1, ACF cuts off clearly after lag 1
acf(diff(train.pts), type = 'partial',lag.max=100, xaxt='n', main='P.ACF plot',cex.axis=2, cex.lab=2)
axis(1, at = 1:100,cex.axis=2)
# PACF may be cut off after lag 1 or lag between 2 and 5 and doesn't seem like there is a sinusoidal decay after then.
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 2, ylab='Periodogram')
axis(1, seq(0,0.5,0.01),cex.axis=2)
# Since the data is noisy, it's hard to tell where the peaks and dips are.
# There may be peaks between 0 and pi assuming that the PACF cuts off after lag between 1 and 5.
# There may be a dip around frequncy = 0 which means there is one positive real root of MA polynomials
# and this matches the result from ACF plot.

# Based on ACF, PACF, and periodogram, the candidate models are ARIMA(1,1,1), ARIMA(2,1,1), ..., ARIMA(5,1,1)

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

# ARIMA(1,1,1) PSSE: 50564.2  Sigma2: 223.3671<===== Significant coef
(fit = arima.diagnostic(train.pts,test.pts,p=1,d=1,q=1))
# Residuals seem to satisfy the assumptions
# All coefficients are significant.
par(mfrow=c(2,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
# The spectrum seems plausible given that the data is noisy.
my.spectrum(phi.of.b=c(1,-fit$coef[1]),theta.of.b = c(1,fit$coef[2]))
Arg(polyroot(c(1,-fit$coef[1])))# zero for AR poly
Arg(polyroot(c(1,fit$coef[2])))# zero for MA poly both peak and dip at zero at the same time??
# Positive real root of AR polynomial and another positive real root of MA polynomial
# which leads to both peak and dip at zero at the same time??


# ARIMA(2,1,1) PSSE: 50735.13  Sigma2: 225.1776
(fit = arima.diagnostic(train.pts,test.pts,p=2,d=1,q=1))
# Residuals seem to satisfy the assumptions
par(mfrow=c(2,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
my.spectrum(phi.of.b=c(1,-fit$coef[1:2]),theta.of.b = c(1,fit$coef[3]))# The spectrum seems plausible
Arg(polyroot(c(1,-fit$coef[1:2])))# zero and pi for AR poly
Arg(polyroot(c(1,fit$coef[3])))# zero for MA poly both peak and dip at zero at the same time??
# The second coefficient of AR is insignificant.

# ARIMA(3,1,1) PSSE: 51189.2  Sigma2: 228.7463<===== Significant coef
(fit = arima.diagnostic(train.pts,test.pts,p=3,d=1,q=1))
# Residuals seem to satisfy the assumptions. 
# Although the second coefficient of AR is insignificant, the third coef is significant.
par(mfrow=c(2,1))
spec.pgram(diff(train.pts),xaxt='n', main='Periodogram of training data', cex.main =2, cex.lab = 1.5, ylab='Periodogram')
my.spectrum(phi.of.b=c(1,-fit$coef[1:3]),theta.of.b = c(1,fit$coef[4]))# The spectrum seems plausible
Arg(polyroot(c(1,-fit$coef[1:3])))# 0.8370717 and pi for AR poly which correspond to the periods of 7.5 and 2
Mod(polyroot(c(1,-fit$coef[1:3])))
Arg(polyroot(c(1,fit$coef[4])))# zero for MA poly
abline(v=0.8370717,lty=2)
abline(v=pi,lty=2)
abline(v=0,lty=2,col=3)

# ARIMA(4,1,1) PSSE: 50997.69  Sigma2: 223.9102 
(fit = arima.diagnostic(train.pts,test.pts,p=4,d=1,q=1))
# Residuals seem to satisfy the assumptions. 
# All AR coefficients are insignificant except for the first one. This may not be proper model.

# ARIMA(5,1,1) PSSE: 50729.94  Sigma2: 221.3176 
(fit = arima.diagnostic(train.pts,test.pts,p=7,d=1,q=1))
# Residuals seem to satisfy the assumptions. 
# All AR coefficients are insignificant except for the first one. This may not be proper model.

## Possible models narrow down to ARIMA(1,1,1) and ARIMA(3,1,1) that have significant coeefficients
# Their predictions and the intervals are similar in validation set.
# ARIMA(1,1,1) has lower PSSE and MSE than ARIMA(3,1,1). Thus, we may choose the simpler model
# with lower PSSE and MSE.
# The AR coefficient of ARIMA(1,1,1) model tell us that there is no periodicity in the data
# which means that the seemingly weak periodicity observed in the time series plot
# may have been just noise.

## Prediction on validation set
predicts = predict(fit, n.ahead = length(test.pts), se.fit = TRUE, newxreg = (length(train.pts)+1):(length(train.pts)+length(test.pts)))
preds <- predicts$pred
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se

par(mfrow=c(1,1))
plot(501:562,train.pts[501:562],xlim=c(501,624),ylim=c(-100,100),type='b',main='ARIMA(1,1,1) predictions on validation set',ylab='Xt values', xlab='The last 62 values of training data and 62 predictions on validation set',cex.lab=1.5, cex.axis = 1.5, cex.main= 1.5)
lines(563:624, preds, type='b', col = 2)
lines(563:624, lower.bound, type='l', col=2)
lines(563:624, upper.bound, type='l', col =2)
points(563:624, test.pts,col='blue')
sqrt(sum((test.pts[16:62] - preds[16:62])^2)/length(test.pts))
## The 62 predictions on validation set are very similar to each other. They all range between 15 and 17 except for a couple
# of points and these are similar to the sample mean of the last 62 training points in the plot.
# The prediction intervals are very wide probably due to the several outliers between 350th and 450th years.
# In the validation set, there was an extreme outlier(568th observation) and the noise caused by this outlier
# in the next few time units that could not be predicted by the model that was fit to the training set.
# Although the model has the periodicity of 7.5 and 2, the modulus are greater than 2 which is far away from the unit circle
# which indicates that these periodicities are too weak and somewhat closer to noise rather than periodicity 
# such that they are not reflected in the predictions.
# This result may indicate that the data is somewhat close to white noise, but the prediction intervals 
# take into account of the variability of the data which manages to capture 90% of the validation set.
# Also, except for the noise between 563th and 577th years, RMPSSE is 6.4 which is comparatively less than the
# standard deviation of the training data = 19.4.
# Except for the noise, model may be able to predict better than the sample mean.

final.fit = arima(deposits[,1], order = c(1,1,1), xreg=1:length(deposits[,1]), method='CSS')
predicts = predict(final.fit, n.ahead = 100, se.fit = TRUE, newxreg = (length(deposits[,1])+1):(length(deposits[,1])+100))

preds <- predicts$pred
lower.bound <- preds - 2*predicts$se
upper.bound <- preds + 2*predicts$se

predictions1 = cbind(lower.bound, preds, upper.bound, upper.bound - lower.bound) # Print prediction intervals

plot(525:624, deposits[525:624,1],xlim=c(525,724),ylim = c(-100,150),type="b", main='LS-ARIMA(1,1,1)',ylab='Xt values', xlab='The last 100 values of data and 100 predictions', cex.lab = 1.5, cex.axis = 1.5, cex.main= 1.5)
lines(625:724, preds, type="b", col=2)
lines(625:724, lower.bound, type="l", col=2)
lines(625:724, upper.bound, type="l", col=2)

# The future predictions are quite similar to the predictions on the validation set in terms of wide prediction intervals
# and some constant predictions.
# The lower bounds of the prediction intervals are not meaningful since the thickness has to be a positive value.

