library(lmtest)
#################################  Proj 2 75 observations  2/sqrt(75) = 0.2309401
# Hold out the last 8 observations
proj2 = read.table('proj2.txt')
sample.mean = mean(proj2[,1])
zero.mean.proj2 = proj2 - sample.mean
train.pts = zero.mean.proj2[1:67,]
test.pts = zero.mean.proj2[68:75,]
plot(density(proj2[,1]), main='Density of dataset 2', cex.axis=2, cex.main=2)
# Looks bimodal, fitting MLE model may not provide a good fit.
summary(proj2)
plot(train.pts, type='b', ylab='Xt values', xlab='Time units',cex.axis=2, cex.lab = 2, main='Training data')
# The length of the periods differ and it is irregular.
# The data seems stationary
spec.pgram(train.pts,plot=F)
spec.pgram(train.pts,xaxt='n', main='Raw Periodogram of training data', cex.main =2, cex.lab = 2)
axis(1, seq(0,0.5,0.01),cex.axis=2)
abline(v=0.208, lty=2)
1/0.208
# There seem to be too many local maxima, which means there are too many frequencies that
# contribute the variability of data.
# Since this data has only 67 data points, having too many frequencies may just indicate
# that the data is noise.
# But if we were to assume that there is period, then the
# first frequency to consider would be 
# 0.208 which is where the highest peak of spectrum happens.
# In this case, the period is 1/0.208
# The spectrum is peak at frequency = 0.208 in (0, 0.5) scale.
par(mar=c(5,5,4,1))
acf(train.pts, type = 'correlation',lag.max=100, xaxt='n', main='ACF plot',cex.axis=2, cex.lab=2)
axis(1, at = 0:100,cex.axis=2)
# It looks like data is white noise.
# acf could die off after lag zero, lag 8 or 17. But the acf at lag 8, 9, and 17 are just a little outside the confidence region.
# It seems highly likely that the acf is zero after lag zero.
# There isn't sinusoidal decay.
acf(train.pts, type = 'partial',lag.max=100, xaxt='n', main='P.ACF plot',cex.axis=2, cex.lab=2)
axis(1, at = 1:100,cex.axis=2)
# p.acf could die off after lag 8
# But only two out of 67 points are outside the confidence region,
# so they can be considered as sampling errors and 
# true p.acf may be zero after lag zero.
# Also it might be unlikely to see all sample p.acf's at lag 1 through lag 7 
# inside the confidence interval if true p.acf at lab 8 is nonzero.
# But the sample pac.f at lag 8 is quite far away from the confidence interval.
# The potential candidate models can be:
# ARMA(0,0) and ARMA(8,0)

##### Step 2. Parameter Estimation for the 2 candidate models:
ar.diagnostic = function(train.pts, test.pts= NULL, p, d=0, q=0, method='CSS-ML'){
  my.fit = arima(train.pts, order = c(p,d,q), method = method)
  tsdiag(my.fit)
  if(p == 1) print(coeftest(my.fit))
  t.test.stat = my.fit$coef/sqrt(diag(my.fit$var.coef))
  cat('t-test stat:',t.test.stat,'\ncritical t-value:',qt(p = 0.975, df = length(train.pts)-1-(p+q)))
  cat('\nExceed crit.val?', abs(t.test.stat) > qt(p = 0.975, df = length(train.pts)-1-(p+q)))
  if(!is.null(test.pts)){
    preds = predict(my.fit, n.ahead = length(test.pts), se.fit = FALSE)
    cat('\nPSSE:',sum((test.pts - preds)^2))
  }
  
  return(my.fit)
}
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
#**************************************** Trying the steps 2 and 3 using 'CSS' and 'Burg' method
# Candidate 2: CSS-ARMA(8, 0)
fit = ar.diagnostic(train.pts, test.pts, p=8, method='CSS')# PSSE: 10326.94
# Residuals look like white noise without particular pattern
# acf of residuals is cut off after lag zero.
# p-values for Ljung-Box statistic show that the first 10 acf's of residuals are zero.
# But all coefficients are nonsignificant

# Candidate 3: Burg-ARMA(8, 0) 
fit = burg.diagnostic(train.pts, test.pts, p=8) # PSSE: 10869.86
# Residuals look like white noise, acf is cut off after lag zero, p-vals show the first 10 acf's are zero
# But all coefficients are insignificant

# Try Candidate 4: CSS-ARMA(1,0)
fit = ar.diagnostic(train.pts, test.pts, p=1, method='CSS')
# Residuals look like white noise, acf is cut off after lag zero, p-values show the first 8 acf's are zero but 9th and 10th aren't.
# The coefficient is not significant

# Conclusion:
# The data is white noise.