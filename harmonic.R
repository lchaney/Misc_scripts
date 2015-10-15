#janks -- natural seperation of the data
#stdev -- statistical method
#quantile / quintile




library(psych) #used for harmonic mean calculation
library(ggplot2) #graphing

logit <- function(x) {1 / (1 + 1/exp(x))} #transforms logit to linear

#sigma <- sqrt(harmonic.mean(logit(modglm$residuals))) #sigma is the square root of the harmonic mean of the residuals

sigma <- sqrt(harmonic.mean(modglm$residuals^2)) #sigma is the square root of the harmonic mean of the residuals


#how to get confidence interval via the web
  #modglm$df.residual #residual degrees of freedom
  ##http://www.danielsoper.com/statcalc3/calc.aspx?id=10
  ##t-value (right-tail):	0.84858826
  ##t-value (two-tailed):	+/- 1.29804501	
  #cfint <- sigma * (1.29804501)
  #cfintrt <- sigma * 0.84858826

alph <- .2 #alpha value
cfintrt <- sigma * qt((1 - alph)/2, modglm$df.residual, lower.tail = FALSE) #right-tail confidence interval from t distribution
cfinttt <- sigma * qt((1 - alph)/4, modglm$df.residual, lower.tail = FALSE) #two-tailed confidence interval from t distribution

#equation <- modglm$coefficients["(Intercept)"] + (popdat$sday * modglm$coefficients["sday"]) + (popdat$gspmtcm * modglm$coefficients["gspmtcm"]) #this didn't take into account type use predict instead

equation <- predict(modglm)



emin <- min(equation)
emax <- max(equation)
espread <- emax - emin
ecats <- round(espread / cfinttt, 0)

harmdat <- data.frame(
  yequ <- equation,
  ymaxtt <- equation + cfinttt,
  ymintt <- equation - cfinttt
)
names(harmdat) <- c("yequ", "ymaxtt", "ymintt" )

ggplot(data = harmdat, 
       aes(x = rownames(harmdat), y = yequ, ymax = ymaxtt, ymin = ymintt)) + 
  geom_point() + 
  geom_errorbar() + 
  geom_hline(yintercept = seq(emin, emax, by = espread/3), alpha = 0.5, color = "blue", linetype = 2) #need to fix the horizontal lines