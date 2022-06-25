install.packages("tidyquant")
install.packages("tseries")
install.packages("fitdistrplus")
install.packages("forecast")
install.packages("lmtest")
install.packages("rugarch")
install.packages("ks")
library("tidyquant")
library("tseries")
library("fitdistrplus")
library("forecast")
library("lmtest")
library("rugarch")
library("ks")
library("car")

#??quantmod
#??tidyquant
#citation(package = "tseries")

tq_index_options()
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

#??tidyquant

getSymbols("AAPL", from = '2017-01-01',
           to = "2018-10-01",warnings = FALSE,
           auto.assign = TRUE)

head(AAPL)

chart_Series(AAPL)

summary(AAPL$AAPL.Close)

AAPL$AAPL.Close.Log = log(AAPL$AAPL.Close)
AAPL$AAPL.Close.Log
tslength = 1:length(AAPL$AAPL.Close)
AAPL.ts = ts(AAPL$AAPL.Close.Log, start = c(2018,1), frequency = 251)
#AAPL.ts.verify = ts(AAPL$AAPL.Close.Log[1:291], start = c(2018,1), frequency = 251)

#--------- decriptive statistics --------------
COUNT(AAPL$AAPL.Close)      # 1. count
mean(AAPL$AAPL.Close)       # 2. mean
median(AAPL$AAPL.Close)     # 3. median
var(AAPL$AAPL.Close)        # 4. variability
sd(x = AAPL$AAPL.Close)     # 5. St. Deviation
skewness(x = AAPL$AAPL.Close) # 6. skewness
kurtosis(x = AAPL$AAPL.Close) # 7. kurtosis

x=rnorm(439)


hist(AAPL$AAPL.Close, #AAPL Close price distribution
     prob = TRUE)
hist(x)             # ideal normal distribution

#----------- trendiness, seasonality, cyclicality ---------

decompose(AAPL$AAPL.Close.Log) #seasonality is not present
stl(AAPL$AAPL.Close.Log)

#----------- stacionarity --------------

# ADF test
#existence = non-stacionar 
#non-existence = stacionar
#p-value 0.0395, null hypothesis is rejected (stacionarity) @alpha = 0.05)
adf.test(AAPL$AAPL.Close.Log)

#KPSS
#existence = stacionar
#non-existence = non-stacionar
#p-value 0.01, null hypothesis is rejected (non-stacionarity) @alpha = 0.05
kpss.test(AAPL$AAPL.Close.Log)

#stacionarity correction - 1st difference
AAPL$AAPL.Close.Diff = diff(AAPL$AAPL.Close.Log)
plot(AAPL$AAPL.Close.Diff)

adf.test(x= na.omit(AAPL$AAPL.Close.Diff)) #p-value 0.01 null hypothesis is rejected (stacionarity) @alpha = 0.05
kpss.test(AAPL$AAPL.Close.Diff)            #p-value > 0.1 null hypothesis is accepted (stacionarity) @alpha = 0.05)

# ----------- autocorrelation ------------
acf(x= AAPL$AAPL.Close.Log)       #correlation exists
pacf(x= AAPL$AAPL.Close.Log)      #correlation exists in lag 1
fit2 = lm(AAPL$AAPL.Close.Log ~ tslength)
bgtest(fit2)                              #correlation exists

AAPL$AAPL.Close.Diff[1] = 0
acf(x= AAPL$AAPL.Close.Diff, main = "ACF Autokorelace - AAPL 1. diff. log. konečných denních cen")       #correlation exists in lag 0, 8
pacf(x= AAPL$AAPL.Close.Diff, main = "PACF Autokorelace - AAPL 1. diff. log. konečných denních cen")      #correlation exists in lag 8
fit2 = lm(AAPL$AAPL.Close.Diff ~ tslength)
bgtest(fit2)

# ----------- ARIMA --------------
# ----------- ARIMA with diff(lof(close)) data ---------

#graph
autoplot(AAPL$AAPL.Close.Diff)

#auto-arima model
fit.arima=auto.arima(AAPL$AAPL.Close.Diff, seasonal = F, )
print(fit.arima)
tsdisplay(residuals(fit.arima), main='Automatic ARIMA (0,0,0) Model Residuals') #PACF and ACF shows exceeding values at 4, suggesting better model aic value (2222.89)

#adjusted arima model
fit.arima=arima(AAPL$AAPL.Close.Diff, order = c(8,1,8))
print(fit.arima)
tsdisplay(residuals(fit.arima), main='Manual ARIMA (8,1,8) Model Residuals')  #PACF and ACF are more or less within limits, furthermore AIC value is lower (2220.65)
checkresiduals(fit.arima)

# ----------- Residuals -----------
checkresiduals(fit.arima)
tsdisplay(residuals(fit.arima), main='(8,1,8) Model Residuals') #residual plot shows signs of minor heteroskedasticity

#normality
jarque.bera.test(fit.arima$residuals)           #p-value 8.572e-05 null hypothesis is rejected (no normal dist) 2 aplha = 0.05

#Stacionarity of model
adf.test(fit.arima$residuals)       #p-value 0.01 null hypothesis is rejected (stacionarity) @alpha = 0.05
kpss.test(fit.arima$residuals)      #p-value > 0.1 null hypothesis is accepted (stacionarity) @alpha = 0.05)

#heteroskedasticity
bptest(lm(fit.arima$residuals ~ tslength)) # p-value 0.56 null hypothesis of homoskedasticity is accepted @ alpha = 0.05
bptest(lm(AAPL$AAPL.Close.Diff ~ tslength))


# ------------- Forecast -----------
#forecast
fcast = forecast(fit.arima, h = 20)

#autoplot
plot(fcast, ylab = "AAPL 1. diff. log. Close Price USD", xlab = "Days")
autoplot(fit.arima)
summary(fcast)

#predict - transfered to absolute values
logfcast=fcast
logfcast$mean[1]=logfcast$mean[1] + AAPL$AAPL.Close.Log[max(tslength)]
logfcast$mean[1]
logfcast$lower[1]=logfcast$lower[1] + AAPL$AAPL.Close.Log[max(tslength)]
logfcast$lower[1]
logfcast$lower[1,2]=logfcast$lower[1,2] + AAPL$AAPL.Close.Log[max(tslength)]
logfcast$lower[1,2]
logfcast$upper[1]=logfcast$upper[1] + AAPL$AAPL.Close.Log[max(tslength)]
logfcast$upper[1]
logfcast$upper[1,2]=logfcast$upper[1,2] + AAPL$AAPL.Close.Log[max(tslength)]
logfcast$upper[1,2]
logfcast$x=AAPL$AAPL.Close
logfcast$x
logfcast
for (i in 2:20) {
  logfcast$mean[i]=logfcast$mean[i-1] + logfcast$mean[i]
  logfcast$lower[i]=logfcast$lower[i-1] + logfcast$lower[i]
  logfcast$lower[i,2]=logfcast$lower[i-1,2] + logfcast$lower[i,2]
  logfcast$upper[i]=logfcast$upper[i-1] + logfcast$upper[i]
  logfcast$upper[i,2]=logfcast$upper[i-1,2] + logfcast$upper[i,2]
}

logfcast$mean=exp(logfcast$mean)
logfcast$lower=exp(logfcast$lower)
logfcast$upper=exp(logfcast$upper)

plot(logfcast, ylab = "AAPL Close Price USD", xlab = "Days")

