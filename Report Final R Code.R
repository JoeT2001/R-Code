library(fds)
library(fda)
library(refund)

###Penalized smoothing example###
Elecdata <- (Electricityconsumption[["y"]][1,])

plot(data, type="l",main="Electricity consumption each January from 1973-2001",
     ylab = "log consumption",xlab="Year",xaxt="n")
axis(1, at = c(2,7,12,17,22,27), label = c(1975,1980,1985,1990,1995,2000))

norder = 6
nbasis = length(data) + norder -2
intbasis = create.bspline.basis(c(1,28), nbasis, norder, c(1:28))
intfdPar = fdPar(intbasis, 4, 0.0001)
intfd = smooth.basis(c(1:28),data, intfdPar)
plot(intfd)

##################################


###The Federal Reserve Dataset###
data("FedYieldcurve")
plot(FedYieldcurve,main="Yield curves for a treasury bond over a period of 10 years")
#################################


###The 10 year bond time series###
mydata <- (FedYieldcurve[["y"]][6,])
plot(mydata, main="Federal Reserve 10 year treasury bonds", type="l", ylab="Interest rate",xaxt = "n", xlab="Year",ylim=c(0,15))
axis(1,
     at = c(0,24,48,72,96,120,144,168,192,216,240,264,288,312,336),
     label=c(1982,1984,1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008,2010))
##################################


###ARIMA FORECASTING OF THE FEDYIELDCURVE###
arimafit <- auto.arima(mydata)
fcast <- forecast(arimafit, h=25)
plot(fcast, main="Federal Reserve 10 year treasury bonds", ylab="Interest rate",xaxt = "n", xlab="Year",ylim=c(0,15))
axis(1,
     at = c(0,24,48,72,96,120,144,168,192,216,240,264,288,312,336,360),
     label=c(1982,1984,1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008,2010,2012))
#PLOTS A FORECAST USING ARIMA (2,1,2) with drift
#############################################