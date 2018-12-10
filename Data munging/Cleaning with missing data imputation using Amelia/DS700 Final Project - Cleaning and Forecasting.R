require(Amelia)
require(lubridate)
require(ggplot2)
require(forecast)

# Read in dataset with invalid and missing data changed to blank in the spreadsheet
# so that it will come in as NA.
abbeville <- read.csv("AbbevilleToCsv.csv")

# Convert YYYY and MM to a end-of-month date for axis label viewing
date = strptime(paste(abbeville$Year, abbeville$Month, '1'), format = "%Y %m %d")
abbeville$DateME = date + months(1) - days(1)

# Set the missing values to zero and tag the original missing items to NA
abbeville$missingZero <- NA
abbeville$missingZero[which(is.na(abbeville$Incoming.Examinations))] <- 0

# Plot pre-cleaning state of dataset
pl <- ggplot(abbeville, aes(x=DateME, y=Incoming.Examinations))  + geom_point() +
      ggtitle("Abbeville Cardiovascular Examination Counts Prior to Cleaning and Imputation") +
      xlab('Date') + ylab("Number of Examinations") +
      annotate("text", label = "Hurricane Outlier", x = as.POSIXlt("2008-10-01"), y = 3400, size = 4, color = "black") +
      annotate("text", label = "Missing Values", x = as.POSIXlt("2010-03-01"), y = 300, size = 4, color = "red")

# Overlay NA values as zero to draw out their existence.
pl <- pl + geom_point(aes(x=DateME, y=missingZero, color = 'red')) + 
        theme(legend.position = "none")
                      
plot(pl)

# Clean up hurricane outlier. Set to NA so it can be imputed through Amelia.
abbeville$Incoming.Examinations[which(abbeville$DateME == "2008-10-31")] <- NA

# Create working object for Amelia and save it to csv.
abbeville2 <- abbeville["Incoming.Examinations"]
abbeville2$index <- 1:length(abbeville2$Incoming.Examinations)
abbeville2$groupID <- as.factor("VisitationPeriods")
write.csv(abbeville2, file="AbbevilleToImpute.csv")

# When running the Amelia imputation simulations (in the accompanying 'Seed Evaluation' file)
# seed 13 was found to have values that corresponded closely to the missing 5129 exams over
# the December 2009 to February 2010 timeframe. No values were averaged, the run is a single one.
seedID = 13
set.seed(seedID)
a.out <- amelia(abbeville2, ts="index", cs="groupID", polytime=2, splinetime=2, m=1, p2s=0)
tscsPlot(a.out, cs="VisitationPeriods", var="Incoming.Examinations", main="Range of Imputed Data Points")

# Calculate the average value for the variables to be imputed across the 
# Amelia simulations.
item03 <- round(a.out$imputations$imp1[3,1])
item06 <- round(a.out$imputations$imp1[6,1])
item34 <- round(a.out$imputations$imp1[34,1])
item36 <- round(a.out$imputations$imp1[36,1])
item41 <- round(a.out$imputations$imp1[41,1])
item48 <- round(a.out$imputations$imp1[48,1])
item49 <- round(a.out$imputations$imp1[49,1])
item50 <- round(a.out$imputations$imp1[50,1])
item54 <- round(a.out$imputations$imp1[54,1])
item61 <- round(a.out$imputations$imp1[61,1])
item72 <- round(a.out$imputations$imp1[72,1])

# Create a new object with the imputed values provided.
abbeville3 = abbeville[c("DateME", "Incoming.Examinations")]
abbeville3$Incoming.Examinations[c(3,6,34,36,41,48,49,50,54,61,72)] <- 
  c(item03, item06, item34, item36, item41, item48, 
    item49, item50, item54, item61, item72)
imputedValues = abbeville3[c(3,6,34,36,41,48,49,50,54,61,72),]

# Show the new layout of the dataset
pl3 <- ggplot(abbeville3, aes(x=DateME, y=Incoming.Examinations))  + geom_point(color="blue") +
  ggtitle("Abbeville Cardiovascular Examination Counts AFTER Cleaning and Imputation") +
  xlab('Date') + ylab("Number of Examinations")
plot(pl3)

# Forecast with Arima and ETS methods.
abbevilleTS <- ts(abbeville3$Incoming.Examinations)
abbevilleArima <- auto.arima(x=abbevilleTS)
print(abbevilleArima)
acf(abbevilleArima$residuals, main="ACF Plot of ARIMA(0,1,1) Resuduals")
pacf(abbevilleArima$residuals, main="PACF Plot of ARIMA (0,1,1) Residuals")
abbevilleArimaForecast <- forecast(abbevilleArima, h=12)
print(abbevilleArimaForecast)
plot(abbevilleArimaForecast, xlim=c(0,108), ylim=c(0,9250), 
     xlab = "Time (months)", ylab="Examinations",
     main="Cleaned and Imputed Abbeville Dataset with ARIMA(0,1,1) Forecast and Intervals")
points(abbevilleArima$fitted, type="l", col="green")
legend("topleft", legend=c("Observed", "Expected", "Forecasted", "Forecasted with Ranges"),
       col=c("black", "green", "blue", "gray"), lty=1)

plot(diff(abbeville3$Incoming.Examinations), xlab="time", ylab = "Difference", main="Plot of Difference from Prior Month's Examinatinon Count", type="l")

# Calculate time series using the ETS function, auto-calculating level/trend/seasonality
fitETS <- ets(abbevilleTS, model="ZZZ")
print(fitETS)
plot(forecast(fitETS,12), xlim=c(0,108), ylim=c(0,9250), xlab = "Time (months)", ylab="Examinations", 
     main="Cleaned and Imputed Abbeville Dataset with ETS Trend, Forecast and 80-95% Intervals")
points(fitETS$fitted, type="l", col="green")
legend("topleft", legend=c("Observed", "Expected", "Forecasted", "Forecasted with Ranges"),
       col=c("black", "green", "blue", "gray"), lty=1)

acf(fitETS$residuals, main="ACF Plot of ETS Resuduals")
pacf(fitETS$residuals, main="PACF Plot of ETS Resuduals")

####
# Determine/display comparative model characteristics for ARIMA and ETS.
####

# length of residual set to calculate with is 96
arimaMAD <- sum(abs(abbevilleArima$residuals))/length(abbevilleArima$residuals)
arimaMAPE <- sum(abs(abbevilleArima$residuals)/abbeville3$Incoming.Examinations)*(100/length(abbevilleArima$residuals))
arimaMSE <- sum(abs(abbevilleArima$residuals)^2)/length(abbevilleArima$residuals)

# ETS data info:
etsMAD <- sum(abs(fitETS$x - fitETS$fitted))/length(fitETS$fitted)
etsMAPE <- sum(abs(fitETS$x - fitETS$fitted)/fitETS$x)*(100/length(fitETS$fitted))
etsMSE <- sum(abs(fitETS$x - fitETS$fitted)^2)/length(fitETS$fitted)

print("Model Characteristics")
cat(sprintf("ARIMA MAD: %5.2f ARIMA MAPE: %4.2f  ARIMA MSE %6.1f  ARIMA AIC:  %6.2f\n", arimaMAD, arimaMAPE, arimaMSE, abbevilleArima$aic))
cat(sprintf("ETS MAD:   %5.2f ETS MAPE:   %4.2f  ETS MSE   %6.1f  ETS AIC:    %6.2f\n", etsMAD, etsMAPE, etsMSE, fitETS$aic))

arimaForecast12 <- as.numeric(abbevilleArimaForecast$mean)
etsForecast12 <- as.numeric(forecast(fitETS,12)$mean)

#See if models exhibit departure from normality
par(mfrow=c(1,2))
qqnorm(fitETS$residuals, main = "ETS Residual Q-Q Plot")
qqline(fitETS$residuals)
qqnorm(abbevilleArima$residuals, main ="ARIMA Residual Q-Q Plot")
qqline(abbevilleArima$residuals)

Box.test(fitETS$residuals, type="Ljung-Box")

# Powerpoint slide assists:
par(mfrow=c(1,1))
# Irregular
a <- rnorm(52,0,.5)
plot(a, type = 'l', main="Irregularities in data")
abline(h=0)

# Irregular + Trend
b <- 1:52
plot(b+(rnorm(52,3,1.5)), main = "Irregularities and Trend in data")

# Irregular + Trend + Seasonal
c <- rnorm(4,1,.8)
d <- rep(c/sum(c),13)
plot(b+(rnorm(52,3,1.5) + 40*d), main = "Irregularities, Trend, and Seasonal in data", type="l")
