# install.packages(c("dplyr", "ggplot2", "olsrr", "PerformanceAnalytics"))
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)
library(lubridate)
library(forecast)
# In-class ----
# Prompt 1
# Question 1: Drivers that influence methane production in reservoirs
# Vegetation soils rich in organic matter, flooded vegetation, low oxygen water 
# environment (Tutorial 6.3)
# These factors contribute to the production of methane gas as it is 
# produced by bacteria in soils saturated with water with little oxygen

# Question 2: Limitations in the Deemer data set to be considered for data analysis and interpretation
# One significant limitation in the Deemer dataset is that it merges multiple data
# sources, which vary in their data collection approaches as well as risk of having 
# lots of missing data. Thus, the quality of the data
# greatly depends on how accurate and reliable these other data set sources are. 
# Thus, it is important to standardize the compiled data so that the interpretation
# is consistent across all combined observations.

# Question 3
# Goal: Increased reservoir creation for hydroelectric power would be expected to affect methane release
# Develop a model that improves the results from the tutorial

# Read in greenhouse gas data from reservoirs
ghg <- read.csv("/cloud/project/activity07/Deemer_GHG_Data.csv")

# log transform  total annual precipitation, age of the reservoir, and dissolved inorganic phosphorous (part of the assumptions of the linear regression)
ghg$log.ch4 <- log(ghg$ch4+1)
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)
ghg$log.surface.area <- log(ghg$surface.area)

# binary variable for boreal region
ghg$BorealV <- ifelse(ghg$Region == "Boreal",1,0)
# binary variable for tropical region
ghg$TropicalV <- ifelse(ghg$Region == "Tropical",1,0)
# binary variable for alpine region
ghg$AlpineV <- ifelse(ghg$Alpine == "yes",1,0)
# binary variable for known hydropower
ghg$HydroV <- ifelse(ghg$hydropower == "yes",1,0)
# factorize the Region
ghg$Region <- factor(ghg$Region)

# Base multiple regression model
mod.full_base <- lm(log.ch4 ~ airTemp+
                 log.age+mean.depth+
                 log.DIP+
                 log.precip+ BorealV, data=ghg) #uses the data argument to specify dataframe
summary(mod.full_base)

# Added extra variables to increase model performance (don't add co2)
mod.full <- lm(log.ch4 ~ airTemp+
                 log.age +
                  mean.depth+
                 log.DIP+ 
                 TropicalV +
                 # AlpineV +
                 log.surface.area +
                 log.precip+ BorealV, data=ghg)
summary(mod.full)

# Checking assumptions
# Isolate standardized residuals (rstandard) and the fitted values from the regression line at each observation (fitted.values) 
res.full <- rstandard(mod.full)
fit.full <- fitted.values(mod.full)
# qq plot
qqnorm(res.full, pch=19, col="grey50")
qqline(res.full)
# shapiro-wilks test: the residuals do not significantly deviate from a normal distribution.
shapiro.test(res.full)
plot(fit.full,res.full, pch=19, col="grey50")
abline(h=0)
# Checking for multicollinearity
# isolate continuous model variables into data frame:

reg.data <- data.frame(ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip,
                       ghg$log.surface.area,
                       ghg$BorealV,
                       ghg$TropicalV,
                       ghg$Alpine)

# make a correlation matrix 
chart.Correlation(reg.data, histogram=TRUE, pch=19)

# Model selection
# run stepwise
full.step <- ols_step_forward_aic(mod.full)
# view table
View(full.step$metrics)
# check full model
full.step$model
# plot AIC over time (smaller the value, the better the model fit)
plot(full.step )

# Question 4
# Compile a narrative interpretation and table of the results from the question three.
# Write your narrative so that the policy makers can best understand what drives high
# methane release from reservoirs and how reservoirs affect methane fluxes.

# According to our model, we can see that factors such as air temperature, DIP, 
# precipitation, and being in a Boreal region have influence on methane release
# from reservoirs. For example, an increase in one degree Celsius is predicted
# to increase methane release by 0.13g. Additionally, being in a Boreal region
# is predicted to increase methane release by 2.09g. 

# We originally thought that surface area of the reservoir and the average depth
# of the reservoir would have influence in the methane release because a wide
# and deep reservoir has more space where, in combination with air temperature
# and dissolved inorganic phosphorous (DIP), it would have more bacteria living 
# there as it is harder to clean those up.

# Prompt 2
ETdat <- read.csv("/cloud/project/activity07/ETdata.csv")
# average fields for each month for almonds
almond <- ETdat %>% # ET data
  filter(crop == "Almonds") %>% # only use almond fields
  group_by(date) %>% # calculate over each date
  summarise(ET.in = mean(Ensemble.ET, na.rm=TRUE)) # average fields

# visualize the data
ggplot(almond, aes(x=ymd(date),y=ET.in))+
  geom_point()+
  geom_line()+
  labs(x="year", y="Monthy evapotranspiration (in)")

# almond ET time series
almond_ts <- ts(almond$ET.in, # data
                start = c(2016,1), #start year 2016, month 1
                #first number is unit of time and second is observations within a unit
                frequency= 12) # frequency of observations in a unit
# decompose almond ET time series
almond_dec <- decompose(almond_ts)
# plot decomposition
plot(almond_dec)
almondTrend <- almond_dec$trend
almondSeason <- almond_dec$seasonal

# Create an autocorrelation graph
acf(na.omit(almond_ts), # remove missing data
    lag.max = 24) # look at 2 years (24 months)
pacf.plot <- pacf(na.omit(almond_ts))

# Run an ARIMA model
almond_y <- na.omit(almond_ts)
model1 <- arima(almond_y , # data 
                order = c(1,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model1
model4 <- arima(almond_y , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
model4
# calculate fit
AR_fit1 <- almond_y - residuals(model1) 
AR_fit4 <- almond_y - residuals(model4)
#plot data
plot(almond_y)
# plot fit
points(AR_fit1, type = "l", col = "tomato3", lty = 2, lwd=2)
points(AR_fit4, type = "l", col = "darkgoldenrod4", lty = 2, lwd=2)
legend("topleft", c("data","AR1","AR4"),
       lty=c(1,2,2), lwd=c(1,2,2), 
       col=c("black", "tomato3","darkgoldenrod4"),
       bty="n")
newAlmond <- forecast(model4)
newAlmond
#make dataframe for plotting
newAlmondF <- data.frame(newAlmond)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newAlmondF$dateF <- ymd(paste(years,"/",month,"/",1))
# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = almond, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(almond$date[1]),newAlmondF$dateF[24])+  # Plotting original data
  geom_line(data = newAlmondF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newAlmondF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

# Homework ----
# Question 1
# Transformed CO2 = 1/(CO2 + 1000) --> target variable

ghg$co2_trans <- 1/(ghg$co2 + 1000)

model_q1 <- lm(co2_trans ~ log.age +
                 log.DIP +
                 log(mean.depth)+
                 log.precip+
                 log.ch4+
                 Region
               , data = ghg)

summary(model_q1)
# Extract the coefficient table
coef_table <- coef(summary(model_q1))

reg_table <- as.data.frame(coef_table)

# write.csv(reg_table, "reg_table.csv", row.names=TRUE)

# Checking assumptions
# Isolate standardized residuals (rstandard) and the fitted values from the regression line at each observation (fitted.values) 
res_q1 <- rstandard(model_q1)
fit_q1 <- fitted.values(model_q1)
# qq plot
qqnorm(res_q1, pch=19, col="grey50")
qqline(res_q1)
# shapiro-wilks test: the residuals do not significantly deviate from a normal distribution.
shapiro.test(res_q1)
plot(fit_q1,res_q1, pch=19, col="grey50")
abline(h=0)
# Create a dataframe
reg.data_q1 <- data.frame(ghg$co2_trans,
                       ghg$airTemp,
                       ghg$log.age,ghg$mean.depth,
                       ghg$log.DIP,
                       ghg$log.precip,
                       ghg$log.surface.area,
                       ghg$BorealV,
                       ghg$TropicalV,
                       ghg$log.ch4)

# make a correlation matrix 
chart.Correlation(reg.data_q1, histogram=TRUE, pch=19)