###################################################
#install.packages(c("gam", "mgcv", "etc."))

library(gam)
library(mgcv)
library(readxl)
yelich <- read_excel("yelich.xlsx")
View(yelich)
attach(yelich)
library(devtools)
library(lattice)
library(ggvis)
library(PerformanceAnalytics)
library(mgcv)
library(ggplot2)
library(corrplot)
library(tidyr)
library(leaps)
library(tidyverse)
library(gam)

# Remove all na's
sum(is.na(yelich))
yelich=na.omit(yelich)

# Check if dates are formatted as dates
class(yelich$`Game Date`)

# Change them to dates
yelich$`Game Date` <- as.Date(yelich$`Game Date`, format = "%m/%d/%Y")
# Check that the variable is now formatted as a date
class(yelich$`Game Date`)

# Separate game_date into "year", "month", and "day":
yelich <- separate(data = yelich, col = `Game Date`,
                  into = c("year", "month", "day"),
                  sep = "-", remove = FALSE )

# Creation of new variable Outcome = "Hit/Out", outcome = 0/1
yelich <- mutate(yelich,
                Outcome = case_when(
                  Event %in% c("double", "single", "triple", "home_run") ~ "Hit",
                  (!Event %in% c("double", "single", "triple", "home_run")) ~ "Out"),
                outcome = ifelse(Outcome=="Hit", 1, 0))

# Generalized additive model
fit_gam_yelich <- gam(outcome ~ s(Launch_Angle, Exit_Velocity), family=binomial, data=yelich)

# Creation of new variable Prob_Hit
yelich <- mutate(yelich, Prob_Hit = exp(predict(fit_gam_yelich)) / (1 + exp(predict(fit_gam_yelich))))

# Scatterplot of Gallo's Event Outcomes by LA and EV
yelich1 <- ggplot(yelich, aes(Launch_Angle, Exit_Velocity, color=Event)) + 
  geom_point() + 
  ggtitle(paste("Yelich", "- BIP")) 
yelich1

# Plot showing fitted hit probabilities
yelich2 = ggplot(yelich, aes(x=Launch_Angle, y=Exit_Velocity, color=Prob_Hit)) +
  geom_point() +
  scale_color_gradient(low='red', high='blue', 
                       breaks=c(0,0.5,1),labels=c("Minimum",0.5,"Maximum"),
                       limits=c(0,1)) + 
  geom_vline(xintercept = 0, color="blue") +
  ggtitle(paste("Christian Yelich", "- Hit Probabilities"))
yelich2

# Creating new dataframes for mean exit velocity/
#10% above and below
v <- round(mean(yelich$Exit_Velocity) * c(.9, 1, 1.1), 1)
la <- seq(-10, 40, length=100) 
data.predict <- rbind(data.frame(Exit_Velocity=v[1], 
                                 Launch_Angle=la),
                      data.frame(Exit_Velocity=v[2], 
                                 Launch_Angle=la),
                      data.frame(Exit_Velocity=v[3], 
                                 Launch_Angle=la)) 

lp <- predict(fit_gam_yelich, data.predict)
data.predict$Probability <- exp(lp) / (1 + exp(lp))
data.predict$Exit_Velocity <- factor(data.predict$Exit_Velocity)

# third plot showing probability of hit as 
#function of launch angle

yelich3 <- ggplot(data.predict, 
             aes(Launch_Angle, Probability, 
                 group=Exit_Velocity, color=Exit_Velocity)) + 
  geom_line() + ggtitle("Christian Yelich") +
  ylab("Probability of Hit")
yelich3
####################################################

# Check if dates are formatted as dates
class(yelich$`Game Date`)

# Change them to dates
yelich$`Game Date` <- as.Date(yelich$`Game Date`, format = "%m/%d/%Y")
# Check that the variable is now formatted as a date
class(yelich$`Game Date`)

# Separate game_date into "year", "month", and "day":
yelich <- separate(data = yelich, col = `Game Date`,
                  into = c("year", "month", "day"),
                  sep = "-", remove = FALSE )
# Convert month to numeric
#######yelich$month <- as.numeric(yelich$month)

#Game-by-game velocity changes

# Create velo_dt
velo_dt <- data.frame(tapply(yelich$Exit_Velocity, yelich$`Game Date`, mean))

# Print the first 6 rows of velo_dt
head(velo_dt)

# Create game_date in velo_dt
velo_dt$`Game Date` <- as.Date(row.names(velo_dt), "%Y-%m-%d")

# Rename the first column
colnames(velo_dt)[1] <- "Exit_Velocity"

# Remove row names
row.names(velo_dt) <- NULL

# View head of velo_dt
head(velo_dt)

par(mfrow = c(1, 1))

# Plot game-by-game Exit Velocity
yelich_month <- plot(velo_dt$Exit_Velocity ~ velo_dt$`Game Date`,
     lwd = 4, type = "l", ylim = c(45, 120),
     main = "2019 Yelich Exit Velocity",
     xlab = "Date", ylab = "Exit Velocity (mph)")

# Add jittered points to the plot
points(yelich$Exit_Velocity ~ jitter(as.numeric(yelich$`Game Date`)),
       pch = 16, col = "#99004450")

# Box and whisker plot
bwplot(Exit_Velocity ~ as.factor(month), data = yelich)
bwplot(`Pitch Type` ~ Exit_Velocity, main = "Yelich EV by Pitch Type",
       par.settings=list(box.rectangle=list(col="salmon",fill="salmon", alpha=0.4,
                                            data = yelich)))


###### OLS Model########
(fit2 = Exit_Velocity ~ month + Launch_Angle + Distance + Direction+ Event
 + `Pitch Type` + `Pitch (MPH)`)
fit2.output = lm(fit2, data=yelich)
fit2.summary = summary(fit2.output)
fit2.confint = confint(fit2.output, level = 0.95)



#library("PerformanceAnalytics")
#my_data2 <- yelich[, c(7,3, 8, 9,12)]
#chart.Correlation(my_data2, main = "Yelich Correlation Matrix", histogram=TRUE, pch=19)

# Generalized Additive Model
fit_gam <- gam(outcome ~ s(Launch_Angle, Exit_Velocity), family=binomial, data=yelich)
summary(fit_gam)

# P-spline smoothers (with lambda=0.6) used for x1 and x2
b1 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps', sp=0.6) + s(Launch_Angle, bs='ps', sp=0.6),data = yelich)
summary(b1)
plot(b1)

# select smoothing parameters with REML, using P-splines
b2 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps') + s(Launch_Angle, bs='ps'),data = yelich, method = "REML", select=TRUE)
summary(b2)
plot(b2)
# select variables and smoothing parameters
#b3 <- mgcv::gam(outcome ~ s(Exit_Velocity) + s(Launch_Angle), data = yelich, method="REML", select=TRUE)
#summary(b3)
# loess smoothers with the gam package (restart R before loading gam)
library(gam)
b4 <- gam::gam(outcome ~ lo(Exit_Velocity, span=0.6) + lo(Launch_Angle, span=0.6), data = yelich)
summary(b4)

# Generalized Cross Validation
b5 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps', k=-1) + s(Launch_Angle, bs='ps', k=-1),data = yelich)
summary(b5)
plot(b5)
