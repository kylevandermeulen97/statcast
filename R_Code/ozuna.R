###################################################
#install.packages(c("gam", "mgcv"))
library(gam)
library(mgcv)
library(readxl)
ozuna <- read_excel("ozuna.xlsx")
View(ozuna)
attach(ozuna)

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
library(ggplot2)
library(ggpubr)
library(mgcv)

#theme_set(theme_pubr())
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

# Remove all na's
sum(is.na(ozuna))
ozuna=na.omit(ozuna)

library(dplyr)
library(stringr)
ozuna %>%
  filter(!str_detect("double_play", Event))

# Creation of new variable Outcome = "Hit/Out", outcome = 0/1
ozuna <- mutate(ozuna,
                Outcome = case_when(
                  Event %in% c("double", "single", "triple", "home_run") ~ "Hit",
                  (!Event %in% c("double", "single", "triple", "home_run")) ~ "Out"),
                outcome = ifelse(Outcome=="Hit", 1, 0))

# Generalized additive model
fit_gam_ozuna <- gam(outcome ~ s(Launch_Angle, Exit_Velocity), family=binomial, data=ozuna)

# Creation of new variable Prob_Hit
ozuna <- mutate(ozuna, Prob_Hit = exp(predict(fit_gam_ozuna)) / (1 + exp(predict(fit_gam_ozuna))))

# Scatterplot of Gallo's Event Outcomes by LA and EV
ozuna1 <- ggplot(test, aes(Launch_Angle, Exit_Velocity, color=Event)) + 
  geom_point() + 
  ggtitle(paste("Ozuna", "- BIP")) 
ozuna1

# Plot showing fitted hit probabilities
ozuna2 = ggplot(ozuna, aes(x=Launch_Angle, y=Exit_Velocity, color=Prob_Hit)) +
  geom_point() +
  scale_color_gradient(low='red', high='blue', 
                       breaks=c(0,0.5,1),labels=c("Minimum",0.5,"Maximum"),
                       limits=c(0,1)) + 
  geom_vline(xintercept = 0, color="blue") +
  ggtitle(paste("Marcell Ozuna", "- Hit Probabilities"))
ozuna2

# Creating new dataframes for mean exit velocity/
#10% above and below
v <- round(mean(ozuna$Exit_Velocity) * c(.9, 1, 1.1), 1)
la <- seq(-10, 40, length=100) 
data.predict <- rbind(data.frame(Exit_Velocity=v[1], 
                                 Launch_Angle=la),
                      data.frame(Exit_Velocity=v[2], 
                                 Launch_Angle=la),
                      data.frame(Exit_Velocity=v[3], 
                                 Launch_Angle=la)) 

lp <- predict(fit_gam_ozuna, data.predict)
data.predict$Probability <- exp(lp) / (1 + exp(lp))
data.predict$Exit_Velocity <- factor(data.predict$Exit_Velocity)

# third plot showing probability of hit as 
#function of launch angle

ozuna3 <- ggplot(data.predict, 
             aes(Launch_Angle, Probability, 
                 group=Exit_Velocity, color=Exit_Velocity)) + 
  geom_line() + ggtitle("Marcell Ozuna") +
  ylab("Probability of Hit")
ozuna3
####################################################

# Check if dates are formatted as dates
class(ozuna$`Game Date`)

# Change them to dates
ozuna$`Game Date` <- as.Date(ozuna$`Game Date`, format = "%m/%d/%Y")
# Check that the variable is now formatted as a date
class(ozuna$`Game Date`)

# Separate game_date into "year", "month", and "day":
ozuna <- separate(data = ozuna, col = `Game Date`,
                  into = c("year", "month", "day"),
                  sep = "-", remove = FALSE )
# Convert month to numeric
ozuna$month <- as.numeric(ozuna$month)

#Game-by-game velocity changes

# Create velo_dt
velo_dt <- data.frame(tapply(ozuna$Exit_Velocity, ozuna$`Game Date`, mean))

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
ozuna_month <- plot(velo_dt$Exit_Velocity ~ velo_dt$`Game Date`,
     lwd = 4, type = "l", ylim = c(45, 120),
     main = "2019 Ozuna Exit Velocity",
     xlab = "Date", ylab = "Exit Velocity (mph)")

# Add jittered points to the plot
points(ozuna$Exit_Velocity ~ jitter(as.numeric(ozuna$`Game Date`)),
       pch = 16, col = "#99004450")

# Box and whisker plot
bwplot(Exit_Velocity ~ as.factor(month), data = ozuna)
bwplot(`Pitch Type` ~ Exit_Velocity, main = "Ozuna EV by Pitch Type",
       par.settings=list(box.rectangle=list(col="salmon",fill="salmon", alpha=0.4,
                                            data = ozuna)))

########## How would you add a legend that shows 
########## how many of that pitch he faced

###### OLS Model########
(fit2 = Exit_Velocity ~ month + Launch_Angle + Distance + Direction+ Event
 + `Pitch Type` + `Pitch (MPH)`)
fit2.output = lm(fit2, data=ozuna)
fit2.summary = summary(fit2.output)
fit2.confint = confint(fit2.output, level = 0.95)

# Generalized Additive Model
fit_gam <- gam(outcome ~ s(Launch_Angle, Exit_Velocity), family=binomial, data=ozuna)
summary(fit_gam)

# P-spline smoothers (with lambda=0.6) used for x1 and x2
b1 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps', sp=0.6) + s(Launch_Angle, bs='ps', sp=0.6),data = ozuna)
summary(b1)
plot(b1)

# select smoothing parameters with REML, using P-splines
b2 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps') + s(Launch_Angle, bs='ps'),data = ozuna, method = "REML", select=TRUE)
summary(b2)
plot(b2)

# select variables and smoothing parameters
b3 <- mgcv::gam(outcome ~ s(Exit_Velocity) + s(Launch_Angle), data = ozuna, method="REML", select=TRUE)
summary(b3)

# loess smoothers with the gam package (restart R before loading gam)
library(gam)
b4 <- gam::gam(outcome ~ lo(Exit_Velocity, span=0.6) + lo(Launch_Angle, span=0.6), data = ozuna)
summary(b4)

# Generalized Cross Validation
b5 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps', k=-1) + s(Launch_Angle, bs='ps', k=-1),data = ozuna)
summary(b5)
plot(b5)

#Combining all four hitter's plots into one

library(ggplot2)
library("ggpubr")
figure <- ggarrange(gallo3,judge3,yelich3,ozuna3,
                    labels = c("AL", "", "NL", ""),
                    ncol = 2, nrow = 2)
figure

event_plot <- ggarrange(gallo1,judge1,yelich1,ozuna1,
                       ncol = 2, nrow = 2)
event_plot
