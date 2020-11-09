############################## Gallo Statcast ############################## 



############################################################################
############################################################################
###                                                                      ###
###                              SECTION 1:                              ###
###                    LOADING THE LIBRARIES AND DATA                    ###
###                                                                      ###
############################################################################
############################################################################

#banner("Section 1:", "Loading the Libraries and Data", emph = TRUE)

library(bannerCommenter)
# Data manipulation
library(dplyr) 
library(tidyr)
library(stringr)
# Modeling
library(mgcv)
library(prclust)
library(pspline)
# Visualization
library(Amelia)
library(corrplot)
library(lattice)
library(hexbin)
library("PerformanceAnalytics")
library(ggvis)
library(ggplot2)
library(ggthemes)
theme_set(theme_bw())

# Reading in the data set and naming the dataframe gallo
library(readxl)
gallo <- read_excel("data/gallo.xlsx")
attach(gallo)
View(gallo)

# First five rows of the gallo data frame
head(gallo)
gallo$...1 <- NULL
# Descriptive statistics
summary(gallo)

# structure of the data frame
str(gallo) # 297 rows of 10 columns


############################################################################
############################################################################
###                                                                      ###
###                              SECTION 2:                              ###
###                          DATA PREPROCESSING                          ###
###                                                                      ###
############################################################################
############################################################################

#banner("Section 2:", "Data Preprocessing", emph = TRUE)


# check for missing or null values
any(is.na(gallo)) # returns True

# plot of missing values by column
missmap(gallo, main="Gallo - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

# NA values by column
na_count <-sapply(gallo, function(y) sum(is.na(y)))
na_count # We have one missing value in direction and 17 in distance / 18 in all

# Let's find the missing rows
na_rows <- gallo[rowSums(is.na(gallo)) > 0,]
na_rows
which(is.na(gallo), arr.ind=TRUE)

# Remove all na's / too many na values to keep them in the dataset
sum(is.na(gallo))
gallo=na.omit(gallo)

# Let's look at the quartile distribution and see if we could fill the na values within the quartiles
summary(gallo$Distance) 

###### No more NA values #######

# Let's look at the unique category counts of our character columns
str(gallo)

sapply(gallo, function(x) length(unique(x)))

# We have 78 unique pitcher names, so we can't do anything with this column / let's drop it
# Also our batter column only includes Gallo's name so let's drop it too
gallo <- subset(gallo, select = -c(Pitcher,Batter) )

# Let's look at the sum of unique categories for the Event, Direction, and Pitch Type columns
count(gallo,Event)
count(gallo,Direction) # Direction column looks evenly distributed
count(gallo,`Pitch Type`)


# Event column has many groups that only have a count of one or two / let's ensure these categories match with judge

# Among the four hitters let's include the same categories for the Event column


# Pitch Type column has pitches that only showed up once or twice / let's remove these rows
gallo <- filter(gallo, `Pitch Type` != "Eephus"  & `Pitch Type` != "Split-Finger")

# rename columns that aren't separated by an underscore
colnames(gallo)

gallo <- rename(gallo,
                Game_Date = `Game Date`,
                Pitch_Speed = `Pitch (MPH)`,
                Pitch_Type = `Pitch Type`
)

colnames(gallo)



######### Game_Date variable

# Check if dates are formatted as dates
class(gallo$Game_Date)

# Change them to dates
gallo$Game_Date <- as.Date(gallo$Game_Date, format = "%m/%d/%Y")
# Check that the variable is now formatted as a date
class(gallo$Game_Date)

# Delimit dates
# Separate game_date into "year", "month", and "day":
gallo <- separate(data = gallo, col = Game_Date,
                  into = c("Year", "Month", "Day"),
                  sep = "-", remove = FALSE )


str(gallo)

# convert the newly created Month and Day columns to numeric
gallo$Month <- as.numeric(gallo$Month)
gallo$Day <- as.numeric(gallo$Day)

# Creation of new variable Outcome = "Hit/Out", outcome = 0/1
gallo <- mutate(gallo,
                Outcome = case_when(
                  Event %in% c("double", "single", "triple", "home_run") ~ "Hit",
                  (!Event %in% c("double", "single", "triple", "home_run")) ~ "Out"),
                outcome = ifelse(Outcome=="Hit", 1, 0))

attach(gallo)
############################################################################
############################################################################
###                                                                      ###
###                              SECTION 3:                              ###
###                          DATA VISUALIZATION                          ###
###                                                                      ###
############################################################################
############################################################################

#banner("Section 3:", "Data Visualization", emph = TRUE)

# Distribution of numerical columns 
gallo %>%
  gather(attributes,value,3:7,10) %>%
  ggplot(aes(x=value)) +
  geom_histogram(fill='blue',color='black') +
  scale_y_continuous(limits = c(0,75)) +
  facet_wrap(~attributes, scales = 'free_x') +
  labs(x='Values',y='Frequency') 

# The columns look to be normally distributed 
# Let's individually graph the launch angle

# Launch angle histogram
gallo %>% 
  ggplot(aes(x=Launch_Angle)) +
  geom_histogram(aes(fill=..count..)) +
  xlab('Degrees') + ylab('Occurences') + ggtitle('Launch Angle Histogram')
# Looks like there are no outliers

# Correlation Matrix
my_data <- gallo[, c(3:7,10,13)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

# Scatterplot between the highest correlated variables: Launch Angle and Distance 0.72
gallo %>%
  ggplot(aes(x=Launch_Angle,y=Distance)) +
  geom_point(aes(color=Outcome)) +
  xlab('Launch Angle') + ylab('Distance') +
  scale_x_continuous(breaks = seq(-90, 100, by = 10))


###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 4:                             ###
###             MODELING FOR FITTED PROBABILITIES OF A HIT              ###
###                                                                     ###
###########################################################################
###########################################################################

# Generalized additive model
fit_gam <- gam(outcome ~ s(Launch_Angle, Exit_Velocity), family=binomial, data=gallo)

# Creation of new variable Prob_Hit
gallo <- mutate(gallo, Prob_Hit = exp(predict(fit_gam)) / (1 + exp(predict(fit_gam))))

# Scatterplot of Gallo's Event Outcomes by LA and EV
gallo1 <- ggplot(gallo, aes(Launch_Angle, Exit_Velocity, color=Event)) + 
  geom_point() + 
  ggtitle(paste("Gallo", "- BIP")) 
gallo1

# Plot showing fitted hit probabilities
gallo2 = ggplot(gallo, aes(x=Launch_Angle, y=Exit_Velocity, color=Prob_Hit)) +
  geom_point() +
  scale_color_gradient(low='red', high='blue', 
                       breaks=c(0,0.5,1),labels=c("Minimum",0.5,"Maximum"),
                       limits=c(0,1)) + 
  geom_vline(xintercept = 0, color="blue") +
  ggtitle(paste("Joey Gallo", "- Hit Probabilities"))
gallo2

# Creating new dataframes for mean exit velocity/
#10% above and below
v <- round(mean(gallo$Exit_Velocity) * c(.9, 1, 1.1), 1)
la <- seq(-10, 40, length=100) 
data.predict <- rbind(data.frame(Exit_Velocity=v[1], 
                                 Launch_Angle=la),
                      data.frame(Exit_Velocity=v[2], 
                                 Launch_Angle=la),
                      data.frame(Exit_Velocity=v[3], 
                                 Launch_Angle=la)) 

lp <- predict(fit_gam, data.predict)
data.predict$Probability <- exp(lp) / (1 + exp(lp))
data.predict$Exit_Velocity <- factor(data.predict$Exit_Velocity)

# third plot showing probability of hit as 
#function of launch angle

gallo3 <- ggplot(data.predict, 
                 aes(Launch_Angle, Probability, 
                     group=Exit_Velocity, color=Exit_Velocity)) + 
  geom_line() + ggtitle("Joey Gallo") +
  ylab("Probability of Hit")
gallo3


# Generalized Additive Model
fit_gam <- gam(outcome ~ s(Launch_Angle, Exit_Velocity), family=binomial, data=gallo)
summary(fit_gam)

# P-spline smoothers (with lambda=0.6) used for x1 and x2
b1 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps', sp=0.6) + s(Launch_Angle, bs='ps', sp=0.6),data = gallo)
summary(b1)
plot(b1)

# select smoothing parameters with REML, using P-splines
b2 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps') + s(Launch_Angle, bs='ps'),data = gallo, method = "REML", select=TRUE)
summary(b2)
plot(b2)
# select variables and smoothing parameters
b3 <- mgcv::gam(outcome ~ s(Exit_Velocity) + s(Launch_Angle), data = gallo, method="REML", select=TRUE)
summary(b3)
# loess smoothers with the gam package (restart R before loading gam)
library(gam)
b4 <- gam::gam(outcome ~ lo(Exit_Velocity, span=0.6) + lo(Launch_Angle, span=0.6), data = gallo)
summary(b4)

# non parametric effects are significant in gallo's exit velo and launch angle
# Generalized Cross Validation
b5 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps', k=-1) + s(Launch_Angle, bs='ps', k=-1),data = gallo)
summary(b5)
plot(b5)



######## Date column ##########

# Check if dates are formatted as dates
class(gallo$Game_Date)

# Change them to dates
gallo$Game_Date <- as.Date(gallo$Game_Date, format = "%m/%d/%Y")
# Check that the variable is now formatted as a date
class(gallo$Game_Date)

# Separate game_date into "year", "month", and "day":
gallo <- separate(data = gallo, col = Game_Date,
                  into = c("Year", "Month", "Day"),
                  sep = "-", remove = FALSE )
# Convert month to numeric
gallo$Month <- as.numeric(gallo$Month)

#Game-by-game velocity changes

# Create velo_dt
velo_dt <- data.frame(tapply(gallo$Exit_Velocity, gallo$Game_Date, mean))

# Print the first 6 rows of velo_dt
head(velo_dt)

# Create game_date in velo_dt
velo_dt$Game_Date <- as.Date(row.names(velo_dt), "%Y-%m-%d")

# Rename the first column
colnames(velo_dt)[1] <- "Exit_Velocity"

# Remove row names
row.names(velo_dt) <- NULL

# View head of velo_dt
head(velo_dt)

par(mfrow = c(1, 1))

# Plot game-by-game Exit Velocity
gallo_month <- plot(velo_dt$Exit_Velocity ~ velo_dt$Game_Date,
                    lwd = 4, type = "l", ylim = c(45, 120),
                    main = "2019 Gallo Exit Velocity",
                    xlab = "Date", ylab = "Exit Velocity (mph)")

# Add jittered points to the plot
points(gallo$Exit_Velocity ~ jitter(as.numeric(gallo$Game_Date)),
       pch = 16, col = "#99004450")



# What factors improve or hurt gallo's chance of a home run

# Logistic Regression
gallo_log <- glm(Event == "home_run" ~ Month + Day + Exit_Velocity + Launch_Angle + Distance
                 + Pitch_Speed + Pitch_Type, data = gallo, family = "binomial")
exp(gallo_log$coefficients)
summary(gallo_log)

# Box and whisker plot
bwplot(Exit_Velocity ~ as.factor(Month), data = gallo)
bwplot(Pitch_Type ~ Exit_Velocity, main = "Gallo EV by Pitch Type",
       par.settings=list(box.rectangle=list(col="salmon",fill="salmon", alpha=0.4,
                                            data = gallo)))


###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 5:                             ###
###               PREDICTORS OF A HITTER'S EXIT VELOCITY                ###
###                                                                     ###
###########################################################################
###########################################################################

#banner("Section 5:", "Predictors of a Hitter's Exit Velocity ", emph = TRUE)
old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)

library(leaps)
# Model Selection
models <- regsubsets(Exit_Velocity ~ Month + Launch_Angle +
                       Distance + Event + Direction + Pitch_Speed + Pitch_Type,  data = gallo)
summary(models)

reg.summary = summary(models)
names(reg.summary)

#plot rsq
rsq <- as.data.frame(reg.summary$rsq)
names(rsq) <- "R2"
rsq %>% 
  ggvis(x=~ c(1:nrow(rsq)), y=~R2 ) %>%
  layer_points(fill = ~ R2 ) %>%
  add_axis("y", title = "R2") %>% 
  add_axis("x", title = "Number of variables")  %>%
  add_legend("fill", properties=legend_props(legend=list(x=scaled_value("x", 6), 
                                                         y=scaled_value("y", 0.18))))

# Plotting RSS, adjusted R2, Cp, and BIC for all of the models

plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
# which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
# which.min(reg.summary$cp )
points(10,reg.summary$cp [10],col="red",cex=2,pch=20)
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
# which.min(reg.summary$bic )
points(6,reg.summary$bic [6],col="red",cex=2,pch=20)

# regsubsets bic graph
plot(models,scale="bic")

#coefficient estimates associated with this model.
coef(models ,8)


###### OLS Model with all factors########
fit2 = Exit_Velocity ~ Month + Launch_Angle + Distance + Direction+ Event+ Pitch_Type + Pitch_Speed
fit2.output = lm(fit2, data=gallo)
fit2.summary = summary(fit2.output)
fit2.confint = confint(fit2.output, level = 0.95)

########### Model based off of bic ############### 
fit = Exit_Velocity ~ Month + Launch_Angle + Distance + Direction+ Event
fit.output = lm(fit, data=gallo)
fit.summary = summary(fit.output)
fit.confint = confint(fit.output, level = 0.95)


