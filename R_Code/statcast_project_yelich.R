############################## Yelich Statcast ############################## 



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

# Reading in the data set and naming the dataframe yelich
library(readxl)
yelich <- read_excel("data/yelich.xlsx")
attach(yelich)
View(yelich)

# First five rows of the yelich data frame
head(yelich)

# Descriptive statistics
summary(yelich)

# structure of the data frame
str(yelich) # 374 rows of 10 columns

par(mfrow=c(1,1))

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
any(is.na(yelich)) # returns True

# plot of missing values by column
missmap(yelich, main="Yelich - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

# NA values by column
na_count <-sapply(yelich, function(y) sum(is.na(y)))
na_count # We have three missing values in Pitch MPH and 29 in distance / 32 in all

# Let's find the missing rows
na_rows <- yelich[rowSums(is.na(yelich)) > 0,]
na_rows
which(is.na(yelich), arr.ind=TRUE)

# Remove all na's / too many na values to keep them in the dataset
sum(is.na(yelich))
yelich=na.omit(yelich)


###### No more NA values #######

# Let's look at the unique category counts of our character columns
str(yelich)

sapply(yelich, function(x) length(unique(x)))

# We have 159 unique pitcher names, so we can't do anything with this column / let's drop it
# Also our batter column only includes Yelich's name so let's drop it too
yelich <- subset(yelich, select = -c(Pitcher,Batter) )

# Let's look at the sum of unique categories for the Event, Direction, and Pitch Type columns
count(yelich,Event)
count(yelich,Direction) # Direction column looks evenly distributed
count(yelich,`Pitch Type`)


# Event column has many groups that only have a count of one or two / let's ensure these categories match with judge
yelich$Event[yelich$Event == 'field_error'] <- 'field_out'
yelich$Event[yelich$Event == 'fielders_choice'] <- 'field_out'
yelich$Event[yelich$Event == 'grounded_into_double_play'] <- 'double_play'

# Among the four hitters let's include the same categories for the Event column


# Pitch Type column has pitches that only showed up once or twice / let's remove these rows
yelich <- filter(yelich, `Pitch Type` != "Split-Finger")

# rename columns that aren't separated by an underscore
colnames(yelich)

yelich <- rename(yelich,
                Game_Date = `Game Date`,
                Pitch_Speed = `Pitch (MPH)`,
                Pitch_Type = `Pitch Type`
)

colnames(yelich)
attach(yelich)

# Check if dates are formatted as dates
class(yelich$Game_Date)

# Change them to dates
yelich$Game_Date <- as.Date(yelich$Game_Date, format = "%m/%d/%Y")
# Check that the variable is now formatted as a date
class(yelich$Game_Date)

# Delimit dates
library(tidyr)
# Separate game_date into "year", "month", and "day":
yelich <- separate(data = yelich, col = Game_Date,
                   into = c("Year", "Month", "Day"),
                   sep = "-", remove = FALSE )

# convert the newly created Month and Day columns to numeric
yelich$Month <- as.numeric(yelich$Month)
yelich$Day <- as.numeric(yelich$Day)


yelich <- mutate(yelich,
                 Outcome = case_when(
                   Event %in% c("double", "single", "triple", "home_run") ~ "Hit",
                   (!Event %in% c("double", "single", "triple", "home_run")) ~ "Out"),
                 outcome = ifelse(Outcome=="Hit", 1, 0)) 


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
yelich %>%
  gather(attributes,value,3:7,10) %>%
  ggplot(aes(x=value)) +
  geom_histogram(fill='blue',color='black') +
  scale_y_continuous(limits = c(0,75)) +
  facet_wrap(~attributes, scales = 'free_x') +
  labs(x='Values',y='Frequency') 

# The columns look to be normally distributed / one launch angle value looks to have potential outliers
# Let's individually graph the launch angle

# Launch angle histogram
yelich %>% 
  ggplot(aes(x=Launch_Angle)) +
  geom_histogram(aes(fill=..count..)) +
  xlab('Degrees') + ylab('Occurences') + ggtitle('Launch Angle Histogram')
# Looks like there is an occurrence that is below -100 degrees / let's look at this further

# Correlation Matrix
my_data <- yelich[, c(3:7,10,13)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

# Scatterplot between the highest correlated variables: Launch Angle and Distance 0.76
yelich %>%
  ggplot(aes(x=Launch_Angle,y=Distance)) +
  geom_point(aes(color=Outcome)) +
  xlab('Launch Angle') + ylab('Distance') +
  scale_x_continuous(breaks = seq(-90, 100, by = 10))

# Christian Yelich", "- In-Play Outcomes scatterplot
yelich1 <- ggplot(yelich, aes(Launch_Angle, Exit_Velocity, color=Event)) + 
  geom_point() + 
  ggtitle(paste("Christian Yelich", "- In-Play Outcomes")) 
yelich1

# Dealing with outliers / unusual values
summary(yelich) #every column looks to have normal values



###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 4:                             ###
###             MODELING FOR FITTED PROBABILITIES OF A HIT              ###
###                                                                     ###
###########################################################################
###########################################################################

#banner("Section 4:", "Modeling for Fitted Probabilities of a Hit ", emph = TRUE)

# gam model fit
yelich_gam <- gam(outcome ~ s(Launch_Angle, Exit_Velocity), family=binomial, data=yelich)

yelich_2 <- mutate(yelich,
                  Prob_Hit = exp(predict(yelich_gam)) / (1 + exp(predict(yelich_gam))))

# second plot showing fitted hit probabilities
yelich2 = ggplot(yelich_2, aes(x=Launch_Angle, y=Exit_Velocity, color=Prob_Hit)) +
  geom_point() +
  scale_color_gradient(low='red', high='blue', 
                       breaks=c(0,0.5,1),labels=c("Minimum",0.5,"Maximum"),
                       limits=c(0,1)) + 
  geom_vline(xintercept = 0, color="blue") +
  ggtitle(paste("Christian Yelich", "- Hit Probabilities"))
yelich2

v <- round(mean(yelich_2$Exit_Velocity) * c(.9, 1, 1.1), 1)
la <- seq(-10, 40, length=100)
data.predict <- rbind(data.frame(Exit_Velocity=v[1], 
                                 Launch_Angle=la),
                      data.frame(Exit_Velocity=v[2], 
                                 Launch_Angle=la),
                      data.frame(Exit_Velocity=v[3], 
                                 Launch_Angle=la)) 

lp <- predict(yelich_gam, data.predict)
data.predict$Probability <- exp(lp) / (1 + exp(lp))
data.predict$Exit_Velocity <- factor(data.predict$Exit_Velocity)

# third plot showing probability of hit as function of
# launch angle

yelich3 <- ggplot(data.predict, 
             aes(Launch_Angle, Probability, 
                 group=Exit_Velocity, color=Exit_Velocity)) + 
  geom_line() + ggtitle("Christian Yelich") +
  ylab("Probability of Hit")
yelich3


# P-spline smoothers (with lambda=0.6) used for x1 and x2
b1 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps', sp=0.6) + s(Launch_Angle, bs='ps', sp=0.6),data = yelich)
summary(b1)
plot(b1)

# select smoothing parameters with REML, using P-splines
b2 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps') + s(Launch_Angle, bs='ps'),data = yelich, method = "REML", select=TRUE)
summary(b2)
plot(b2)
# select variables and smoothing parameters
b3 <- mgcv::gam(outcome ~ s(Exit_Velocity) + s(Launch_Angle), data = yelich, method="REML", select=TRUE)
summary(b3)
# loess smoothers with the gam package (restart R before loading gam)
library(gam)
b4 <- gam::gam(outcome ~ lo(Exit_Velocity, span=0.6) + lo(Launch_Angle, span=0.6), data = yelich)
summary(b4)

# Generalized Cross Validation
b5 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps', k=-1) + s(Launch_Angle, bs='ps', k=-1),data = yelich)
summary(b5)
plot(b5)




######### Date column ##########

# Check if dates are formatted as dates
class(yelich$Game_Date)

# Change them to dates
yelich$Game_Date <- as.Date(yelich$Game_Date, format = "%m/%d/%Y")
# Check that the variable is now formatted as a date
class(yelich$Game_Date)

# Separate game_date into "year", "month", and "day":
yelich <- separate(data = yelich, col = Game_Date,
                  into = c("Year", "Month", "Day"),
                  sep = "-", remove = FALSE )
# Convert month to numeric
yelich$Month <- as.numeric(yelich$Month)

#Game-by-game velocity changes

# Create velo_dt
velo_dt <- data.frame(tapply(yelich$Exit_Velocity, yelich$Game_Date, mean))

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
yelich_month <- plot(velo_dt$Exit_Velocity ~ velo_dt$Game_Date,
                    lwd = 4, type = "l", ylim = c(45, 120),
                    main = "2019 Yelich Exit Velocity",
                    xlab = "Date", ylab = "Exit Velocity (mph)")

# Add jittered points to the plot
points(yelich$Exit_Velocity ~ jitter(as.numeric(yelich$Game_Date)),
       pch = 16, col = "#99004450")



# What factors improve or hurt judge's chance of a home run

# Logistic Regression
yelich_log <- glm(Event == "home_run" ~ Month + Day + Exit_Velocity + Launch_Angle + Distance
                 + Pitch_Speed + Pitch_Type, data = yelich, family = "binomial")
exp(yelich_log$coefficients)
summary(yelich_log)

bwplot(Exit_Velocity ~ as.factor(Month), data = yelich)
bwplot(Pitch_Type ~ Exit_Velocity, par.settings=list(box.rectangle=list(col="salmon",fill="salmon", alpha=0.4, data = yelich)))


###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 5:                             ###
###               PREDICTORS OF A HITTER'S EXIT VELOCITY                ###
###                                                                     ###
###########################################################################
###########################################################################

#banner("Section 5:", "Predictors of a Hitter's Exit Velocity ", emph = TRUE)


library(leaps)
# Model Selection
models <- regsubsets(Exit_Velocity ~ Month + Launch_Angle +
                       Distance + Event + Direction + Pitch_Speed + Pitch_Type,  data = yelich)
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

# Linear Fit based off of regsubsets bic
fit <- lm(Exit_Velocity~Distance+Event+Direction)
summary(fit)

########### Other Linear Fits ############### 
(fit2 = Exit_Velocity ~ Month + Launch_Angle + Distance + Direction+ Event
 + Pitch_Type)
fit2.output = lm(fit2, data=yelich)
fit2.summary = summary(fit2.output)
fit2.confint = confint(fit2.output, level = 0.95)

(fit3 = Exit_Velocity ~ Month + Launch_Angle + Distance
  + Event + Pitch_Speed)
fit3.output = lm(fit3, data=yelich)
fit3.summary = summary(fit3.output)
fit3.confint = confint(fit3.output)

