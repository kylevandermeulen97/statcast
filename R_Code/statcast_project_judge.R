############################## Judge Statcast ############################## 



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
library(gam)
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

# Reading in the data set and naming the dataframe judge
library(readxl)
judge <- read_excel("data/judge.xlsx")
attach(judge)
View(judge)

# First five rows of the judge data frame
head(judge)

# Descriptive statistics
summary(judge)

# structure of the data frame
str(judge) # 238 rows of 10 columns

# rename column names with spaces 


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
any(is.na(judge)) # returns True

# plot of missing values by column
missmap(judge, main="Judge - Missings Map", 
col=c("yellow", "black"), legend=FALSE)

# NA values by column
na_count <-sapply(judge, function(y) sum(is.na(y)))
na_count # We have one missing value in direction and 17 in distance / 18 in all

# Let's find the missing rows
na_rows <- judge[rowSums(is.na(judge)) > 0,]
na_rows
which(is.na(judge), arr.ind=TRUE)

# Let's remove the na value in the Direction variable
judge <- judge[-c(51),]

# Let's look at the quartile distribution and see if we could fill the na values within the quartiles
summary(judge$Distance) 
# The distribution is so wide that we will just fill the Distance na values with the mean
judge$Distance[is.na(judge$Distance)] <- mean(judge$Distance, na.rm = TRUE)
sum(is.na(judge))

###### No more NA values #######

# Let's look at the unique category counts of our character columns
str(judge)

sapply(judge, function(x) length(unique(x)))

# We have 134 unique pitcher names, so we can't do anything with this column / let's drop it
# Also our batter column only includes Judge's name so let's drop it too
judge <- subset(judge, select = -c(Pitcher,Batter) )

# Let's look at the sum of unique categories for the Event, Direction, and Pitch Type columns
count(judge,Event)
count(judge,Direction) # Direction column looks evenly distributed
count(judge,`Pitch Type`)


# Event column has many groups that only have a count of one or two / let's move them into another category
judge$Event[judge$Event == 'field_error'] <- 'field_out'
judge$Event[judge$Event == 'fielders_choice_out'] <- 'field_out'
judge$Event[judge$Event == 'grounded_into_double_play'] <- 'double_play'
# Among the four hitters let's include the same categories for the Event column


# Pitch Type column has pitches that only showed up once or twice / let's remove these rows
judge <- filter(judge, `Pitch Type` != "Knuckle Curve" & `Pitch Type` != "Knuckleball" & `Pitch Type` != "Split-Finger")

# rename columns that aren't separated by an underscore
colnames(judge)

# optional
judge <- rename(judge,
    Game_Date = `Game Date`,
    Pitch_Speed = `Pitch (MPH)`,
    Pitch_Type = `Pitch Type`)

######### Game_Date variable

# Check if dates are formatted as dates
class(judge$Game_Date)

# Change them to dates
judge$Game_Date <- as.Date(judge$Game_Date, format = "%m/%d/%Y")
# Check that the variable is now formatted as a date
class(judge$Game_Date)

# Delimit dates
# Separate game_date into "year", "month", and "day":
judge <- separate(data = judge, col = Game_Date,
                  into = c("Year", "Month", "Day"),
                  sep = "-", remove = FALSE )


str(judge)

# convert the newly created Month and Day columns to numeric
judge$Month <- as.numeric(judge$Month)
judge$Day <- as.numeric(judge$Day)

# Create an Outcome variable that has strings of hit or out and another
# outcome variable that is a function of a hit ==  1 or an out == 0
judge <- mutate(judge,
                Outcome = case_when(
                  Event %in% c("double", "single", "triple", "home_run") ~ "Hit",
                  (!Event %in% c("double", "single", "triple", "home_run")) ~ "Out"),
                outcome = ifelse(Outcome=="Hit", 1, 0))

str(judge)


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
judge %>%
  gather(attributes,value,3:7,10) %>%
  ggplot(aes(x=value)) +
  geom_histogram(fill='blue',color='black') +
  scale_y_continuous(limits = c(0,75)) +
  facet_wrap(~attributes, scales = 'free_x') +
  labs(x='Values',y='Frequency') 

# The columns look to be normally distributed / one launch angle value looks to be a potential outlier
# Let's individually graph the launch angle

# Launch angle histogram
judge %>% 
  ggplot(aes(x=Launch_Angle)) +
  geom_histogram(aes(fill=..count..)) +
  xlab('Degrees') + ylab('Occurences') + ggtitle('Launch Angle Histogram')
# Looks like there is an occurrence that is below -100 degrees / let's look at this further

# Correlation Matrix
my_data <- judge[, c(3:7,10,13)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

# Scatterplot between the highest correlated variables: Launch Angle and Distance 0.73
judge %>%
  ggplot(aes(x=Launch_Angle,y=Distance)) +
  geom_point(aes(color=Outcome)) +
  xlab('Launch Angle') + ylab('Distance') +
  scale_x_continuous(breaks = seq(-90, 100, by = 10))
  
# Aaron Judge", "- In-Play Outcomes scatterplot
  judge1 <- ggplot(judge, aes(Launch_Angle, Exit_Velocity, color=Event)) + 
    geom_point() + 
    ggtitle(paste("Aaron Judge", "- In-Play Outcomes")) 
  judge1

# Dealing with outliers / unusual values
summary(judge) # the min launch angle of -82 looks suspicious. Let's perform a boxplot on it

judge %>%
  ggplot(aes(x='', y=Launch_Angle)) + 
  geom_boxplot(fill='blue')

# Let's drop this row
judge <- subset(judge,Launch_Angle > -82)
summary(judge$Launch_Angle)


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
judge_gam <- gam(outcome ~ s(Launch_Angle, Exit_Velocity), family=binomial, data=judge)

judge_2 <- mutate(judge,
                 Prob_Hit = exp(predict(judge_gam)) / (1 + exp(predict(judge_gam))))
  
  # second plot showing fitted hit probabilities
judge2 = ggplot(judge_2, aes(x=Launch_Angle, y=Exit_Velocity, color=Prob_Hit)) +
    geom_point() +
    scale_color_gradient(low='red', high='blue', 
                         breaks=c(0,0.5,1),labels=c("Minimum",0.5,"Maximum"),
                         limits=c(0,1)) + 
    geom_vline(xintercept = 0, color="blue") +
    ggtitle(paste("Aaron Judge", "- Hit Probabilities"))
judge2

v <- round(mean(judge_2$Exit_Velocity) * c(.9, 1, 1.1), 1)
la <- seq(-10, 40, length=100)
data.predict <- rbind(data.frame(Exit_Velocity=v[1], 
                                   Launch_Angle=la),
                        data.frame(Exit_Velocity=v[2], 
                                   Launch_Angle=la),
                        data.frame(Exit_Velocity=v[3], 
                                   Launch_Angle=la)) 
  
lp <- predict(judge_gam, data.predict)
data.predict$Probability <- exp(lp) / (1 + exp(lp))
data.predict$Exit_Velocity <- factor(data.predict$Exit_Velocity)
  
  # third plot showing probability of hit as function of
  # launch angle
  
judge3 <- ggplot(data.predict, 
               aes(Launch_Angle, Probability, 
                   group=Exit_Velocity, color=Exit_Velocity)) + 
    geom_line() + ggtitle("Aaron Judge") +
    ylab("Probability of Hit")
judge3


# P-spline smoothers (with lambda=0.6) used for x1 and x2
b1 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps', sp=0.6) + s(Launch_Angle, bs='ps', sp=0.6),data = judge)
summary(b1)
plot(b1)

# select smoothing parameters with REML, using P-splines
b2 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps') + s(Launch_Angle, bs='ps'),data = judge, method = "REML", select=TRUE)
summary(b2)
plot(b2)
# select variables and smoothing parameters
b3 <- mgcv::gam(outcome ~ s(Exit_Velocity) + s(Launch_Angle), data = judge, method="REML", select=TRUE)
summary(b3)
# loess smoothers with the gam package (restart R before loading gam)
b4 <- gam::gam(outcome ~ lo(Exit_Velocity, span=0.6) + lo(Launch_Angle, span=0.6), data = judge)
summary(b4)

# Generalized Cross Validation
b5 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps', k=-1) + s(Launch_Angle, bs='ps', k=-1),data = judge)
summary(b5)
plot(b5)





######### Date column ##########

# Check if dates are formatted as dates
class(judge$Game_Date)

# Change them to dates
judge$Game_Date <- as.Date(judge$Game_Date, format = "%m/%d/%Y")
# Check that the variable is now formatted as a date
class(judge$Game_Date)

# Separate game_date into "year", "month", and "day":
judge <- separate(data = judge, col = Game_Date,
                   into = c("Year", "Month", "Day"),
                   sep = "-", remove = FALSE )
# Convert month to numeric
judge$Month <- as.numeric(judge$Month)

#Game-by-game velocity changes

# Create velo_dt
velo_dt <- data.frame(tapply(judge$Exit_Velocity, judge$Game_Date, mean))

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
judge_month <- plot(velo_dt$Exit_Velocity ~ velo_dt$Game_Date,
                     lwd = 4, type = "l", ylim = c(45, 120),
                     main = "2019 Judge Exit Velocity",
                     xlab = "Date", ylab = "Exit Velocity (mph)")

# Add jittered points to the plot
points(judge$Exit_Velocity ~ jitter(as.numeric(judge$Game_Date)),
       pch = 16, col = "#99004450")


# What factors improve or hurt judge's chance of a home run

# Logistic Regression
judge_log <- glm(Event == "home_run" ~ Month + Day + Exit_Velocity + Launch_Angle + Distance
+ Pitch_Speed + Pitch_Type, data = judge, family = "binomial")
exp(judge_log$coefficients)
summary(judge_log)

bwplot(Exit_Velocity ~ as.factor(Month), data = judge)
bwplot(Pitch_Type ~ Exit_Velocity, par.settings=list(box.rectangle=list(col="salmon",fill="salmon", alpha=0.4, data = judge)))

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 5:                             ###
###               PREDICTORS OF A HITTER'S EXIT VELOCITY                ###
###                                                                     ###
###########################################################################
###########################################################################

#banner("Section 5:", "Predictors of a Hitter's Exit Velocity ", emph = TRUE)



# Model Selection
models <- regsubsets(Exit_Velocity ~ Month + Launch_Angle +
Distance + Event + Direction + Pitch_Speed + Pitch_Type,  data = judge)
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



###########Utilize these models ############### 
(fit2 = Exit_Velocity ~ Month + Launch_Angle + Distance + Direction+ Event
   + Pitch_Type)
fit2.output = lm(fit2, data=judge)
fit2.summary = summary(fit2.output)
fit2.confint = confint(fit2.output, level = 0.95)

(fit3 = Exit_Velocity ~ Month + Launch_Angle + Distance
  + Event + Pitch_Speed)
fit3.output = lm(fit3, data=judge)
fit3.summary = summary(fit3.output)
fit3.confint = confint(fit3.output)

