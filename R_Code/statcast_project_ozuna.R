############################## Ozuna Statcast ############################## 



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

# Reading in the data set and naming the dataframe ozuna
library(readxl)
ozuna <- read_excel("ozuna.xlsx")
attach(ozuna)
View(ozuna)

# First five rows of the ozuna data frame
head(ozuna)

# Descriptive statistics
summary(ozuna)

# structure of the data frame
str(ozuna) # 549 rows of 10 columns

# remove index column
ozuna$...1 <- NULL




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
any(is.na(ozuna)) # returns True

# plot of missing values by column
missmap(ozuna, main="Ozuna - Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

# NA values by column
na_count <-sapply(ozuna, function(y) sum(is.na(y)))
na_count # We have many missing values in a variety of columns / 18 in all

# Let's find the missing rows
na_rows <- ozuna[rowSums(is.na(ozuna)) > 0,]
na_rows
which(is.na(ozuna), arr.ind=TRUE)

# Remove all na's
sum(is.na(ozuna))
ozuna=na.omit(ozuna)


###### No more NA values #######

# Let's look at the unique category counts of our character columns
str(ozuna)

sapply(ozuna, function(x) length(unique(x)))

# We have 169 unique pitcher names, so we can't do anything with this column / let's drop it
# Also our batter column only includes Ozuna's name so let's drop it too
ozuna <- subset(ozuna, select = -c(Pitcher,Batter) )

# Let's look at the sum of unique categories for the Event, Direction, and Pitch Type columns
count(ozuna,Event)
count(ozuna,Direction) # Direction column looks evenly distributed
count(ozuna,`Pitch Type`)


# Event column has many groups that only have a count of one or two / let's move them into another category
ozuna$Event[ozuna$Event == 'field_error'] <- 'field_out'
ozuna$Event[ozuna$Event == 'fielders_choice'] <- 'field_out'
ozuna$Event[ozuna$Event == 'grounded_into_double_play'] <- 'double_play'
# Among the four hitters let's include the same categories for the Event column


# Pitch Type column has pitches that only showed up once or twice / let's remove these rows
ozuna <- filter(ozuna, `Pitch Type` != "Split-Finger")



ozuna <- rename(ozuna,
                Game_Date = `Game Date`,
                Pitch_Speed = `Pitch (MPH)`,
                Pitch_Type = `Pitch Type`)

# Delimit dates
library(tidyr)
# Separate game_date into "year", "month", and "day":
ozuna <- separate(data = ozuna, col = Game_Date,
                   into = c("Year", "Month", "Day"),
                   sep = "-", remove = FALSE )

# convert the newly created Month and Day columns to numeric
ozuna$Month <- as.numeric(ozuna$Month)
ozuna$Day <- as.numeric(ozuna$Day)


# Creation of new variable Outcome = "Hit/Out", outcome = 0/1
ozuna <- mutate(ozuna,
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
ozuna %>%
  gather(attributes,value,5:7,10) %>%
  ggplot(aes(x=value)) +
  geom_histogram(fill='blue',color='black') +
  scale_y_continuous(limits = c(0,75)) +
  facet_wrap(~attributes, scales = 'free_x') +
  labs(x='Values',y='Frequency') 

# The columns look to be normally distributed 
# Let's individually graph the launch angle

# Launch angle histogram
ozuna %>% 
  ggplot(aes(x=Launch_Angle)) +
  geom_histogram(aes(fill=..count..)) +
  xlab('Degrees') + ylab('Occurences') + ggtitle('Launch Angle Histogram')

# Correlation Matrix
my_data <- ozuna[, c(3,5:7,10,13)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

# Scatterplot between the highest correlated variables: Launch Angle and Distance 0.68
ozuna %>%
  ggplot(aes(x=Launch_Angle,y=Distance)) +
  geom_point(aes(color=Outcome)) +
  xlab('Launch Angle') + ylab('Distance') +
  scale_x_continuous(breaks = seq(-90, 100, by = 10))

# Marcell Ozuna", "- In-Play Outcomes scatterplot
ozuna1 <- ggplot(ozuna, aes(Launch_Angle, Exit_Velocity, color=Event)) + 
  geom_point() + 
  ggtitle(paste("Marcell Ozuna", "- In-Play Outcomes")) 
ozuna1

# Dealing with outliers / unusual values
summary(ozuna) 

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 4:                             ###
###             MODELING FOR FITTED PROBABILITIES OF A HIT              ###
###                                                                     ###
###########################################################################
###########################################################################

#banner("Section 4:", "Modeling for Fitted Probabilities of a Hit ", emph = TRUE)

library(gam)
# Generalized additive model
fit_gam_ozuna <- gam(outcome ~ s(Launch_Angle, Exit_Velocity), family=binomial, data=ozuna)

# Creation of new variable Prob_Hit
ozuna <- mutate(ozuna, Prob_Hit = exp(predict(fit_gam_ozuna)) / (1 + exp(predict(fit_gam_ozuna))))

# Scatterplot of Gallo's Event Outcomes by LA and EV
ozuna1 <- ggplot(ozuna, aes(Launch_Angle, Exit_Velocity, color=Event)) + 
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
b4 <- gam::gam(outcome ~ lo(Exit_Velocity, span=0.6) + lo(Launch_Angle, span=0.6), data = ozuna)
summary(b4)

# Generalized Cross Validation
b5 <- mgcv::gam(outcome ~ s(Exit_Velocity, bs='ps', k=-1) + s(Launch_Angle, bs='ps', k=-1),data = ozuna)
summary(b5)
plot(b5)







######### Date column ##########

# Check if dates are formatted as dates
class(ozuna$Game_Date)

# Change them to dates
ozuna$Game_Date <- as.Date(ozuna$Game_Date, format = "%m/%d/%Y")
# Check that the variable is now formatted as a date
class(ozuna$Game_Date)

# Separate game_date into "year", "month", and "day":
ozuna <- separate(data = ozuna, col = Game_Date,
                  into = c("year", "month", "day"),
                  sep = "-", remove = FALSE )
# Convert month to numeric
ozuna$month <- as.numeric(ozuna$month)

#Game-by-game velocity changes

# Create velo_dt
velo_dt <- data.frame(tapply(ozuna$Exit_Velocity, ozuna$Game_Date, mean))

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
ozuna_month <- plot(velo_dt$Exit_Velocity ~ velo_dt$Game_Date,
                    lwd = 4, type = "l", ylim = c(45, 120),
                    main = "2019 Ozuna Exit Velocity",
                    xlab = "Date", ylab = "Exit Velocity (mph)")

# Add jittered points to the plot
points(ozuna$Exit_Velocity ~ jitter(as.numeric(ozuna$Game_Date)),
       pch = 16, col = "#99004450")



# Box and whisker plot
bwplot(Exit_Velocity ~ as.factor(month), data = ozuna)
bwplot(Pitch_Type ~ Exit_Velocity, main = "Ozuna EV by Pitch Type",
       par.settings=list(box.rectangle=list(col="salmon",fill="salmon", alpha=0.4,
                                            data = ozuna)))

# Logistic Regression
ozuna_log <- glm(Event == "home_run" ~ month + Exit_Velocity + Launch_Angle + Distance
                 + Pitch_Speed + Pitch_Type, data = ozuna, family = "binomial")
exp(ozuna_log$coefficients)
summary(ozuna_log)


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
library(leaps)
models <- regsubsets(Exit_Velocity ~ Month + Launch_Angle +
                       Distance + Event + Direction + Pitch_Speed + Pitch_Type,  data = ozuna)
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

# regsubsets bic model
fit <- lm(Exit_Velocity ~ Distance + Event + Direction)
summary(fit)
########### More Linear Fits ############### 
(fit2 = Exit_Velocity ~ month + Launch_Angle + Distance + Direction+ Event
 + Pitch_Type)
fit2.output = lm(fit2, data=ozuna)
fit2.summary = summary(fit2.output)
fit2.confint = confint(fit2.output, level = 0.95)

(fit3 = Exit_Velocity ~ month + Launch_Angle + Distance
  + Event + Pitch_Speed)
fit3.output = lm(fit3, data=ozuna)
fit3.summary = summary(fit3.output)
fit3.confint = confint(fit3.output)





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







