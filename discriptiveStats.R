getwd()

sleepData <- read.csv("sleep.csv")
str(sleepData)

colnames(sleepData) <- c('SnoringRate', 'RespirationRate', 
                         'BodyTemp', 'Movement', 'BloodOxygen', 'REM', 
                         'SleepHrs', 'HeartRate', 'StressLevel')
sleepData[1:15, ]

# zero sleepHrs is wrong. Rows stating that sleepHrs are 0 have a value > 0 for SnoringRate and REM
# create a new column 

summary(sleepData)
dim(sleepData) # 630 rows 9 columns

colSums(sleepData == 0) # shows sleepHrs has 127 entries with zero sleepHrs and StressLevel with 126 entries as zero
noSleep <- sleepData[sleepData$SleepHrs == 0 & sleepData$REM > 0 & sleepData$SnoringRate > 0,]
noSleep
nrow(noSleep) # 127 rows where sleephrs = 0
nrow(noSleep[noSleep$StressLevel == 4,]) # 126

plot(sleepData$REM, sleepData$StressLevel, main = "Effects of REM on Stress", xlab = "REM", 
     ylab = "Stress",  col = "blue", pch = 19, ) #lty = 2, pch = 17,type = "o"

# convert rem into hours
sleepData$REMinHrs <- sleepData$REM/60
sleepData[1:15, ]

# create a new dataset where sleephrs is greater than REM
#trustedData <- sleepData[sleepData$SleepHrs > sleepData$REMinHrs,]
trustedData <- sleepData[sleepData$SleepHrs != 0,]
trustedData
nrow(trustedData)

opar <- par(no.readonly = TRUE) # records current settings for plots
par(mfrow = c(2,2)) # plot side by side

# testing for normality
hist(trustedData$SleepHrs, 
     main = "Sleep hours", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Sleep hours")

hist(trustedData$Movement, 
     main = "Movement", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Movement")

hist(trustedData$REM, 
     main = "REM in minutes", 
     col = "blue",
     ylab = "Frequency",
     xlab = "REM")

hist(trustedData$HeartRate, 
     main = "Heart rate in beats per minute", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Heart rate")

par(opar)
par(mfrow = c(1,1))
# StressLevel is categorical and ordinal therefore it must be factored to be analysed correctly
sleepData$StressCat <- factor(sleepData$StressLevel, 
                              labels = c("no stress", "mild stress", "moderated stress", "high stress", "extreme stress"),
                              ordered = TRUE)

# to analyse correlation of stressCat against HR
# this demonstrates a correlation between the variables
plot(sleepData$StressCat, sleepData$HeartRate, main = "Stress Category vs heart rate")

# this histogram tells us nothing
hist(trustedData$StressLevel, 
    main = "Stress level from 0 to 4", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Stress level")

# use lattice library to resolve the matter
# plot the distribution of Stress Category against heart rate
library("lattice")
attach(sleepData)
histogram(~ HeartRate | StressCat,
          data = sleepData,
          main = "Distribution of Stress Category data",
          xlab = "Heart rate in beats per minute",
          ylab = "Stress Category")

tapply(HeartRate, StressCat, mean)
# report that the the mean of HR\no stress is 52.5 suggesting that HR\No stress may be normally distributed
# report that the the mean of HR\mild stress is 57.5 suggesting that HR\mild stress may be normally distributed 
# the mean of HR\no stress is 52.5 suggesting that HR\No stress may be normally distributed
# the mean of HR\moderate stress is 62.5 suggesting that HR\moderate stress may be normally distributed
# HR\High mean is 70 suggesting that is may be normally distributed
# HR\extreme stress mean HR is 80 suggesting that it may be slightly negatively skewed
par(opar)
# Q-Q plot of HR
qqnorm(HeartRate, main = "Q-Q plot of first dataset variable")
qqline(HeartRate)
# the points on Q-Q plot mostly fall on or close to the 45-degree reference line.
# The main departure from this line occur at the high and low values
par(mfrow = c(2,3))
# Q-Q plot 
with(sleepData, {qqnorm(HeartRate[StressCat == "no stress"],
                          main = "No stress data") 
  qqline(HeartRate[StressCat == "no stress"])})

with(sleepData, {qqnorm(HeartRate[StressCat == "mild stress"],
                        main = "mild stress data") 
  qqline(HeartRate[StressCat == "mild stress"])})

with(sleepData, {qqnorm(HeartRate[StressCat == "moderated stress"],
                        main = "moderate stress data") 
  qqline(HeartRate[StressCat == "moderated stress"])})

with(sleepData, {qqnorm(HeartRate[StressCat == "high stress"],
                        main = "high stress data") 
  qqline(HeartRate[StressCat == "high stress"])})

with(sleepData, {qqnorm(HeartRate[StressCat == "extreme stress"],
                        main = "extreme stress data") 
  qqline(HeartRate[StressCat == "extreme stress"])})


# the points on Q-Q plot mostly fall on or close to the 45-degree reference line.
# The main departure from this line occur at the high and low values

with(sleepData, tapply(HeartRate, StressCat, shapiro.test))
# all categories of stress fall below the cutoff p-value of 0.05 confirming that the data is not normally distributed 
# this means that the data is non-parametric in nature and should be analysed by non-parametric means

# after consulting the chart, i am examining a dependent continuous variable (HeartRate)
# whit an independant categorical variable (StressCat) 
# therefore I use the Kruskal-Wallis test
kruskal.test(HeartRate, StressCat)

# p-value is < 0.05 so we reject H0 and conclude
# that Heart Rate is effected by stress during sleep
########################################################################################################################
# to answer - REM is between 18% and 25% of sleep hours for low stress sufferers (< 2)
# create a new column REMRate to represent the proportion of rem sleep in total sleep
#sleepData$REMRate <- sleepData$REMinHrs/sleepData$SleepHrs
#sleepData[1:15, ]
# trustedData is data where sleepHrs is not = 0
trustedData$REMRate <- trustedData$REMinHrs/trustedData$SleepHrs
summary(trustedData)
str(trustedData)

# create a df of sleepers where REM rate is between 18% and 25%
goodREMRate <- trustedData[trustedData$REMRate >= 0.18 & trustedData$REMRate <= 0.25,]

variables_of_interest <- c("REMRate", 
                           "StressLevel")
goodREMRate <- goodREMRate[variables_of_interest]
summary(goodREMRate)

plot(goodREMRate$StressLevel, goodREMRate$REMRate, main = "Effects of REM on Stress", xlab = "Stress", 
     ylab = "REM rate",  col = "blue", pch = 19, ) #lty = 2, pch = 17,type = "o"


cor(sleepData$REMRate, sleepData$StressLevel)

plot(sleepData$StressLevel, sleepData$REMRate, main = "Effects of REM on Stress", xlab = "Stress", 
     ylab = "REM rate",  col = "blue", pch = 19, ) #lty = 2, pch = 17,type = "o"

# people who reported 0 hrs sleep have also reported  a stress level of 4
# create new df with only non-sleepers
# noSleep <- sleepData[which(sleepData$SleepHrs == 0), ]
# noSleep[1:15, ]

summary(noSleep)
nrow(noSleep)

opar <- par(no.readonly = TRUE) # records current settings for plots
par(mfrow = c(3,3)) # plot side by side

# to test for normal distribution


hist(sleepData$SleepHrs, 
     main = "Sleep hours", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Sleep hours")

hist(sleepData$Movement, 
     main = "Movement", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Movement")

hist(sleepData$REM, 
     main = "REM in minutes", 
     col = "blue",
     ylab = "Frequency",
     xlab = "REM in minutes")

hist(sleepData$HeartRate, 
     main = "Heart rate in beats per minutes", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Heart rate")

hist(sleepData$StressLevel, 
     main = "Stress level 0-4", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Stress level")
par(opar)

# test for normality
shapiro.test(sleepData$SnoringRate)
shapiro.test(sleepData$SleepHrs)
shapiro.test(sleepData$BodyTemp)
shapiro.test(sleepData$Movement)
shapiro.test(sleepData$BloodOxygen)
shapiro.test(sleepData$REM)
shapiro.test(sleepData$HeartRate)
shapiro.test(sleepData$StressLevel)

variables_of_interest <- c("SnoringRate", 
                           "RespirationRate", 
                           "BodyTemp", 
                           "Movement", 
                           "BloodOxygen",
                           "REM",
                           "SleepHrs",
                           "HeartRate",
                           "StressLevel",
                           "REMinHrs")

pairs(sleepData[variables_of_interest])

# shows pos and neg correlations data in the data
# when building a neural network you need correlations first 
# to identify related variables
install.packages("corrplot")
library(corrplot)
corrplot(corr = cor(sleepData),
         t1.col = "Black",
         method = "number",
         t1.cex = 0.5)
