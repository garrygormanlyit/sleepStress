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

nrow(sleepData$SleepHrs[sleepData$SleepHrs <= 0.001])

plot(sleepData$REM, sleepData$StressLevel, main = "Effects of REM on Stress", xlab = "REM", 
     ylab = "Stress",  col = "blue", pch = 19, ) #lty = 2, pch = 17,type = "o"

# convert rem into hours
sleepData$REMinHrs <- sleepData$REM/60
sleepData[1:15, ]

# create a new column REMRate to represent the proportion of rem sleep in total sleep
sleepData$REMRate <- sleepData$REMinHrs/sleepData$SleepHrs
sleepData[1:15, ]

plot(sleepData$REMRate, sleepData$StressLevel, main = "Effects of REM on Stress", xlab = "REM", 
     ylab = "Stress",  col = "blue", pch = 19, ) #lty = 2, pch = 17,type = "o"

# people who reported 0 hrs sleep have also reported  a stress level of 4
# create new df with only non-sleepers
noSleep <- sleepData[which(sleepData$SleepHrs == 0), ]
noSleep[1:15, ]

summary(noSleep)
nrow(noSleep)

# use this as an example
hist(sleepData$SleepHrs, 
     main = "Stress level from 1 to 4", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Stress Level")

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
