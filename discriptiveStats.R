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
par(mfrow = c(3,3)) # plot side by side

# testing for normality
hist(trustedData$SleepHrs, 
     main = "Sleep hours", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Sleep hours")

hist(trustedData$SnoringRate, 
     main = "Snoring Rate", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Snoring rate")

hist(trustedData$RespirationRate, 
     main = "Snoring Rate", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Snoring rate")

hist(trustedData$BodyTemp, 
     main = "Body Temperature in F", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Body Temp")

hist(trustedData$Movement, 
     main = "Movement", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Movement")

hist(trustedData$BloodOxygen, 
     main = "Blood oxygen level in %", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Blood oxygen level")

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

hist(trustedData$StressLevel, 
     main = "Stress level fro 0 to 4", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Stress level")
par(opar)


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
hist(sleepData$SnoringRate, 
     main = "Snoring Rate", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Snoring rate")

hist(sleepData$SleepHrs, 
     main = "Sleep hours", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Sleep hours")

hist(sleepData$BodyTemp, 
     main = "Body Temperature F", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Temperature")

hist(sleepData$Movement, 
     main = "Movement", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Movement")

hist(sleepData$BloodOxygen, 
     main = "Blood Oxygen %", 
     col = "blue",
     ylab = "Frequency",
     xlab = "Blood Oxygen %")

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
