sleepData <- read.csv("factoredStress.csv")
str(sleepData)

# create df with REM, sleephrs and Stresslevel only
sleepData <- sleepData[, c('REM', 'SleepHrs', 'StressLevel')]
str(sleepData)
# factor
sleepData$StressCat <- factor(sleepData$StressLevel, 
                              labels = c("no stress", "mild stress", "moderated stress", "high stress", "extreme stress"),
                              ordered = TRUE)

is.factor(sleepData$StressCat) # true

summary(sleepData)

# discard those who did not sleep as a expressing REM as a proportion of nothing is impossible
sleepData <- sleepData[sleepData$SleepHrs != 0,]
# new column REM in hours
sleepData$REMinHrs <- sleepData$REM/60
# new column showing REM as a proportion of sleep
sleepData$REMrate <- sleepData$REMinHrs/sleepData$SleepHrs
str(sleepData)

plot(StressCat, REMrate, main = "Stress category vs movement",
     xlab = "Stress Category",
     ylab = "Movement")


attach(sleepData)
plot(StressCat[StressCat == "no stress"], REMrate[StressCat == "no stress"], main = "Stress category vs REM rate",
     xlab = "Stress Category",
     ylab = "REM rate")# ylim=c(0.1, 0.27)

opar <- par(no.readonly = TRUE) # records current settings for plots
par(mfrow = c(2,2))
# range is too wide to extract insights from a regular boxplot
noStress <- sleepData[sleepData$StressCat == "no stress",]
boxplot(noStress$REMrate, main = "Boxplot for REM rate for no stress",
        xlab = "No Stress",
        ylab = "REM rate")

mildStress <- sleepData[sleepData$StressCat == "mild stress",]
boxplot(mildStress$REMrate, main = "Boxplot for REM rate for mild stress",
        xlab = "Mild Stress",
        ylab = "REM rate")
head(mildStress)

moderateStress <- sleepData[sleepData$StressCat == "moderated stress",]
boxplot(moderateStress$REMrate, main = "Boxplot for REM rate for moderate stress",
        xlab = "moderate Stress",
        ylab = "REM rate")


highStress <- sleepData[sleepData$StressCat == "high stress",]
boxplot(highStress$REMrate, main = "Boxplot for REM rate for high stress",
        xlab = "High Stress",
        ylab = "REM rate")

par(opar)

library("lattice")
attach(sleepData)
histogram(~ REMrate | StressCat,
          data = sleepData,
          main = "Distribution of REM rate by stress category",
          xlab = "Rem rate (%)",
          ylab = "Frequency",
          xlim=c(0, 20))

opar <- par(no.readonly = TRUE) # records current settings for plots
par(mfrow = c(2,2))
hist(noStress$REMrate, 
     main = "Distribution of REM rate for no stress", 
     col = "blue",
     ylab = "Frequency",
     xlab = "REM rate")

hist(mildStress$REMrate, 
     main = "Distribution of REM rate for mild stress", 
     col = "blue",
     ylab = "Frequency",
     xlab = "REM rate")

hist(moderateStress$REMrate, 
     main = "Distribution of REM rate for moderate stress", 
     col = "blue",
     ylab = "Frequency",
     xlab = "REM rate")

hist(highStress$REMrate, 
     main = "Distribution of REM rate for high stress", 
     col = "blue",
     ylab = "Frequency",
     xlab = "REM rate")
par(opar)

tapply(REMrate, StressCat, mean)

opar <- par(no.readonly = TRUE) # records current settings for plots
par(mfrow = c(2,2)) # plot side by side
# Q-Q plot between both variables
with(sleepData, {qqnorm(REMrate[StressCat == "no stress"],
                        main = "No stress data") 
  qqline(REMrate[StressCat == "no stress"])})

with(sleepData, {qqnorm(REMrate[StressCat == "mild stress"],
                        main = "mild stress data") 
  qqline(REMrate[StressCat == "mild stress"])})

with(sleepData, {qqnorm(REMrate[StressCat == "moderated stress"],
                        main = "moderate stress data") 
  qqline(REMrate[StressCat == "moderated stress"])})

with(sleepData, {qqnorm(REMrate[StressCat == "high stress"],
                        main = "high stress data") 
  qqline(REMrate[StressCat == "high stress"])})

par(opar)

normality_test <- shapiro.test(sleepData$REMrate[sleepData$StressCat == "no stress"])
normality_test$p.value # p=0.00033636
# the shapiro-wilk test shows that the Movement is not normally distributed

normality_test <- shapiro.test(sleepData$REMrate[sleepData$StressCat == "mild stress"])
normality_test$p.value # p= 0.00033636

normality_test <- shapiro.test(sleepData$REMrate[sleepData$StressCat == "moderated stress"])
normality_test$p.value # p= 0.00033636

normality_test <- shapiro.test(sleepData$REMrate[sleepData$StressCat == "high stress"])
normality_test$p.value # p= 0.00033636

ks.test(sleepData$REMrate[sleepData$StressCat == "no stress"], 'pnorm')# p-value < 2.2e-16
ks.test(sleepData$REMrate[sleepData$StressCat == "mild stress"], 'pnorm')# p-value < 2.2e-16
ks.test(sleepData$REMrate[sleepData$StressCat == "moderated stress"], 'pnorm')# p-value < 2.2e-16
ks.test(sleepData$REMrate[sleepData$StressCat == "high stress"], 'pnorm')# p-value < 2.2e-16

