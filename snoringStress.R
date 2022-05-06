sleepData <- read.csv("factoredStress.csv")
str(sleepData)

# create df with SnoringRate and StressCat only
sleepData <- sleepData[, c('SnoringRate', 'StressLevel')]
str(sleepData)
sleepData$StressCat <- factor(sleepData$StressLevel, 
                              labels = c("no stress", "mild stress", "moderated stress", "high stress", "extreme stress"),
                              ordered = TRUE)

is.factor(sleepData$StressCat) # true

summary(sleepData)

attach(sleepData)
plot(StressCat, SnoringRate, main = "Stress Category vs Snoring Rate",
     xlab = "Stress Category",
     ylab = "Snoring Rate (%)")

library("lattice")
attach(sleepData)
histogram(~ SnoringRate | StressCat,
          data = sleepData,
          main = "Distribution of Stress Category data vs snoring rate",
          xlab = "Snoring Rate",
          ylab = "Frequency")

tapply(SnoringRate, StressCat, mean)

opar <- par(no.readonly = TRUE) # records current settings for plots
par(mfrow = c(2,3)) # plot side by side
# Q-Q plot between both variables
with(sleepData, {qqnorm(SnoringRate[StressCat == "no stress"],
                        main = "No stress data") 
  qqline(SnoringRate[StressCat == "no stress"])})

with(sleepData, {qqnorm(SnoringRate[StressCat == "mild stress"],
                        main = "mild stress data") 
  qqline(SnoringRate[StressCat == "mild stress"])})

with(sleepData, {qqnorm(SnoringRate[StressCat == "moderated stress"],
                        main = "moderate stress data") 
  qqline(SnoringRate[StressCat == "moderated stress"])})

with(sleepData, {qqnorm(SnoringRate[StressCat == "high stress"],
                        main = "high stress data") 
  qqline(SnoringRate[StressCat == "high stress"])})

with(sleepData, {qqnorm(SnoringRate[StressCat == "extreme stress"],
                        main = "extreme stress data") 
  qqline(SnoringRate[StressCat == "extreme stress"])})
par(opar) 

#formal test for normality
# using shapiro-wilk

normality_test <- shapiro.test(sleepData$SnoringRate[sleepData$StressCat == "no stress"])
normality_test$p.value # p=0.00033636
# the shapiro-wilk test shows that the SnoringRate is not normally distributed

normality_test <- shapiro.test(sleepData$SnoringRate[sleepData$StressCat == "mild stress"])
normality_test$p.value # p= 0.00033636

normality_test <- shapiro.test(sleepData$SnoringRate[sleepData$StressCat == "moderated stress"])
normality_test$p.value # p= 0.00033636

normality_test <- shapiro.test(sleepData$SnoringRate[sleepData$StressCat == "high stress"])
normality_test$p.value # p= 0.00033636

normality_test <- shapiro.test(sleepData$SnoringRate[sleepData$StressCat == "extreme stress"]) 
normality_test$p.value # p= 0.00033636

# Kolmogorov-Smirnov test to confirm normality
ks.test(sleepData$SnoringRate[sleepData$StressCat == "no stress"], 'pnorm')# p-value < 2.2e-16
ks.test(sleepData$SnoringRate[sleepData$StressCat == "mild stress"], 'pnorm')# p-value < 2.2e-16
ks.test(sleepData$SnoringRate[sleepData$StressCat == "moderated stress"], 'pnorm')# p-value < 2.2e-16
ks.test(sleepData$SnoringRate[sleepData$StressCat == "high stress"], 'pnorm')# p-value < 2.2e-16
ks.test(sleepData$SnoringRate[sleepData$StressCat == "extreme stress"], 'pnorm') #p-value < 2.2e-16
# not normally distributed. Use a non-parametric test

kruskal.test(SnoringRate, StressCat) #p-value < 2.2e-16. Therefore, p<0.05. Variables are related
