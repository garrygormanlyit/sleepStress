sleepData <- read.csv("factoredStress.csv")
str(sleepData)

# create df with Movement and StressCat only
sleepData <- sleepData[, c('Movement', 'StressLevel')]
str(sleepData)
sleepData$StressCat <- factor(sleepData$StressLevel, 
                              labels = c("no stress", "mild stress", "moderated stress", "high stress", "extreme stress"),
                              ordered = TRUE)

is.factor(sleepData$StressCat) # true

summary(sleepData)

attach(sleepData)
plot(StressCat, Movement, main = "Stress category vs movement",
     xlab = "Stress Category",
     ylab = "Movement")

library("lattice")
attach(sleepData)
histogram(~ Movement | StressCat,
          data = sleepData,
          main = "Distribution of stress category data vs movement",
          xlab = "Movement",
          ylab = "Frequency")

tapply(Movement, StressCat, mean)

opar <- par(no.readonly = TRUE) # records current settings for plots
par(mfrow = c(2,3)) # plot side by side
# Q-Q plot between both variables
with(sleepData, {qqnorm(Movement[StressCat == "no stress"],
                        main = "No stress data") 
  qqline(Movement[StressCat == "no stress"])})

with(sleepData, {qqnorm(Movement[StressCat == "mild stress"],
                        main = "mild stress data") 
  qqline(Movement[StressCat == "mild stress"])})

with(sleepData, {qqnorm(Movement[StressCat == "moderated stress"],
                        main = "moderate stress data") 
  qqline(Movement[StressCat == "moderated stress"])})

with(sleepData, {qqnorm(Movement[StressCat == "high stress"],
                        main = "high stress data") 
  qqline(Movement[StressCat == "high stress"])})

with(sleepData, {qqnorm(Movement[StressCat == "extreme stress"],
                        main = "extreme stress data") 
  qqline(Movement[StressCat == "extreme stress"])})
par(opar) 

#formal test for normality
# using shapiro-wilk

normality_test <- shapiro.test(sleepData$Movement[sleepData$StressCat == "no stress"])
normality_test$p.value # p=0.00033636
# the shapiro-wilk test shows that the Movement is not normally distributed

normality_test <- shapiro.test(sleepData$Movement[sleepData$StressCat == "mild stress"])
normality_test$p.value # p= 0.00033636

normality_test <- shapiro.test(sleepData$Movement[sleepData$StressCat == "moderated stress"])
normality_test$p.value # p= 0.00033636

normality_test <- shapiro.test(sleepData$Movement[sleepData$StressCat == "high stress"])
normality_test$p.value # p= 0.00033636

normality_test <- shapiro.test(sleepData$Movement[sleepData$StressCat == "extreme stress"]) 
normality_test$p.value # p= 0.00033636

# Kolmogorov-Smirnov test to confirm normality
ks.test(sleepData$Movement[sleepData$StressCat == "no stress"], 'pnorm')# p-value < 2.2e-16
ks.test(sleepData$Movement[sleepData$StressCat == "mild stress"], 'pnorm')# p-value < 2.2e-16
ks.test(sleepData$Movement[sleepData$StressCat == "moderated stress"], 'pnorm')# p-value < 2.2e-16
ks.test(sleepData$Movement[sleepData$StressCat == "high stress"], 'pnorm')# p-value < 2.2e-16
ks.test(sleepData$Movement[sleepData$StressCat == "extreme stress"], 'pnorm') #p-value < 2.2e-16
# not normally distributed. Use a non-parametric test

kruskal.test(Movement, StressCat) #p-value < 2.2e-16. Therefore, p<0.05. Variables are related
