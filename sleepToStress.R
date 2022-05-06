# H0: Sleep is not inversely proportional to stress

sleepData <- read.csv("sleep.csv")
str(sleepData)

# check for incomplete data
incompletData <- sleepData[!complete.cases(sleepData),]
nrow(incompletData)

#change column names
colnames(sleepData) <- c('SnoringRate', 'RespirationRate', 
                         'BodyTemp', 'Movement', 'BloodOxygen', 'REM', 
                         'SleepHrs', 'HeartRate', 'StressLevel')


install.packages("psych")
library(psych)

pairs.panels(sleepData,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# examine does stress level effect sleep?
# vars are wt and mpg?


# We would like to examine whether there is a link between the stress level and sleep hours
# Devise a hypothesis test that we use for this analysis.
# Describe the variables you will use for the analysis. What type of variables are they?
# Do they need to be converted? If they do, convert them to the relevant data types
# in R.



# StressLevel is categorical and ordinal therefore it must be factored to be analysed correctly
sleepData$StressCat <- factor(sleepData$StressLevel, 
                              labels = c("no stress", "mild stress", "moderated stress", "high stress", "extreme stress"),
                              ordered = TRUE)

head(sleepData)

# sleepStress <- sleepData[, c('StressCat', 'SleepHrs')]
# summary(sleepStress)

plot(sleepData$StressCat, sleepData$SleepHrs, main = "Stress Category vs Sleep hours",
     xlab = "Stress level",
     ylab = "Sleep hours")


attach(sleepData)
# use lattice library to resolve the matter
# plot the distribution of Stress Category against heart rate
library("lattice")
attach(sleepData)
histogram(~ SleepHrs | StressCat,
          data = sleepData,
          main = "Distribution of Stress Category data vs sleep hours",
          xlab = "Sleep in hours",
          ylab = "Stress Category")

tapply(SleepHrs, StressCat, mean)
# look at the histogram plot and compare to the means from here to help determine normality
# report that the the mean of Sleephrs\no stress is 8 suggesting that HR\No stress may be normally distributed
# report that the the mean of Sleephrs\mild stress is 6 suggesting that Sleephrs\mild stress may be normally distributed 
# the mean of Sleephrs\moderate stress is 3.5 suggesting that Sleephrs\moderate stress may be normally distributed
# Sleephrs\High mean is 1 suggesting that it may be slightly negatively skewed
# Sleephrs\extreme stress mean HR is 0 suggesting that it is not normally distributed


opar <- par(no.readonly = TRUE) # records current settings for plots
par(mfrow = c(2,3)) # plot side by side
# Q-Q plot between both variables
with(sleepData, {qqnorm(SleepHrs[StressCat == "no stress"],
                        main = "No stress data") 
  qqline(SleepHrs[StressCat == "no stress"])})

with(sleepData, {qqnorm(SleepHrs[StressCat == "mild stress"],
                        main = "mild stress data") 
  qqline(SleepHrs[StressCat == "mild stress"])})

with(sleepData, {qqnorm(SleepHrs[StressCat == "moderated stress"],
                        main = "moderate stress data") 
  qqline(SleepHrs[StressCat == "moderated stress"])})

with(sleepData, {qqnorm(SleepHrs[StressCat == "high stress"],
                        main = "high stress data") 
  qqline(SleepHrs[StressCat == "high stress"])})

with(sleepData, {qqnorm(SleepHrs[StressCat == "extreme stress"],
                       main = "extreme stress data") 
  qqline(SleepHrs[StressCat == "extreme stress"])})
par(opar) 

summary(sleepData$SleepHrs[sleepData$StressCat == "extreme stress"])
# all SleepHrs where StressCat = "extreme stress" are 0

#formal test for normality
# using shapiro-wilk

normality_test <- shapiro.test(sleepData$SleepHrs[sleepData$StressCat == "no stress"])
normality_test$p.value # p=2.176*10**-20
# the shapiro-wilk test shows that the SleepHrs is not normally distributed

normality_test <- shapiro.test(sleepData$SleepHrs[sleepData$StressCat == "mild stress"])
normality_test$p.value # p= 0.0003336

normality_test <- shapiro.test(sleepData$SleepHrs[sleepData$StressCat == "moderated stress"])
normality_test$p.value # p= 0.0003336

normality_test <- shapiro.test(sleepData$SleepHrs[sleepData$StressCat == "high stress"])
normality_test$p.value # p= 0.0003336

normality_test <- shapiro.test(sleepData$SleepHrs[sleepData$StressCat == "extreme stress"]) 
normality_test$p.value # cant be done because they are all 0s

# ks.test(sleepData$SleepHrs[sleepData$StressCat == "extreme stress"], 'pnorm') 
# cant be done as there are repeated values

# the shapiro-wilk test shows that the SleepHrs is not normally distributed

# Kolmogorov-Smirnov test to confirm normality
ks.test(sleepData$SleepHrs[sleepData$StressCat == "no stress"], 'pnorm')
ks.test(sleepData$SleepHrs[sleepData$StressCat == "mild stress"], 'pnorm')
ks.test(sleepData$SleepHrs[sleepData$StressCat == "moderated stress"], 'pnorm')
ks.test(sleepData$SleepHrs[sleepData$StressCat == "high stress"], 'pnorm')
ks.test(sleepData$SleepHrs[sleepData$StressCat == "extreme stress"], 'pnorm')

# after consulting the chart, i am examining a dependent continuous variable (SleepHrs)
# with an independant categorical variable (StressCat) 
# therefore I use the Kruskal-Wallis test
kruskal.test(SleepHrs, StressCat) # p=2.2*10**-16
# the correlation coefficient between SleepHrs and StressCat is p = 2.2*10**-16. 
# This is below the 0.5 cut-off, therefore the null hypothesis can be rejected.

write.csv(sleepData,"factoredStress.csv", row.names = FALSE)
