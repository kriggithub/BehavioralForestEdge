# 9/20/25
# Kurt Riggin
# Fitting models to binned data
library(ggplot2)
library(ggpubr)
library(segmented)
library(strucchange)


anthBinData <- read.csv("anthBinData.csv")

# create prediction dataframe

predData <- data.frame(
  wtAvgAnthDist = seq(min(anthBinData$wtAvgAnthDist, na.rm = T),
                      max(anthBinData$wtAvgAnthDist, na.rm = T),
                      length.out = 200)
)


# Distance from NN
# null
null <- lm(data = anthBinData, formula = wtAvgDistNN ~ 1, weights = 1/((wtSeDistNN)^2))
AIC(null) # 71.2517
predData$null <- predict(null, newdata = predData)

nullplot <- ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = "Null Model (AIC: 71.25)") +
  theme_bw() +
  geom_line(data = predData, aes(y = null))

# linear
linear <- lm(data = anthBinData, formula = wtAvgDistNN ~ wtAvgAnthDist, weights = 1/((wtSeDistNN)^2))
AIC(linear) # 69.93471
predData$linear <- predict(linear, newdata = predData)

linearplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = "Linear Model (AIC: 69.93)") +
  theme_bw() +
  geom_line(data = predData, aes(y = linear))



# find starting values for a and b
# power (ab parameters)
lmStart <- lm(log(wtAvgDistNN) ~ log(wtAvgAnthDist), data = anthBinData, weights = 1/((wtSeDistNN)^2))
bStart <- coef(lmStart)[2]
aStart <- exp(coef(lmStart)[1])

powerab <- nls(wtAvgDistNN ~ a * wtAvgAnthDist^b, data = anthBinData, start = list(a = aStart, b = bStart), weights = 1/((wtSeDistNN)^2))
AIC(powerab) # 72.44315
predData$powerab <- predict(powerab, newdata = predData)
coef(powerab)



powerabplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = "Power Model (ab param) (AIC 72.44)") +
  theme_bw() +
  geom_line(data = predData, aes(y = powerab))


# power (abc parameters)
# guessed a,b,c by visual inspection and graphing on Desmos
powerabc <- nls(wtAvgDistNN ~ a * ((wtAvgAnthDist/400)^b) + c, data = anthBinData, start = list(a = 3, b = 5, c = 3.75), weights = 1/((wtSeDistNN)^2))
AIC(powerabc) # 67.33387
predData$powerabc <- predict(powerabc, newdata = predData)

powerabcplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = "Power Model (abc param) (AIC 67.34)") +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabc))

coef(powerabc)





# exponential
exponential <- nls(wtAvgDistNN ~ a * exp((wtAvgAnthDist/400)*b) + c, data = anthBinData, start = list(a = 0.5, b = 1, c = 3), weights = 1/((wtSeDistNN)^2))
AIC(exponential) # 67.52176
predData$exponential <- predict(exponential, newdata = predData)

exponentialplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = "Exponential Model (AIC 67.52)") +
  theme_bw() +
  geom_line(data = predData, aes(y = exponential))

coef(exponential)




# 3 parameter logistic
# is there a better way than visual inspection?
logistic <- nls(wtAvgDistNN ~ a/(1+(b * exp(-c*(wtAvgAnthDist-300)/100))) + d, data = anthBinData, start = list(a = 7, b = 10, c = 4, d = 3), weights = 1/((wtSeDistNN)^2))
AIC(logistic) # 68.2534
predData$logistic <- predict(logistic, newdata = predData)

logisticplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = "Logistic Model (AIC 68.25)") +
  theme_bw() +
  geom_line(data = predData, aes(y = logistic))

coef(logistic)






# segmented
# weights carry over
segmented <- segmented(linear, seg.Z = ~ wtAvgAnthDist, psi = 250)
AIC(segmented) # 67.81512
predData$segmented <- predict(segmented, newdata = predData)

segmentedplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = "Segmented Model (AIC 67.82)") +
  theme_bw() +
  geom_line(data = predData, aes(y = segmented))





plottitle <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Model Fit Distance from Nearest Neighbor", 
           hjust = 0.5, vjust = 0, size = 5)




allPlots <- ggarrange(nullplot, linearplot, powerabplot, powerabcplot, exponentialplot, logisticplot, segmentedplot, plottitle, ncol = 3, nrow = 3)

allPlots

ggexport(allPlots, filename = "distNNplots.pdf", height = 15, width = 15)

