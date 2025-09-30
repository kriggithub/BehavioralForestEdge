# 9/20/25
# Kurt Riggin
# Fitting models to binned anthropogenic edge data
library(ggplot2)
library(ggpubr)
library(segmented)
library(strucchange)
library(chngpt)
library(minpack.lm)


anthBinDataSub <- read.csv("anthBinData.csv")

# create prediction dataframe

predDataSub <- data.frame(
  wtAvgAnthDist = seq(min(anthBinDataSub$wtAvgAnthDist, na.rm = T),
                      max(anthBinDataSub$wtAvgAnthDist, na.rm = T),
                      length.out = 200)
)

########################################################################################
# Distance from NN
anthBinDataSubDistNN <- anthBinDataSub[!is.na(anthBinDataSub$wtAvgDistNN),]
########################################################################################

# null
nullDistNNSub <- lm(data = anthBinDataSubDistNN, formula = wtAvgDistNN ~ 1, weights = 1/((wtSeDistNN)^2))
nullDistNNSubAIC <- AIC(nullDistNNSub) # 71.2517
predDataSub$nullDistNNSub <- predict(nullDistNNSub, newdata = predDataSub)

nullDistNNSubplot <- ggplot(anthBinDataSubDistNN, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Null Model (AIC = ", round(nullDistNNSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = nullDistNNSub))

# linear
linearDistNNSub <- lm(data = anthBinDataSubDistNN, formula = wtAvgDistNN ~ wtAvgAnthDist, weights = 1/((wtSeDistNN)^2))
linearDistNNSubAIC <- AIC(linearDistNNSub) # 69.93471
predDataSub$linearDistNNSub <- predict(linearDistNNSub, newdata = predDataSub)

linearDistNNSubplot <-ggplot(anthBinDataSubDistNN, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Linear Model (AIC = ", round(linearDistNNSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = linearDistNNSub))

# power (ab parameters)
# find starting values for a and b
lmDistNNStart <- lm(log(wtAvgDistNN) ~ log(wtAvgAnthDist), data = anthBinDataSubDistNN, weights = 1/((wtSeDistNN)^2))
bDistNNStart <- coef(lmDistNNStart)[2]
aDistNNStart <- exp(coef(lmDistNNStart)[1])

powerabDistNNSub <- nls(wtAvgDistNN ~ a * wtAvgAnthDist^b, data = anthBinDataSubDistNN, start = list(a = aDistNNStart, b = bDistNNStart), weights = 1/((wtSeDistNN)^2))
powerabDistNNSubAIC <- AIC(powerabDistNNSub) # 72.44315
predDataSub$powerabDistNNSub <- predict(powerabDistNNSub, newdata = predDataSub)


powerabDistNNSubplot <- ggplot(anthBinDataSubDistNN, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Power (ab) Model (AIC = ", round(powerabDistNNSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = powerabDistNNSub))


# power (abc parameters)
# guessed a,b,c by visual inspection and graphing on Desmos
powerabcDistNNSub <- nls(wtAvgDistNN ~ a * ((wtAvgAnthDist/400)^b) + c, data = anthBinDataSubDistNN, start = list(a = 3, b = 5, c = 3.75), weights = 1/((wtSeDistNN)^2))
powerabcDistNNSubAIC <- AIC(powerabcDistNNSub) # 67.33387
predDataSub$powerabcDistNNSub <- predict(powerabcDistNNSub, newdata = predDataSub)

powerabcDistNNSubplot <-ggplot(anthBinDataSubDistNN, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Power (abc) Model (AIC = ", round(powerabcDistNNSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = powerabcDistNNSub))

# exponential
exponentialDistNNSub <- nls(wtAvgDistNN ~ a * exp((wtAvgAnthDist/400)*b) + c, data = anthBinDataSubDistNN, start = list(a = 0.5, b = 1, c = 3), weights = 1/((wtSeDistNN)^2))
exponentialDistNNSubAIC <- AIC(exponentialDistNNSub) # 67.52176
predDataSub$exponentialDistNNSub <- predict(exponentialDistNNSub, newdata = predDataSub)

exponentialDistNNSubplot <-ggplot(anthBinDataSubDistNN, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Exponential Model (AIC = ", round(exponentialDistNNSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = exponentialDistNNSub))

# 3 parameter logistic
logisticDistNNSub <- nls(wtAvgDistNN ~ a/(1+(b * exp(-c*(wtAvgAnthDist-300)/100))) + d, data = anthBinDataSubDistNN, start = list(a = 7, b = 10, c = 4, d = 3), weights = 1/((wtSeDistNN)^2))
logisticDistNNSubAIC <- AIC(logisticDistNNSub) # 68.2534
predDataSub$logisticDistNNSub <- predict(logisticDistNNSub, newdata = predDataSub)

logisticDistNNSubplot <-ggplot(anthBinDataSubDistNN, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Logistic Model (AIC = ", round(logisticDistNNSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = logisticDistNNSub))

# segmented
# weights carry over
segmentedDistNNSub <- segmented(linearDistNNSub, seg.Z = ~ wtAvgAnthDist, psi = 250)
segmentedDistNNSubAIC<- AIC(segmentedDistNNSub) # 67.81512
predDataSub$segmentedDistNNSub <- predict(segmentedDistNNSub, newdata = predDataSub)

segmentedDistNNSubplot <-ggplot(anthBinDataSubDistNN, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Segmented Model (AIC = ", round(segmentedDistNNSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = segmentedDistNNSub))



# stepwise

stepwiseDistNNSub <- chngptm(
  formula.1 = wtAvgDistNN ~ 1,
  formula.2 =  ~ wtAvgAnthDist,
  type = "step",
  family = "gaussian",
  data = anthBinDataSubDistNN,
  weights = 1/((anthBinDataSubDistNN$wtSeDistNN)^2)
)


stepwiseDistNNSubAIC <- AIC(stepwiseDistNNSub)
predDataSub$stepwiseDistNNSub <- predict(stepwiseDistNNSub, newdata = predDataSub)



stepwiseDistNNSubplot <-ggplot(anthBinDataSubDistNN, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseDistNNSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = stepwiseDistNNSub))



# plot title
plottitleDistNNSub <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Model Fit Distance from Nearest Neighbor", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsDistNNSub <- ggarrange(nullDistNNSubplot, 
                            linearDistNNSubplot, 
                            powerabDistNNSubplot, 
                            powerabcDistNNSubplot, 
                            exponentialDistNNSubplot, 
                            logisticDistNNSubplot, 
                            segmentedDistNNSubplot, 
                            stepwiseDistNNSubplot,
                            plottitleDistNNSub, ncol = 3, nrow = 3)

# allPlotsDistNNSub

# ggexport(allPlotsDistNNSub, filename = "DistNNPlotsSub.pdf", height = 15, width = 15)


########################################################################################
# % Time Resting
anthBinDataSubRest <- anthBinDataSub %>% 
  filter(!is.na(wtAvgRestPct),
         bin > 0)
########################################################################################

# null
nullRestSub <- lm(data = anthBinDataSubRest, formula = wtAvgRestPct ~ 1, weights = 1/((wtSeRestPct)^2))
nullRestSubAIC <- AIC(nullRestSub) # 71.2517
predDataSub$nullRestSub <- predict(nullRestSub, newdata = predDataSub)

nullRestSubplot <- ggplot(anthBinDataSubRest, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Null Model (AIC = ", round(nullRestSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = nullRestSub))

# linear
linearRestSub <- lm(data = anthBinDataSubRest, formula = wtAvgRestPct ~ wtAvgAnthDist, weights = 1/((wtSeRestPct)^2))
linearRestSubAIC <- AIC(linearRestSub) # 69.93471
predDataSub$linearRestSub <- predict(linearRestSub, newdata = predDataSub)

linearRestSubplot <-ggplot(anthBinDataSubRest, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Linear Model (AIC = ", round(linearRestSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = linearRestSub))

# power (ab parameters)
# find starting values for a and b
# lmRestStart <- lm(log(wtAvgRestPct) ~ log(wtAvgAnthDist), data = anthBinDataSubRest, weights = 1/((wtSeRestPct)^2))
# bRestStart <- coef(lmRestStart)[2]
# aRestStart <- exp(coef(lmRestStart)[1])
# 
powerabRestSub <- nls(wtAvgRestPct ~ a * wtAvgAnthDist^b, data = anthBinDataSubRest, start = list(a = 1, b = 1), weights = 1/((wtSeRestPct)^2))
powerabRestSubAIC <- AIC(powerabRestSub) # 72.44315
predDataSub$powerabRestSub <- predict(powerabRestSub, newdata = predDataSub)


powerabRestSubplot <- ggplot(anthBinDataSubRest, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() +
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)",
       y = "(Weighted) Mean Distance from Nearest Neighbor",
       title = paste0("Power (ab) Model (AIC = ", round(powerabRestSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = powerabRestSub))


# power (abc parameters)
# guessed a,b,c by visual inspection and graphing on Desmos
powerabcRestSub <- nls(wtAvgRestPct ~ a * ((wtAvgAnthDist/400)^b) + c, data = anthBinDataSubRest, 
                       start = list(a = 3, b = 5, c = 3.75), weights = 1/((wtSeRestPct)^2))
powerabcRestSubAIC <- AIC(powerabcRestSub) # 67.33387
predDataSub$powerabcRestSub <- predict(powerabcRestSub, newdata = predDataSub)

powerabcRestSubplot <-ggplot(anthBinDataSubRest, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Power (abc) Model (AIC = ", round(powerabcRestSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = powerabcRestSub))

# exponential
exponentialRestSub <- nlsLM(wtAvgRestPct ~ a * exp((wtAvgAnthDist/400)*b) + c, data = anthBinDataSubRest, 
                          start = list(a = -32, b = -2, c = 92), weights = 1/((wtSeRestPct)^2))
exponentialRestSubAIC <- AIC(exponentialRestSub) # 67.52176
predDataSub$exponentialRestSub <- predict(exponentialRestSub, newdata = predDataSub)

exponentialRestSubplot <-ggplot(anthBinDataSubRest, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Exponential Model (AIC = ", round(exponentialRestSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = exponentialRestSub))

# 3 parameter logistic
logisticRestSub <- nlsLM(wtAvgRestPct ~ a/(1+(b * exp(-c*(wtAvgAnthDist-300)/100))) + d, data = anthBinDataSubRest, 
                       start = list(a = 7, b = 10, c = 4, d = 3), weights = 1/((wtSeRestPct)^2))
logisticRestSubAIC <- AIC(logisticRestSub) # 68.2534
predDataSub$logisticRestSub <- predict(logisticRestSub, newdata = predDataSub)

logisticRestSubplot <-ggplot(anthBinDataSubRest, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Logistic Model (AIC = ", round(logisticRestSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = logisticRestSub))

# segmented
# weights carry over
segmentedRestSub <- segmented(linearRestSub, seg.Z = ~ wtAvgAnthDist, psi = 300)
segmentedRestSubAIC<- AIC(segmentedRestSub) # 67.81512
predDataSub$segmentedRestSub <- predict(segmentedRestSub, newdata = predDataSub)

segmentedRestSubplot <-ggplot(anthBinDataSubRest, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Segmented Model (AIC = ", round(segmentedRestSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = segmentedRestSub))



# stepwise

stepwiseRestSub <- chngptm(
  formula.1 = wtAvgRestPct ~ 1,
  formula.2 =  ~ wtAvgAnthDist,
  type = "step",
  family = "gaussian",
  data = anthBinDataSubRest,
  weights = 1/((anthBinDataSubRest$wtSeRestPct)^2)
)


stepwiseRestSubAIC <- AIC(stepwiseRestSub)
predDataSub$stepwiseRestSub <- predict(stepwiseRestSub, newdata = predDataSub)



stepwiseRestSubplot <-ggplot(anthBinDataSubRest, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseRestSubAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predDataSub, aes(y = stepwiseRestSub))



# plot title
plottitleRestSub <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Model Fit Percent Time Resting", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsRestSub <- ggarrange(nullRestSubplot, 
                                linearRestSubplot, 
                                powerabRestSubplot, 
                                powerabcRestSubplot, 
                                exponentialRestSubplot, 
                                logisticRestSubplot, 
                                segmentedRestSubplot, 
                                stepwiseRestSubplot,
                                plottitleRestSub, ncol = 3, nrow = 3)

# allPlotsRestSub

# ggexport(allPlotsRestSub, filename = "RestPctPlotsSub.pdf", height = 15, width = 15)



