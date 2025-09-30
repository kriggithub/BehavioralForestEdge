# 9/20/25
# Kurt Riggin
# Fitting models to binned anthropogenic edge data
library(ggplot2)
library(ggpubr)
library(segmented)
library(strucchange)
library(chngpt)
library(minpack.lm)



anthBinData <- read.csv("anthBinData.csv")

# create prediction dataframe

predData <- data.frame(
  wtAvgAnthDist = seq(min(anthBinData$wtAvgAnthDist, na.rm = T),
                      max(anthBinData$wtAvgAnthDist, na.rm = T),
                      length.out = 200)
)

########################################################################################
# Distance from NN
########################################################################################

# null
nullDistNN <- lm(data = anthBinData, formula = wtAvgDistNN ~ 1, weights = 1/((wtSeDistNN)^2))
nullDistNNAIC <- AIC(nullDistNN) # 71.2517
predData$nullDistNN <- predict(nullDistNN, newdata = predData)

nullDistNNplot <- ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Null Model (AIC = ", round(nullDistNNAIC, 2), ")")
       ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullDistNN))

# linear
linearDistNN <- lm(data = anthBinData, formula = wtAvgDistNN ~ wtAvgAnthDist, weights = 1/((wtSeDistNN)^2))
linearDistNNAIC <- AIC(linearDistNN) # 69.93471
predData$linearDistNN <- predict(linearDistNN, newdata = predData)

linearDistNNplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Linear Model (AIC = ", round(linearDistNNAIC, 2), ")")
       ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearDistNN))

# power (ab parameters)
# find starting values for a and b
lmDistNNStart <- lm(log(wtAvgDistNN) ~ log(wtAvgAnthDist), data = anthBinData, weights = 1/((wtSeDistNN)^2))
bDistNNStart <- coef(lmDistNNStart)[2]
aDistNNStart <- exp(coef(lmDistNNStart)[1])

powerabDistNN <- nls(wtAvgDistNN ~ a * wtAvgAnthDist^b, data = anthBinData, start = list(a = aDistNNStart, b = bDistNNStart), weights = 1/((wtSeDistNN)^2))
powerabDistNNAIC <- AIC(powerabDistNN) # 72.44315
predData$powerabDistNN <- predict(powerabDistNN, newdata = predData)


powerabDistNNplot <- ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Power (ab) Model (AIC = ", round(powerabDistNNAIC, 2), ")")
       ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabDistNN))


# power (abc parameters)
# guessed a,b,c by visual inspection and graphing on Desmos
powerabcDistNN <- nls(wtAvgDistNN ~ a * ((wtAvgAnthDist/400)^b) + c, data = anthBinData, start = list(a = 3, b = 5, c = 3.75), weights = 1/((wtSeDistNN)^2))
powerabcDistNNAIC <- AIC(powerabcDistNN) # 67.33387
predData$powerabcDistNN <- predict(powerabcDistNN, newdata = predData)

powerabcDistNNplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Power (abc) Model (AIC = ", round(powerabcDistNNAIC, 2), ")")
       ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcDistNN))

# exponential
exponentialDistNN <- nls(wtAvgDistNN ~ a * exp((wtAvgAnthDist/400)*b) + c, data = anthBinData, start = list(a = 0.5, b = 1, c = 3), weights = 1/((wtSeDistNN)^2))
exponentialDistNNAIC <- AIC(exponentialDistNN) # 67.52176
predData$exponentialDistNN <- predict(exponentialDistNN, newdata = predData)

exponentialDistNNplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Exponential Model (AIC = ", round(exponentialDistNNAIC, 2), ")")
       ) +
  theme_bw() +
  geom_line(data = predData, aes(y = exponentialDistNN))

# 3 parameter logistic
logisticDistNN <- nls(wtAvgDistNN ~ a/(1+(b * exp(-c*(wtAvgAnthDist-300)/100))) + d, data = anthBinData, 
                      start = list(a = 7, b = 10, c = 4, d = 3), weights = 1/((wtSeDistNN)^2))
logisticDistNNAIC <- AIC(logisticDistNN) # 68.2534
predData$logisticDistNN <- predict(logisticDistNN, newdata = predData)

logisticDistNNplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Logistic Model (AIC = ", round(logisticDistNNAIC, 2), ")")
       ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticDistNN))

# segmented
# weights carry over
segmentedDistNN <- segmented(linearDistNN, seg.Z = ~ wtAvgAnthDist, psi = 250)
segmentedDistNNAIC<- AIC(segmentedDistNN) # 67.81512
predData$segmentedDistNN <- predict(segmentedDistNN, newdata = predData)

segmentedDistNNplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Segmented Model (AIC = ", round(segmentedDistNNAIC, 2), ")")
       ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedDistNN))



# stepwise

# stepwiseDistNNweights <- 1 / (anthBinData$wtSeDistNN^2)
# stepwiseDistNNweights[!is.finite(stepwiseDistNNweights)] <- 0 

stepwiseDistNN <- chngptm(
  formula.1 = wtAvgDistNN ~ 1,
  formula.2 =  ~ wtAvgAnthDist,
  type = "step",
  family = "gaussian",
  data = anthBinData
)


stepwiseDistNNAIC <- AIC(stepwiseDistNN)
predData$stepwiseDistNN <- predict(stepwiseDistNN, newdata = predData)



stepwiseDistNNplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseDistNN))



# plot title
plottitleDistNN <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Model Fit Distance from Nearest Neighbor", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsDistNN <- ggarrange(nullDistNNplot, 
                            linearDistNNplot, 
                            powerabDistNNplot, 
                            powerabcDistNNplot, 
                            exponentialDistNNplot, 
                            logisticDistNNplot, segmentedDistNNplot, 
                            stepwiseDistNNplot,
                            plottitleDistNN, ncol = 3, nrow = 3)

# allPlotsDistNN

# ggexport(allPlotsDistNN, filename = "DistNNplots.pdf", height = 15, width = 15)

########################################################################################
# % Time Resting
########################################################################################

# null
nullRestPct <- lm(data = anthBinData, formula = wtAvgRestPct ~ 1, weights = 1/((wtSeRestPct)^2))
nullRestPctAIC <- AIC(nullRestPct) # 71.2517
predData$nullRestPct <- predict(nullRestPct, newdata = predData)

nullRestPctplot <- ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Null Model (AIC = ", round(nullRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullRestPct))

# linear
linearRestPct <- lm(data = anthBinData, formula = wtAvgRestPct ~ wtAvgAnthDist, weights = 1/((wtSeRestPct)^2))
linearRestPctAIC <- AIC(linearRestPct) # 69.93471
predData$linearRestPct <- predict(linearRestPct, newdata = predData)

linearRestPctplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Linear Model (AIC = ", round(linearRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearRestPct))

# power (ab parameters)
# find starting values for a and b
# lmRestPctStart <- lm(log(wtAvgRestPct) ~ log(wtAvgAnthDist), data = anthBinData, weights = 1/((wtSeRestPct)^2))
# bRestPctStart <- coef(lmRestPctStart)[2]
# aRestPctStart <- exp(coef(lmRestPctStart)[1])

powerabRestPct <- nlsLM(wtAvgRestPct ~ a * wtAvgAnthDist^b, data = anthBinData, start = list(a = 1, b = 1), weights = 1/((wtSeRestPct)^2))
powerabRestPctAIC <- AIC(powerabRestPct) # 72.44315
predData$powerabRestPct <- predict(powerabRestPct, newdata = predData)


powerabRestPctplot <- ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Power (ab) Model (AIC = ", round(powerabRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabRestPct))


# power (abc parameters)
# guessed a,b,c by visual inspection and graphing on Desmos
powerabcRestPct <- nls(wtAvgRestPct ~ a * ((wtAvgAnthDist/400)^b) + c, data = anthBinData, start = list(a = 3, b = 5, c = 3.75), weights = 1/((wtSeRestPct)^2))
powerabcRestPctAIC <- AIC(powerabcRestPct) # 67.33387
predData$powerabcRestPct <- predict(powerabcRestPct, newdata = predData)

powerabcRestPctplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Power (abc) Model (AIC = ", round(powerabcRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcRestPct))

# exponential
exponentialRestPct <- nlsLM(wtAvgRestPct ~ a * exp((wtAvgAnthDist/100)*b) + c, data = anthBinData, start = list(a = 1, b = 25, c = 75), weights = 1/((wtSeRestPct)^2))
exponentialRestPctAIC <- AIC(exponentialRestPct) # 67.52176
predData$exponentialRestPct <- predict(exponentialRestPct, newdata = predData)

exponentialRestPctplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Exponential Model (AIC = ", round(exponentialRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = exponentialRestPct))

# 3 parameter logistic
logisticRestPct <- nlsLM(wtAvgRestPct ~ a/(1+(b * exp(-c*(wtAvgAnthDist)/100))) + d, data = anthBinData, 
                         start = list(a = 10, b = 10, c = 10, d = 80), weights = 1/((wtSeRestPct)^2))
logisticRestPctAIC <- AIC(logisticRestPct) # 68.2534
predData$logisticRestPct <- predict(logisticRestPct, newdata = predData)

logisticRestPctplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Logistic Model (AIC = ", round(logisticRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticRestPct))

# segmented
# weights carry over
segmentedRestPct <- segmented(linearRestPct, seg.Z = ~ wtAvgAnthDist, psi = 250)
segmentedRestPctAIC<- AIC(segmentedRestPct) # 67.81512
predData$segmentedRestPct <- predict(segmentedRestPct, newdata = predData)

segmentedRestPctplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Segmented Model (AIC = ", round(segmentedRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedRestPct))



# stepwise

stepwiseRestPct <- chngptm(
  formula.1 = wtAvgRestPct ~ 1,
  formula.2 =  ~ wtAvgAnthDist,
  type = "step",
  family = "gaussian",
  data = anthBinData
)


stepwiseRestPctAIC <- AIC(stepwiseRestPct)
predData$stepwiseRestPct <- predict(stepwiseRestPct, newdata = predData)



stepwiseRestPctplot <-ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbor", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseRestPct))



# plot title
plottitleRestPct <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "Model Fit Percent Time Resting", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsRestPct <- ggarrange(nullRestPctplot, 
                            linearRestPctplot, 
                            powerabRestPctplot, 
                            powerabcRestPctplot, 
                            exponentialRestPctplot, 
                            logisticRestPctplot, 
                            segmentedRestPctplot, 
                            stepwiseRestPctplot,
                            plottitleRestPct, ncol = 3, nrow = 3)

# allPlotsRestPct

# ggexport(allPlotsRestPct, filename = "RestPctplots.pdf", height = 15, width = 15)


