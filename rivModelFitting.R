# 9/20/25
# Kurt Riggin
# Fitting models to binned river edge data
library(ggplot2)
library(ggpubr)
library(segmented)
library(strucchange)
library(chngpt)
library(minpack.lm)



rivBinData <- read.csv("rivBinData.csv")

# create prediction dataframe

predData <- data.frame(
  wtAvgRivDist = seq(min(rivBinData$wtAvgRivDist, na.rm = T),
                      max(rivBinData$wtAvgRivDist, na.rm = T),
                      length.out = 200)
)

########################################################################################
# % Time Resting
rivBinDataRestSub <- rivBinData %>% 
  filter(!is.na(wtAvgRestPct),
         wtSdRestPct > 0)
########################################################################################

# null
nullRestPct <- lm(data = rivBinDataRestSub, formula = wtAvgRestPct ~ 1, weights = 1/((wtSeRestPct)^2))
nullRestPctAIC <- AIC(nullRestPct)
predData$nullRestPct <- predict(nullRestPct, newdata = predData)

nullRestPctplot <- ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Null Model (AIC = ", round(nullRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullRestPct))

# linear
linearRestPct <- lm(data = rivBinDataRestSub, formula = wtAvgRestPct ~ wtAvgRivDist, weights = 1/((wtSeRestPct)^2))
linearRestPctAIC <- AIC(linearRestPct)
predData$linearRestPct <- predict(linearRestPct, newdata = predData)

linearRestPctplot <-ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Linear Model (AIC = ", round(linearRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearRestPct))

# power (ab parameters)
powerabRestPct <- nlsLM(wtAvgRestPct ~ a * wtAvgRivDist^b, data = rivBinDataRestSub, start = list(a = 1, b = 1), weights = 1/((wtSeRestPct)^2))
powerabRestPctAIC <- AIC(powerabRestPct)
predData$powerabRestPct <- predict(powerabRestPct, newdata = predData)


powerabRestPctplot <- ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Power (ab) Model (AIC = ", round(powerabRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabRestPct))


# power (abc parameters)
powerabcRestPct <- nls(wtAvgRestPct ~ a * ((wtAvgRivDist/400)^b) + c, data = rivBinDataRestSub, start = list(a = 3, b = 5, c = 3.75), weights = 1/((wtSeRestPct)^2))
powerabcRestPctAIC <- AIC(powerabcRestPct)
predData$powerabcRestPct <- predict(powerabcRestPct, newdata = predData)

powerabcRestPctplot <-ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Power (abc) Model (AIC = ", round(powerabcRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcRestPct))

# exponential
exponentialRestPct <- nlsLM(wtAvgRestPct ~ a * exp((wtAvgRivDist/100)*b) + c, data = rivBinDataRestSub, start = list(a = 1, b = 25, c = 75), weights = 1/((wtSeRestPct)^2))
exponentialRestPctAIC <- AIC(exponentialRestPct)
predData$exponentialRestPct <- predict(exponentialRestPct, newdata = predData)

exponentialRestPctplot <-ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Exponential Model (AIC = ", round(exponentialRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = exponentialRestPct))

# 3 parameter logistic
logisticRestPct <- nlsLM(wtAvgRestPct ~ a/(1+(b * exp(-c*(wtAvgRivDist)/100))) + d, data = rivBinDataRestSub, 
                         start = list(a = 0.1, b = -1.2, c = 0.6, d = 82), weights = 1/((wtSeRestPct)^2))
logisticRestPctAIC <- AIC(logisticRestPct)
predData$logisticRestPct <- predict(logisticRestPct, newdata = predData)

logisticRestPctplot <-ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Logistic Model (AIC = ", round(logisticRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticRestPct))

# segmented
segmentedRestPct <- segmented(linearRestPct, seg.Z = ~ wtAvgRivDist, psi = 250)
segmentedRestPctAIC<- AIC(segmentedRestPct)
predData$segmentedRestPct <- predict(segmentedRestPct, newdata = predData)

segmentedRestPctplot <-ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Segmented Model (AIC = ", round(segmentedRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedRestPct))



# stepwise
stepwiseRestPct <- chngptm(
  formula.1 = wtAvgRestPct ~ 1,
  formula.2 =  ~ wtAvgRivDist,
  type = "step",
  family = "gaussian",
  data = rivBinDataRestSub
)


stepwiseRestPctAIC <- AIC(stepwiseRestPct)
predData$stepwiseRestPct <- predict(stepwiseRestPct, newdata = predData)



stepwiseRestPctplot <-ggplot(rivBinDataRestSub, aes(x = wtAvgRivDist, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Resting", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseRestPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseRestPct))



# plot title
plottitleRestPct <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "River Model Fit Percent Time Resting", 
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

# ggexport(allPlotsRestPct, filename = "rivRestPctModels.pdf", height = 15, width = 15)

########################################################################################
# % Time Moving
rivBindDataMovingSub <- rivBinData %>% 
  filter(!is.na(wtAvgMovingPct),
         wtSdMovingPct > 0)
########################################################################################

# null
nullMovingPct <- lm(data = rivBindDataMovingSub, formula = wtAvgMovingPct ~ 1, weights = 1/((wtSeMovingPct)^2))
nullMovingPctAIC <- AIC(nullMovingPct) 
predData$nullMovingPct <- predict(nullMovingPct, newdata = predData)

nullMovingPctplot <- ggplot(rivBindDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Null Model (AIC = ", round(nullMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullMovingPct))

# linear
linearMovingPct <- lm(data = rivBindDataMovingSub, formula = wtAvgMovingPct ~ wtAvgRivDist, weights = 1/((wtSeMovingPct)^2))
linearMovingPctAIC <- AIC(linearMovingPct) 
predData$linearMovingPct <- predict(linearMovingPct, newdata = predData)

linearMovingPctplot <-ggplot(rivBindDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Linear Model (AIC = ", round(linearMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearMovingPct))

# power (ab parameters)

# lmMovingPctStart <- lm(log(wtAvgMovingPct) ~ log(wtAvgRivDist), data = rivBindDataMovingSub, weights = 1/((wtSeMovingPct)^2))
# bMovingPctStart <- coef(lmMovingPctStart)[2]
# aMovingPctStart <- exp(coef(lmMovingPctStart)[1])

powerabMovingPct <- nlsLM(wtAvgMovingPct ~ a * wtAvgRivDist^b, data = rivBindDataMovingSub, start = list(a = 1, b = 1), weights = 1/((wtSeMovingPct)^2))
powerabMovingPctAIC <- AIC(powerabMovingPct) 
predData$powerabMovingPct <- predict(powerabMovingPct, newdata = predData)


powerabMovingPctplot <- ggplot(rivBindDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Power (ab) Model (AIC = ", round(powerabMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabMovingPct))


# power (abc parameters)

powerabcMovingPct <- nls(wtAvgMovingPct ~ a * ((wtAvgRivDist/400)^b) + c, data = rivBindDataMovingSub, start = list(a = 3, b = 5, c = 3.75), weights = 1/((wtSeMovingPct)^2))
powerabcMovingPctAIC <- AIC(powerabcMovingPct) 
predData$powerabcMovingPct <- predict(powerabcMovingPct, newdata = predData)

powerabcMovingPctplot <-ggplot(rivBindDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Power (abc) Model (AIC = ", round(powerabcMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcMovingPct))

# exponential
exponentialMovingPct <- nlsLM(wtAvgMovingPct ~ a * exp((wtAvgRivDist/100)*b) + c, data = rivBindDataMovingSub, start = list(a = 1, b = 25, c = 75), weights = 1/((wtSeMovingPct)^2))
exponentialMovingPctAIC <- AIC(exponentialMovingPct) 
predData$exponentialMovingPct <- predict(exponentialMovingPct, newdata = predData)

exponentialMovingPctplot <-ggplot(rivBindDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Exponential Model (AIC = ", round(exponentialMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = exponentialMovingPct))

# 3 parameter logistic (MAX ITERATIONS)
logisticMovingPct <- nlsLM(wtAvgMovingPct ~ a/(1+(b * exp(-c*(wtAvgRivDist)/100))) + d, data = rivBindDataMovingSub, 
                         start = list(a = -1.3, b = -2864, c = 30, d = 7.94), weights = 1/((wtSeMovingPct)^2))
logisticMovingPctAIC <- AIC(logisticMovingPct) #154
predData$logisticMovingPct <- predict(logisticMovingPct, newdata = predData)

logisticMovingPctplot <-ggplot(rivBindDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Logistic Model (AIC = ", round(logisticMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticMovingPct))

# segmented
# weights carry over
segmentedMovingPct <- segmented(linearMovingPct, seg.Z = ~ wtAvgRivDist, psi = 250)
segmentedMovingPctAIC<- AIC(segmentedMovingPct) 
predData$segmentedMovingPct <- predict(segmentedMovingPct, newdata = predData)

segmentedMovingPctplot <-ggplot(rivBindDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Segmented Model (AIC = ", round(segmentedMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedMovingPct))



# stepwise

stepwiseMovingPct <- chngptm(
  formula.1 = wtAvgMovingPct ~ 1,
  formula.2 =  ~ wtAvgRivDist,
  type = "step",
  family = "gaussian",
  data = rivBindDataMovingSub
)


stepwiseMovingPctAIC <- AIC(stepwiseMovingPct)
predData$stepwiseMovingPct <- predict(stepwiseMovingPct, newdata = predData)



stepwiseMovingPctplot <-ggplot(rivBindDataMovingSub, aes(x = wtAvgRivDist, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Moving", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseMovingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseMovingPct))



# plot title
plottitleMovingPct <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "River Model Fit Percent Time Moving", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsMovingPct <- ggarrange(nullMovingPctplot, 
                             linearMovingPctplot, 
                             powerabMovingPctplot, 
                             powerabcMovingPctplot, 
                             exponentialMovingPctplot, 
                             logisticMovingPctplot, 
                             segmentedMovingPctplot, 
                             stepwiseMovingPctplot,
                             plottitleMovingPct, ncol = 3, nrow = 3)

# allPlotsMovingPct

# ggexport(allPlotsMovingPct, filename = "rivMovingPctModels.pdf", height = 15, width = 15)


########################################################################################
# % Time Feeding
rivBindDataFeedingSub <- rivBinData %>% 
  filter(!is.na(wtAvgFeedingPct),
         wtSdFeedingPct > 0)
########################################################################################

# null
nullFeedingPct <- lm(data = rivBindDataFeedingSub, formula = wtAvgFeedingPct ~ 1, weights = 1/((wtSeFeedingPct)^2))
nullFeedingPctAIC <- AIC(nullFeedingPct) 
predData$nullFeedingPct <- predict(nullFeedingPct, newdata = predData)

nullFeedingPctplot <- ggplot(rivBindDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Null Model (AIC = ", round(nullFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullFeedingPct))

# linear
linearFeedingPct <- lm(data = rivBindDataFeedingSub, formula = wtAvgFeedingPct ~ wtAvgRivDist, weights = 1/((wtSeFeedingPct)^2))
linearFeedingPctAIC <- AIC(linearFeedingPct) 
predData$linearFeedingPct <- predict(linearFeedingPct, newdata = predData)

linearFeedingPctplot <-ggplot(rivBindDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Linear Model (AIC = ", round(linearFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearFeedingPct))

# power (ab parameters)

# lmFeedingPctStart <- lm(log(wtAvgFeedingPct) ~ log(wtAvgRivDist), data = rivBindDataFeedingSub, weights = 1/((wtSeFeedingPct)^2))
# bFeedingPctStart <- coef(lmFeedingPctStart)[2]
# aFeedingPctStart <- exp(coef(lmFeedingPctStart)[1])

powerabFeedingPct <- nlsLM(wtAvgFeedingPct ~ a * wtAvgRivDist^b, data = rivBindDataFeedingSub, start = list(a = 1, b = 1), weights = 1/((wtSeFeedingPct)^2))
powerabFeedingPctAIC <- AIC(powerabFeedingPct) 
predData$powerabFeedingPct <- predict(powerabFeedingPct, newdata = predData)


powerabFeedingPctplot <- ggplot(rivBindDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Power (ab) Model (AIC = ", round(powerabFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabFeedingPct))


# power (abc parameters) (MAX ITERATIONS)

powerabcFeedingPct <- nlsLM(wtAvgFeedingPct ~ a * ((wtAvgRivDist/400)^b) + c, data = rivBindDataFeedingSub, 
                            start = list(a = 0, b = 0, c = 10), weights = 1/((wtSeFeedingPct)^2))
powerabcFeedingPctAIC <- AIC(powerabcFeedingPct) 
predData$powerabcFeedingPct <- predict(powerabcFeedingPct, newdata = predData)

powerabcFeedingPctplot <-ggplot(rivBindDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Power (abc) Model (AIC = ", round(powerabcFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcFeedingPct))

# exponential
exponentialFeedingPct <- nlsLM(wtAvgFeedingPct ~ a * exp((wtAvgRivDist/100)*b) + c, data = rivBindDataFeedingSub, start = list(a = 1, b = 25, c = 75), weights = 1/((wtSeFeedingPct)^2))
exponentialFeedingPctAIC <- AIC(exponentialFeedingPct) 
predData$exponentialFeedingPct <- predict(exponentialFeedingPct, newdata = predData)

exponentialFeedingPctplot <-ggplot(rivBindDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Exponential Model (AIC = ", round(exponentialFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = exponentialFeedingPct))

# 3 parameter logistic
logisticFeedingPct <- nlsLM(wtAvgFeedingPct ~ a/(1+(b * exp(-c*(wtAvgRivDist)/100))) + d, data = rivBindDataFeedingSub, 
                           start = list(a = 10, b = 10, c = 10, d = 10), weights = 1/((wtSeFeedingPct)^2))
logisticFeedingPctAIC <- AIC(logisticFeedingPct) 
predData$logisticFeedingPct <- predict(logisticFeedingPct, newdata = predData)

logisticFeedingPctplot <-ggplot(rivBindDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Logistic Model (AIC = ", round(logisticFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticFeedingPct))

# segmented
# weights carry over
segmentedFeedingPct <- segmented(linearFeedingPct, seg.Z = ~ wtAvgRivDist, psi = 250)
segmentedFeedingPctAIC<- AIC(segmentedFeedingPct) 
predData$segmentedFeedingPct <- predict(segmentedFeedingPct, newdata = predData)

segmentedFeedingPctplot <-ggplot(rivBindDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Segmented Model (AIC = ", round(segmentedFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedFeedingPct))



# stepwise

stepwiseFeedingPct <- chngptm(
  formula.1 = wtAvgFeedingPct ~ 1,
  formula.2 =  ~ wtAvgRivDist,
  type = "step",
  family = "gaussian",
  data = rivBindDataFeedingSub
)


stepwiseFeedingPctAIC <- AIC(stepwiseFeedingPct)
predData$stepwiseFeedingPct <- predict(stepwiseFeedingPct, newdata = predData)



stepwiseFeedingPctplot <-ggplot(rivBindDataFeedingSub, aes(x = wtAvgRivDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseFeedingPctAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseFeedingPct))



# plot title
plottitleFeedingPct <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "River Model Fit Percent Time Feeding", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsFeedingPct <- ggarrange(nullFeedingPctplot, 
                               linearFeedingPctplot, 
                               powerabFeedingPctplot, 
                               powerabcFeedingPctplot, 
                               exponentialFeedingPctplot, 
                               logisticFeedingPctplot, 
                               segmentedFeedingPctplot, 
                               stepwiseFeedingPctplot,
                               plottitleFeedingPct, ncol = 3, nrow = 3)

# allPlotsFeedingPct

# ggexport(allPlotsFeedingPct, filename = "rivFeedingPctModels.pdf", height = 15, width = 15)

########################################################################################
# Number of Nearest Neighbors
rivBindDataNumNNSub <- rivBinData %>% 
  filter(!is.na(wtAvgNumNN),
         wtSdNumNN > 0)
########################################################################################

# null
nullNumNN <- lm(data = rivBindDataNumNNSub, formula = wtAvgNumNN ~ 1, weights = 1/((wtSeNumNN)^2))
nullNumNNAIC <- AIC(nullNumNN) 
predData$nullNumNN <- predict(nullNumNN, newdata = predData)

nullNumNNplot <- ggplot(rivBindDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Null Model (AIC = ", round(nullNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullNumNN))

# linear
linearNumNN <- lm(data = rivBindDataNumNNSub, formula = wtAvgNumNN ~ wtAvgRivDist, weights = 1/((wtSeNumNN)^2))
linearNumNNAIC <- AIC(linearNumNN) 
predData$linearNumNN <- predict(linearNumNN, newdata = predData)

linearNumNNplot <-ggplot(rivBindDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Linear Model (AIC = ", round(linearNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearNumNN))

# power (ab parameters)
powerabNumNN <- nlsLM(wtAvgNumNN ~ a * wtAvgRivDist^b, data = rivBindDataNumNNSub, start = list(a = 1, b = 1), weights = 1/((wtSeNumNN)^2))
powerabNumNNAIC <- AIC(powerabNumNN) 
predData$powerabNumNN <- predict(powerabNumNN, newdata = predData)


powerabNumNNplot <- ggplot(rivBindDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Power (ab) Model (AIC = ", round(powerabNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabNumNN))


# power (abc parameters) (MAX ITERATIONS)

powerabcNumNN <- nlsLM(wtAvgNumNN ~ a * ((wtAvgRivDist/400)^b) + c, data = rivBindDataNumNNSub, 
                            start = list(a = 0, b = 0, c = 10), weights = 1/((wtSeNumNN)^2))
powerabcNumNNAIC <- AIC(powerabcNumNN) 
predData$powerabcNumNN <- predict(powerabcNumNN, newdata = predData)

powerabcNumNNplot <-ggplot(rivBindDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Power (abc) Model (AIC = ", round(powerabcNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcNumNN))

# exponential
exponentialNumNN <- nlsLM(wtAvgNumNN ~ a * exp((wtAvgRivDist/100)*b) + c, data = rivBindDataNumNNSub, start = list(a = 1, b = 25, c = 75), weights = 1/((wtSeNumNN)^2))
exponentialNumNNAIC <- AIC(exponentialNumNN) 
predData$exponentialNumNN <- predict(exponentialNumNN, newdata = predData)

exponentialNumNNplot <-ggplot(rivBindDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Exponential Model (AIC = ", round(exponentialNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = exponentialNumNN))

# 3 parameter logistic
logisticNumNN <- nlsLM(wtAvgNumNN ~ a/(1+(b * exp(-c*(wtAvgRivDist)/100))) + d, data = rivBindDataNumNNSub, 
                            start = list(a = 10, b = 10, c = 10, d = 10), weights = 1/((wtSeNumNN)^2))
logisticNumNNAIC <- AIC(logisticNumNN) 
predData$logisticNumNN <- predict(logisticNumNN, newdata = predData)

logisticNumNNplot <-ggplot(rivBindDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Logistic Model (AIC = ", round(logisticNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticNumNN))

# segmented
# weights carry over
segmentedNumNN <- segmented(linearNumNN, seg.Z = ~ wtAvgRivDist, psi = 250)
segmentedNumNNAIC<- AIC(segmentedNumNN) 
predData$segmentedNumNN <- predict(segmentedNumNN, newdata = predData)

segmentedNumNNplot <-ggplot(rivBindDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Segmented Model (AIC = ", round(segmentedNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedNumNN))



# stepwise

stepwiseNumNN <- chngptm(
  formula.1 = wtAvgNumNN ~ 1,
  formula.2 =  ~ wtAvgRivDist,
  type = "step",
  family = "gaussian",
  data = rivBindDataNumNNSub
)


stepwiseNumNNAIC <- AIC(stepwiseNumNN)
predData$stepwiseNumNN <- predict(stepwiseNumNN, newdata = predData)



stepwiseNumNNplot <-ggplot(rivBindDataNumNNSub, aes(x = wtAvgRivDist, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean # of Nearest Neighbors", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseNumNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseNumNN))



# plot title
plottitleNumNN <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "River Model Fit # of Nearest Neighbors", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsNumNN <- ggarrange(nullNumNNplot, 
                                linearNumNNplot, 
                                powerabNumNNplot, 
                                powerabcNumNNplot, 
                                exponentialNumNNplot, 
                                logisticNumNNplot, 
                                segmentedNumNNplot, 
                                stepwiseNumNNplot,
                                plottitleNumNN, ncol = 3, nrow = 3)

# allPlotsNumNN

# ggexport(allPlotsNumNN, filename = "rivNumNNModels.pdf", height = 15, width = 15)

########################################################################################
# Distance Nearest Neighbors
rivBindDataDistNNSub <- rivBinData %>% 
  filter(!is.na(wtAvgDistNN),
         wtSdDistNN > 0)
########################################################################################

# null
nullDistNN <- lm(data = rivBindDataDistNNSub, formula = wtAvgDistNN ~ 1, weights = 1/((wtSeDistNN)^2))
nullDistNNAIC <- AIC(nullDistNN) 
predData$nullDistNN <- predict(nullDistNN, newdata = predData)

nullDistNNplot <- ggplot(rivBindDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Distance From Nearest Neighbors", 
       title = paste0("Null Model (AIC = ", round(nullDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = nullDistNN))

# linear
linearDistNN <- lm(data = rivBindDataDistNNSub, formula = wtAvgDistNN ~ wtAvgRivDist, weights = 1/((wtSeDistNN)^2))
linearDistNNAIC <- AIC(linearDistNN) 
predData$linearDistNN <- predict(linearDistNN, newdata = predData)

linearDistNNplot <-ggplot(rivBindDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Distance From Nearest Neighbors", 
       title = paste0("Linear Model (AIC = ", round(linearDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = linearDistNN))

# power (ab parameters)

# lmDistNNStart <- lm(log(wtAvgDistNN) ~ log(wtAvgRivDist), data = rivBindDataDistNNSub, weights = 1/((wtSeDistNN)^2))
# bDistNNStart <- coef(lmDistNNStart)[2]
# aDistNNStart <- exp(coef(lmDistNNStart)[1])

powerabDistNN <- nlsLM(wtAvgDistNN ~ a * wtAvgRivDist^b, data = rivBindDataDistNNSub, start = list(a = 1, b = 1), weights = 1/((wtSeDistNN)^2))
powerabDistNNAIC <- AIC(powerabDistNN) 
predData$powerabDistNN <- predict(powerabDistNN, newdata = predData)


powerabDistNNplot <- ggplot(rivBindDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Distance From Nearest Neighbors", 
       title = paste0("Power (ab) Model (AIC = ", round(powerabDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabDistNN))


# power (abc parameters) 
powerabcDistNN <- nlsLM(wtAvgDistNN ~ a * ((wtAvgRivDist/400)^b) + c, data = rivBindDataDistNNSub, 
                       start = list(a = 0, b = 400, c = 2), weights = 1/((wtSeDistNN)^2))
powerabcDistNNAIC <- AIC(powerabcDistNN) 
predData$powerabcDistNN <- predict(powerabcDistNN, newdata = predData)

powerabcDistNNplot <-ggplot(rivBindDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Distance From Nearest Neighbors", 
       title = paste0("Power (abc) Model (AIC = ", round(powerabcDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcDistNN))

# exponential
exponentialDistNN <- nlsLM(wtAvgDistNN ~ a * exp((wtAvgRivDist/100)*b) + c, data = rivBindDataDistNNSub, start = list(a = 1, b = 25, c = 75), weights = 1/((wtSeDistNN)^2))
exponentialDistNNAIC <- AIC(exponentialDistNN) 
predData$exponentialDistNN <- predict(exponentialDistNN, newdata = predData)

exponentialDistNNplot <-ggplot(rivBindDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Distance From Nearest Neighbors", 
       title = paste0("Exponential Model (AIC = ", round(exponentialDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = exponentialDistNN))

# 3 parameter logistic
logisticDistNN <- nlsLM(wtAvgDistNN ~ a/(1+(b * exp(-c*(wtAvgRivDist)/100))) + d, data = rivBindDataDistNNSub, 
                       start = list(a = 10, b = 10, c = 10, d = 10), weights = 1/((wtSeDistNN)^2))
logisticDistNNAIC <- AIC(logisticDistNN) 
predData$logisticDistNN <- predict(logisticDistNN, newdata = predData)

logisticDistNNplot <-ggplot(rivBindDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Distance From Nearest Neighbors", 
       title = paste0("Logistic Model (AIC = ", round(logisticDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = logisticDistNN))

# segmented
# weights carry over
segmentedDistNN <- segmented(linearDistNN, seg.Z = ~ wtAvgRivDist, psi = 250)
segmentedDistNNAIC<- AIC(segmentedDistNN) 
predData$segmentedDistNN <- predict(segmentedDistNN, newdata = predData)

segmentedDistNNplot <-ggplot(rivBindDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Distance From Nearest Neighbors", 
       title = paste0("Segmented Model (AIC = ", round(segmentedDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = segmentedDistNN))



# stepwise

stepwiseDistNN <- chngptm(
  formula.1 = wtAvgDistNN ~ 1,
  formula.2 =  ~ wtAvgRivDist,
  type = "step",
  family = "gaussian",
  data = rivBindDataDistNNSub
)


stepwiseDistNNAIC <- AIC(stepwiseDistNN)
predData$stepwiseDistNN <- predict(stepwiseDistNN, newdata = predData)



stepwiseDistNNplot <-ggplot(rivBindDataDistNNSub, aes(x = wtAvgRivDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from River Edge (m)", 
       y = "(Weighted) Mean Distance From Nearest Neighbors", 
       title = paste0("Stepwise Model (AIC = ", round(stepwiseDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = stepwiseDistNN))



# plot title
plottitleDistNN <- ggplot() +
  theme_void() +
  annotate("text",
           x = 0, y = 0,
           label = "River Model Fit Distance from Nearest Neighbors", 
           hjust = 0.5, vjust = 0, size = 5)




allPlotsDistNN <- ggarrange(nullDistNNplot, 
                           linearDistNNplot, 
                           powerabDistNNplot, 
                           powerabcDistNNplot, 
                           exponentialDistNNplot, 
                           logisticDistNNplot, 
                           segmentedDistNNplot, 
                           stepwiseDistNNplot,
                           plottitleDistNN, ncol = 3, nrow = 3)

# allPlotsDistNN

# ggexport(allPlotsDistNN, filename = "rivDistNNModels.pdf", height = 15, width = 15)

