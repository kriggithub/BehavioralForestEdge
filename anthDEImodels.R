# 10/20/25 
# Lowest AIC Models Selection (Anth)
library(tidyverse)
library(ggpubr)
library(segmented)
library(strucchange)
library(chngpt)
library(minpack.lm)
library(investr)

anthBinData <- read.csv("anthBinData.csv")

# create prediction dataframe

predData <- data.frame(
  wtAvgAnthDist = seq(min(anthBinData$wtAvgAnthDist, na.rm = T),
                      max(anthBinData$wtAvgAnthDist, na.rm = T),
                      length.out = 200)
)


# DistNN
anthBinDataDistNNSub <- anthBinData %>% 
  filter(!is.na(wtAvgDistNN))




# Power Model AIC 65.95
powerabcDistNN <- nlsLM(wtAvgDistNN ~ a * ((wtAvgAnthDist/400)^b) + c, data = anthBinDataDistNNSub, 
                        start = list(a = 0, b = 1, c = 3), weights = nMonkeys, control = nls.lm.control(maxiter = 1000))
powerabcDistNNAIC <- AIC(powerabcDistNN)

predData$powerabcDistNN <- predict(powerabcDistNN, newdata = predData)


powerabcDistNN$call$control$maxiter <- 1000
powerabcDistNN$call$start <- as.list(coef(powerabcDistNN))

# max observed x
xmax <- max(anthBinDataDistNNSub$wtAvgAnthDist, na.rm = TRUE)

# model-predicted y at that x
ymax <- predict(powerabcDistNN,
                         newdata = data.frame(wtAvgAnthDist = xmax))
ydei <- (2/3)*ymax

plotFit(powerabcDistNN)
abline(h = ydei, col = "red", lty = 2)
invest(powerabcDistNN, y0 = ydei, lower = 250, upper = 350, interval = "none")
powerinvest <- invest(powerabcDistNN,
                  seed = 123,
                  y0 = 5,
                  lower = 200, upper = 400,
                  interval = "percentile",
                  nsim = 10000,  
                  boot.type = "nonparametric", 
                  progress = TRUE)

powerPE <- as.numeric(powerinvest$estimate)
powerlower <- as.numeric(powerinvest$lower)
powerupper <- as.numeric(powerinvest$upper)

ciLabel <- paste0("95% CI = (", round(powerlower, 1), ", ", round(powerupper, 1), ")")
peLabel <- paste0("Point estimate = ", round(powerPI, 1))

ciBand <- data.frame(
  xmin = powerlower,
  xmax = powerupper,
  ymin = -Inf,
  ymax = Inf,
  type = ciLabel
)

pointLine <- data.frame(
  xintercept = powerPE,
  type = peLabel
)




powerabcDistNNplot <- ggplot(anthBinDataDistNNSub, aes(x = wtAvgAnthDist, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean Distance from Nearest Neighbors", 
       title = paste0("Power Model (AIC = ", round(powerabcDistNNAIC, 2), ")")
  ) +
  theme_bw() +
  geom_line(data = predData, aes(y = powerabcDistNN)) +
  geom_rect(data = ciBand,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type),
            alpha = 0.2, inherit.aes = FALSE) +
  geom_vline(data = pointLine,
             aes(xintercept = xintercept, color = type),
             linetype = "dashed", size = 0.8, inherit.aes = FALSE) +
  scale_fill_manual(values = setNames("red", ciLabel)) +
  scale_color_manual(values = setNames("red", peLabel)) +
  labs(fill = NULL, color = NULL) +
  theme(legend.position = "bottom")  # put legend under plot (optional)
  


powerabcDistNNplot


