library(ggplot2)


# monkeyIdData, AnthDist
ggplot(monkeyIdData, mapping = aes(x = AnthDist, y = RestPct,)) +
  geom_point()

ggplot(monkeyIdData, mapping = aes(x = AnthDist, y = MovingPct,)) +
  geom_point()


###########################################################
ggplot(monkeyIdData, mapping = aes(x = AnthDist, y = FeedingPct,)) +
  geom_point() +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "% Time Feeding", 
       subtitle = "% Time Feeding Per Monkey") +
  theme_bw()

#######################################################


ggplot(monkeyIdData, mapping = aes(x = AnthDist, y = AvgNumNN,)) +
  geom_point()

ggplot(monkeyIdData, mapping = aes(x = AnthDist, y = AvgDistNN,)) +
  geom_point()


# monkeyIdData, RivDist
ggplot(monkeyIdData, mapping = aes(x = RivDist, y = RestPct,)) +
  geom_point()

ggplot(monkeyIdData, mapping = aes(x = RivDist, y = MovingPct,)) +
  geom_point()

ggplot(monkeyIdData, mapping = aes(x = RivDist, y = FeedingPct,)) +
  geom_point()

ggplot(monkeyIdData, mapping = aes(x = RivDist, y = AvgNumNN,)) +
  geom_point()

ggplot(monkeyIdData, mapping = aes(x = RivDist, y = AvgDistNN,)) +
  geom_point()











# anthBinData

ggplot(anthBinData, aes(x = bin, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct))

ggplot(anthBinData, aes(x = bin, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct))

#####################

ggplot(anthBinData, aes(x = wtAvgAnthDist, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct)) +
  labs(x = "Distance from Anthropogenic Edge (m)", 
       y = "(Weighted) Mean % Time Spent Feeding", 
       subtitle = "Average % Time Spent Feeding per 15 meters from Anthropogenic Edge") +
  theme_bw()


#########################

ggplot(anthBinData, aes(x = bin, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN))

ggplot(anthBinData, aes(x = bin, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN))




# rivBinData


ggplot(rivBinData, aes(x = bin, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSeRestPct, ymax = wtAvgRestPct + wtSeRestPct))

ggplot(rivBinData, aes(x = bin, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSeMovingPct, ymax = wtAvgMovingPct + wtSeMovingPct))

ggplot(rivBinData, aes(x = bin, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSeFeedingPct, ymax = wtAvgFeedingPct + wtSeFeedingPct))

ggplot(rivBinData, aes(x = bin, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSeNumNN, ymax = wtAvgNumNN + wtSeNumNN))

ggplot(rivBinData, aes(x = bin, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSeDistNN, ymax = wtAvgDistNN + wtSeDistNN))







