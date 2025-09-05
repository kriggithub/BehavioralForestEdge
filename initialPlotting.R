library(ggplot2)


# monkeyIdData, AnthDist
ggplot(monkeyIdData, mapping = aes(x = AnthDist, y = RestPct,)) +
  geom_point()

ggplot(monkeyIdData, mapping = aes(x = AnthDist, y = MovingPct,)) +
  geom_point()

ggplot(monkeyIdData, mapping = aes(x = AnthDist, y = FeedingPct,)) +
  geom_point()

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
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSdRestPct, ymax = wtAvgRestPct + wtSdRestPct))

ggplot(anthBinData, aes(x = bin, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSdMovingPct, ymax = wtAvgMovingPct + wtSdMovingPct))

ggplot(anthBinData, aes(x = bin, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSdFeedingPct, ymax = wtAvgFeedingPct + wtSdFeedingPct))

ggplot(anthBinData, aes(x = bin, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSdNumNN, ymax = wtAvgNumNN + wtSdNumNN))

ggplot(anthBinData, aes(x = bin, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSdDistNN, ymax = wtAvgDistNN + wtSdDistNN))




# rivBinData


ggplot(rivBinData, aes(x = bin, y = wtAvgRestPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgRestPct - wtSdRestPct, ymax = wtAvgRestPct + wtSdRestPct))

ggplot(rivBinData, aes(x = bin, y = wtAvgMovingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgMovingPct - wtSdMovingPct, ymax = wtAvgMovingPct + wtSdMovingPct))

ggplot(rivBinData, aes(x = bin, y = wtAvgFeedingPct)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgFeedingPct - wtSdFeedingPct, ymax = wtAvgFeedingPct + wtSdFeedingPct))

ggplot(rivBinData, aes(x = bin, y = wtAvgNumNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgNumNN - wtSdNumNN, ymax = wtAvgNumNN + wtSdNumNN))

ggplot(rivBinData, aes(x = bin, y = wtAvgDistNN)) +
  geom_point() + 
  geom_errorbar(aes(ymin = wtAvgDistNN - wtSdDistNN, ymax = wtAvgDistNN + wtSdDistNN))







