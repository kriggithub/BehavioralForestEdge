#### 9/5/2025
#### Kurt Riggin
#### Data Subsetting Scripts

# read in packages
library(dplyr)
library(Hmisc)



# read in working data
primateData <- read.csv("BehavioralData.csv", header = T)

# capitalize all IDs to standardize
primateData$ID <- toupper(primateData$ID)


### Data Frame 1: Summarized by ID (monkey)

monkeyIdData <- primateData %>% 
  group_by(ID) %>% 
  dplyr::summarize(RestPct = mean(Activity == "R")*100,
            MovingPct = mean(Activity == "L")*100,
            FeedingPct = mean(Activity == "F")*100,
            OtherPct = mean(!(Activity %in% c("R", "L", "F")))*100,
            AvgNumNN = mean(NumNN, na.rm = T),
            AvgDistNN = mean(DistNN, na.rm = T),
            RivDist = mean(RivDist),
            AnthDist = mean(AnthDist),
            nObs = n())

# write.csv(monkeyIdData, file = "monkeyIdData.csv")



### Data Frame 2: Summarized within band (Anth)

anthBinData <- primateData %>% 
  mutate(bin = as.factor(ceiling(AnthDist / 15))) %>% 
  group_by(ID) %>% 
  mutate(RestPct = mean(Activity == "R")*100,
         MovingPct = mean(Activity == "L")*100,
         FeedingPct = mean(Activity == "F")*100,
         OtherPct = mean(!(Activity %in% c("R", "L", "F")))*100,
         AvgNumNN = mean(NumNN, na.rm = T),
         AvgDistNN = mean(DistNN, na.rm = T),
         RivDist = mean(RivDist),
         AnthDist = mean(AnthDist),
         nObs = n()) %>% 
  ungroup() %>% 
  group_by(bin) %>% 
  dplyr::summarize(wtAvgRestPct = weighted.mean(RestPct, nObs),
                   wtSdRestPct = sqrt(wtd.var(RestPct, nObs)),
                   wtAvgMovingPct = weighted.mean(MovingPct, nObs),
                   wtSdMovingPct = sqrt(wtd.var(MovingPct, nObs)),
                   wtAvgFeedingPct = weighted.mean(FeedingPct, nObs),
                   wtSdFeedingPct = sqrt(wtd.var(FeedingPct, nObs)),
                   wtAvgOtherPct = weighted.mean(OtherPct, nObs),
                   wtSdOtherPct = sqrt(wtd.var(OtherPct, nObs)),
                   wtAvgNumNN = weighted.mean(AvgNumNN, nObs, na.rm = T),
                   wtSdNumNN = sqrt(wtd.var(AvgNumNN, nObs, na.rm = T)),
                   wtAvgDistNN = weighted.mean(AvgDistNN, nObs, na.rm = T),
                   wtSdDistNN = sqrt(wtd.var(AvgDistNN, nObs, na.rm = T)),
                   wtAvgAnthDist = weighted.mean(AnthDist, nObs, na.rm = T),
                   wtSdAnthDist = sqrt(wtd.var(AnthDist, nObs, na.rm = T)))


# write.csv(anthBinData, file = "anthBinData.csv")



### Data Frame 3: Summarized within band (Riv)

rivBinData <- primateData %>% 
  mutate(bin = as.factor(ceiling(RivDist / 15))) %>% 
  group_by(ID) %>% 
  mutate(RestPct = mean(Activity == "R")*100,
         MovingPct = mean(Activity == "L")*100,
         FeedingPct = mean(Activity == "F")*100,
         OtherPct = mean(!(Activity %in% c("R", "L", "F")))*100,
         AvgNumNN = mean(NumNN, na.rm = T),
         AvgDistNN = mean(DistNN, na.rm = T),
         RivDist = mean(RivDist),
         AnthDist = mean(AnthDist),
         nObs = n()) %>% 
  ungroup() %>% 
  group_by(bin) %>% 
  dplyr::summarize(wtAvgRestPct = weighted.mean(RestPct, nObs),
                   wtSdRestPct = sqrt(wtd.var(RestPct, nObs)),
                   wtAvgMovingPct = weighted.mean(MovingPct, nObs),
                   wtSdMovingPct = sqrt(wtd.var(MovingPct, nObs)),
                   wtAvgFeedingPct = weighted.mean(FeedingPct, nObs),
                   wtSdFeedingPct = sqrt(wtd.var(FeedingPct, nObs)),
                   wtAvgOtherPct = weighted.mean(OtherPct, nObs),
                   wtSdOtherPct = sqrt(wtd.var(OtherPct, nObs)),
                   wtAvgNumNN = weighted.mean(AvgNumNN, nObs, na.rm = T),
                   wtSdNumNN = sqrt(wtd.var(AvgNumNN, nObs, na.rm = T)),
                   wtAvgDistNN = weighted.mean(AvgDistNN, nObs, na.rm = T),
                   wtSdDistNN = sqrt(wtd.var(AvgDistNN, nObs, na.rm = T)),
                   wtAvgAnthDist = weighted.mean(RivDist, nObs, na.rm = T),
                   wtSdAnthDist = sqrt(wtd.var(RivDist, nObs, na.rm = T)))


# write.csv(rivBinData, file = "rivBinData.csv")


save.image("DataCleaning.RData")
