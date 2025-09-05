#### 9/5/2025
#### Kurt Riggin
#### Data Subsetting Scripts

# read in packages
library(dplyr)


# read in working data
primateData <- read.csv("BehavioralData.csv", header = T)

# capitalize all IDs to standardize
primateData$ID <- toupper(primateData$ID)


### Data Frame 1: Summarized by ID (monkey)

monkeyIdData <- primateData %>% 
  group_by(ID) %>% 
  summarize(RestPct = mean(Activity == "R")*100,
            MovingPct = mean(Activity == "L")*100,
            FeedingPct = mean(Activity == "F")*100,
            AvgNumNN = mean(NumNN, na.rm = T),
            AvgDistNN = mean(DistNN, na.rm = T),
            RivDist = mean(RivDist),
            AnthDist = mean(AnthDist),
            nObs = n())

write.csv(monkeyIdData, file = "monkeyIdData.csv")



### Data Frame 2: Summarized within band (Anth)

anthBandData <- primateData %>% 
  mutate(bin = ceiling(AnthDist / 15))






save.image("DataCleaning.RData")
