##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## Statistical analysis - LMMs
## Authors: Ines Fürtbauer, Marina Papadopoulou (m.papadopoulou.rug@gmail.com)
## Publication: Linking energy availability, movement, and sociality in a wild primate (Papio ursinus)
##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

## Load data & Packages
Data <- read.csv('data/data.csv')
library(dplyr)
#library(lmerTest)

##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## 1. Does energetic condition (fT3) predict movement (Distance,RT,Sinuosity,SL); males and females ##
SubData1 <- subset(Data, !is.na(fT3) & !is.na(RT))

## Within subject centering ##
SubData1 <- SubData1 %>% 
  group_by(BaboonID) %>% 
  mutate(fT3_M = mean(fT3),
         fT3_E = fT3 - fT3_M) %>% 
  ungroup()

## Transformations ##
SubData1$ZDaylength <- as.vector(scale(SubData1$Daylength))
SubData1$logRT <- log(SubData1$RT)
SubData1$logSin <- log(SubData1$Sinuosity)
SubData1$logSL <- log(SubData1$SL)
SubData1$ZfT3_E <- as.vector(scale(SubData1$fT3_E))
SubData1$ZfT3_M <- as.vector(scale(SubData1$fT3_M))

##  LMM1 - Distance ##
model.Dist <- lmerTest::lmer(Distance ~ ZfT3_E + ZfT3_M + Repstate + (1 + ZfT3_E | BaboonID) + (1 | Date), data = SubData1, REML = FALSE)
summary(model.Dist)

## LMM2 - Step length  ##
model.SL <-lmerTest::lmer(logSL ~ ZfT3_E + ZfT3_M + Repstate + (1 + ZfT3_E | BaboonID) + (1 | Date), data = SubData1, REML = FALSE) 
summary(model.SL)

## LMM3 - Sinuosity  ##
model.Sin <-lmerTest::lmer(logSin ~ ZfT3_E + ZfT3_M + Repstate + (1 + ZfT3_E | BaboonID) + (1 | Date), data = SubData1, REML = FALSE) 
summary(model.Sin)

## LMM4 - Residence time ##
model.RT <-lmerTest::lmer(logRT ~ ZfT3_E + ZfT3_M + Repstate+ (1 + ZfT3_E | BaboonID) + (1 | Date), data = SubData1, REML = FALSE) 
summary(model.RT) 


##########################################################
## -------------------------------------------------------
##2. Does movement predict social opportunities?##
## Carry over variable predicted by fT3, i.e. Residence Time ##
## only use data for females and when 10 or more collars are active ##
SubData2 <- subset(Data, Sex == "F" & Ncollars > 9 & !is.na(RT))

## Transformations##
SubData2$ZlogRT <- as.vector(scale(log(SubData2$RT)))
SubData2$ZDaylength <- as.vector(scale(SubData2$Daylength))
SubData2$ZRank <- as.vector(scale(SubData2$Rank))
SubData2$sqrtsocopp <- sqrt(SubData2$socopp)
SubData2$ZNcollars <- as.vector(SubData2$Ncollars)

## LMM5 - Social Opportunities ##
socopp <- lmerTest::lmer(sqrtsocopp ~ ZlogRT + ZRank + ZDaylength + Repstate + ZNcollars + (1 + ZlogRT | BaboonID) + (1 | Date), data = SubData2, REML = FALSE) 
summary(socopp)

##########################################################
##3.Do social opportunities predict grooming?##
SubData3 <- subset(Data, Sex == "F" & !is.na(socopp))

## Transformations ##
RGmin <- SubData3$RG/60 ## seconds to minutes
SubData3$Zsocopp <- as.vector(scale(sqrt(SubData3$socopp)))
SubData3$ZDaylength <- as.vector(scale(SubData3$Daylength))
SubData3$ZRank <- as.vector(scale(SubData3$Rank))
SubData3$sqrtRG <- sqrt(RGmin)

## LMM6 - Grooming received  ##
RG <- lmerTest::lmer(sqrtRG ~ Zsocopp + ZRank + ZDaylength + Repstate + (1 + Zsocopp | BaboonID) + (1 | Date), data = SubData3) 
summary(RG)

## Include interaction between social opportunities and fT3 ##
SubData4 <- subset(Data, Sex == "F" & !is.na(socopp) & !is.na(fT3))

## Transformations ##
GGmin <- SubData4$GG/60
SubData4$Zsocopp <- as.vector(scale(sqrt(SubData4$socopp)))
SubData4$ZDaylength <- as.vector(scale(SubData4$Daylength))
SubData4$ZRank <- as.vector(scale(SubData4$Rank))
SubData4$ZfT3 <- as.vector(scale(SubData4$fT3))
SubData4$sqrtGG <- sqrt(GGmin)

## LMM7 - Grooming given ##

GG <- lmerTest::lmer (sqrtGG ~ Zsocopp * ZfT3 + ZRank + ZDaylength + Repstate + (1 | BaboonID) + (1 | Date), data = SubData4)
summary(GG)

## Supplementary analysis ##
## LMMS1 ##
SubData1$ZlogSin <- as.vector(scale(log(SubData1$Sinuosity)))
SubData1$ZlogSL <- as.vector(scale(log(SubData1$SL)))
LMMS1 <- lmerTest::lmer(logRT ~ ZlogSin + ZlogSL + Repstate + (1 + ZlogSin | BaboonID) + (1 + ZlogSL | BaboonID) + (1 | Date), data = SubData1, REML = FALSE)
summary(LMMS1)


####################################
## -- The end

