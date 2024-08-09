##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## Statistical analysis - LMMs
## Authors: Ines Fürtbauer, Marina Papadopoulou (m.papadopoulou.rug@gmail.com)
## Publication: Linking energy availability, movement, and sociality in a wild primate (Papio ursinus)
##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

## Load data & Packages
Data <- read.csv('data/Data frame phil trans 08052024.csv')

library(car)
library(effects)
library(jtools)
library(performance)
library(sjPlot)
library(interactions)
library(dplyr)

##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## 1. Does energetic condition (fT3) predict movement (Distance,RT,Sinuosity,SL); males and females ##
SubData1 <- subset(Data, !is.na(fT3) & !is.na(RT))

## Within subject centering ##
SubData1 <- SubData1 %>% 
  group_by(BaboonID) %>% 
  mutate(fT3_M = mean(fT3),
         fT3_E = fT3 - fT3_M,
  ) %>% 
  ungroup()

## Transformations ##
SubData1$ZDaylength <- as.vector(scale(SubData1$Daylength))
SubData1$logRT <- log(SubData1$RT)
SubData1$logSin <- log(SubData1$Sinuosity)
SubData1$logSL <- log(SubData1$SL)
SubData1$ZfT3_E <- as.vector(scale(SubData1$fT3_E))
SubData1$ZfT3_M <- as.vector(scale(SubData1$fT3_M))
SubData1$ZfT3 <- as.vector(scale(SubData1$fT3))

## Distance LMM1 ##
model.Dist <- lmerTest::lmer(Distance ~ ZfT3_E+ ZfT3_M + Repstate + (1+ZfT3_E |BaboonID) + (1|Date), data = SubData1, REML = FALSE)
summary(model.Dist)
model.Distnull<-lmerTest::lmer(Distance ~ (1|BaboonID) + (1|Date), data = SubData1, REML = FALSE)
summary(model.Distnull)
anova(model.Dist, model.Distnull)
check_model(model.Dist) 
vif(model.Dist)
table.Dist <- tab_model(model.Dist, 
                        show.stat = TRUE, 
                        string.stat = "t value", 
                        show.se = TRUE,
                        string.se = "SE",
                        string.est = "Estimate",
                        show.icc = FALSE, 
                        pred.labels = c("Intercept", "fT3", "Daylength", "Reproductive state (lactating)", "Reproductive state (pregnant)"), 
                        show.re.var = FALSE, 
                        show.r2 = FALSE, 
                        show.ngroups = FALSE, 
                        show.obs = FALSE)
table.Dist

## Step length LMM2 ##
model.SL <-lmerTest::lmer(logSL ~ ZfT3_E+ ZfT3_M + Repstate + (1+ZfT3_E|BaboonID) + (1|Date), data = SubData1, REML = FALSE) 
summary(model.SL)
model.SLnull <-lmerTest::lmer(logSL ~  (1|BaboonID) + (1|Date), data = SubData1, REML = FALSE) 
summary(model.SLnull)
anova(model.SL, model.SLnull)
check_model(model.SL) 
vif(model.SL)
table.SL <- tab_model(model.SL, 
                      show.stat = TRUE,
                      string.stat = "t value", 
                      show.se = TRUE,
                      string.se = "SE",
                      string.est = "Estimate",
                      show.icc = FALSE, 
                      pred.labels = c("Intercept", "fT3", "Daylength", "Reproductive state (lactating)", "Reproductive state (pregnant)"), 
                      show.re.var = FALSE, 
                      show.r2 = FALSE,
                      show.ngroups = FALSE,
                      show.obs = FALSE)
table.SL

## Sinuosity LMM3 ##
model.Sin <-lmerTest::lmer(logSin ~ ZfT3_E+ ZfT3_M + Repstate + (1+ZfT3_E|BaboonID) + (1|Date), data = SubData1, REML = FALSE) 
summary(model.Sin)
model.Sinnull <-lmerTest::lmer(logSin ~ (1|BaboonID) + (1|Date), data = SubData1, REML = FALSE) 
summary(model.Sinnull)
anova(model.Sin, model.Sinnull)
check_model(model.Sin) 
vif(model.Sin)
table.Sin <- tab_model(model.Sin,
                       show.stat = TRUE, 
                       string.stat = "t value",
                       show.se = TRUE,
                       string.se = "SE",
                       string.est = "Estimate",
                       show.icc = FALSE,
                       pred.labels = c("Intercept", "fT3", "Daylength", "Reproductive state (lactating)", "Reproductive state (pregnant)"),
                       show.re.var = FALSE,
                       show.r2 = FALSE,
                       show.ngroups = FALSE,
                       show.obs = FALSE)
table.Sin

## Residence time LMM4 ##
model.RT <-lmerTest::lmer(logRT ~ ZfT3_E + ZfT3_M + Repstate+ (1 +ZfT3_E|BaboonID) + (1|Date), data = SubData1, REML = FALSE) 
summary(model.RT) 
drop1(model.RT)
model.RTnull <-lmerTest::lmer(logRT ~ (1 |BaboonID) + (1|Date), data = SubData1, REML = FALSE) 
summary(model.RTnull)
anova(model.RT, model.RTnull)
check_model(model.RT) 
vif(model.RT)
table.RT <- tab_model(model.RT,
                      show.stat = TRUE,
                      string.stat = "t value",
                      show.se = TRUE,
                      string.se = "SE",
                      string.est = "Estimate",
                      show.icc = FALSE, 
                      pred.labels = c("Intercept", "fT3 (mean-centred)", "fT3 (mean)", "Reproductive state (cycling)", "Reproductive state (lactating)", "Reproductive state (male)", "Reproductive state (pregnant)"),
                      show.re.var = FALSE,
                      show.r2 = FALSE,
                      show.ngroups = FALSE,
                      show.obs = FALSE)
table.RT

##########################################################
## -------------------------------------------------------
##2. Does movement predict social opportunities?##
## Carry over variable predicted by fT3, i.e. Residence Time ##
## only use data for females and when 10 or more collars are active ##
SubData2 <- subset(Data, Sex=="F" & Ncollars > 9 & !is.na(RT))

## Transformations##
SubData2$ZlogRT <- as.vector(scale(log(SubData2$RT)))
SubData2$logRT <- as.vector(log(SubData2$RT))
SubData2$ZlogRT <- as.vector(scale(log(SubData2$RT)))
SubData2$ZDaylength <- as.vector(scale(SubData2$Daylength))
SubData2$ZRank <- as.vector(scale(SubData2$Rank))
SubData2$sqrtsocopp<-sqrt(SubData2$socopp)
SubData2$ZNcollars <- as.vector(SubData2$Ncollars)

## Social Opportunities LMM5 ##
socopp <- lmerTest::lmer(sqrtsocopp ~ ZlogRT + ZRank + ZDaylength + Repstate + ZNcollars+(1+ZlogRT|BaboonID) + (1|Date), data = SubData2, REML = FALSE) 
summary(socopp)
socoppnull <- lmerTest::lmer(sqrtsocopp ~   ZDaylength + ZNcollars+(1 |BaboonID) + (1|Date), data = SubData2, REML = FALSE) 
summary(socoppnull)
anova(socopp,socoppnull)
check_model(socopp)
vif(socopp)
drop1(socopp)

##########################################################
##3.Do social opportunities predict grooming?##
SubData3 <- subset(Data, Sex == "F" & !is.na(socopp))

## Transformations ##
SubData3$Zsocopp <- as.vector(scale(sqrt(SubData3$socopp)))
SubData3$ZDaylength <- as.vector(scale(SubData3$Daylength))
SubData3$ZRank <- as.vector(scale(SubData3$Rank))
RGmin <- SubData3$RG/60 ## seconds to minutes

## Grooming received LMM6 ##
SubData3$sqrtRG=sqrt(RGmin)
RG <- lmerTest::lmer (sqrtRG ~ Zsocopp + ZRank + ZDaylength + Repstate + (1 +Zsocopp|BaboonID) + (1|Date), data = SubData3) 
summary(RG)
RGnull <- lmerTest::lmer (sqrtRG ~ ZDaylength +  (1 |BaboonID) + (1|Date), data = SubData3) 
summary(RGnull)
anova(RG, RGnull)
check_model(RG)
vif(RG)
table.RG <- tab_model(RG, 
                      show.stat = TRUE, 
                      string.stat = "t value", 
                      show.se = TRUE,
                      string.se = "SE",
                      string.est = "Estimate",
                      show.icc = FALSE, 
                      pred.labels = c("Intercept", "Social opportunities", "Rank", "Daylength", "Reproductive state (lactating)", "Reproductive state (pregnant)"), 
                      show.re.var = FALSE, 
                      show.r2 = FALSE,
                      show.ngroups = FALSE,
                      show.obs = FALSE)
table.RG

## Grooming given LMM7 ##
## include interaction between social opportunities and fT3 ##
SubData4 <-subset(Data, Sex == "F" & !is.na(socopp) & !is.na(fT3))
GGmin <- SubData4$GG/60
SubData4$Zsocopp <- as.vector(scale(sqrt(SubData4$socopp)))
SubData4$ZDaylength <- as.vector(scale(SubData4$Daylength))
SubData4$ZRank <- as.vector(scale(SubData4$Rank))
SubData4$ZfT3 <- as.vector(scale(SubData4$fT3))
SubData4$sqrtGG <- sqrt(GGmin)

GG <- lmerTest::lmer (sqrtGG ~ Zsocopp*ZfT3 + ZRank + ZDaylength + Repstate + (1 |BaboonID) + (1|Date), data = SubData4)
summary(GG)
GGnull <- lmerTest::lmer (sqrtGG ~  ZDaylength + (1 |BaboonID) + (1|Date), data = SubData4)
summary(GGnull)
anova(GG,GGnull)
check_model(GG)
vif(GG)
table.GG <- tab_model(GG,
                      show.stat = TRUE, 
                      string.stat = "t value", 
                      show.se = TRUE,
                      string.se = "SE", 
                      string.est = "Estimate", 
                      show.icc = FALSE, 
                      pred.labels = c("Intercept", "Social opportunities", "fT3","Rank", "Daylength", "Reproductive state (lactating)", "Reproductive state (pregnant)", "Social opportunities x fT3"), 
                      show.re.var = FALSE,
                      show.r2 = FALSE,
                      show.ngroups = FALSE, 
                      show.obs = FALSE)
table.GG

####################################
## -- The end

