##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## Figure 1
## Authors: Marina Papadopoulou (m.papadopoulou.rug@gmail.com), Ines Fürtbauer
## Publication: Linking energy availability, movement, and sociality in a wild primate (Papio ursinus)
##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

## For exact replication of the figures the font Times New Roman is needed
#extrafont::loadfonts()

##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## Load data & Packages
Data <- read.csv('data/Data frame phil trans 08052024.csv')
library(dplyr)

##########################################################
## -------------------------------------------------------
## Does energetic condition (fT3) predict movement (Distance,RT,Sinuosity,SL); males and females ##
SubData1 <- subset(Data, !is.na(fT3)&!is.na(RT))

SubData1 <- SubData1 %>% 
  group_by(BaboonID) %>% 
  mutate(fT3_M = mean(fT3),
         fT3_E = fT3 - fT3_M) %>% 
  ungroup()

## Transformations ##
SubData1$ZDaylength <- as.vector(scale(SubData1$Daylength))
SubData1$logRT<-log(SubData1$RT)
SubData1$logSin<-log(SubData1$Sinuosity)
SubData1$logSL<-log(SubData1$SL)
SubData1$ZfT3_E <- as.vector(scale(SubData1$fT3_E))
SubData1$ZfT3_M <- as.vector(scale(SubData1$fT3_M))
SubData1$ZfT3 <- as.vector(scale(SubData1$fT3))

## Residence time LMM ##
## Actual model:
model.RT <-lmerTest::lmer(logRT ~ ZfT3_E + ZfT3_M + Repstate+ (1 +ZfT3_E|BaboonID) + (1|Date), data = SubData1, REML = FALSE) 

## Figure 1:
## Untransformed hormones for plots
model.RT <- lmerTest::lmer(logRT ~ fT3_E + fT3_M + Repstate+ (1 + fT3_E|BaboonID) + (1|Date), data = SubData1, REML = FALSE) 

RT <- plot_model(model.RT, 
                 type = "eff",
                 terms = c("fT3_E"),
                 show.data = FALSE, 
                 color = 'darkgreen')

gpal <- wesanderson::wes_palette(name = 'Cavalcanti1',
                                 n = 20, 
                                 type = 'continuous')
gpal <- rev(gpal[3:6])

p1A <- RT  +
  geom_point(data = SubData1, 
             aes(x = fT3_E, 
                 y = logRT), 
             shape = 21, 
             color = 'black',
             fill = 'grey70',
             alpha = 0.8,
             inherit.aes = F) +
  scale_fill_gradientn(colours = gpal) +
  theme_bw() +
  labs(y = expression("Median daily residence time (log(min))"),
       x = expression("Deviation from"~bar(fT3[i])~ "(ng/g)"),
       tag = "A",
       fill = expression(bar(fT3[i])))+
  theme(legend.position = 'none', 
        plot.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.tag.position = c(0.01,0.98),
        plot.tag = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title.y = ggplot2::element_text(color = 'black', margin = margin(r = 10),
                                             size = 14, family = 'Times New Roman'),
        axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman') )
  

# -----------------
p1B <- ggplot(data = SubData1, aes(fT3_M, logRT)) + 
  stat_summary(fun = mean, geom = 'point', aes(fill = fT3_M), size = 1.5, shape = 21) +
  geom_smooth(method=lm, col = 'darkgreen', fill = 'palegreen4', size = 0.5, se = T) +
  theme_bw()+
  ylim(c(0,3.5))+ # same as Fig1A
  scale_x_continuous(breaks = c(35,40, 45, 50, 55))+
  scale_fill_gradientn(colours = gpal )+
  labs(y = expression("Median daily residence time (log(min))"),
       x = expression(bar(fT3[i])~ "(ng/g)"),
       tag = "B")+
  theme(legend.position = 'none',
        plot.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.tag.position = c(0.01,0.98),
        plot.tag = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title.y = ggplot2::element_text(color = 'white', margin = margin(r = 10),
                                             size = 14, family = 'Times New Roman'),
        axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman'))


P1 <- cowplot::plot_grid(p1A, p1B, labels = NA)
ggsave(P1, filename = 'media/Fig1.png',
       width = 9, height = 4.3)


#####################################
## Supplementary Figures 
## related to the same question

## Figure S1
## Not significant models:

## Distance, LMM1##
model.Dist <- lmerTest::lmer(Distance ~ fT3_E + fT3_M + Repstate + (1 + fT3_E|BaboonID) + (1|Date), data = SubData1, REML = FALSE)
## Step length LMM2 ##
model.SL <-lmerTest::lmer(logSL ~ fT3_E + fT3_M + Repstate + (1 + fT3_E|BaboonID) + (1|Date), data = SubData1, REML = FALSE) 
## Sinuosity LMM3 ##
model.Sin <-lmerTest::lmer(logSin ~ fT3_E + fT3_M + Repstate + (1 + fT3_E|BaboonID) + (1|Date), data = SubData1, REML = FALSE) 

DT <- plot_model(model.Dist, type = "eff", terms = c("fT3_E"),
                 show.data = FALSE, color = 'black')
SL <- plot_model(model.SL, type = "eff", terms = c("fT3_E"),
                 show.data = FALSE, color = 'black')
Sin <- plot_model(model.Sin, type = "eff", terms = c("fT3_E"),
                  show.data = FALSE, color = 'black')

#######
## Plot a panel with subpanels in for each ##

##----------
## Distance 
sp1 <- DT  +
  geom_point(data = SubData1, 
             aes(x = fT3_E, y = Distance), 
             shape = 21, 
             color = 'black',
             fill = 'grey50',
             size = 1.7,
             alpha = 0.7,
             stroke = 1,
             inherit.aes = F) +
  scale_fill_gradientn(colours = gpal) +
  theme_bw() +
  labs(y = expression("Distance (m)"),
       x = expression("Deviation from"~bar(fT3[i])~ "(ng/g)"),
       tag = "A") +
  scale_x_continuous(limits = c(-30,50), breaks = c(-20, 0, 20, 40))+ 
  theme(legend.position = 'none', 
        plot.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.tag.position = c(0.02,0.98),
        plot.tag = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title.y = ggplot2::element_text(color = 'black', hjust = 0.5, size = 14, family = 'Times New Roman'),
        axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman'),
  )


sp1B <- ggplot(data=SubData1, aes(fT3_M, Distance))+ 
  stat_summary(fun = mean, geom = 'point', aes(fill = fT3_M), size = 1.5, shape = 21)+
  geom_smooth(method=lm, col = 'black', fill = 'grey40', size = 0.5,  se = T)+
  theme_bw()+
  ylim(c(6,18))+ 
  labs(x = expression(bar(fT3[i])),
       tag = "B")+
  theme(legend.position = 'none',
        plot.title = element_blank(),
        panel.grid.minor = element_blank(),
        rect = element_rect(fill = "transparent",
                            color = "transparent"),
        panel.background = element_rect(fill = "transparent", 
                                        color = "transparent"),
        plot.background = element_rect(fill = "transparent", 
                                       color = "transparent"),
        panel.grid.major = element_blank(),
        plot.tag.position = c(-0.12,0.94),
        plot.tag = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman'),
        axis.title.y = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman'),
        )

##------------
## Step length 
sp2 <- SL  +
  geom_point(data = SubData1, 
             aes(x = fT3_E, y = logSL), 
             shape = 21, color = 'black', fill = 'grey50',size = 1.7,
             alpha = 0.7, stroke = 1,
             inherit.aes = F)+
  scale_fill_gradientn(colours = gpal )+
  theme_bw()+
  labs(y = expression("Step Length (log(m))"),
       x = expression("Deviation from"~bar(fT3[i])~ "(ng/g)"),
       tag = "C",
       fill = expression(bar(fT3[i]))
  )+
  scale_x_continuous(limits = c(-30,50), breaks = c(-20, 0, 20, 40))+ 
  theme(legend.position = 'none', #c(0.8,0.8),
        plot.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.tag.position = c(0.02,0.98),
        plot.tag = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title.y = ggplot2::element_text(color = 'black', hjust = 0.5, size = 14, family = 'Times New Roman'),
        axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman'),
  )

sp2B <- ggplot(data=SubData1, aes(fT3_M, logSL))+ 
  stat_summary(fun = mean, geom = 'point', aes(fill = fT3_M), size = 1.5, shape = 21)+
  geom_smooth(method=lm, col = 'black', fill = 'grey40', size = 0.5,  se = T)+
  theme_bw()+
  ylim(c(-0.5, 2.5))+ 
  labs(x = expression(bar(fT3[i])),
       tag = "D"
  )+
  theme(legend.position = 'none',
        plot.title = element_blank(),
        panel.grid.minor = element_blank(),
        rect = element_rect(fill = "transparent", color = "transparent"),
        panel.background = element_rect(fill = "transparent", 
                                        color = "transparent"),
        plot.background = element_rect(fill = "transparent", 
                                       color = "transparent"),
        panel.grid.major = element_blank(),
        plot.tag.position = c(-0.12,0.94),
        plot.tag = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman'),
        axis.title.y = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman'),
        #axis.text.y = ggplot2::element_blank(),
        legend.title = ggplot2::element_text( size = 12, family = 'Times New Roman'),
        legend.text = ggplot2::element_text( size = 12, family = 'Times New Roman'))

sp2B

##------------
## Sinuosity

sp3 <- Sin  +
  geom_point(data = SubData1, 
             aes(x = fT3_E, y = logSin), 
             shape = 21, 
             color = 'black',
             fill = 'grey50',
             size = 1.7,
             alpha = 0.7, 
             stroke = 1,
             inherit.aes = F)+
  scale_fill_gradientn(colours = gpal )+
  theme_bw()+
  labs(y = expression("Sinuosity (log)"),
       x = expression("Deviation from"~bar(fT3[i])~ "(ng/g)"),
       tag = "E"
       )+
  scale_x_continuous(limits = c(-30,50), breaks = c(-20, 0, 20, 40))+ 
  theme(legend.position = 'none', 
        plot.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.tag.position = c(0.02,0.98),
        plot.tag = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title.y = ggplot2::element_text(color = 'black', hjust = 0.5, size = 14, family = 'Times New Roman'),
        axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman'),
  )

sp3

sp3B <- ggplot(data = SubData1, aes(fT3_M, logSin))+ 
  stat_summary(fun = mean, geom = 'point', aes(fill = fT3_M), size = 1.5, shape = 21)+
  geom_smooth(method=lm, col = 'black', fill = 'grey40', size = 0.5,  se = T)+
  theme_bw()+
  ylim(c(-2, 0.5))+ 
  labs(x = expression(bar(fT3[i])),
       tag = "F")+
  theme(legend.position = 'none',
        plot.title = element_blank(),
        panel.grid.minor = element_blank(),
        rect = element_rect(fill = "transparent", color = "transparent"),
        panel.background = element_rect(fill = "transparent", 
                                        color = "transparent"),
        plot.background = element_rect(fill = "transparent", 
                                       color = "transparent"),
        panel.grid.major = element_blank(),
        plot.tag.position = c(-0.25,0.94),
        plot.tag = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman'),
        axis.title.y = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman')
        )

# Add inserts
Supf1 <- grid.arrange(sp1)
Supf2 <- grid.arrange(sp2)
Supf3 <- grid.arrange(sp3)

sp1f <- ggdraw(Supf1) +
  draw_plot(sp1B, x = .685, y = .61, width = 0.30, height = 0.375)
sp2f <- ggdraw(Supf2) +
  draw_plot(sp2B, x = .685, y = .61, width = 0.30, height = 0.375)
sp3f <- ggdraw(Supf3) +
  draw_plot(sp3B, x = .685, y = .61, width = 0.30, height = 0.375)

## Full figure:
SF1 <- grid.arrange(sp1f,sp2f, sp3f, nrow = 1)
ggsave(SF1, filename = 'media/FigS1.png',
       width = 13, height = 4)

###########################################
##----------------------------------------
## Figure S2
## Residence time, sinuosity and step length
thetheme <- theme(legend.position = 'none', 
                  plot.title = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  plot.tag.position = c(0.01,0.98),
                  plot.tag = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
                  axis.title = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
                  axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman'),
                  axis.title.y = ggplot2::element_text(color = 'black', margin = margin(r = 10),
                                                       size = 14, family = 'Times New Roman'),
                 )

model.RTsup <- lmerTest::lmer(logRT ~ logSin + logSL + Repstate + (1|BaboonID) + (1|Date), data = SubData1, REML = FALSE) 

## Sinuosity
RTsupi <- plot_model(model.RTsup, 
                     type = "eff", 
                     terms = c("logSin"),
                     show.data = FALSE,
                     color = 'darkgreen')
p1Si <- RTsupi  +
  geom_point(data = SubData1, 
             aes(x = logSin, y = logRT), 
             shape = 21,
             color = 'black', 
             fill = 'grey70', 
             alpha = 0.8,
             inherit.aes = F)+
  theme_bw()+
  labs(y = expression("Median daily residence time (log(min))"),
       x ="Sinuosity (log)",
       tag = "A",
       fill = expression(bar(fT3[i]))
  ) + thetheme

## Step Length
RTsupii <- plot_model(model.RTsup, type="eff", terms = c( "logSL"),
                      show.data = FALSE, color = 'darkgreen')
p1Sii <- RTsupii  +
  geom_point(data = SubData1, 
             aes(x = logSL, y = logRT), 
             shape = 21,
             color = 'black',
             fill = 'grey70', 
             alpha = 0.8, 
             inherit.aes = F) +
  theme_bw() +
  labs(y = expression("Median daily residence time (log(min))"),
       x ="Step Length (log(m))",
       tag = "B",
       fill = expression(bar(fT3[i]))
  ) + thetheme

pS2 <- cowplot::plot_grid(p1Si, p1Sii, labels = NA)

ggsave(pS2, filename = 'media/FigS2.png',
       width = 9, height = 4)

####################################
## -- The end