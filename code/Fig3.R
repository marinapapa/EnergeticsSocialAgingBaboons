##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## Figure 3
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
##3.Do social opportunities predict grooming?##
SubData3 <- subset(Data, Sex == "F" & !is.na(socopp))

## Transformations #
SubData3$Zsocopp <- as.vector(scale(sqrt(SubData3$socopp)))
SubData3$ZDaylength <- as.vector(scale(SubData3$Daylength))
SubData3$ZRank <- as.vector(scale(SubData3$Rank))
RGmin <- SubData3$RG/60 ## secondes to minutes

## Grooming received ##
## Actual LMM6: 
SubData3$sqrtRG <- sqrt(RGmin)
RG <- lmerTest::lmer (sqrtRG ~ Zsocopp + ZRank + ZDaylength + Repstate + (1 + Zsocopp|BaboonID) + (1|Date), data = SubData3) 

## Untransformed for plot
RG <- lmerTest::lmer (sqrtRG ~ socopp + ZRank + ZDaylength + Repstate + (1 + socopp|BaboonID) + (1|Date), data = SubData3) 

tp <- ggpredict(RG, terms = "socopp")
tpall <- SubData3
tpall$type <- 'read'
tp$type <- 'fake'
tpall <- bind_rows(tpall, tp)

img_gr <- readPNG("media/receive_icon.png")
img_gr <- rasterGrob(img_gr, interpolate=TRUE)

p3A <- ggplot(tpall, aes(x = socopp, y = sqrtRG))+
  geom_point(fill = "#5c5992", shape = 21, size = 2, alpha = 0.8)+
  geom_ribbon(data = tpall[tpall$type == 'fake',],
              aes(x = x, ymin = conf.low, ymax = conf.high), 
              fill = "orchid4",
              alpha = 0.2) +
  geom_line(data = tpall[tpall$type == 'fake',], 
            aes(x = x, y = predicted), 
            color = "navyblue",
            size = 0.9)+
  theme_bw() +
  ylim(c(3,17.5))+
  xlim(c(0,760))+
  annotation_custom(img_gr, xmin = 540, xmax = 760, ymin = 2.5, ymax = 7) +
  labs(y = expression("Receiving grooming ("~sqrt(min)~")"),
       x = "Daily social opportunities (N)",
       tag = "A"
  )+
  theme(panel.grid = element_blank(),
        plot.tag.position = c(0.01,0.98),
        plot.tag = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.title = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman'),
        legend.title = ggplot2::element_text( size = 12, family = 'Times New Roman'),
        legend.text = ggplot2::element_text( size = 10, family = 'Times New Roman'),
  )

## Grooming given ##
## Actual LMM7:
## include interaction between social opportunities and fT3 ##
SubData4 <- subset(Data, Sex == "F" & !is.na(socopp) & !is.na(fT3))

## Transformations #
GGmin <- SubData4$GG/60 ## seconds to minutes
SubData4$Zsocopp <- as.vector(scale(sqrt(SubData4$socopp)))
SubData4$ZDaylength <- as.vector(scale(SubData4$Daylength))
SubData4$ZRank <- as.vector(scale(SubData4$Rank))
SubData4$ZfT3 <- as.vector(scale(SubData4$fT3))
SubData4$sqrtGG <- sqrt(GGmin)

## The model:
GG <- lmerTest::lmer (sqrtGG ~ Zsocopp * ZfT3 + ZRank + ZDaylength + Repstate + (1|BaboonID) + (1|Date), data = SubData4)

## Untransformed for plot
GG <- lmerTest::lmer(sqrtGG ~ socopp * fT3 + ZRank + ZDaylength + Repstate + (1|BaboonID) + (1|Date), data = SubData4)

img_gg <- readPNG("media/give_icon.png")
img_gg <- rasterGrob(img_gg, interpolate=TRUE)

intPlot <- interact_plot(model = GG, 
                         pred = socopp, 
                         modx = fT3, 
                         legend_title = "", 
                         legend.main = 'fT3:', 
                         plot.points=FALSE, 
                         point.size = 0.0001,
                         point.alpha = 0.9,
                         line.thickness = 1.2
)

p3B <- intPlot  +
  geom_point(data = SubData4, 
             aes(x = socopp, y = sqrtGG, fill = fT3), 
             shape = 21, 
             color = 'black',
             size = 2,
             alpha = 0.8,
             inherit.aes = F)+
  scale_linetype_manual(breaks = c('+ 1 SD' , 'Mean','- 1 SD'),
                        values = c('dotted', 'solid', 'twodash')
  )+
  theme_bw()+
  labs(y = expression("Giving grooming ("~sqrt(min)~")"),
       x = "Daily social opportunities (N)",
       tag="B",
       linetype = 'fT3:',
       color = 'fT3:',
  )+
  xlim(c(0,760))+
  ylim(c(3,17.5))+
  annotation_custom(img_gg, xmin = 540, xmax = 760, ymin = 2.5, ymax = 7) +
  theme(legend.position = c(0.86,0.8875),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.key.width = unit(1, 'cm'),
        plot.tag.position = c(0.01,0.98),
        plot.tag = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        legend.key.height = unit(0.05, 'cm'),
        legend.margin=margin(2,2,2,2),
        legend.box.margin=margin(0, 0, 0,0),
        axis.title = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman'),
        legend.title = ggplot2::element_text( size = 12, family = 'Times New Roman'),
        legend.text = ggplot2::element_text( size = 12, family = 'Times New Roman'))


## Combine to final figure ##
p3 <- grid.arrange(p3A, p3B, nrow=1)
ggsave(p3, filename = 'media/Fig3.png',
       width = 9, height = 4)

####################################
## -- The end