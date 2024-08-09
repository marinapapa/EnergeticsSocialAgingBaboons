##\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
## Figure 2
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
## 2. Does movement predict social opportunities?##
## Carry over variable predicted by fT3, i.e. Residence Time ##
## only use data for females and when 10 or more collars are active ##
SubData2 <- subset(Data, Sex == "F" & Ncollars > 9 & !is.na(RT))

## Transformations ##
SubData2$ZlogRT <- as.vector(scale(log(SubData2$RT)))
SubData2$logRT <- as.vector(log(SubData2$RT))
SubData2$ZlogRT <- as.vector(scale(log(SubData2$RT)))
SubData2$ZDaylength <- as.vector(scale(SubData2$Daylength))
SubData2$ZRank <- as.vector(scale(SubData2$Rank))
SubData2$sqrtsocopp<-sqrt(SubData2$socopp)
SubData2$ZNcollars <- as.vector(SubData2$Ncollars)

## Social Opportunities LMM ##
## Actual model:
socopp <- lmerTest::lmer(socopp ~ RT + ZRank + ZDaylength + Repstate + Ncollars + (1 + RT|BaboonID) + (1|Date), data = SubData2, REML = FALSE) 

socoppRT <- plot_model(socopp, type = "eff", terms = c("RT"), title = "", colors = "#8DCA6B", show.data = TRUE)
tp <- ggpredict(socopp, terms = "RT")

tpall <- SubData2
tpall$type <- 'real'
tp$type <- 'fake'
tpall <- bind_rows(tpall, tp)
tpall <- tidyr::pivot_longer(tpall, 
                             cols = c('RT', 'logRT'), 
                             names_to = 'trans')

img_so <- png::readPNG("media/soc_op.png")
img_so <- grid::rasterGrob(img_so, interpolate = TRUE)

p2 <- ggplot(tpall[tpall$type == 'real' & tpall$trans == 'RT',], 
             aes(y = socopp, x = value, fill = trans,
                 color = trans))+
  scale_color_manual(values = c("black", 'darkgreen'),
                     labels = c('log-transformed', 'raw'))+
  scale_fill_manual(values = c("black", 'darkgreen'),
                    labels = c('log-transformed', 'raw'))+
  geom_point(fill = "#8DCA6B", color = 'black', alpha = 0.7, shape = 21) +
  geom_ribbon(data = tpall[tpall$type == 'fake',], fill = "orchid4",
              color = NA,
              aes(x = x, ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = tpall[tpall$type == 'fake',], 
            aes(x = x, y = predicted), color = "black", size = 0.8)+
  theme_bw()+
  annotation_custom(img_so, xmin = 15, xmax = 35, ymin = 520, ymax = 720) +
  labs( x = "Median daily residence time (min)",
        y = "Daily social opportunities (N)",
        fill = '',
        color = ''
  )+ 
  theme(panel.grid = element_blank(),
        legend.position = c(0.7, 0.95),
        legend.direction = 'horizontal',
        ggside.panel.scale = 0.2,
        legend.margin=margin(2,2,2,2),
        legend.box.margin=margin(0, 0, 0,0),
        axis.title = ggplot2::element_text(color = 'black', size = 14, family = 'Times New Roman'),
        axis.text = ggplot2::element_text(color = 'black', size = 10, family = 'Times New Roman'),
        legend.title = ggplot2::element_text( size = 12, family = 'Times New Roman'),
        legend.text = ggplot2::element_text( size = 10, family = 'Times New Roman'))
p2

ggsave(p2, filename = 'media/Fig2.png',
       width = 4.4, height = 4)

####################################
## -- The end