# Linking Energetics, Movement, and Sociality in Baboons

This repository contains the data and code accompanying the paper: 

*Ines Fürtbauer, Chloe Shergold, Charlotte Christensen, Anna M. Bracken, Michael Heistermann, Marina Papadopoulou, M. Justin O’Riain, & Andrew J. King (2024)
"Linking energy availability, movement, and sociality in a wild primate (Papio ursinus)". Philosophical Transactions of the Royal Society B: Biological Sciences, <doi:10.1098/rstb.2022-0466>*

The code provided is open source, but we kindly ask you to cite the above paper if you make use of it. 

## Data 

All the data used in this work are composed into the _data.csv_ file in the **data** folder

## Code

All _.R_ files in the **code** folder reproduce the figures and statistical analysis of the study.
All analysis was performed in _R_, version 4.2. Files:

- *Fig1.R*: reproduces Figure 1 and Supplementary Figures S1 and S2. 
- *Fig2.R*: reproduces Figure 2.
- *Fig3.R*: reproduces Figure 3.
- *LMMs.R*: performs the statistical analysis of the paper, fits and checks the linear-mixed models, using the package _lmerTest_. 

Package dependencies:
_dplyr, lmerTest, sjPlot, interactions, tidyr, ggplot2, ggeffects, cowplot, wesanderson, png, grid, gridExtra, (extrafont)._

## Contact
* For any technical issues, email **Marina Papadopoulou** at: <m.papadopoulou.rug@gmail.com>
* For any further information about the analysis, email **Ines Fürtbauer** at: <i.fuertbauer@swansea.ac.uk>
