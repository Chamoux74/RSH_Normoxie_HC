library(readxl)
library(ggplot2)
library(dplyr)
library(lme4)
library(lmerTest)
library(performance)
library(MuMIn)
library(phia)
library(emmeans)
library(sjPlot)
library(ggpubr)

#extract data

gaz_HC <- read_excel("DATA/HC sans nirs.xlsx")
gaz_HC$Condition <- sub("PC", "HC", gaz_HC$Condition)

#factorization
