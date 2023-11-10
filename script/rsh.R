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

stim_HC <- read.csv("DATA/stim_HC.csv", sep = ",")
stim_HC$Condition <- sub("PC", "HC", stim_HC$Condition)

#factorization

stim_HC$Condition <- factor(stim_HC$Condition, levels = c("NOR", "HC"))
stim_HC$Timing <- factor(stim_HC$Timing, levels = c("pre", "post"))

##LMM

M1 <- lmer(
    MVC ~ Condition * Timing + (1 |
                                       Sujet),
    data = stim_HC,
    REML = TRUE
  )

tab_model(M1)

M2 <- lmer(
  PeakTds ~ Condition * Timing + (1 |
                                Sujet),
  data = stim_HC,
  REML = TRUE
)

tab_model(M2)

M3 <- lmer(
  tb ~ Condition * Timing + (1 |
                                    Sujet),
  data = stim_HC,
  REML = TRUE
)

tab_model(M3)

M4 <- lmer(
  tds.super ~ Condition * Timing + (1 |
                               Sujet),
  data = stim_HC,
  REML = TRUE
)

tab_model(M4)

M5 <- lmer(
  td ~ Condition * Timing + (1 |
                                      Sujet),
  data = stim_HC,
  REML = TRUE
)

tab_model(M5)

M6 <- lmer(
  Mwavemax ~ Condition * Timing + (1 |
                               Sujet),
  data = stim_HC,
  REML = TRUE
)

tab_model(M6)

M7 <- lmer(
  tf ~ Condition * Timing + (1 |
                                     Sujet),
  data = stim_HC,
  REML = TRUE
)

tab_model(M7)

M8 <- lmer(
  Naprime ~ Condition * Timing + (1 |
                               Sujet),
  data = stim_HC,
  REML = TRUE
)

tab_model(M8)

#post hoc

emm_options(lmer.df = "satterthwaite")
emmeans_out <- emmeans(M8, ~Timing*Condition, weights = "show.levels")
emmeans_out
plot(emmeans_out)
pair1 <- pairs(emmeans_out, adjust ="holm")
summary(pair1)
pair2 <- pairs(emmeans_out, by = c("Timing"), adjust = "holm", )
summary(pair2)
pair3 <- pairs(emmeans_out, by = c("Condition"), adjust = "holm")
summary(pair3)

