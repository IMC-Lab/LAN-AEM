#!/usr/bin/Rscript
library(ggplot2)
library(dplyr)
library(emmeans)

data.LAN <- read.csv('data/LAN_processed_data_1_13_20.csv') %>%
    mutate(normality=factor(normality, levels=c('normal', 'abnormal')))
data.AEM <- read.csv('data/AEM_processed_data_1_13_20.csv') %>%
    mutate(action=factor(action, levels=c('inaction', 'action')))

m.LAN <- lm(agreement ~ structure * normality, data=data.LAN)
summary(m.LAN)
emmeans(m.LAN, ~ normality, by="structure")

ggplot(data.LAN) +
    aes(x=structure, y=agreement,
        group=interaction(normality,structure), fill=normality) +
    ylim(1, 7) +
    geom_violin(bw=0.45, position=position_dodge(width=1)) +
    stat_summary(fun.data=mean_cl_normal, position=position_dodge(width=1))
    
m.LAN <- lm(confidence ~ structure * normality, data=data.LAN)
summary(m.LAN)
emmeans(m.LAN, ~ normality, by="structure")

ggplot(data.LAN) +
    aes(x=structure, y=confidence,
        group=interaction(normality,structure), fill=normality) +
    ylim(1, 7) +
    geom_violin(bw=0.45, position=position_dodge(width=1)) +
    stat_summary(fun.data=mean_cl_normal, position=position_dodge(width=1))

m.AEM <- lm(agreement ~ structure * action + confidence, data=data.AEM)
summary(m.AEM)
emmeans(m.AEM, ~ action, by="structure")

ggplot(data.AEM) +
    aes(x=structure, y=agreement,
        group=interaction(action,structure), fill=action) +
    ylim(1, 7) +
    geom_violin(bw=0.45, position=position_dodge(width=1)) +
    stat_summary(fun.data=mean_cl_normal, position=position_dodge(width=1))

m.AEM <- lm(confidence ~ structure * action, data=data.AEM)
summary(m.AEM)
emmeans(m.AEM, ~ action, by="structure")

ggplot(data.AEM) +
    aes(x=structure, y=confidence,
        group=interaction(action,structure), fill=action) +
    ylim(1, 7) +
    geom_violin(bw=0.45, position=position_dodge(width=1)) +
    stat_summary(fun.data=mean_cl_normal, position=position_dodge(width=1))
