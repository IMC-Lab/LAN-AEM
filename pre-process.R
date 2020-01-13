#!/usr/bin/Rscript
library(dplyr)
library(tidyr)
library(stringr)

args <- commandArgs(trailingOnly=T)
if (length(args) != 3) {
    writeLines('Usage: ./pre-process.r <input-file-name> <LAN-output-file-name> <AEM_output-file-name>')
    quit()
}
in_file <- args[1]
LAN_out_file <- args[2]
AEM_out_file <- args[3]

data_wide <- read.csv(in_file, header=TRUE, stringsAsFactors=FALSE)


## remove unneeded rows/cols
df <- data_wide[-c(1, 2),
                -c(1:5, 7:17, 19:40, 43:64, 67:88, 91:112,
                   115:136, 139:160, 163:184, 187:208)] %>%
    ## remove by consent & attention check
    filter(Q41 == 'Accept HIT' & attend == 'Yes.') %>%
    select(-c(Q41, attend)) %>%
    rename(gender=Genderr, gender_txt=Genderr_4_TEXT, age=Age,
           hispanic=Hispanic.Latino, race=Race, race_txt=Race_7_TEXT,
           education=Education, id=mTurkCode,
           duration=Duration..in.seconds.,
           LAN_OD_normal_agreement=Agreement, LAN_OD_normal_confidence=Confidence,
           LAN_OD_abnormal_agreement=Q81, LAN_OD_abnormal_confidence=Q82,
           LAN_JC_normal_agreement=Q147, LAN_JC_normal_confidence=Q148,
           LAN_JC_abnormal_agreement=Q92, LAN_JC_abnormal_confidence=Q93,
           AEM_OD_action_agreement=Q177, AEM_OD_action_confidence=Q178,
           AEM_OD_inaction_agreement=Q197, AEM_OD_inaction_confidence=Q198,
           AEM_JC_action_agreement=Q216, AEM_JC_action_confidence=Q217,
           AEM_JC_inaction_agreement=Q235, AEM_JC_inaction_confidence=Q236) %>%

    ## separate LAN.Group and AEM.Group into structure x normality
    separate('LAN.Group', c('LAN_exp', 'LAN_structure', 'LAN_normality'),
             sep='_', extra='drop') %>%
    separate('AEM.Group', c('AEM_exp', 'AEM_structure', 'AEM_action'),
             sep='_', extra='drop') %>%

    ## gather questions for each condition into a single column
    unite(LAN_agreement, LAN_OD_normal_agreement, LAN_OD_abnormal_agreement,
          LAN_JC_normal_agreement, LAN_JC_abnormal_agreement, sep='') %>%
    unite(LAN_confidence, LAN_OD_normal_confidence, LAN_OD_abnormal_confidence,
          LAN_JC_normal_confidence, LAN_JC_abnormal_confidence, sep='') %>%
    unite(AEM_agreement, AEM_OD_action_agreement, AEM_OD_inaction_agreement,
          AEM_JC_action_agreement, AEM_JC_inaction_agreement, sep='') %>%
    unite(AEM_confidence, AEM_OD_action_confidence, AEM_OD_inaction_confidence,
          AEM_JC_action_confidence, AEM_JC_inaction_confidence, sep='') %>%
    
    ## convert to long format
    pivot_longer(LAN_agreement:AEM_confidence, names_pattern='(...)_(.*)',
                 names_to=c('exp', 'measure'), values_to='response') %>%
    pivot_wider(names_from='measure', values_from='response') %>%
    select(-c(LAN_exp, AEM_exp)) %>%
    mutate(duration=as.numeric(duration)/60,
           age=as.numeric(age),
           agreement=str_remove_all(agreement, "\\D"), ## remove everything but digits
           confidence=str_remove_all(confidence, "\\D"),
           LAN_normality=paste0(LAN_normality, 'al'))


df %>% filter(exp=='LAN') %>%
    select(-c(exp, AEM_structure, AEM_action)) %>%
    rename(structure=LAN_structure, normality=LAN_normality) %>%
    write.csv(LAN_out_file, row.names=FALSE)


df %>% filter(exp=='AEM') %>%
    select(-c(exp, LAN_structure, LAN_normality)) %>%
    rename(structure=AEM_structure, action=AEM_action) %>%
    write.csv(AEM_out_file, row.names=FALSE)
