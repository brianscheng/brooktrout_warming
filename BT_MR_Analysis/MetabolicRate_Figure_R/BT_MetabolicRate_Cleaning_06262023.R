library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(broom)
library(stringi)
library(EnvStats)
library(dunn.test)
library(multcomp)

RMR_15_1_Initial <- read.delim("results_lowest25_15C_1_Initial_08192021_R2_0.9.txt")
RMR_15_2_Initial <- read.delim("results_lowest25_15C_2_Initial_08272021_R2_0.9.txt")
RMR_20_1_Initial <- read.delim("results_lowest25_20C_1_Initial_08272021_R2_0.9.txt")
RMR_20_2_Initial <- read.delim("results_lowest25_20C_2_Initial_08272021_R2_0.9.txt")
RMR_15_1_3 <- read.delim("results_lowest25_15C_1_3month_06262023_R2>0.9.txt")
RMR_15_2_3 <- read.delim("results_lowest25_15C_2_3month_06262023_R2>0.9.txt")
RMR_20_1_3 <- read.delim("results_lowest25_20C_1_3month_06262023_R2>0.9.txt")
RMR_20_2_3 <- read.delim("results_lowest25_20C_2_3month_06262023_R2>0.9.txt")
RMR_15_1_6 <- read.delim("results_lowest25_15C_1_6month_06262023_R2>0.9.txt")
RMR_15_2_6 <- read.delim("results_lowest25_15C_2_6month_06262023_R2>0.9.txt")
RMR_20_1_6 <- read.delim("results_lowest25_20C_1_6month_06262023_R2>0.9.txt")
RMR_20_2_6 <- read.delim("results_lowest25_20C_2_6month_06262023_R2>0.9.txt")

## Creating Mean #s for 15C Calculations

RMR_15_1_Initial_calculated <- RMR_15_1_Initial %>%
  group_by(Ind) %>%
  summarise_at(vars(Temp,Mass,MR.mass,MR.abs),mean) %>%
  mutate(Replicate = 1, .before = Temp) %>%
  mutate(TimePoint = 0, .before = Replicate)

RMR_15_2_Initial_calculated <- RMR_15_2_Initial %>%
  group_by(Ind) %>%
  summarise_at(vars(Temp,Mass,MR.mass,MR.abs),mean) %>%
  mutate(Replicate = 2, .before = Temp) %>%
  mutate(TimePoint = 0, .before = Replicate)

RMR_15_1_3month_calculated <- RMR_15_1_3 %>%
  group_by(Ind) %>%
  summarise_at(vars(Temp,Mass,MR.mass,MR.abs),mean) %>%
  mutate(Replicate = 1, .before = Temp) %>%
  mutate(TimePoint = 3, .before = Replicate)

RMR_15_2_3month_calculated <- RMR_15_2_3 %>%
  group_by(Ind) %>%
  summarise_at(vars(Temp,Mass,MR.mass,MR.abs),mean) %>%
  mutate(Replicate = 2, .before = Temp) %>%
  mutate(TimePoint = 3, .before = Replicate)

RMR_15_1_6month_calculated <- RMR_15_1_6 %>%
  group_by(Ind) %>%
  summarise_at(vars(Temp,Mass,MR.mass,MR.abs),mean) %>%
  mutate(Replicate = 1, .before = Temp) %>%
  mutate(TimePoint = 6, .before = Replicate)

RMR_15_2_6month_calculated <- RMR_15_2_6 %>%
  group_by(Ind) %>%
  summarise_at(vars(Temp,Mass,MR.mass,MR.abs),mean) %>%
  mutate(Replicate = 2, .before = Temp) %>%
  mutate(TimePoint = 6, .before = Replicate)

## Bind Rows Together

RMR_15_compiled <- bind_rows(RMR_15_1_Initial_calculated, 
                          RMR_15_2_Initial_calculated, 
                          RMR_15_1_3month_calculated, 
                          RMR_15_2_3month_calculated, 
                          RMR_15_1_6month_calculated,
                          RMR_15_2_6month_calculated)

RMR_15_compiled <- RMR_15_compiled %>%
  mutate(Treatment = 15, .before = Replicate)

## Calculations

RMR_15_compiled2 <- RMR_15_compiled %>%
  subset(MR.mass > 0) %>%
  mutate(ln_Mass = log(Mass)) %>%
  mutate(ln_MR.mass = log(MR.mass)) %>%
  mutate(ln_MR.abs = log(MR.abs))

## Creating Mean #s for 20C Calculations

RMR_20_1_Initial_calculated <- RMR_20_1_Initial %>%
  group_by(Ind) %>%
  summarise_at(vars(Temp,Mass,MR.mass,MR.abs),mean) %>%
  mutate(Replicate = 1, .before = Temp) %>%
  mutate(TimePoint = 0, .before = Replicate)

RMR_20_2_Initial_calculated <- RMR_20_2_Initial %>%
  group_by(Ind) %>%
  summarise_at(vars(Temp,Mass,MR.mass,MR.abs),mean) %>%
  mutate(Replicate = 2, .before = Temp) %>%
  mutate(TimePoint = 0, .before = Replicate)

RMR_20_1_3month_calculated <- RMR_20_1_3 %>%
  group_by(Ind) %>%
  summarise_at(vars(Temp,Mass,MR.mass,MR.abs),mean) %>%
  mutate(Replicate = 1, .before = Temp) %>%
  mutate(TimePoint = 3, .before = Replicate)

RMR_20_2_3month_calculated <- RMR_20_2_3 %>%
  group_by(Ind) %>%
  summarise_at(vars(Temp,Mass,MR.mass,MR.abs),mean) %>%
  mutate(Replicate = 2, .before = Temp) %>%
  mutate(TimePoint = 3, .before = Replicate)

RMR_20_1_6month_calculated <- RMR_20_1_6 %>%
  group_by(Ind) %>%
  summarise_at(vars(Temp,Mass,MR.mass,MR.abs),mean) %>%
  mutate(Replicate = 1, .before = Temp) %>%
  mutate(TimePoint = 6, .before = Replicate)

RMR_20_2_6month_calculated <- RMR_20_2_6 %>%
  group_by(Ind) %>%
  summarise_at(vars(Temp,Mass,MR.mass,MR.abs),mean) %>%
  mutate(Replicate = 2, .before = Temp) %>%
  mutate(TimePoint = 6, .before = Replicate)

## Bind Rows Together 

RMR_20_compiled <- bind_rows(RMR_20_1_Initial_calculated, 
                             RMR_20_2_Initial_calculated, 
                             RMR_20_1_3month_calculated, 
                             RMR_20_2_3month_calculated, 
                             RMR_20_1_6month_calculated,
                             RMR_20_2_6month_calculated)
  
RMR_20_compiled <- RMR_20_compiled %>%
  mutate(Treatment = 20, .before = Replicate)

## Calculations

RMR_20_compiled2 <- RMR_20_compiled %>%
  subset(MR.mass > 0) %>%
  mutate(ln_Mass = log(Mass)) %>%
  mutate(ln_MR.mass = log(MR.mass)) %>%
  mutate(ln_MR.abs = log(MR.abs))

## Bind Rows Together from 15C and 20C RMR Data Frames

RMR_compiled <- bind_rows(RMR_15_compiled2, 
                          RMR_20_compiled2) 

RMR_compiled <- subset(RMR_compiled, RMR_compiled$ln_MR.mass >= 0)

# Rosner Test for Outliers for RMR

rosner.out_RMR <- rosnerTest(RMR_compiled$MR.mass, k=10)$all.stats
rosner.out_RMR <- subset(rosner.out_RMR, Outlier %in% "TRUE")

## Create Compiled CSV for RMR

write.csv(RMR_compiled, "BT_RMR_Merged_06262023.csv")

## Start Compiling MMR Files

MMR_15_1_Initial <- read.csv("_MMR_all_15C_1_042523.csv")
MMR_15_2_Initial <- read.csv("_MMR_all_15C_2_042523.csv")
MMR_20_1_Initial <- read.csv("_MMR_all_20C_1_042523.csv")
MMR_20_2_Initial <- read.csv("_MMR_all_20C_2_042523.csv")
MMR_15_1_3 <- read.csv("_MMR_all_15C_1_3month_042523.csv")
MMR_15_2_3 <- read.csv("_MMR_all_15C_2_3month_042523.csv")
MMR_20_1_3 <- read.csv("_MMR_all_20C_1_3month_042523.csv")
MMR_20_2_3 <- read.csv("_MMR_all_20C_2_3month_042523.csv")
MMR_15_1_6 <- read.csv("_MMR_all_15C_1_6month_042523.csv")
MMR_15_2_6 <- read.csv("_MMR_all_15C_2_6month_042523.csv")
MMR_20_1_6 <- read.csv("_MMR_all_20C_1_6month_042523.csv")
MMR_20_2_6 <- read.csv("_MMR_all_20C_2_6month_042523.csv")

## Clean 15C MMR Data Files

MMR_15_1_Initial_calculated <- data.frame(t(MMR_15_1_Initial[-1]))
MMR_15_1_Initial_calculated <- MMR_15_1_Initial_calculated %>%
  rename(MMR_1minute = X1) %>%
  mutate(Replicate = 1, .before = MMR_1minute) %>%
  mutate(TimePoint = 0, .before = Replicate)
MMR_15_1_Initial_calculated <- MMR_15_1_Initial_calculated[,c(1,2,3)]

MMR_15_2_Initial_calculated <- data.frame(t(MMR_15_2_Initial[-1]))
MMR_15_2_Initial_calculated <- MMR_15_2_Initial_calculated %>%
  rename(MMR_1minute = X1) %>%
  mutate(Replicate = 2, .before = MMR_1minute) %>%
  mutate(TimePoint = 0, .before = Replicate)
MMR_15_2_Initial_calculated <- MMR_15_2_Initial_calculated[,c(1,2,3)]

MMR_15_1_3_calculated <- data.frame(t(MMR_15_1_3[-1]))
MMR_15_1_3_calculated <- MMR_15_1_3_calculated %>%
  rename(MMR_1minute = X1) %>%
  mutate(Replicate = 1, .before = MMR_1minute) %>%
  mutate(TimePoint = 3, .before = Replicate)
MMR_15_1_3_calculated <- MMR_15_1_3_calculated[,c(1,2,3)]

MMR_15_2_3_calculated <- data.frame(t(MMR_15_2_3[-1]))
MMR_15_2_3_calculated <- MMR_15_2_3_calculated %>%
  rename(MMR_1minute = X1) %>%
  mutate(Replicate = 2, .before = MMR_1minute) %>%
  mutate(TimePoint = 3, .before = Replicate)
MMR_15_2_3_calculated <- MMR_15_2_3_calculated[,c(1,2,3)]

MMR_15_1_6_calculated <- data.frame(t(MMR_15_1_6[-1]))
MMR_15_1_6_calculated <- MMR_15_1_6_calculated %>%
  rename(MMR_1minute = X1) %>%
  mutate(Replicate = 1, .before = MMR_1minute) %>%
  mutate(TimePoint = 6, .before = Replicate)
MMR_15_1_6_calculated <- MMR_15_1_6_calculated[,c(1,2,3)]

MMR_15_2_6_calculated <- data.frame(t(MMR_15_2_6[-1]))
MMR_15_2_6_calculated <- MMR_15_2_6_calculated %>%
  rename(MMR_1minute = X1) %>%
  mutate(Replicate = 2, .before = MMR_1minute) %>%
  mutate(TimePoint = 6, .before = Replicate)
MMR_15_2_6_calculated <- MMR_15_2_6_calculated[,c(1,2,3)]

## Bind Rows Together

MMR_15_compiled <- bind_rows(MMR_15_1_Initial_calculated, 
                             MMR_15_2_Initial_calculated, 
                             MMR_15_1_3_calculated, 
                             MMR_15_2_3_calculated, 
                             MMR_15_1_6_calculated,
                             MMR_15_2_6_calculated)
RMR_15_compiled_meta <- RMR_15_compiled[c(1,3,5:6)]
MMR_15_compiled <-cbind(RMR_15_compiled_meta,MMR_15_compiled)

## Calculate 15C MMR LN Data

MMR_15_compiled2 <- MMR_15_compiled %>%
  mutate(MR.abs = MMR_1minute*(Mass/1000)) %>%
  mutate(ln_Mass = log(Mass)) %>%
  mutate(ln_MR.mass = log(MMR_1minute)) %>%
  mutate(ln_MR.abs = log(MR.abs))

## Clean 20C MMR Data Files

MMR_20_1_Initial_calculated <- data.frame(t(MMR_20_1_Initial[-1]))
MMR_20_1_Initial_calculated <- MMR_20_1_Initial_calculated %>%
  rename(MMR_1minute = X1) %>%
  mutate(Replicate = 1, .before = MMR_1minute) %>%
  mutate(TimePoint = 0, .before = Replicate)
MMR_20_1_Initial_calculated <- MMR_20_1_Initial_calculated[,c(1,2,3)]

MMR_20_2_Initial_calculated <- data.frame(t(MMR_20_2_Initial[-1]))
MMR_20_2_Initial_calculated <- MMR_20_2_Initial_calculated %>%
  rename(MMR_1minute = X1) %>%
  mutate(Replicate = 2, .before = MMR_1minute) %>%
  mutate(TimePoint = 0, .before = Replicate)
MMR_20_2_Initial_calculated <- MMR_20_2_Initial_calculated[,c(1,2,3)]

MMR_20_1_3_calculated <- data.frame(t(MMR_20_1_3[-1]))
MMR_20_1_3_calculated <- MMR_20_1_3_calculated %>%
  rename(MMR_1minute = X1) %>%
  mutate(Replicate = 1, .before = MMR_1minute) %>%
  mutate(TimePoint = 3, .before = Replicate)
MMR_20_1_3_calculated <- MMR_20_1_3_calculated[,c(1,2,3)]

MMR_20_2_3_calculated <- data.frame(t(MMR_20_2_3[-1]))
MMR_20_2_3_calculated <- MMR_20_2_3_calculated %>%
  rename(MMR_1minute = X1) %>%
  mutate(Replicate = 2, .before = MMR_1minute) %>%
  mutate(TimePoint = 3, .before = Replicate)
MMR_20_2_3_calculated <- MMR_20_2_3_calculated[,c(1,2,3)]

MMR_20_1_6_calculated <- data.frame(t(MMR_20_1_6[-1]))
MMR_20_1_6_calculated <- MMR_20_1_6_calculated %>%
  rename(MMR_1minute = X1) %>%
  mutate(Replicate = 1, .before = MMR_1minute) %>%
  mutate(TimePoint = 6, .before = Replicate)
MMR_20_1_6_calculated <- MMR_20_1_6_calculated[,c(1,2,3)]

MMR_20_2_6_calculated <- data.frame(t(MMR_20_2_6[-1]))
MMR_20_2_6_calculated <- MMR_20_2_6_calculated %>%
  rename(MMR_1minute = X1) %>%
  mutate(Replicate = 2, .before = MMR_1minute) %>%
  mutate(TimePoint = 6, .before = Replicate)
MMR_20_2_6_calculated <- MMR_20_2_6_calculated[,c(1,2,3)]

## Bind Rows Together

MMR_20_compiled <- bind_rows(MMR_20_1_Initial_calculated, 
                             MMR_20_2_Initial_calculated, 
                             MMR_20_1_3_calculated, 
                             MMR_20_2_3_calculated, 
                             MMR_20_1_6_calculated,
                             MMR_20_2_6_calculated)
RMR_20_compiled_meta <- RMR_20_compiled[c(1,3,5:6)]
MMR_20_compiled <-cbind(RMR_20_compiled_meta,MMR_20_compiled)

## Calculate MMR 20 LN Data

MMR_20_compiled2 <- MMR_20_compiled %>%
  mutate(MR.abs = MMR_1minute*(Mass/1000)) %>%
  mutate(ln_Mass = log(Mass)) %>%
  mutate(ln_MR.mass = log(MMR_1minute)) %>%
  mutate(ln_MR.abs = log(MR.abs))

## Bind Rows Together from 15C and 20C RMR Data Frames

MMR_compiled <- bind_rows(MMR_15_compiled2, 
                          MMR_20_compiled2) 

# Rosner Test for Outliers for MMR

rosner.out_MMR <- rosnerTest(MMR_compiled$MMR_1minute, k=10)$all.stats
rosner.out_MMR <- subset(rosner.out_MMR, Outlier %in% "TRUE")

MMR_compiled <- MMR_compiled[-rosner.out_MMR$Obs.Num, ]

write.csv(MMR_compiled, "BT_MMR_Merged_04062023.csv")
