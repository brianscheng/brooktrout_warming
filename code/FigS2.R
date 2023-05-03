### Script for Figure S2 - supporting gill metrics

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)


####bring data in
data<-read.csv(here('data/GSA_BT_Compiled_03122023_BSC.csv'), stringsAsFactors = T)
str(data)
summary(data)

data2<-data %>%
  mutate (Temp = factor(Temp)) %>%
  mutate (Replicate = case_when (Temp == "15" & Replicate == "1"~ "1",
                                 Temp == "15" & Replicate == "2"~ "2",
                                 Temp == "20" & Replicate == "1"~ "3",
                                 Temp == "20" & Replicate == "2"~ "4")) %>%
  mutate (Replicate = factor (Replicate)) %>%
  mutate (log10_GSA = log10(GSA)) %>%
  mutate (log10_Weight = log10(Weight)) %>%
  rename (Lamellar_area = Lamallae.Area,
          Total_filament_length = total_filament_length_mm,
          Lamellar_frequency = Lamallae.Density )

#units for future reference
# Frequency = Lamellae / mm
# Surface Area = mm2
# Filament Length = mm 

####basic EDA
ggplot(data = data2, aes(x= Temp, y = Total_filament_length))+geom_boxplot()+geom_point()
ggplot(data = data2, aes(x= Temp, y = Lamellar_area))+geom_boxplot()+geom_point()
ggplot(data = data2, aes(x= Temp, y = Lamellar_frequency))+geom_boxplot()+geom_point()

####basic models
