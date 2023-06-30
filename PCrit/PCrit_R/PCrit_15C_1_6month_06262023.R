library(tidyverse) ##contains ggplot for visualizing and many commands I use
library(cowplot) ##for saving plots and arranging if you so choose
library(afex) ##this is needed for the proper two-way ANOVA 
library("respR")
library(ggplot2)
library(respirometry)

MR1 = 123.4733246
MR2 = 149.23978
MR3 = 118.7167757
MR4 = 123.0479838
MR5 = 119.9909452
MR6 = 126.8816148

vr1 <- 4.012 # Chamber volume in litres
vr2 <- 4.012
vr3 <- 4.012
vr4 <- 4.012
vr5 <- 4.012
vr6 <- 4.012

vf1 <- .2049 # Fish volume in litres (1L = 1kg)
vf2 <- .1175
vf3 <- .2098
vf4 <- .1599
vf5 <- .2072
vf6 <- .1402

LOE1 <- 25.18
LOE2 <- 16.00
LOE3 <- 20.28
LOE4 <- 13.76
LOE5 <- 14.52
LOE6 <- 11.00

PCrit_Raw <- read.delim("PCRIT_1_15C_6month_raw.txt")
write_csv(PCrit_Raw,'PCRIT_1_15C_6month_raw.csv')
PCrit_Raw_modified <- read_delim("PCRIT_1_15C_6month_raw.csv", 
                                 delim = ",", skip = 37) %>% # Skip the first blank rows and put the column names in the right spot
  drop_na(Phase) %>%                      
  mutate(DateTime = `Date &Time [DD-MM-YYYY/HH:MM:SS]`, 
         patm = `Barometric pressure [hPa]`,
         Salinity = `Salinity [<89>]`,
         Ch1_DO = `CH1 O2 [%air sat.]`, 
         Ch2_DO = `CH2 O2 [%air sat.]`, 
         Ch3_DO = `CH3 O2 [%air sat.]`, 
         Ch4_DO = `CH4 O2 [%air sat.]`,
         Ch5_DO = `CH5 O2 [%air sat.]`, 
         Ch6_DO = `CH6 O2 [%air sat.]`, 
         Ch1_temp = `CH1 temp (<b0>C)`, 
         Ch2_temp = `CH2 temp (<b0>C)`,
         Ch3_temp = `CH3 temp (<b0>C)`, 
         Ch4_temp = `CH4 temp (<b0>C)`,
         Ch5_temp = `CH5 temp (<b0>C)`, 
         Ch6_temp = `CH6 temp (<b0>C)`,
         TimeMinutes = seq(0, by=1/60, length.out=nrow(.)),) %>%
  dplyr::select(TimeMinutes, Phase, 
                Ch1_DO, Ch2_DO, Ch3_DO, Ch4_DO, Ch5_DO, Ch6_DO,
                Ch1_temp, Ch2_temp, Ch3_temp, Ch4_temp, Ch5_temp, Ch6_temp,
                DateTime, patm, Salinity)

##This brings in the PCrit Raw File

PCrit_Raw_df2 <- PCrit_Raw_modified %>% 
  mutate(Ch1_O2mgL = ((respR::convert_DO(x=100, from = "%Air", to = "mg/L", S = Salinity, 
                                         t = Ch1_temp, P = (1.0322))) * (Ch1_DO/100)))%>%
  mutate(Ch2_O2mgL = ((respR::convert_DO(x=100, from = "%Air", to = "mg/L", S = Salinity, 
                                         t = Ch2_temp, P = (1.0322))) * (Ch2_DO/100)))%>%
  mutate(Ch3_O2mgL = ((respR::convert_DO(x=100, from = "%Air", to = "mg/L", S = Salinity, 
                                         t = Ch3_temp, P = (1.0322))) * (Ch3_DO/100)))%>%
  mutate(Ch4_O2mgL = ((respR::convert_DO(x=100, from = "%Air", to = "mg/L", S = Salinity, 
                                         t = Ch4_temp, P = (1.0322))) * (Ch4_DO/100)))%>% 
  mutate(Ch5_O2mgL = ((respR::convert_DO(x=100, from = "%Air", to = "mg/L", S = Salinity, 
                                         t = Ch5_temp, P = (1.0322))) * (Ch5_DO/100))) %>%
  mutate(Ch6_O2mgL = ((respR::convert_DO(x=100, from = "%Air", to = "mg/L", S = Salinity, 
                                         t = Ch6_temp, P = (1.0322))) * (Ch6_DO/100)))

Tank1_Pcrit <- data.frame(PCrit_Raw_df2$Ch1_DO,
                          PCrit_Raw_df2$Phase,
                          PCrit_Raw_df2$Salinity,
                          PCrit_Raw_df2$Ch1_temp,
                          PCrit_Raw_df2$patm,
                          PCrit_Raw_df2$Ch1_O2mgL)

Tank2_Pcrit <- data.frame(PCrit_Raw_df2$Ch2_DO,
                          PCrit_Raw_df2$Phase,
                          PCrit_Raw_df2$Salinity,
                          PCrit_Raw_df2$Ch2_temp,
                          PCrit_Raw_df2$patm,
                          PCrit_Raw_df2$Ch2_O2mgL)

Tank3_Pcrit <- data.frame(PCrit_Raw_df2$Ch3_DO,
                          PCrit_Raw_df2$Phase,
                          PCrit_Raw_df2$Salinity,
                          PCrit_Raw_df2$Ch3_temp,
                          PCrit_Raw_df2$patm,
                          PCrit_Raw_df2$Ch3_O2mgL)

Tank4_Pcrit <- data.frame(PCrit_Raw_df2$Ch4_DO,
                          PCrit_Raw_df2$Phase,
                          PCrit_Raw_df2$Salinity,
                          PCrit_Raw_df2$Ch4_temp,
                          PCrit_Raw_df2$patm,
                          PCrit_Raw_df2$Ch4_O2mgL)

Tank5_Pcrit <- data.frame(PCrit_Raw_df2$Ch5_DO,
                          PCrit_Raw_df2$Phase,
                          PCrit_Raw_df2$Salinity,
                          PCrit_Raw_df2$Ch5_temp,
                          PCrit_Raw_df2$patm,
                          PCrit_Raw_df2$Ch5_O2mgL)

Tank6_Pcrit <- data.frame(PCrit_Raw_df2$Ch6_DO,
                          PCrit_Raw_df2$Phase,
                          PCrit_Raw_df2$Salinity,
                          PCrit_Raw_df2$Ch6_temp,
                          PCrit_Raw_df2$patm,
                          PCrit_Raw_df2$Ch6_O2mgL)

Tank1_Pcrit$Loop <- 1:nrow(PCrit_Raw_modified)
Tank2_Pcrit$Loop <- 1:nrow(PCrit_Raw_modified)
Tank3_Pcrit$Loop <- 1:nrow(PCrit_Raw_modified)
Tank4_Pcrit$Loop <- 1:nrow(PCrit_Raw_modified)
Tank5_Pcrit$Loop <- 1:nrow(PCrit_Raw_modified)
Tank6_Pcrit$Loop <- 1:nrow(PCrit_Raw_modified)

Tank1_Pcrit_converted <- Tank1_Pcrit %>%
  rename(DOpercent = 1 , Phase = 2, Salinity = 3, Temperature = 4, Pressure = 5, O2mgl = 6)
Tank2_Pcrit_converted <- Tank2_Pcrit %>%
  rename(DOpercent = 1 , Phase = 2, Salinity = 3, Temperature = 4, Pressure = 5, O2mgl = 6)
Tank3_Pcrit_converted <- Tank3_Pcrit %>%
  rename(DOpercent = 1 , Phase = 2, Salinity = 3, Temperature = 4, Pressure = 5, O2mgl = 6)
Tank4_Pcrit_converted <- Tank4_Pcrit %>%
  rename(DOpercent = 1 , Phase = 2, Salinity = 3, Temperature = 4, Pressure = 5, O2mgl = 6)
Tank5_Pcrit_converted <- Tank5_Pcrit %>%
  rename(DOpercent = 1 , Phase = 2, Salinity = 3, Temperature = 4, Pressure = 5, O2mgl = 6)
Tank6_Pcrit_converted <- Tank6_Pcrit %>%
  rename(DOpercent = 1 , Phase = 2, Salinity = 3, Temperature = 4, Pressure = 5, O2mgl = 6)


## Subset data, so that it does not include Flush Period or after chamber was opened ##

Tank1_Pcrit_converted_subset <- subset(Tank1_Pcrit_converted, Phase != "Flush")
Tank1_Pcrit_converted_subset <- subset(Tank1_Pcrit_converted_subset, DOpercent < 90)

## Removes all data after chamber opened, by running the data through a rolling subtraction (at 120s). Number greater than 0 are showing that the chamber was opened ##

x1 <- Tank1_Pcrit_converted_subset$DOpercent
y1 <- 1:length(x1)

Tank1_Pcrit_converted_subset <- data.frame(Tank1_Pcrit_converted_subset %>%
                                             mutate(diffx = x1 - lag(x1, 180), diffy = y1 - lag(y1, 1)))

Tank1_Pcrit_converted_subset <- Tank1_Pcrit_converted_subset[c(1:first(which(Tank1_Pcrit_converted_subset$diffx > 0)))-1,]

Tank1_Pcrit_converted_subset <- subset(Tank1_Pcrit_converted_subset, DOpercent > LOE1)

## Tank 2 Subset ##

Tank2_Pcrit_converted_subset <- subset(Tank2_Pcrit_converted, Phase != "Flush")
Tank2_Pcrit_converted_subset <- subset(Tank2_Pcrit_converted_subset, DOpercent < 90)

x2 <- Tank2_Pcrit_converted_subset$DOpercent
y2 <- 1:length(x2)

Tank2_Pcrit_converted_subset <- data.frame(Tank2_Pcrit_converted_subset %>%
                                             mutate(diffx = x2 - lag(x2, 180), diffy = y2 - lag(y2, 1)))

Tank2_Pcrit_converted_subset <- Tank2_Pcrit_converted_subset[c(1:first(which(Tank2_Pcrit_converted_subset$diffx > 0)))-1,]

Tank2_Pcrit_converted_subset <- subset(Tank2_Pcrit_converted_subset, DOpercent > LOE2)

## Tank 3 Subset ##

Tank3_Pcrit_converted_subset <- subset(Tank3_Pcrit_converted, Phase != "Flush")
Tank3_Pcrit_converted_subset <- subset(Tank3_Pcrit_converted_subset, DOpercent < 90)

x3 <- Tank3_Pcrit_converted_subset$DOpercent
y3 <- 1:length(x3)

Tank3_Pcrit_converted_subset <- data.frame(Tank3_Pcrit_converted_subset %>%
                                             mutate(diffx = x3 - lag(x3, 180), diffy = y3 - lag(y3, 1)))

Tank3_Pcrit_converted_subset <- Tank3_Pcrit_converted_subset[c(1:first(which(Tank3_Pcrit_converted_subset$diffx > 0)))-1,]

Tank3_Pcrit_converted_subset <- subset(Tank3_Pcrit_converted_subset, DOpercent > LOE3)

## Tank 4 Subset ##

Tank4_Pcrit_converted_subset <- subset(Tank4_Pcrit_converted, Phase != "Flush")
Tank4_Pcrit_converted_subset <- subset(Tank4_Pcrit_converted_subset, DOpercent < 90)

x4 <- Tank4_Pcrit_converted_subset$DOpercent
y4 <- 1:length(x4)

Tank4_Pcrit_converted_subset <- data.frame(Tank4_Pcrit_converted_subset %>%
                                             mutate(diffx = x4 - lag(x4, 180), diffy = y4 - lag(y4, 1)))

Tank4_Pcrit_converted_subset <- Tank4_Pcrit_converted_subset[c(1:first(which(Tank4_Pcrit_converted_subset$diffx > 0)))-1,]

Tank4_Pcrit_converted_subset <- subset(Tank4_Pcrit_converted_subset, DOpercent > LOE4)

## Tank 5 Subset ##

Tank5_Pcrit_converted_subset <- subset(Tank5_Pcrit_converted, Phase != "Flush")
Tank5_Pcrit_converted_subset <- subset(Tank5_Pcrit_converted_subset, DOpercent < 90)

x5 <- Tank5_Pcrit_converted_subset$DOpercent
y5 <- 1:length(x5)

Tank5_Pcrit_converted_subset <- data.frame(Tank5_Pcrit_converted_subset %>%
                                             mutate(diffx = x5 - lag(x5, 180), diffy = y5 - lag(y5, 1)))

Tank5_Pcrit_converted_subset <- Tank5_Pcrit_converted_subset[c(1:first(which(Tank5_Pcrit_converted_subset$diffx > 0)))-1,]

Tank5_Pcrit_converted_subset <- subset(Tank5_Pcrit_converted_subset, DOpercent > LOE5)

## Tank 6 Subset ##

Tank6_Pcrit_converted_subset <- subset(Tank6_Pcrit_converted, Phase != "Flush")
Tank6_Pcrit_converted_subset <- subset(Tank6_Pcrit_converted_subset, DOpercent < 90)

x6 <- Tank6_Pcrit_converted_subset$DOpercent
y6 <- 1:length(x6)

Tank6_Pcrit_converted_subset <- data.frame(Tank6_Pcrit_converted_subset %>%
                                             mutate(diffx = x6 - lag(x6, 180), diffy = y6 - lag(y6, 1)))

Tank6_Pcrit_converted_subset <- Tank6_Pcrit_converted_subset[c(1:first(which(Tank6_Pcrit_converted_subset$diffx > 0)))-1,]

Tank6_Pcrit_converted_subset <- subset(Tank6_Pcrit_converted_subset, DOpercent > LOE6)

## This makes bins of varying sizes - larger bins at the higher DO and smaller bins at lower DO ##

Tank1_Pcrit_converted_subset$Duration <- Tank1_Pcrit_converted_subset$Loop/60
Tank2_Pcrit_converted_subset$Duration <- Tank2_Pcrit_converted_subset$Loop/60
Tank3_Pcrit_converted_subset$Duration <- Tank3_Pcrit_converted_subset$Loop/60
Tank4_Pcrit_converted_subset$Duration <- Tank4_Pcrit_converted_subset$Loop/60
Tank5_Pcrit_converted_subset$Duration <- Tank5_Pcrit_converted_subset$Loop/60
Tank6_Pcrit_converted_subset$Duration <- Tank6_Pcrit_converted_subset$Loop/60

## TANK1 MO2 Calc ##

Tank1_Pcrit_bins <- make_bins(o2 = Tank1_Pcrit_converted_subset$DOpercent,
                              duration = Tank1_Pcrit_converted_subset$Duration,
                              min_o2_width = 1/100,
                              max_o2_width = 1/5, n_bins = 8)
Tank1_Pcrit_MO2 <- calc_MO2(duration = Tank1_Pcrit_converted_subset$Duration,
                            o2 = Tank1_Pcrit_converted_subset$DOpercent,
                            bin_width = Tank1_Pcrit_bins,
                            vol = vr1,
                            temp = Tank1_Pcrit_converted_subset$Temperature,
                            sal = Tank1_Pcrit_converted_subset$Salinity)
Tank1_Pcrit_MO2_converted <- data.frame(Tank1_Pcrit_MO2 %>%
                                          mutate(conv_o2(o2 = Tank1_Pcrit_MO2$MO2, 
                                                         from = "umol_per_l", 
                                                         to = "mg_per_l", 
                                                         temp = mean(Tank1_Pcrit_converted_subset$Temperature), 
                                                         sal = mean(Tank1_Pcrit_converted_subset$Salinity), 
                                                         atm_pres = mean(Tank1_Pcrit_converted_subset$Pressure))))

Tank1_Pcrit_MO2_converted <- Tank1_Pcrit_MO2_converted %>%
  rename(MO2mgl = 9)
Tank1_Pcrit_MO2_converted <- data.frame(Tank1_Pcrit_MO2_converted %>%
                                          mutate(MO2mgl/vf1))
Tank1_Pcrit_MO2_converted <- Tank1_Pcrit_MO2_converted %>%
  rename(MO2mgkghr = 10)


Tank1_Pcrit_MO2_duration <- data.frame(Tank1_Pcrit_MO2_converted$DUR_RANGE)
Tank1_PCrit_MO2_duration_subset <- Tank1_Pcrit_MO2_duration %>%
  mutate(Tank1_Pcrit_MO2_converted.DUR_RANGE = strsplit(as.character(Tank1_Pcrit_MO2_converted.DUR_RANGE), "-")) %>%
  unnest(cols = Tank1_Pcrit_MO2_converted.DUR_RANGE)
Tank1_PCrit_MO2_duration_start <- Tank1_PCrit_MO2_duration_subset %>%
  filter(row_number() %% 2 == 1) ## Select odd rows
Tank1_PCrit_MO2_duration_start <- data.frame(Tank1_PCrit_MO2_duration_start)
Tank1_Pcrit_MO2_converted <- data.frame(Tank1_Pcrit_MO2_converted %>%
                                          mutate(as.numeric(Tank1_PCrit_MO2_duration_start$Tank1_Pcrit_MO2_converted.DUR_RANGE)))
Tank1_Pcrit_MO2_converted <- Tank1_Pcrit_MO2_converted %>%
  rename(DUR_START = 11)

## TANK2 MO2 Calc ##

Tank2_Pcrit_bins <- make_bins(o2 = Tank2_Pcrit_converted_subset$DOpercent,
                              duration = Tank2_Pcrit_converted_subset$Duration,
                              min_o2_width = 1/100,
                              max_o2_width = 1/5,
                              n_bins = 8)
Tank2_Pcrit_MO2 <- calc_MO2(duration = Tank2_Pcrit_converted_subset$Duration,
                            o2 = Tank2_Pcrit_converted_subset$DOpercent,
                            bin_width = Tank2_Pcrit_bins,
                            vol = vr2,
                            temp = Tank2_Pcrit_converted_subset$Temperature,
                            sal = Tank2_Pcrit_converted_subset$Salinity)
Tank2_Pcrit_MO2_converted <- data.frame(Tank2_Pcrit_MO2 %>%
                                          mutate(conv_o2(o2 = Tank2_Pcrit_MO2$MO2, 
                                                         from = "umol_per_l", 
                                                         to = "mg_per_l", 
                                                         temp = mean(Tank2_Pcrit_converted_subset$Temperature), 
                                                         sal = mean(Tank2_Pcrit_converted_subset$Salinity), 
                                                         atm_pres = mean(Tank2_Pcrit_converted_subset$Pressure))))

Tank2_Pcrit_MO2_converted <- Tank2_Pcrit_MO2_converted %>%
  rename(MO2mgl = 9)
Tank2_Pcrit_MO2_converted <- data.frame(Tank2_Pcrit_MO2_converted %>%
                                          mutate(MO2mgl/vf2))
Tank2_Pcrit_MO2_converted <- Tank2_Pcrit_MO2_converted %>%
  rename(MO2mgkghr = 10)


Tank2_Pcrit_MO2_duration <- data.frame(Tank2_Pcrit_MO2_converted$DUR_RANGE)
Tank2_PCrit_MO2_duration_subset <- Tank2_Pcrit_MO2_duration %>%
  mutate(Tank2_Pcrit_MO2_converted.DUR_RANGE = strsplit(as.character(Tank2_Pcrit_MO2_converted.DUR_RANGE), "-")) %>%
  unnest(cols = Tank2_Pcrit_MO2_converted.DUR_RANGE)
Tank2_PCrit_MO2_duration_start <- Tank2_PCrit_MO2_duration_subset %>%
  filter(row_number() %% 2 == 1) ## Select odd rows
Tank2_PCrit_MO2_duration_start <- data.frame(Tank2_PCrit_MO2_duration_start)
Tank2_Pcrit_MO2_converted <- data.frame(Tank2_Pcrit_MO2_converted %>%
                                          mutate(as.numeric(Tank2_PCrit_MO2_duration_start$Tank2_Pcrit_MO2_converted.DUR_RANGE)))
Tank2_Pcrit_MO2_converted <- Tank2_Pcrit_MO2_converted %>%
  rename(DUR_START = 11)

## TANK 3 MO2 Calc ##

Tank3_Pcrit_bins <- make_bins(o2 = Tank3_Pcrit_converted_subset$DOpercent,
                              duration = Tank3_Pcrit_converted_subset$Duration,
                              min_o2_width = 1/100,
                              max_o2_width = 1/5,
                              n_bins = 8)
Tank3_Pcrit_MO2 <- calc_MO2(duration = Tank3_Pcrit_converted_subset$Duration,
                            o2 = Tank3_Pcrit_converted_subset$DOpercent,
                            bin_width = Tank3_Pcrit_bins,
                            vol = vr3,
                            temp = Tank3_Pcrit_converted_subset$Temperature,
                            sal = Tank3_Pcrit_converted_subset$Salinity)
Tank3_Pcrit_MO2_converted <- data.frame(Tank3_Pcrit_MO2 %>%
                                          mutate(conv_o2(o2 = Tank3_Pcrit_MO2$MO2, 
                                                         from = "umol_per_l", 
                                                         to = "mg_per_l", 
                                                         temp = mean(Tank3_Pcrit_converted_subset$Temperature), 
                                                         sal = mean(Tank3_Pcrit_converted_subset$Salinity), 
                                                         atm_pres = mean(Tank3_Pcrit_converted_subset$Pressure))))

Tank3_Pcrit_MO2_converted <- Tank3_Pcrit_MO2_converted %>%
  rename(MO2mgl = 9)
Tank3_Pcrit_MO2_converted <- data.frame(Tank3_Pcrit_MO2_converted %>%
                                          mutate(MO2mgl/vf3))
Tank3_Pcrit_MO2_converted <- Tank3_Pcrit_MO2_converted %>%
  rename(MO2mgkghr = 10)


Tank3_Pcrit_MO2_duration <- data.frame(Tank3_Pcrit_MO2_converted$DUR_RANGE)
Tank3_PCrit_MO2_duration_subset <- Tank3_Pcrit_MO2_duration %>%
  mutate(Tank3_Pcrit_MO2_converted.DUR_RANGE = strsplit(as.character(Tank3_Pcrit_MO2_converted.DUR_RANGE), "-")) %>%
  unnest(cols = Tank3_Pcrit_MO2_converted.DUR_RANGE)
Tank3_PCrit_MO2_duration_start <- Tank3_PCrit_MO2_duration_subset %>%
  filter(row_number() %% 2 == 1) ## Select odd rows
Tank3_PCrit_MO2_duration_start <- data.frame(Tank3_PCrit_MO2_duration_start)
Tank3_Pcrit_MO2_converted <- data.frame(Tank3_Pcrit_MO2_converted %>%
                                          mutate(as.numeric(Tank3_PCrit_MO2_duration_start$Tank3_Pcrit_MO2_converted.DUR_RANGE)))
Tank3_Pcrit_MO2_converted <- Tank3_Pcrit_MO2_converted %>%
  rename(DUR_START = 11)


## TANK4 MO2 Calc ##

Tank4_Pcrit_bins <- make_bins(o2 = Tank4_Pcrit_converted_subset$DOpercent,
                              duration = Tank4_Pcrit_converted_subset$Duration,
                              min_o2_width = 1/100,
                              max_o2_width = 1/5,
                              n_bins = 8)
Tank4_Pcrit_MO2 <- calc_MO2(duration = Tank4_Pcrit_converted_subset$Duration,
                            o2 = Tank4_Pcrit_converted_subset$DOpercent,
                            bin_width = Tank4_Pcrit_bins,
                            vol = vr4,
                            temp = Tank4_Pcrit_converted_subset$Temperature,
                            sal = Tank4_Pcrit_converted_subset$Salinity)
Tank4_Pcrit_MO2_converted <- data.frame(Tank4_Pcrit_MO2 %>%
                                          mutate(conv_o2(o2 = Tank4_Pcrit_MO2$MO2, 
                                                         from = "umol_per_l", 
                                                         to = "mg_per_l", 
                                                         temp = mean(Tank4_Pcrit_converted_subset$Temperature), 
                                                         sal = mean(Tank4_Pcrit_converted_subset$Salinity), 
                                                         atm_pres = mean(Tank4_Pcrit_converted_subset$Pressure))))

Tank4_Pcrit_MO2_converted <- Tank4_Pcrit_MO2_converted %>%
  rename(MO2mgl = 9)
Tank4_Pcrit_MO2_converted <- data.frame(Tank4_Pcrit_MO2_converted %>%
                                          mutate(MO2mgl/vf4))
Tank4_Pcrit_MO2_converted <- Tank4_Pcrit_MO2_converted %>%
  rename(MO2mgkghr = 10)


Tank4_Pcrit_MO2_duration <- data.frame(Tank4_Pcrit_MO2_converted$DUR_RANGE)
Tank4_PCrit_MO2_duration_subset <- Tank4_Pcrit_MO2_duration %>%
  mutate(Tank4_Pcrit_MO2_converted.DUR_RANGE = strsplit(as.character(Tank4_Pcrit_MO2_converted.DUR_RANGE), "-")) %>%
  unnest(cols = Tank4_Pcrit_MO2_converted.DUR_RANGE)
Tank4_PCrit_MO2_duration_start <- Tank4_PCrit_MO2_duration_subset %>%
  filter(row_number() %% 2 == 1) ## Select odd rows
Tank4_PCrit_MO2_duration_start <- data.frame(Tank4_PCrit_MO2_duration_start)
Tank4_Pcrit_MO2_converted <- data.frame(Tank4_Pcrit_MO2_converted %>%
                                          mutate(as.numeric(Tank4_PCrit_MO2_duration_start$Tank4_Pcrit_MO2_converted.DUR_RANGE)))
Tank4_Pcrit_MO2_converted <- Tank4_Pcrit_MO2_converted %>%
  rename(DUR_START = 11)

## TANK5 MO2 Calc ##

Tank5_Pcrit_bins <- make_bins(o2 = Tank5_Pcrit_converted_subset$DOpercent,
                              duration = Tank5_Pcrit_converted_subset$Duration,
                              min_o2_width = 1/100,
                              max_o2_width = 1/5,
                              n_bins = 8)
Tank5_Pcrit_MO2 <- calc_MO2(duration = Tank5_Pcrit_converted_subset$Duration,
                            o2 = Tank5_Pcrit_converted_subset$DOpercent,
                            bin_width = Tank5_Pcrit_bins,
                            vol = vr5,
                            temp = Tank5_Pcrit_converted_subset$Temperature,
                            sal = Tank5_Pcrit_converted_subset$Salinity)
Tank5_Pcrit_MO2_converted <- data.frame(Tank5_Pcrit_MO2 %>%
                                          mutate(conv_o2(o2 = Tank5_Pcrit_MO2$MO2, 
                                                         from = "umol_per_l", 
                                                         to = "mg_per_l", 
                                                         temp = mean(Tank5_Pcrit_converted_subset$Temperature), 
                                                         sal = mean(Tank5_Pcrit_converted_subset$Salinity), 
                                                         atm_pres = mean(Tank5_Pcrit_converted_subset$Pressure))))

Tank5_Pcrit_MO2_converted <- Tank5_Pcrit_MO2_converted %>%
  rename(MO2mgl = 9)
Tank5_Pcrit_MO2_converted <- data.frame(Tank5_Pcrit_MO2_converted %>%
                                          mutate(MO2mgl/vf5))
Tank5_Pcrit_MO2_converted <- Tank5_Pcrit_MO2_converted %>%
  rename(MO2mgkghr = 10)


Tank5_Pcrit_MO2_duration <- data.frame(Tank5_Pcrit_MO2_converted$DUR_RANGE)
Tank5_PCrit_MO2_duration_subset <- Tank5_Pcrit_MO2_duration %>%
  mutate(Tank5_Pcrit_MO2_converted.DUR_RANGE = strsplit(as.character(Tank5_Pcrit_MO2_converted.DUR_RANGE), "-")) %>%
  unnest(cols = Tank5_Pcrit_MO2_converted.DUR_RANGE)
Tank5_PCrit_MO2_duration_start <- Tank5_PCrit_MO2_duration_subset %>%
  filter(row_number() %% 2 == 1) ## Select odd rows
Tank5_PCrit_MO2_duration_start <- data.frame(Tank5_PCrit_MO2_duration_start)
Tank5_Pcrit_MO2_converted <- data.frame(Tank5_Pcrit_MO2_converted %>%
                                          mutate(as.numeric(Tank5_PCrit_MO2_duration_start$Tank5_Pcrit_MO2_converted.DUR_RANGE)))
Tank5_Pcrit_MO2_converted <- Tank5_Pcrit_MO2_converted %>%
  rename(DUR_START = 11)

## TANK6 MO2 Calc ##

Tank6_Pcrit_bins <- make_bins(o2 = Tank6_Pcrit_converted_subset$DOpercent,
                              duration = Tank6_Pcrit_converted_subset$Duration,
                              min_o2_width = 1/100,
                              max_o2_width = 1/5,
                              n_bins = 8)
Tank6_Pcrit_MO2 <- calc_MO2(duration = Tank6_Pcrit_converted_subset$Duration,
                            o2 = Tank6_Pcrit_converted_subset$DOpercent,
                            bin_width = Tank6_Pcrit_bins,
                            vol = vr6,
                            temp = Tank6_Pcrit_converted_subset$Temperature,
                            sal = Tank6_Pcrit_converted_subset$Salinity)
Tank6_Pcrit_MO2_converted <- data.frame(Tank6_Pcrit_MO2 %>%
                                          mutate(conv_o2(o2 = Tank6_Pcrit_MO2$MO2, 
                                                         from = "umol_per_l", 
                                                         to = "mg_per_l", 
                                                         temp = mean(Tank6_Pcrit_converted_subset$Temperature), 
                                                         sal = mean(Tank6_Pcrit_converted_subset$Salinity), 
                                                         atm_pres = mean(Tank6_Pcrit_converted_subset$Pressure))))

Tank6_Pcrit_MO2_converted <- Tank6_Pcrit_MO2_converted %>%
  rename(MO2mgl = 9)
Tank6_Pcrit_MO2_converted <- data.frame(Tank6_Pcrit_MO2_converted %>%
                                          mutate(MO2mgl/vf6))
Tank6_Pcrit_MO2_converted <- Tank6_Pcrit_MO2_converted %>%
  rename(MO2mgkghr = 10)


Tank6_Pcrit_MO2_duration <- data.frame(Tank6_Pcrit_MO2_converted$DUR_RANGE)
Tank6_PCrit_MO2_duration_subset <- Tank6_Pcrit_MO2_duration %>%
  mutate(Tank6_Pcrit_MO2_converted.DUR_RANGE = strsplit(as.character(Tank6_Pcrit_MO2_converted.DUR_RANGE), "-")) %>%
  unnest(cols = Tank6_Pcrit_MO2_converted.DUR_RANGE)
Tank6_PCrit_MO2_duration_start <- Tank6_PCrit_MO2_duration_subset %>%
  filter(row_number() %% 2 == 1) ## Select odd rows
Tank6_PCrit_MO2_duration_start <- data.frame(Tank6_PCrit_MO2_duration_start)
Tank6_Pcrit_MO2_converted <- data.frame(Tank6_Pcrit_MO2_converted %>%
                                          mutate(as.numeric(Tank6_PCrit_MO2_duration_start$Tank6_Pcrit_MO2_converted.DUR_RANGE)))
Tank6_Pcrit_MO2_converted <- Tank6_Pcrit_MO2_converted %>%
  rename(DUR_START = 11)

## This calculates the metabolic rate subtracting the background at each loop

Tank1_Pcrit_MO2_converted_final <- subset(Tank1_Pcrit_MO2_converted, MO2mgkghr > 0)
Tank2_Pcrit_MO2_converted_final <- subset(Tank2_Pcrit_MO2_converted, MO2mgkghr > 0)
Tank3_Pcrit_MO2_converted_final <- subset(Tank3_Pcrit_MO2_converted, MO2mgkghr > 0)
Tank4_Pcrit_MO2_converted_final <- subset(Tank4_Pcrit_MO2_converted, MO2mgkghr > 0)
Tank5_Pcrit_MO2_converted_final <- subset(Tank5_Pcrit_MO2_converted, MO2mgkghr > 0)
Tank6_Pcrit_MO2_converted_final <- subset(Tank6_Pcrit_MO2_converted, MO2mgkghr > 0)

## This calculates and plots PCrit ##

calc_pcrit(Tank1_Pcrit_MO2_converted_final$O2_MEAN, Tank1_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR1)
calc_pcrit(Tank2_Pcrit_MO2_converted_final$O2_MEAN, Tank2_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR2)
calc_pcrit(Tank3_Pcrit_MO2_converted_final$O2_MEAN, Tank3_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR3)
calc_pcrit(Tank4_Pcrit_MO2_converted_final$O2_MEAN, Tank4_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR4)
calc_pcrit(Tank5_Pcrit_MO2_converted_final$O2_MEAN, Tank5_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR5)
calc_pcrit(Tank6_Pcrit_MO2_converted_final$O2_MEAN, Tank6_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR6)


combined_PCrit_FinalData <- cbind(calc_pcrit(Tank1_Pcrit_MO2_converted_final$O2_MEAN, Tank1_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR1),
                                  calc_pcrit(Tank2_Pcrit_MO2_converted_final$O2_MEAN, Tank2_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR2),
                                  calc_pcrit(Tank3_Pcrit_MO2_converted_final$O2_MEAN, Tank3_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR3),
                                  calc_pcrit(Tank4_Pcrit_MO2_converted_final$O2_MEAN, Tank4_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR4),
                                  calc_pcrit(Tank5_Pcrit_MO2_converted_final$O2_MEAN, Tank5_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR5),
                                  calc_pcrit(Tank6_Pcrit_MO2_converted_final$O2_MEAN, Tank6_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR6))

combined_PCrit_FinalData <- data.frame(combined_PCrit_FinalData) %>%
  rename(Tank_1 = 1,
         Tank_2 = 2,
         Tank_3 = 3,
         Tank_4 = 4,
         Tank_5 = 5,
         Tank_6 = 6)

write_csv(combined_PCrit_FinalData,'15C_1_PCrit_6month_06262023.csv')

tiff(filename = "Tank1_Final_PCrit_15C_1_06262023.tiff",
     width = 6, height = 6, units = "in", res = 600)
plot_pcrit(Tank1_Pcrit_MO2_converted_final$O2_MEAN, Tank1_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR1)
dev.off()

tiff(filename = "Tank2_Final_PCrit_15C_1_06262023.tiff",
     width = 6, height = 6, units = "in", res = 600)
plot_pcrit(Tank2_Pcrit_MO2_converted_final$O2_MEAN, Tank2_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR2)
dev.off()

tiff(filename = "Tank3_Final_PCrit_15C_1_06262023.tiff",
     width = 6, height = 6, units = "in", res = 600)
plot_pcrit(Tank3_Pcrit_MO2_converted_final$O2_MEAN, Tank3_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR3)
dev.off()

tiff(filename = "Tank4_Final_PCrit_15C_1_06262023.tiff",
     width = 6, height = 6, units = "in", res = 600)
plot_pcrit(Tank4_Pcrit_MO2_converted_final$O2_MEAN, Tank4_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR4)
dev.off()

tiff(filename = "Tank5_Final_PCrit_15C_1_06262023.tiff",
     width = 6, height = 6, units = "in", res = 600)
plot_pcrit(Tank5_Pcrit_MO2_converted_final$O2_MEAN, Tank5_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR5)
dev.off()

tiff(filename = "Tank6_Final_PCrit_15C_1_06262023.tiff",
     width = 6, height = 6, units = "in", res = 600)
plot_pcrit(Tank6_Pcrit_MO2_converted_final$O2_MEAN, Tank6_Pcrit_MO2_converted_final$MO2mgkghr, MR = MR6)
dev.off()

## Figures for Data Cleaning Confirmation - NOT REQUIRED FOR ANALYSIS ##

## Initial Quality Control

ggplot(Tank1_Pcrit_converted, aes(x = Loop, y = DOpercent)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 1 Trial 15C 1", x = "Time", y = "O2 (%)") +
  theme_classic()

ggsave('PCrit_FullRun_Tank1_15C_1_qualitycontrol.tiff')

ggplot(Tank2_Pcrit_converted, aes(x = Loop, y = DOpercent)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 2 Trial 15C 1", x = "Time", y = "O2 (%)") +
  theme_classic()

ggsave('PCrit_FullRun_Tank2_15C_1_qualitycontrol.tiff')

ggplot(Tank3_Pcrit_converted, aes(x = Loop, y = DOpercent)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 3 Trial 15C 1", x = "Time", y = "O2 (%)") +
  theme_classic()

ggsave('PCrit_FullRun_Tank3_15C_1_qualitycontrol.tiff')

ggplot(Tank4_Pcrit_converted, aes(x = Loop, y = DOpercent)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 4 Trial 15C 1", x = "Time", y = "O2 (%)") +
  theme_classic()

ggsave('PCrit_FullRun_Tank4_15C_1_qualitycontrol.tiff')

ggplot(Tank5_Pcrit_converted, aes(x = Loop, y = DOpercent)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 5 Trial 15C 1", x = "Time", y = "O2 (%)") +
  theme_classic()

ggsave('PCrit_FullRun_Tank5_15C_1_qualitycontrol.tiff')

ggplot(Tank6_Pcrit_converted, aes(x = Loop, y = DOpercent)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 6 Trial 15C 1", x = "Time", y = "O2 (%)") +
  theme_classic()

ggsave('PCrit_FullRun_Tank6_15C_1_qualitycontrol.tiff')


## This creates graphs of cleaned up data for further quality control ##

ggplot(Tank1_Pcrit_converted_subset, aes(x = Loop, y = DOpercent)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 1 Trial 15C 1", x = "Time", y = "O2 (%)") +
  theme_classic()

ggsave('PCrit_FullRun_Tank1_15C_1_postqualitycontrol.tiff')

ggplot(Tank2_Pcrit_converted_subset, aes(x = Loop, y = DOpercent)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 2 Trial 15C 1", x = "Time", y = "O2 (%)") +
  theme_classic()

ggsave('PCrit_FullRun_Tank2_15C_1_postqualitycontrol.tiff')

ggplot(Tank3_Pcrit_converted_subset, aes(x = Loop, y = DOpercent)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 3 Trial 15C 1", x = "Time", y = "O2 (%)") +
  theme_classic()

ggsave('PCrit_FullRun_Tank3_15C_1_postqualitycontrol.tiff')

ggplot(Tank4_Pcrit_converted_subset, aes(x = Loop, y = DOpercent)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 4 Trial 15C 1", x = "Time", y = "O2 (%)") +
  theme_classic()

ggsave('PCrit_FullRun_Tank4_15C_1_postqualitycontrol.tiff')

ggplot(Tank5_Pcrit_converted_subset, aes(x = Loop, y = DOpercent)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 5 Trial 15C 1", x = "Time", y = "O2 (%)") +
  theme_classic()

ggsave('PCrit_FullRun_Tank5_15C_1_postqualitycontrol.tiff')

ggplot(Tank6_Pcrit_converted_subset, aes(x = Loop, y = DOpercent)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 6 Trial 15C 1", x = "Time", y = "O2 (%)") +
  theme_classic()

ggsave('PCrit_FullRun_Tank6_15C_1_postqualitycontrol.tiff')

## This plots MO2 vs O2 ##

ggplot(Tank1_Pcrit_MO2_converted_final, aes(x = O2_MEAN, y = MO2mgkghr)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 1 Trial 15C 1", x = "O2 (%)", y = "MO2") +
  theme_classic()

ggsave('PCrit_Tank1_15C_1_06262023.tiff')

ggplot(Tank2_Pcrit_MO2_converted_final, aes(x = O2_MEAN, y = MO2mgkghr)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 2 Trial 15C 1", x = "O2 (%)", y = "MO2") +
  theme_classic()

ggsave('PCrit_Tank2_15C_1_06262023.tiff')

ggplot(Tank3_Pcrit_MO2_converted_final, aes(x = O2_MEAN, y = MO2mgkghr)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 3 Trial 15C 1", x = "O2 (%)", y = "MO2") +
  theme_classic()

ggsave('PCrit_Tank3_15C_1_06262023.tiff')

ggplot(Tank4_Pcrit_MO2_converted_final, aes(x = O2_MEAN, y = MO2mgkghr)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 4 Trial 15C 1", x = "O2 (%)", y = "MO2") +
  theme_classic()

ggsave('PCrit_Tank4_15C_1_06262023.tiff')

ggplot(Tank5_Pcrit_MO2_converted_final, aes(x = O2_MEAN, y = MO2mgkghr)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 5 Trial 15C 1", x = "O2 (%)", y = "MO2") +
  theme_classic()

ggsave('PCrit_Tank5_15C_1_06262023.tiff')

ggplot(Tank6_Pcrit_MO2_converted_final, aes(x = O2_MEAN, y = MO2mgkghr)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Pcrit Tank 6 Trial 15C 1", x = "O2 (%)", y = "MO2") +
  theme_classic()

ggsave('PCrit_Tank6_15C_1_06262023.tiff')
