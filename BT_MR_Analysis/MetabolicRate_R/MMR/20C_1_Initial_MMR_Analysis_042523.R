# Install respR, the package we need to convert from %DO to mg/L
install.packages("devtools")
install.packages("cowplot")
devtools::install_github("januarharianto/respR")

# Libraries we need
library("readr")
library("tidyverse")
library("rollRegres")
library("respR")
library("cowplot")



############################ ---------------------------------- ################################


# Read in raw data, rename columns so they're easier to work with, add a TimeMinutes
# column to represent the time each O2 reading was taken. The Date&Time column in the 
# raw data itself is too annoying to work with. 

MMR <- read.delim("MMR_1_20C_raw.txt")
write_csv(MMR,'MMR_1_20C.csv')
MMR_1_raw_df <- read_delim("MMR_1_20C.csv", 
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
         Ch7_DO = `CH7 O2 [%air sat.]`, 
         Ch8_DO = `CH8 O2 [%air sat.]`,
         Ch1_temp = `CH1 temp (<b0>C)`, 
         Ch2_temp = `CH2 temp (<b0>C)`,
         Ch3_temp = `CH3 temp (<b0>C)`, 
         Ch4_temp = `CH4 temp (<b0>C)`,
         Ch5_temp = `CH5 temp (<b0>C)`, 
         Ch6_temp = `CH6 temp (<b0>C)`,
         Ch7_temp = `CH7 temp (<b0>C)`, 
         Ch8_temp = `CH8 temp (<b0>C)`,
         TimeMinutes = seq(0, by=1/60, length.out=nrow(.)),) %>%
  dplyr::select(TimeMinutes, Phase, 
                Ch1_DO, Ch2_DO, Ch3_DO, Ch4_DO, Ch5_DO, Ch6_DO, Ch7_DO, Ch8_DO,
                Ch1_temp, Ch2_temp, Ch3_temp, Ch4_temp, Ch5_temp, Ch6_temp, Ch7_temp, Ch8_temp,
                DateTime, patm, Salinity) %>%
  filter(TimeMinutes <= 90) # Filter to just the first x mins of the df. Using the entire thing makes things run slow later

# Convert % DO to mg/L 
Salinity_mean <- mean(MMR_1_raw_df$Salinity)
Pressure_mean <- mean(MMR_1_raw_df$patm)/1000

MMR_1_converted_df2 <- MMR_1_raw_df %>% 
  mutate(Ch1_O2mgL = convert_DO(x= MMR_1_raw_df$Ch1_DO, from = "%Air", to = "mg/L", S = Salinity_mean, 
                                t = MMR_1_raw_df$Ch1_temp, P = Pressure_mean)) %>%
  mutate(Ch2_O2mgL = convert_DO(x= MMR_1_raw_df$Ch2_DO, from = "%Air", to = "mg/L", S = Salinity_mean, 
                                t = MMR_1_raw_df$Ch2_temp, P = Pressure_mean)) %>%
  mutate(Ch3_O2mgL = convert_DO(x= MMR_1_raw_df$Ch3_DO, from = "%Air", to = "mg/L", S = Salinity_mean, 
                                t = MMR_1_raw_df$Ch3_temp, P = Pressure_mean)) %>%
  mutate(Ch4_O2mgL = convert_DO(x= MMR_1_raw_df$Ch4_DO, from = "%Air", to = "mg/L", S = Salinity_mean, 
                                t = MMR_1_raw_df$Ch4_temp, P = Pressure_mean)) %>%
  mutate(Ch5_O2mgL = convert_DO(x= MMR_1_raw_df$Ch5_DO, from = "%Air", to = "mg/L", S = Salinity_mean, 
                                t = MMR_1_raw_df$Ch5_temp, P = Pressure_mean)) %>%
  mutate(Ch6_O2mgL = convert_DO(x= MMR_1_raw_df$Ch6_DO, from = "%Air", to = "mg/L", S = Salinity_mean, 
                                t = MMR_1_raw_df$Ch6_temp, P = Pressure_mean)) %>%
  mutate(Ch7_O2mgL = convert_DO(x= MMR_1_raw_df$Ch7_DO, from = "%Air", to = "mg/L", S = Salinity_mean, 
                                t = MMR_1_raw_df$Ch7_temp, P = Pressure_mean)) %>%
  mutate(Ch8_O2mgL = convert_DO(x= MMR_1_raw_df$Ch8_DO, from = "%Air", to = "mg/L", S = Salinity_mean, 
                                t = MMR_1_raw_df$Ch8_temp, P = Pressure_mean))

MMR_1_converted_df <- subset(MMR_1_converted_df2, Phase!="Flush")

# Plot to see what the traces look like
par(mfrow=c(4,2))

sp1 <- ggplot(data = MMR_1_converted_df, aes(x = TimeMinutes, y = Ch1_O2mgL)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 110, 0.1)) + 
  scale_x_continuous(breaks = seq(0, 2000, 10)) + theme_bw()

sp2 <- ggplot(data = MMR_1_converted_df, aes(x = TimeMinutes, y = Ch2_O2mgL)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 110, 0.1)) + 
  scale_x_continuous(breaks = seq(0, 2000, 10)) + theme_bw()

sp3 <- ggplot(data = MMR_1_converted_df, aes(x = TimeMinutes, y = Ch3_O2mgL)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 110, 0.1)) + 
  scale_x_continuous(breaks = seq(0, 2000, 10)) + theme_bw()

sp4 <- ggplot(data = MMR_1_converted_df, aes(x = TimeMinutes, y = Ch4_O2mgL)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 110, 0.1)) + 
  scale_x_continuous(breaks = seq(0, 2000, 10)) + theme_bw()

sp5 <- ggplot(data = MMR_1_converted_df, aes(x = TimeMinutes, y = Ch5_O2mgL)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 110, 0.1)) + 
  scale_x_continuous(breaks = seq(0, 2000, 10)) + theme_bw()

sp6 <- ggplot(data = MMR_1_converted_df, aes(x = TimeMinutes, y = Ch6_O2mgL)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 110, 0.1)) + 
  scale_x_continuous(breaks = seq(0, 2000, 10)) + theme_bw()

sp7 <- ggplot(data = MMR_1_converted_df, aes(x = TimeMinutes, y = Ch7_O2mgL)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 110, 0.1)) + 
  scale_x_continuous(breaks = seq(0, 2000, 10)) + theme_bw()

sp8 <- ggplot(data = MMR_1_converted_df, aes(x = TimeMinutes, y = Ch8_O2mgL)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 110, 0.1)) + 
  scale_x_continuous(breaks = seq(0, 2000, 10)) + theme_bw()

## this puts everything in a grid, so that it is easy to see
plot_grid(sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8,
          labels= "auto",
          align = "h",
          label_size = 14,
          label_colour = "red",
          ncol=4,nrow=2)

ggsave('MMR_all_20C_1_082721.tiff', width = 8, height = 4, units = "in")

# -- Calculate MMR using rolling regression for one of the channels -- #
# Set chamber and fish volume
vr1 <- .6564 # Chamber volume in litres
vr2 <- .6592
vr3 <- .654
vr4 <- .6586
vr5 <- .6576
vr6 <- .6592
vr7 <- .6544
vr8 <- .6564

vf1 <- .006 # Fish volume in litres (1L = 1kg)
vf2 <- .007
vf3 <- .0063
vf4 <- .0038
vf5 <- .0046
vf6 <- .0067
vf7 <- .0049
vf8 <- .0068

# Rolling regression - this model estimates every possible regression widow within the trace, 
# filters out the steepest slope (highest O2 consumption rate) and uses that with vr and vf
# to estimate MMR
BT_1_RR_df_1minute <- as.data.frame(roll_regres(Ch1_O2mgL ~ TimeMinutes, 
                                             MMR_1_converted_df, 
                                             do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                             width = 60)) %>% # Set regression window to 1 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr1 - vf1) * coefs.TimeMinutes)/vf1)* -60)) %>%
  slice(which.max(MMR))

BT_1_RR_df_2minute <- as.data.frame(roll_regres(Ch1_O2mgL ~ TimeMinutes, 
                                                     MMR_1_converted_df, 
                                                     do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                     width = 120)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr1 - vf1) * coefs.TimeMinutes)/vf1)* -60)) %>%
  slice(which.max(MMR))

BT_1_RR_df_3minute <- as.data.frame(roll_regres(Ch1_O2mgL ~ TimeMinutes, 
                                             MMR_1_converted_df, 
                                             do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                             width = 180)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr1 - vf1) * coefs.TimeMinutes)/vf1)* -60)) %>%
  slice(which.max(MMR))

BT_1_RR_df_5minute <- as.data.frame(roll_regres(Ch1_O2mgL ~ TimeMinutes, 
                                             MMR_1_converted_df, 
                                             do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                             width = 300)) %>% # Set regression window to 5 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr1 - vf1) * coefs.TimeMinutes)/vf1)* -60)) %>%
  slice(which.max(MMR))

BT_2_RR_df_1minute <- as.data.frame(roll_regres(Ch2_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 60)) %>% # Set regression window to 1 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr2 - vf2) * coefs.TimeMinutes)/vf2)* -60)) %>%
  slice(which.max(MMR))

BT_2_RR_df_2minute <- as.data.frame(roll_regres(Ch2_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 120)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr2 - vf2) * coefs.TimeMinutes)/vf2)* -60)) %>%
  slice(which.max(MMR))

BT_2_RR_df_3minute <- as.data.frame(roll_regres(Ch2_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 180)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr2 - vf2) * coefs.TimeMinutes)/vf2)* -60)) %>%
  slice(which.max(MMR))

BT_2_RR_df_5minute <- as.data.frame(roll_regres(Ch2_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 300)) %>% # Set regression window to 5 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr2 - vf2) * coefs.TimeMinutes)/vf2)* -60)) %>%
  slice(which.max(MMR))

BT_3_RR_df_1minute <- as.data.frame(roll_regres(Ch3_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 60)) %>% # Set regression window to 1 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr3 - vf3) * coefs.TimeMinutes)/vf3)* -60)) %>%
  slice(which.max(MMR))

BT_3_RR_df_2minute <- as.data.frame(roll_regres(Ch3_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 120)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr3 - vf3) * coefs.TimeMinutes)/vf3)* -60)) %>%
  slice(which.max(MMR))

BT_3_RR_df_3minute <- as.data.frame(roll_regres(Ch3_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 180)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr3 - vf3) * coefs.TimeMinutes)/vf3)* -60)) %>%
  slice(which.max(MMR))

BT_3_RR_df_5minute <- as.data.frame(roll_regres(Ch3_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 300)) %>% # Set regression window to 5 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr3 - vf3) * coefs.TimeMinutes)/vf3)* -60)) %>%
  slice(which.max(MMR))

BT_4_RR_df_1minute <- as.data.frame(roll_regres(Ch4_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 60)) %>% # Set regression window to 1 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr4 - vf4) * coefs.TimeMinutes)/vf4)* -60)) %>%
  slice(which.max(MMR))

BT_4_RR_df_2minute <- as.data.frame(roll_regres(Ch4_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 120)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr4 - vf4) * coefs.TimeMinutes)/vf4)* -60)) %>%
  slice(which.max(MMR))

BT_4_RR_df_3minute <- as.data.frame(roll_regres(Ch4_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 180)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr4 - vf4) * coefs.TimeMinutes)/vf4)* -60)) %>%
  slice(which.max(MMR))

BT_4_RR_df_5minute <- as.data.frame(roll_regres(Ch4_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 300)) %>% # Set regression window to 5 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr4 - vf4) * coefs.TimeMinutes)/vf4)* -60)) %>%
  slice(which.max(MMR))

BT_5_RR_df_1minute <- as.data.frame(roll_regres(Ch5_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 60)) %>% # Set regression window to 1 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr5 - vf5) * coefs.TimeMinutes)/vf5)* -60)) %>%
  slice(which.max(MMR))

BT_5_RR_df_2minute <- as.data.frame(roll_regres(Ch5_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 120)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr5 - vf5) * coefs.TimeMinutes)/vf5)* -60)) %>%
  slice(which.max(MMR))

BT_5_RR_df_3minute <- as.data.frame(roll_regres(Ch5_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 180)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr5 - vf5) * coefs.TimeMinutes)/vf5)* -60)) %>%
  slice(which.max(MMR))

BT_5_RR_df_5minute <- as.data.frame(roll_regres(Ch5_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 300)) %>% # Set regression window to 5 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr5 - vf5) * coefs.TimeMinutes)/vf5)* -60)) %>%
  slice(which.max(MMR))

BT_6_RR_df_1minute <- as.data.frame(roll_regres(Ch6_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 60)) %>% # Set regression window to 1 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr6 - vf6) * coefs.TimeMinutes)/vf6)* -60)) %>%
  slice(which.max(MMR))

BT_6_RR_df_2minute <- as.data.frame(roll_regres(Ch6_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 120)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr6 - vf6) * coefs.TimeMinutes)/vf6)* -60)) %>%
  slice(which.max(MMR))

BT_6_RR_df_3minute <- as.data.frame(roll_regres(Ch6_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 180)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr6 - vf6) * coefs.TimeMinutes)/vf6)* -60)) %>%
  slice(which.max(MMR))

BT_6_RR_df_5minute <- as.data.frame(roll_regres(Ch6_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 300)) %>% # Set regression window to 5 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr6 - vf6) * coefs.TimeMinutes)/vf6)* -60)) %>%
  slice(which.max(MMR))

BT_7_RR_df_1minute <- as.data.frame(roll_regres(Ch7_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 60)) %>% # Set regression window to 1 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr7 - vf7) * coefs.TimeMinutes)/vf7)* -60)) %>%
  slice(which.max(MMR))

BT_7_RR_df_2minute <- as.data.frame(roll_regres(Ch7_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 120)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr7 - vf7) * coefs.TimeMinutes)/vf7)* -60)) %>%
  slice(which.max(MMR))

BT_7_RR_df_3minute <- as.data.frame(roll_regres(Ch7_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 180)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr7 - vf7) * coefs.TimeMinutes)/vf7)* -60)) %>%
  slice(which.max(MMR))

BT_7_RR_df_5minute <- as.data.frame(roll_regres(Ch7_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 300)) %>% # Set regression window to 5 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr7 - vf7) * coefs.TimeMinutes)/vf7)* -60)) %>%
  slice(which.max(MMR))

BT_8_RR_df_1minute <- as.data.frame(roll_regres(Ch8_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 60)) %>% # Set regression window to 1 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr8 - vf8) * coefs.TimeMinutes)/vf8)* -60)) %>%
  slice(which.max(MMR))

BT_8_RR_df_2minute <- as.data.frame(roll_regres(Ch8_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 120)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr8 - vf8) * coefs.TimeMinutes)/vf8)* -60)) %>%
  slice(which.max(MMR))

BT_8_RR_df_3minute <- as.data.frame(roll_regres(Ch8_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 180)) %>% # Set regression window to 3 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr8 - vf8) * coefs.TimeMinutes)/vf8)* -60)) %>%
  slice(which.max(MMR))

BT_8_RR_df_5minute <- as.data.frame(roll_regres(Ch8_O2mgL ~ TimeMinutes, 
                                                MMR_1_converted_df, 
                                                do_compute = c("sigmas", "r.squareds", "1_step_forecasts"), 
                                                width = 300)) %>% # Set regression window to 5 minute
  filter(!is.na(sigmas)) %>% 
  mutate(RegressionTime = seq(0, by=1/60, length.out=nrow(.)),) %>% 
  mutate(MMR = ((((vr8 - vf8) * coefs.TimeMinutes)/vf8)* -60)) %>%
  slice(which.max(MMR))

# --- Highest 1-minute window width MMR estimate (mg/kg/L)--- #
BT_Ch1_MMR_1minute <- BT_1_RR_df_1minute$MMR
BT_Ch1_MMR_1minute

BT_Ch1_MMR_2minute <- BT_1_RR_df_2minute$MMR
BT_Ch1_MMR_2minute

BT_Ch1_MMR_3minute <- BT_1_RR_df_3minute$MMR
BT_Ch1_MMR_3minute

BT_Ch1_MMR_5minute <- BT_1_RR_df_5minute$MMR
BT_Ch1_MMR_5minute

BT_Ch2_MMR_1minute <- BT_2_RR_df_1minute$MMR
BT_Ch2_MMR_1minute

BT_Ch2_MMR_2minute <- BT_2_RR_df_2minute$MMR
BT_Ch2_MMR_2minute

BT_Ch2_MMR_3minute <- BT_2_RR_df_3minute$MMR
BT_Ch2_MMR_3minute

BT_Ch2_MMR_5minute <- BT_2_RR_df_5minute$MMR
BT_Ch2_MMR_5minute

BT_Ch3_MMR_1minute <- BT_3_RR_df_1minute$MMR
BT_Ch3_MMR_1minute

BT_Ch3_MMR_2minute <- BT_3_RR_df_2minute$MMR
BT_Ch3_MMR_2minute

BT_Ch3_MMR_3minute <- BT_3_RR_df_3minute$MMR
BT_Ch3_MMR_3minute

BT_Ch3_MMR_5minute <- BT_3_RR_df_5minute$MMR
BT_Ch3_MMR_5minute       

BT_Ch4_MMR_1minute <- BT_4_RR_df_1minute$MMR
BT_Ch4_MMR_1minute

BT_Ch4_MMR_2minute <- BT_4_RR_df_2minute$MMR
BT_Ch4_MMR_2minute

BT_Ch4_MMR_3minute <- BT_4_RR_df_3minute$MMR
BT_Ch4_MMR_3minute

BT_Ch4_MMR_5minute <- BT_4_RR_df_5minute$MMR
BT_Ch4_MMR_5minute  

BT_Ch5_MMR_1minute <- BT_5_RR_df_1minute$MMR
BT_Ch5_MMR_1minute

BT_Ch5_MMR_2minute <- BT_5_RR_df_2minute$MMR
BT_Ch5_MMR_2minute

BT_Ch5_MMR_3minute <- BT_5_RR_df_3minute$MMR
BT_Ch5_MMR_3minute

BT_Ch5_MMR_5minute <- BT_5_RR_df_5minute$MMR
BT_Ch5_MMR_5minute

BT_Ch6_MMR_1minute <- BT_6_RR_df_1minute$MMR
BT_Ch6_MMR_1minute

BT_Ch6_MMR_2minute <- BT_6_RR_df_2minute$MMR
BT_Ch6_MMR_2minute

BT_Ch6_MMR_3minute <- BT_6_RR_df_3minute$MMR
BT_Ch6_MMR_3minute

BT_Ch6_MMR_5minute <- BT_6_RR_df_5minute$MMR
BT_Ch6_MMR_5minute

BT_Ch7_MMR_1minute <- BT_7_RR_df_1minute$MMR
BT_Ch7_MMR_1minute

BT_Ch7_MMR_2minute <- BT_7_RR_df_2minute$MMR
BT_Ch7_MMR_2minute

BT_Ch7_MMR_3minute <- BT_7_RR_df_3minute$MMR
BT_Ch7_MMR_3minute

BT_Ch7_MMR_5minute <- BT_7_RR_df_5minute$MMR
BT_Ch7_MMR_5minute  

BT_Ch8_MMR_1minute <- BT_8_RR_df_1minute$MMR
BT_Ch8_MMR_1minute

BT_Ch8_MMR_2minute <- BT_8_RR_df_2minute$MMR
BT_Ch8_MMR_2minute

BT_Ch8_MMR_3minute <- BT_8_RR_df_3minute$MMR
BT_Ch8_MMR_3minute

BT_Ch8_MMR_5minute <- BT_8_RR_df_5minute$MMR
BT_Ch8_MMR_5minute  

# --- Merges data frame for analysis of MMR calculations 

mergedBT_RR_df_ch1 <- data.frame(c(BT_Ch1_MMR_1minute, BT_Ch1_MMR_2minute, BT_Ch1_MMR_3minute, BT_Ch1_MMR_5minute))
mergedBT_RR_df_ch2 <- data.frame(c(BT_Ch2_MMR_1minute, BT_Ch2_MMR_2minute, BT_Ch2_MMR_3minute, BT_Ch2_MMR_5minute))
mergedBT_RR_df_ch3 <- data.frame(c(BT_Ch3_MMR_1minute, BT_Ch3_MMR_2minute, BT_Ch3_MMR_3minute, BT_Ch3_MMR_5minute))
mergedBT_RR_df_ch4 <- data.frame(c(BT_Ch4_MMR_1minute, BT_Ch4_MMR_2minute, BT_Ch4_MMR_3minute, BT_Ch4_MMR_5minute))
mergedBT_RR_df_ch5 <- data.frame(c(BT_Ch5_MMR_1minute, BT_Ch5_MMR_2minute, BT_Ch5_MMR_3minute, BT_Ch5_MMR_5minute))
mergedBT_RR_df_ch6 <- data.frame(c(BT_Ch6_MMR_1minute, BT_Ch6_MMR_2minute, BT_Ch6_MMR_3minute, BT_Ch6_MMR_5minute))
mergedBT_RR_df_ch7 <- data.frame(c(BT_Ch7_MMR_1minute, BT_Ch7_MMR_2minute, BT_Ch7_MMR_3minute, BT_Ch7_MMR_5minute))
mergedBT_RR_df_ch8 <- data.frame(c(BT_Ch8_MMR_1minute, BT_Ch8_MMR_2minute, BT_Ch8_MMR_3minute, BT_Ch8_MMR_5minute))


mergedBT_RR_df_ch1 <- mergedBT_RR_df_ch1 %>% 
  rename(Ch1_MMR = 1)
mergedBT_RR_df_ch2 <- mergedBT_RR_df_ch2 %>% 
  rename(Ch2_MMR = 1)
mergedBT_RR_df_ch3 <- mergedBT_RR_df_ch3 %>% 
  rename(Ch3_MMR = 1)
mergedBT_RR_df_ch4 <- mergedBT_RR_df_ch4 %>% 
  rename(Ch4_MMR = 1)
mergedBT_RR_df_ch5 <- mergedBT_RR_df_ch5 %>% 
  rename(Ch5_MMR = 1)
mergedBT_RR_df_ch6 <- mergedBT_RR_df_ch6 %>% 
  rename(Ch6_MMR = 1)
mergedBT_RR_df_ch7 <- mergedBT_RR_df_ch7 %>% 
  rename(Ch7_MMR = 1)
mergedBT_RR_df_ch8 <- mergedBT_RR_df_ch8 %>% 
  rename(Ch8_MMR = 1)

mergedBT_RR_df <- cbind(c('1 minute','2 minute','3 minute', '5 minute'), 
                             mergedBT_RR_df_ch1$Ch1_MMR, 
                             mergedBT_RR_df_ch2$Ch2_MMR, 
                             mergedBT_RR_df_ch3$Ch3_MMR, 
                             mergedBT_RR_df_ch4$Ch4_MMR,
                             mergedBT_RR_df_ch5$Ch5_MMR, 
                             mergedBT_RR_df_ch6$Ch6_MMR, 
                             mergedBT_RR_df_ch7$Ch7_MMR, 
                             mergedBT_RR_df_ch8$Ch8_MMR)

mergedBT_RR_df <- data.frame(mergedBT_RR_df)

mergedBT_RR_df <- mergedBT_RR_df %>% 
  rename(TimePeriod = 1,
         Ch1_MMR = 2,
         Ch2_MMR = 3,
         Ch3_MMR = 4,
         Ch4_MMR = 5,
         Ch5_MMR = 6,
         Ch6_MMR = 7,
         Ch7_MMR = 8,
         Ch8_MMR = 9)

write_csv(mergedBT_RR_df,'_MMR_all_20C_1_042523.csv')
view(mergedBT_RR_df)

# --- MMR calculation

meanMMR <- mean(mergedBT_RR_df$MMR)

sdMMR <- sd(mergedBT_RR_df$MMR)

semMMR <- sd(mergedBT_RR_df$MMR)/sqrt(length(mergedBT_RR_df$MMR))

meanMMR
sdMMR
semMMR







