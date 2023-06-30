library(FishResp)
library(tidyverse) ##contains ggplot for visualizing and many commands I use
library(cowplot) ##for saving plots and arranging if you so choose
library(afex) ##this is needed for the proper two-way ANOVA 
library("respR")
library(ggplot2)
library(respirometry)

convert.respirometry(import.file = "RMR_2_15C_02082022_raw.txt", 
                            export.file = "RMR_2_15C_raw_020822.txt", 
                            n.chamber = 6,
                            logger = "AutoResp",
                            from = "percent_a.s.", 
                            to = "mg_per_l", 
                            sal = 0,
                            atm_pres = 1013.9)

convert.respirometry(import.file = "Background_After_2_15C_02092022_raw.txt", 
                     export.file = "Background_After_2_15C_raw_020922.txt", 
                     n.chamber = 6,
                     logger = "AutoResp",
                     from = "percent_a.s.", 
                     to = "mg_per_l", 
                     sal = 0,
                     atm_pres = 1013.9)

convert.respirometry(import.file = "Background_Before_2_15C_02072022_raw.txt", 
                     export.file = "Background_Before_2_15C_raw_020722.txt", 
                     n.chamber = 6,
                     logger = "AutoResp",
                     from = "percent_a.s.", 
                     to = "mg_per_l", 
                     sal = 0,
                     atm_pres = 1013.9)

info <- input.info(ID = c("BT_1", "BT_2", "BT_3", "BT_4", "BT_5", "BT_6"),
                   Mass = c(172.2, 122.2, 95.3, 124, 221, 176.8),
                   Volume = c(4012, 4012, 4012, 4012, 4012, 4012),
                   DO.unit = "mg/L")

post <- import.test(file = "background_after_2_15C_raw_020922.txt", 
                    info.data = info, 
                    logger = "AutoResp",
                    n.chamber = 6,
                    plot.temperature = FALSE,
                    plot.oxygen = FALSE)

pre <- import.test(file = "background_before_2_15C_raw_020722.txt", 
                    info.data = info, 
                    logger = "AutoResp",
                    n.chamber = 6,
                    plot.temperature = FALSE,
                    plot.oxygen = FALSE)

RMR.raw <- import.meas(file = "RMR_2_15C_raw_020822.txt",
                       info.data = info,
                       logger = "AutoResp",
                       n.chamber = 6,
                       date.format = "MDY",
                       plot.temperature = FALSE,
                       plot.oxygen = FALSE)

RMR.clean <- correct.meas(info.data = info,
                          pre.data = pre,
                          post.data = post,
                          meas.data = RMR.raw,
                          method = "linear")

Chamber1_RMR.clean <- filter(RMR.clean, Chamber.No == "CH1")
Chamber1_RMR.clean_graph <- ggplot(Chamber1_RMR.clean, aes(x = Date.Time, y = O2)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Chamber 1", x = "Date and Time", y = "DO (mg O2/L)") +
  theme_classic()

Chamber2_RMR.clean <- filter(RMR.clean, Chamber.No == "CH2")
Chamber2_RMR.clean_graph <- ggplot(Chamber2_RMR.clean, aes(x = Date.Time, y = O2)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Chamber 2", x = "Date and Time", y = "DO (mg O2/L)") +
  theme_classic()

Chamber3_RMR.clean <- filter(RMR.clean, Chamber.No == "CH3")
Chamber3_RMR.clean_graph <- ggplot(Chamber3_RMR.clean, aes(x = Date.Time, y = O2)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Chamber 3", x = "Date and Time", y = "DO (mg O2/L)") +
  theme_classic()

Chamber4_RMR.clean <- filter(RMR.clean, Chamber.No == "CH4")
Chamber4_RMR.clean_graph <- ggplot(Chamber4_RMR.clean, aes(x = Date.Time, y = O2)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Chamber 4", x = "Date and Time", y = "DO (mg O2/L)") +
  theme_classic()

Chamber5_RMR.clean <- filter(RMR.clean, Chamber.No == "CH5")
Chamber5_RMR.clean_graph <- ggplot(Chamber5_RMR.clean, aes(x = Date.Time, y = O2)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Chamber 5", x = "Date and Time", y = "DO (mg O2/L)") +
  theme_classic()

Chamber6_RMR.clean <- filter(RMR.clean, Chamber.No == "CH6")
Chamber6_RMR.clean_graph <- ggplot(Chamber6_RMR.clean, aes(x = Date.Time, y = O2)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Chamber 6", x = "Date and Time", y = "DO (mg O2/L)") +
  theme_classic()

RMR.slope_all <- extract.slope(RMR.clean,
                           method = "all",
                           r2 = 0.9)

RMR_all <- calculate.MR(RMR.slope_all,
                    density = 1000,
                    plot.BR = FALSE,
                    plot.MR.abs = FALSE,
                    plot.MR.mass = FALSE)

RMR.slope_lowest25 <- extract.slope(RMR.clean,
                               method = "lower.tail",
                               percent = 25,
                               r2 = 0.9)

RMR_lowest25 <- calculate.MR(RMR.slope_lowest25,
                        density = 1000,
                        plot.BR = FALSE,
                        plot.MR.abs = FALSE,
                        plot.MR.mass = FALSE)

results_all <- export.MR(RMR_all,
                     file = "results_all_15C_2_02092022_R2>0.9.txt",
                     simplify = TRUE,
                     MS = FALSE,
                     plot.MS.abs = FALSE,
                     plot.MS.mass = FALSE,
                     plot.MS.fact = FALSE)

results_lowest25 <- export.MR(RMR_lowest25,
                              file = "results_lowest25_15C_2_02092022_R2>0.9.txt",
                              simplify = TRUE,
                              MS = FALSE,
                              plot.MS.abs = FALSE,
                              plot.MS.mass = FALSE,
                              plot.MS.fact = FALSE)
