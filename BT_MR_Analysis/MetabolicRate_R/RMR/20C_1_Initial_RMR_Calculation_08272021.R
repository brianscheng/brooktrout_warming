library(FishResp)
library(tidyverse) ##contains ggplot for visualizing and many commands I use
library(cowplot) ##for saving plots and arranging if you so choose
library(afex) ##this is needed for the proper two-way ANOVA 
library("respR")
library(ggplot2)
library(respirometry)

convert.respirometry(import.file = "RMR_1_20C_raw.txt", 
                            export.file = "RMR_1_20C_raw_082721.txt", 
                            n.chamber = 8,
                            logger = "AutoResp",
                            from = "percent_a.s.", 
                            to = "mg_per_l", 
                            sal = 0,
                            atm_pres = 1012.5)

convert.respirometry(import.file = "Background_After_1_20C_raw.txt", 
                     export.file = "Background_After_1_20C_raw_082721.txt", 
                     n.chamber = 8,
                     logger = "AutoResp",
                     from = "percent_a.s.", 
                     to = "mg_per_l", 
                     sal = 0,
                     atm_pres = 1012.5)

convert.respirometry(import.file = "Background_Before_1_20C_raw.txt", 
                     export.file = "Background_Before_1_20C_raw_082721.txt", 
                     n.chamber = 8,
                     logger = "AutoResp",
                     from = "percent_a.s.", 
                     to = "mg_per_l", 
                     sal = 0,
                     atm_pres = 1012.5)

info <- input.info(ID = c("BT_1", "BT_2", "BT_3", "BT_4", "BT_5", "BT_6", "BT_7", "BT_8"),
                   Mass = c(6, 7, 6.3, 3.8, 4.6, 6.7, 4.9, 6.8),
                   Volume = c(656.4, 659.2, 654, 658.6, 657.6, 659.2, 654.4, 656.4),
                   DO.unit = "mg/L")

tiff(filename = "FullRun_Background_After_20C_1_082721.tiff",
     width = 6, height = 6, units = "in", res = 600)
post <- import.test(file = "Background_After_1_20C_raw_082721.txt", 
                    info.data = info, 
                    logger = "AutoResp",
                    n.chamber = 8,
                    plot.temperature = TRUE,
                    plot.oxygen = TRUE)

tiff(filename = "FullRun_Background_Before_20C_1_082721.tiff",
     width = 6, height = 6, units = "in", res = 600)
pre <- import.test(file = "Background_Before_1_20C_raw_082721.txt", 
                    info.data = info, 
                    logger = "AutoResp",
                    n.chamber = 8,
                    plot.temperature = TRUE,
                    plot.oxygen = TRUE)

tiff(filename = "FullRun_RMR_20C_1_082721.tiff",
     width = 6, height = 6, units = "in", res = 600)
RMR.raw <- import.meas(file = "RMR_1_20C_raw_082721.txt",
                       info.data = info,
                       logger = "AutoResp",
                       n.chamber = 8,
                       date.format = "MDY",
                       plot.temperature = TRUE,
                       plot.oxygen = TRUE)

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

Chamber7_RMR.clean <- filter(RMR.clean, Chamber.No == "CH7")
Chamber7_RMR.clean_graph <- ggplot(Chamber7_RMR.clean, aes(x = Date.Time, y = O2)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Chamber 7", x = "Date and Time", y = "DO (mg O2/L)") +
  theme_classic()

Chamber8_RMR.clean <- filter(RMR.clean, Chamber.No == "CH8")
Chamber8_RMR.clean_graph <- ggplot(Chamber8_RMR.clean, aes(x = Date.Time, y = O2)) + 
  geom_point(na.rm = TRUE) +  ##This means don't plot anything that has an NA 
  labs(title = "Chamber 8", x = "Date and Time", y = "DO (mg O2/L)") +
  theme_classic()

RMR.slope_all <- extract.slope(RMR.clean,
                           method = "all",
                           r2 = 0.9)
tiff(filename = "RMR_alldata_20C_1_08272021_r2_0.9.tiff",
     width = 6, height = 6, units = "in", res = 600)
RMR_all <- calculate.MR(RMR.slope_all,
                    density = 1000,
                    plot.BR = TRUE,
                    plot.MR.abs = TRUE,
                    plot.MR.mass = TRUE)

calculate.MR(RMR.slope_all,
             density = 1000,
             plot.BR = TRUE,
             plot.MR.abs = TRUE,
             plot.MR.mass = TRUE)

RMR.slope_lowest25 <- extract.slope(RMR.clean,
                               method = "lower.tail",
                               percent = 25,
                               r2 = 0.9)

tiff(filename = "RMR_lowest25_20C_1_08272021_r2_0.9.tiff",
     width = 6, height = 6, units = "in", res = 600)

RMR_lowest25 <- calculate.MR(RMR.slope_lowest25,
                        density = 1000,
                        plot.BR = FALSE,
                        plot.MR.abs = FALSE,
                        plot.MR.mass = TRUE)

calculate.MR(RMR.slope_lowest25,
             density = 1000,
             plot.BR = FALSE,
             plot.MR.abs = TRUE,
             plot.MR.mass = TRUE)

results_all <- export.MR(RMR_all,
                     file = "results_all_20C_1_08272021_R2>0.9.txt",
                     simplify = TRUE,
                     MS = FALSE,
                     plot.MS.abs = FALSE,
                     plot.MS.mass = FALSE,
                     plot.MS.fact = FALSE)

results_lowest25 <- export.MR(RMR_lowest25,
                              file = "results_lowest25_20C_1_08272021_R2>0.9.txt",
                              simplify = TRUE,
                              MS = FALSE,
                              plot.MS.abs = FALSE,
                              plot.MS.mass = FALSE,
                              plot.MS.fact = FALSE)
