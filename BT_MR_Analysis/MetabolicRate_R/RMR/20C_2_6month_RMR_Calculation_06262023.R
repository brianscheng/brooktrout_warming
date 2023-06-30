library(FishResp)
library(tidyverse) ##contains ggplot for visualizing and many commands I use
library(cowplot) ##for saving plots and arranging if you so choose
library(afex) ##this is needed for the proper two-way ANOVA 
library("respR")
library(ggplot2)
library(respirometry)

convert.respirometry(import.file = "RMR_2_20C_02152022_raw.txt", 
                            export.file = "RMR_2_20C_raw_021722.txt", 
                            n.chamber = 6,
                            logger = "AutoResp",
                            from = "percent_a.s.", 
                            to = "mg_per_l", 
                            sal = 0,
                            atm_pres = 1015.9)

convert.respirometry(import.file = "Background_After_2_20C_02162022_raw.txt", 
                     export.file = "Background_After_2_20C_raw_021722.txt", 
                     n.chamber = 6,
                     logger = "AutoResp",
                     from = "percent_a.s.", 
                     to = "mg_per_l", 
                     sal = 0,
                     atm_pres = 1015.9)

convert.respirometry(import.file = "Background_Before_2_20C_02142022_raw.txt", 
                     export.file = "Background_Before_2_20C_raw_021722.txt", 
                     n.chamber = 6,
                     logger = "AutoResp",
                     from = "percent_a.s.", 
                     to = "mg_per_l", 
                     sal = 0,
                     atm_pres = 1015.9)

info <- input.info(ID = c("BT_1", "BT_2", "BT_3", "BT_4", "BT_5", "BT_6"),
                   Mass = c(161.2, 102.9, 108.1, 113.9, 90.5, 116.6),
                   Volume = c(4012, 4012, 4012, 4012, 4012, 4012),
                   DO.unit = "mg/L")

post <- import.test(file = "Background_After_2_20C_raw_021722.txt", 
                    info.data = info, 
                    logger = "AutoResp",
                    n.chamber = 6,
                    plot.temperature = FALSE,
                    plot.oxygen = FALSE)

pre <- import.test(file = "Background_Before_2_20C_raw_021722.txt", 
                    info.data = info, 
                    logger = "AutoResp",
                    n.chamber = 6,
                    plot.temperature = FALSE,
                    plot.oxygen = FALSE)

RMR.raw <- import.meas(file = "RMR_2_20C_raw_021722.txt",
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

results_lowest25 <- export.MR(RMR_lowest25,
                              file = "results_lowest25_20C_2_06262023_R2>0.9.txt",
                              simplify = TRUE,
                              MS = FALSE,
                              plot.MS.abs = FALSE,
                              plot.MS.mass = FALSE,
                              plot.MS.fact = FALSE)
