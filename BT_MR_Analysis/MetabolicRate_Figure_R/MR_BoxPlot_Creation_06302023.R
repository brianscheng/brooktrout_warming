library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(broom)
library(stringi)
library(EnvStats)
library(dunn.test)
library(multcomp)
library(car)

RMR_15_Scaling <- 0.872
RMR_20_Scaling <- 0.830
MMR_15_Scaling <- 0.943
MMR_20_Scaling <- 0.882

A_col <- "#9933FF"
B_col <- "#FF6666"

## Pull in Raw Files from Repository

MMR <- read.csv("BT_MMR_LongForm_Merged_04062023.csv")
RMR <- read.csv("BT_RMR_LongForm_Merged_06262023.csv")

# Mass Correction of RMR  

RMR15 <- subset(RMR, Treatment %in% "15")
RMR20 <- subset(RMR, Treatment %in% "20")

RMR15$Mass_Corrected <- (exp(mean(log(RMR15$Mass)))^(RMR_15_Scaling-1))*((RMR15$Mass)^(1-RMR_15_Scaling))*(RMR15$MR.mass)
RMR20$Mass_Corrected <- (exp(mean(log(RMR20$Mass)))^(RMR_20_Scaling-1))*((RMR20$Mass)^(1-RMR_20_Scaling))*(RMR20$MR.mass)

RMR_MassCorrected <- rbind(RMR15, RMR20)

RMR_MassCorrected$TimePoint <- ifelse(RMR_MassCorrected$TimePoint == 0, 20, RMR_MassCorrected$TimePoint)
RMR_MassCorrected$TimePoint <- ifelse(RMR_MassCorrected$TimePoint == 3, 98, RMR_MassCorrected$TimePoint)
RMR_MassCorrected$TimePoint <- ifelse(RMR_MassCorrected$TimePoint == 6, 190, RMR_MassCorrected$TimePoint)

RMR_MassCorrected$TimePoint <- as.character(RMR_MassCorrected$TimePoint)
RMR_MassCorrected$Treatment <- as.character(RMR_MassCorrected$Treatment)
RMR_MassCorrected$Mass_Corrected <- as.numeric(RMR_MassCorrected$Mass_Corrected)

write.csv(RMR_MassCorrected, "RMR_MassCorrected_LongForm_Merged_06302023.csv")

## Plot RMR

RMR_MassCorrected <- RMR_MassCorrected %>%
  mutate(TimePoint = reorder(TimePoint, as.numeric(TimePoint)))

ggplot(RMR_MassCorrected, aes(x = TimePoint, y = Mass_Corrected, fill = Treatment)) +
  geom_boxplot() +
  labs(x = "Days in Experiment", y = expression("RMR Mass-Corrected (mgO"[2]~"kg"^-1~"hr"^-1~")"), fill = "Treatment") +
  scale_fill_manual(values = c("15" = A_col, "20" = B_col)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(),
        axis.text = element_text(size = 12), axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16)) +  
  scale_y_continuous(limits = c(50, 250))

ggsave("rmr_boxplot_063023.png", width = 6, height = 6, dpi = 600)

# Calculation of Two-Way ANOVA of RMR
RMR_MassCorrected$Treatment <- factor(RMR_MassCorrected$Treatment)
RMR_MassCorrected$TimePoint <- factor(RMR_MassCorrected$TimePoint)

my.RMRanova <- aov(Mass_Corrected ~ Treatment * TimePoint, data = RMR_MassCorrected)
summary(my.RMRanova)

Anova(my.RMRanova, type = "III")

# perform the Holm-Sidak test with the multcomp package
hs <- glht(my.RMRanova, linfct = mcp(Treatment = "Tukey", TimePoint = "Tukey"))
hs_summary <- summary(hs, test = adjusted(type = "holm-sidak"))

# print the results
hs_summary

# Mass Correction of MMR

MMR15 <- subset(MMR, Treatment %in% "15")
MMR20 <- subset(MMR, Treatment %in% "20")

MMR15$Mass_Corrected <- (exp(mean(log(MMR15$Mass)))^(MMR_15_Scaling-1))*((MMR15$Mass)^(1-MMR_15_Scaling))*(MMR15$MMR_1minute)
MMR20$Mass_Corrected <- (exp(mean(log(MMR20$Mass)))^(MMR_20_Scaling-1))*((MMR20$Mass)^(1-MMR_20_Scaling))*(MMR20$MMR_1minute)

MMR_MassCorrected <- rbind(MMR15, MMR20)

MMR_MassCorrected$TimePoint <- ifelse(MMR_MassCorrected$TimePoint == 0, 20, MMR_MassCorrected$TimePoint)
MMR_MassCorrected$TimePoint <- ifelse(MMR_MassCorrected$TimePoint == 3, 98, MMR_MassCorrected$TimePoint)
MMR_MassCorrected$TimePoint <- ifelse(MMR_MassCorrected$TimePoint == 6, 190, MMR_MassCorrected$TimePoint)

MMR_MassCorrected$TimePoint <- as.character(MMR_MassCorrected$TimePoint)
MMR_MassCorrected$Treatment <- as.character(MMR_MassCorrected$Treatment)
MMR_MassCorrected$Mass_Corrected <- as.numeric(MMR_MassCorrected$Mass_Corrected)

write.csv(MMR_MassCorrected, "MMR_MassCorrected_LongForm_Merged_06302023.csv")

# Graph Box Plot

MMR_MassCorrected <- MMR_MassCorrected %>%
  mutate(TimePoint = reorder(TimePoint, as.numeric(TimePoint)))

ggplot(MMR_MassCorrected, aes(x = TimePoint, y = Mass_Corrected, fill = Treatment)) +
  geom_boxplot() +
  labs(x = "Days in Experiment", y = expression("MMR Mass-Corrected (mgO"[2]~"kg"^-1~"hr"^-1~")"), fill = "Treatment") +
  scale_fill_manual(values = c("15" = A_col, "20" = B_col)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(),
        axis.text = element_text(size = 12), axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16)) 

ggsave("mmr_boxplot_06302023.png", width = 6, height = 6, dpi = 600)

# Calculation of Two-Way ANOVA of MMR

my.MMRanova <- aov(Mass_Corrected ~ Treatment * TimePoint, data = MMR_MassCorrected)
summary(my.MMRanova)

Anova(my.MMRanova, type = "III")

pairwise.t.test(MMR_MassCorrected$Mass_Corrected, MMR_MassCorrected$TimePoint,
                p.adjust.method = "bonferroni")

# Calculation of FAS

FAS_RMR <- RMR_MassCorrected[,c("Ind","TimePoint","Treatment","Mass_Corrected","Replicate")]
FAS_MMR <- MMR_MassCorrected[,c("Ind","TimePoint","Treatment","Mass_Corrected","Replicate")]
FAS <- merge(FAS_RMR,FAS_MMR, by = c("Ind","TimePoint","Treatment","Replicate"))

FAS$FAS <- FAS$Mass_Corrected.y / FAS$Mass_Corrected.x

# Rosner Outlier Test for FAS

rosner.out_FAS <- rosnerTest(FAS$FAS, k=10)$all.stats
rosner.out_FAS <- subset(rosner.out_FAS, Outlier %in% "TRUE")

FAS <- FAS[-rosner.out_FAS$Obs.Num, ]

# Plot FAS

FAS <- FAS %>%
  mutate(TimePoint = reorder(TimePoint, as.numeric(TimePoint)))

ggplot(FAS, aes(x = TimePoint, y = FAS, fill = Treatment)) +
  geom_boxplot() +
  labs(x = "Time Point (months)", y = "FAS", fill = "Treatment") +
  scale_fill_manual(values = c("15" = A_col, "20" = B_col)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(),
        axis.text = element_text(size = 12), axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16)) +  
  scale_y_continuous(limits = c(0, 10))

ggsave("FAS_boxplot_06302023.png", width = 6, height = 6, dpi = 600)

# Two-Way ANOVA FSA

my.FASanova <- aov(FAS ~ Treatment * TimePoint, data = FAS)
summary(my.FASanova)

Anova(my.FASanova, type = "III")

# Create CVS of Data

write.csv(FAS, "MetabolicRate_MassCorrected_LongForm_Merged_06302023.csv")
