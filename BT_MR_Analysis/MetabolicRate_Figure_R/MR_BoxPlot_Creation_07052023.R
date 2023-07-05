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

names(RMR_MassCorrected)[names(RMR_MassCorrected) == "Replicate"] <- "Tank"

## Analysis of RMR
str(RMR_MassCorrected)
RMR01  <-lmer(Mass_Corrected ~ Treatment * TimePoint + (1|Tank), data = RMR_MassCorrected)
RMR01a <-lmer(log10(Mass_Corrected) ~ Treatment * TimePoint + (1|Tank), data = RMR_MassCorrected)

summary(RMR01)
summary(RMR01a)

plot(RMR01a)
RMR_MassCorrected$residuals<-residuals(RMR01a)
ggplot(RMR_MassCorrected,aes(x=TimePoint, y= residuals))+geom_boxplot()
ggplot(RMR_MassCorrected,aes(x=Treatment, y= residuals))+geom_boxplot()+facet_grid(.~TimePoint)

anova(RMR01a, ddf = "Kenward-Roger")

emmRMR = emmeans (RMR01a, ~ Treatment | TimePoint)
contrast(emmRMR, contrast = TRUE)
pairs(emmRMR)
pairs(emmRMR, by = "Treatment")
plot(emmRMR)

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

names(MMR_MassCorrected)[names(MMR_MassCorrected) == "Replicate"] <- "Tank"

## Analysis of MMR
str(MMR_MassCorrected)
MMR01  <-lmer(Mass_Corrected ~ Treatment * TimePoint + (1|Tank), data = MMR_MassCorrected)
MMR01a <-lmer(log10(Mass_Corrected) ~ Treatment * TimePoint + (1|Tank), data = MMR_MassCorrected)

summary(MMR01)
summary(MMR01a)

plot(MMR01)
plot(MMR01a)

MMR_MassCorrected$residuals<-residuals(MMR01a)
ggplot(MMR_MassCorrected,aes(x=TimePoint, y= residuals))+geom_boxplot()
ggplot(MMR_MassCorrected,aes(x=Treatment, y= residuals))+geom_boxplot()+facet_grid(.~TimePoint)

anova(MMR01a, ddf = "Kenward-Roger")

emmMMR = emmeans (MMR01a, ~ Treatment | TimePoint)
contrast(emmMMR, contrast = TRUE)
pairs(emmMMR)
pairs(emmMMR, by = "Treatment")
plot(emmMMR)

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

# Calculation of FAS

FAS_RMR <- RMR_MassCorrected[,c("Ind","TimePoint","Treatment","Mass_Corrected","Tank")]
FAS_MMR <- MMR_MassCorrected[,c("Ind","TimePoint","Treatment","Mass_Corrected","Tank")]
FAS <- merge(FAS_RMR,FAS_MMR, by = c("Ind","TimePoint","Treatment","Tank"))

FAS$FAS <- FAS$Mass_Corrected.y / FAS$Mass_Corrected.x

# Rosner Outlier Test for FAS

rosner.out_FAS <- rosnerTest(FAS$FAS, k=10)$all.stats
rosner.out_FAS <- subset(rosner.out_FAS, Outlier %in% "TRUE")

# Analysis of FAS

str(FAS)
FAS01  <-lmer(FAS ~ Treatment * TimePoint + (1|Tank), data = FAS)
FAS01a <-lmer(log10(FAS) ~ Treatment * TimePoint + (1|Tank), data = FAS)

summary(FAS01)
summary(FAS01a)

plot(FAS01)
plot(FAS01a)

FAS$residuals<-residuals(FAS01a)
ggplot(FAS,aes(x=TimePoint, y= residuals))+geom_boxplot()
ggplot(FAS,aes(x=Treatment, y= residuals))+geom_boxplot()+facet_grid(.~TimePoint)

anova(FAS01a, ddf = "Kenward-Roger")

emmFAS = emmeans (FAS01a, ~ Treatment | TimePoint)
contrast(emmFAS, contrast = TRUE)
pairs(emmFAS)
pairs(emmFAS, by = "Treatment")
plot(emmFAS)

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

# Create CVS of Data

write.csv(RMR_MassCorrected, "RMR_MassCorrected_LongForm_Merged_07052023.csv")
write.csv(MMR_MassCorrected, "MMR_MassCorrected_LongForm_Merged_07052023.csv")
write.csv(FAS, "FAS_MassCorrected_LongForm_Merged_07052023.csv")
