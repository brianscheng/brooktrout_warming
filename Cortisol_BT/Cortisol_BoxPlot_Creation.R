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
library(here)
library(emmeans)
Cortisol <- read_excel(here("Cortisol_BT/GSA_Cortisol_raw_05042023.xlsx"))

Cortisol$Date <- as.character(Cortisol$Date)

Cortisol$TimePoint <- ifelse(Cortisol$Date == "2021-08-25", 20,
                         ifelse(Cortisol$Date == "2021-11-09", 98, 190))

Cortisol <- Cortisol[Cortisol$Tx != "seasonal",]

names(Cortisol)[8] <- "BleedTime"

Cortisol$BleedTime <- ifelse(is.na(Cortisol$BleedTime), 0, Cortisol$BleedTime)

Cortisol <- Cortisol[Cortisol$BleedTime <= .25,]

Cortisol <- Cortisol %>%
  mutate(TimePoint = reorder(TimePoint, as.numeric(TimePoint)))

names(Cortisol)[3] <- "Cortisol"

Cortisol <- Cortisol[complete.cases(Cortisol$Cortisol), ]

write.csv(Cortisol, "BT_Cortisol_merged_07042023.csv")

## Cortisol Analysis

str(Cortisol)
Cortisol$TimePoint<-factor(Cortisol$TimePoint)
Cortisol$Tx<-factor(Cortisol$Tx)

#Cortisol01  <-lmer(Cortisol ~ Tx * TimePoint + (1|tank), data = Cortisol)
#Cortisol01a <-lmer(log10(Cortisol) ~ Tx * TimePoint + (1|tank), data = Cortisol)
Cortisol02  <-lm(Cortisol ~ Tx *TimePoint, data = Cortisol)
Cortisol02a <-lm(log10(Cortisol) ~ Tx *TimePoint, data = Cortisol)

#summary(Cortisol01)
#summary(Cortisol01a)
summary(Cortisol02)
summary(Cortisol02a)

Anova(Cortisol02,  type = 3)
Anova(Cortisol02a, type = 3)

plot(Cortisol02)
Cortisol$residuals<-residuals(Cortisol02)
ggplot(Cortisol,aes(x=TimePoint, y= residuals))+geom_boxplot()
ggplot(Cortisol,aes(x=Tx, y= residuals))+geom_boxplot()+facet_grid(.~TimePoint)

Cortisol$residuals02a<-residuals(Cortisol02a)
ggplot(Cortisol,aes(x=TimePoint, y= residuals02a))+geom_boxplot()
ggplot(Cortisol,aes(x=Tx, y= residuals02a))+geom_boxplot()+facet_grid(.~TimePoint)



cort02 = emmeans (Cortisol02, ~ Tx | TimePoint)
contrast(cort02, contrast = TRUE)
pairs(cort02, by = "Tx")

cort02a = emmeans (Cortisol02a, ~ Tx | TimePoint)
contrast(cort02a, contrast = TRUE)
pairs(cort02a, by = "Tx")
## Plot Cortisol

A_col <- "#9933FF"
B_col <- "#FF6666"

ggplot(Cortisol, aes(x = TimePoint, y = log10(Cortisol), fill = Tx)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(shape = Tx), position = position_jitterdodge(), size = 2.5) +
  labs(x = "Days in Experiment", y = "Cortisol (ng/mL)", fill = "Treatment", shape = "Treatment") +
  scale_fill_manual(values = c("15" = A_col, "20" = B_col)) +
  scale_shape_manual(values = c("15" = 16, "20" = 2)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(),
        axis.text = element_text(size = 12), axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16)) +  
  scale_y_continuous(limits = c(0, 3))

ggsave("Cortisol_cleaned_jitter_boxplot_07042023.png", width = 6, height = 6, dpi = 600)