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

Cortisol <- read_excel("GSA_Cortisol_raw_05042023.xlsx")

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
Cortisol01  <-lmer(Cortisol ~ Tx * TimePoint + (1|tank), data = Cortisol)
Cortisol01a <-lmer(log10(Cortisol) ~ Tx * TimePoint + (1|tank), data = Cortisol)

summary(Cortisol01)
summary(Cortisol01a)

plot(Cortisol01)
Cortisol$residuals<-residuals(Cortisol01)
ggplot(Cortisol,aes(x=TimePoint, y= residuals))+geom_boxplot()
ggplot(Cortisol,aes(x=Tx, y= residuals))+geom_boxplot()+facet_grid(.~TimePoint)

plot(Cortisol01a)
Cortisol$residuals<-residuals(Cortisol01a)
ggplot(Cortisol,aes(x=TimePoint, y= residuals))+geom_boxplot()
ggplot(Cortisol,aes(x=Tx, y= residuals))+geom_boxplot()+facet_grid(.~TimePoint)

anova(Cortisol01a, ddf = "Kenward-Roger")

emm2 = emmeans (Cortisol01a, ~ Tx | TimePoint)
contrast(emm2, contrast = TRUE)
pairs(emm2)
pairs(emm2, by = "Tx")
plot(emm2)

## Plot Cortisol

A_col <- "#9933FF"
B_col <- "#FF6666"

ggplot(Cortisol, aes(x = TimePoint, y = Cortisol, fill = Tx)) +
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
  scale_y_continuous(limits = c(0, 100))

ggsave("Cortisol_cleaned_jitter_boxplot_07042023.png", width = 6, height = 6, dpi = 600)