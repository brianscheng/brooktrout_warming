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
library(lme4)
library(lmerTest)
library(emmeans)


ConditionFactor_df <- read.csv(here("ConditionFactory_BT/BT_Morphometrics_LongForm_Merged_05032023.csv"))

A_col <- "#9933FF"
B_col <- "#FF6666"

ConditionFactor_df$ConditionFactor <- (ConditionFactor_df$WEIGHT)*((ConditionFactor_df$LENGTH/10)^-3)*100
ConditionFactor_df <- ConditionFactor_df[ConditionFactor_df$ConditionFactor != 0, ]

ConditionFactor_df2 <- ConditionFactor_df[complete.cases(ConditionFactor_df$ConditionFactor), ]

ConditionFactor_df2 <- ConditionFactor_df2 %>%
  mutate (temp = factor(substr(TANK_Real, 1, 2)))%>%
  mutate (TANK_Real = factor(TANK_Real)) %>%
  mutate (rdate = as.POSIXct(x=TIME, format = "%Y-%m-%d")) %>%
  rename ("tank" = "TANK_Real") %>%
  rename ("time_step" = "condition3") %>%
  filter (time_step !="T7")

#relative condition factor

plot(ConditionFactor_df2$LENGTH, ConditionFactor_df2$WEIGHT)

ConditionFactor_df2$ConditionFactor_relative <- (ConditionFactor_df2$WEIGHT)*((ConditionFactor_df2$LENGTH/10)^-3.085)*100

#create summary df for weight mean and sem
ConditionFactor_rel_means <-tapply(ConditionFactor_df2$ConditionFactor_relative,list(ConditionFactor_df2$temp,ConditionFactor_df2$time_step),mean)
ConditionFactor_rel_std   <-tapply(ConditionFactor_df2$ConditionFactor_relative,list(ConditionFactor_df2$temp,ConditionFactor_df2$time_step),sd)
ConditionFactor_rel_count <-tapply(ConditionFactor_df2$ConditionFactor_relative,list(ConditionFactor_df2$temp,ConditionFactor_df2$time_step),length)
ConditionFactor_rel_sem   <-ConditionFactor_rel_means/sqrt(ConditionFactor_rel_count)

ConditionFactor_rel_means<-data.frame(ConditionFactor_rel_means)
ConditionFactor_rel_means$C_temp<-row.names(ConditionFactor_rel_means)
ConditionFactor_rel_means <-ConditionFactor_rel_means %>%
  pivot_longer(cols = starts_with("T"),
               names_to = "time_step",
               names_prefix = "T",
               values_to = "mean_ConditionFactor")

ConditionFactor_rel_sem<-data.frame(ConditionFactor_rel_sem)
ConditionFactor_rel_sem$C_temp<-row.names(ConditionFactor_rel_sem)
ConditionFactor_rel_sem <-ConditionFactor_rel_sem %>%
  pivot_longer(cols = starts_with("T"),
               names_to = "time_step",
               names_prefix = "T",
               values_to = "se_ConditionFactor")

ConditionFactor_rel_mean_se<- left_join(ConditionFactor_rel_means, ConditionFactor_rel_sem, by = c("time_step", "C_temp"))
ConditionFactor_rel_mean_se<- ConditionFactor_rel_mean_se %>%
  mutate (time_step = factor(time_step))%>%
  mutate (Days = case_when (time_step == "1"~ 0,
                            time_step == "2"~ 20,
                            time_step == "3"~ 62,
                            time_step == "4"~ 98,
                            time_step == "5"~ 133,
                            time_step == "6"~ 161,
                            time_step == "8"~ 244))
#### K analysis
str(ConditionFactor_df2)
K01  <-lmer(ConditionFactor_relative ~ temp * time_step + (1|TAGNUMBER), data = ConditionFactor_df2)
K01a <-lmer(log10(ConditionFactor_relative) ~ temp * time_step + (1|TAGNUMBER), data = ConditionFactor_df2)

summary(K01)
summary(K01a)

plot(K01)
plot(K01a)

ConditionFactor_df2$residuals<-residuals(K01)
ggplot(ConditionFactor_df2,aes(x=time_step, y= residuals))+geom_boxplot()
ggplot(ConditionFactor_df2,aes(x=temp, y= residuals))+geom_boxplot()+facet_grid(.~time_step)

anova(K01, ddf = "Kenward-Roger")

emm2a = emmeans (K01, ~ temp | time_step)
contrast(emm2a, contrast = TRUE)
pairs(emm2a)
pairs(emm2a, by = "temp")
plot(emm2a)

#K Plotting

ggplot(ConditionFactor_rel_mean_se, aes(x = Days, y = mean_ConditionFactor, color = C_temp, group = C_temp)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.2)) +
  geom_errorbar(data = ConditionFactor_rel_mean_se, aes(ymin = mean_ConditionFactor - se_ConditionFactor, ymax = mean_ConditionFactor + se_ConditionFactor),
                width = 0, size = 1, position = position_dodge(width = 0.2)) +
  geom_line(aes(group = C_temp), position = position_dodge(width = 0.2)) +
  labs(y = expression(paste("Relative Condition Factor (K) (mean \U00B1 SEM)")), x = "Days in experiment") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(),
        axis.text = element_text(size = 12), axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16), legend.position = "none") +
  scale_color_manual(values = c("15" = "#9933FF", "20" = "#FF6666")) +
  scale_shape_manual(values = c("15" = 16, "20" = 2)) +
  coord_cartesian(xlim = c(0, 250), ylim = c(0, 2))

ggsave("relative_K_07042023.png", width = 6, height = 6, dpi = 600)

# Create CVS of Data

write.csv(ConditionFactor_df2, "ConditionFactor_LongForm_Merged_07052023.csv")
