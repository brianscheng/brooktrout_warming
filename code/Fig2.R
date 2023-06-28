### Script for Figure 2 - mass and SGR time series

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(glmmTMB)

#### read data
morph_long<-read.csv(here('data/BT_Morphometrics_LongForm_Merged_05032023.csv'), stringsAsFactors = T)
str(morph_long)
which(morph_long$WEIGHT == 0) #erroneous data point


morph_long <- morph_long %>%
  mutate (temp = factor(substr(TANK_Real, 1, 2)))%>%
  mutate (TANK_Real = factor(TANK_Real)) %>%
  mutate (rdate = as.POSIXct(x=TIME, format = "%Y-%m-%d")) %>%
  mutate (K = WEIGHT*100*(LENGTH^-3)) %>%
  mutate (Days_in_Exp2 = factor(Days_in_Exp)) %>%
  rename ("tank" = "TANK_Real") %>%
  rename ("time_step" = "condition3") %>%
  filter (time_step !="T7") %>%
  filter (WEIGHT != 0)

str(morph_long)
#unique(morph_long$TIME)
#K = Mass*(Length^-3)
morph_long_inital<-morph_long %>% filter (time_step =="T1")

#### tidying
sum(is.na(morph_long$WEIGHT)) #676 Weight NAs
sum(is.na(morph_long$LENGTH)) #678 Length NAs
morph_long2<-morph_long %>%
  drop_na(WEIGHT) %>%
  drop_na(LENGTH)
summary(morph_long2) #now has 2100 obs
freq_tab<-morph_long2 %>%
  count(tank, time_step) %>%
  group_by(tank)
freq_tab
ggplot(data=freq_tab, aes(x=time_step,y=n, group = tank, color = tank))+geom_line()
ggplot(data=morph_long2, aes (x=time_step, y=Days_in_Exp))+geom_point()

#### summarize means
bt_means2<-tapply(morph_long2$WEIGHT,list(morph_long2$temp,morph_long2$time_step),mean)
bt_sd2   <-tapply(morph_long2$WEIGHT,list(morph_long2$temp,morph_long2$time_step),sd)
bt_count2<-tapply(morph_long2$WEIGHT,list(morph_long2$temp,morph_long2$time_step),length)
bt_sem2  <-bt_means2/sqrt(bt_count2)

weight_means<-data.frame(bt_means2)
weight_means$C_temp<-row.names(weight_means)
weight_means2<-weight_means %>%
  pivot_longer(cols = starts_with("T"),
               names_to = "time_step",
               names_prefix = "T",
               values_to = "mean_weight")

weight_se<-data.frame(bt_sem2)
weight_se$C_temp<-row.names(weight_se)
weight_se2<-weight_se %>%
  pivot_longer(cols = starts_with("T"),
               names_to = "time_step",
               names_prefix = "T",
               values_to = "se_weight")
#now merge the dataframes
weight_mean_se<- left_join(weight_means2, weight_se2, by = c("time_step", "C_temp"))
weight_mean_se<- weight_mean_se %>%
  mutate (time_step = factor(time_step))%>%
  mutate (Days = case_when (time_step == "1"~ 0,
                            time_step == "2"~ 20,
                            time_step == "3"~ 62,
                            time_step == "4"~ 98,
                            time_step == "5"~ 133,
                            time_step == "6"~ 161,
                            time_step == "8"~ 244))

#### differences in initial size?
str(morph_long2)
ggplot(data = morph_long_inital, aes(x=tank, y=WEIGHT))+geom_boxplot()
ggplot(data = morph_long_inital, aes(x=tank, y=LENGTH))+geom_boxplot()
init_w<-glm(data=morph_long_inital, formula = WEIGHT ~ tank, family = gaussian())
init_l<-glm(data=morph_long_inital, formula = LENGTH ~ tank, family = gaussian())
summary(init_w)
summary(init_l)

#### create MS plots, 2A first
A_col <- "#9933FF"
B_col <- "#FF6666"
str(weight_mean_se)
plot1<-ggplot(weight_mean_se, aes(x=Days, y=mean_weight, color = C_temp,group = C_temp))+
  geom_point(size = 3.5, position = position_dodge(width = 0.2))+
  geom_errorbar(data=weight_mean_se, aes(ymin=mean_weight-se_weight, ymax = mean_weight+se_weight),
                width=0, size = 1, position = position_dodge(width = 0.2))+
  geom_line(aes(group = C_temp), position = position_dodge(width = 0.2))+
  labs(y=expression(paste("Mass (mean \U00B1SEM)")), x="Days in experiment")+
  theme_bw()+theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
                   axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
                   axis.text.x=element_text(size=16),axis.title.x=element_text(size=18),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   legend.position = c(0.1,0.85))+
  scale_fill_manual(values  = c(A_col, B_col)) +
  scale_color_manual(values = c("15" = A_col, "20" = B_col))+
  coord_cartesian(xlim = c(0,250), ylim = c(-30,250))

plot1
ppi=300
png('figures/mass_time_series.png', width=9*ppi, height=6*ppi, res=ppi)
plot1
dev.off()

####Mass analysis
str(morph_long2)
weight01  <-lmer(WEIGHT ~ temp * time_step + (1|TAGNUMBER), data = morph_long2)
weight02  <-lmer(WEIGHT ~ temp * time_step + (1|TAGNUMBER) + (1|tank), data = morph_long2)
weight01a <-lmer(log10(WEIGHT) ~ temp * time_step + (1|TAGNUMBER), data = morph_long2)

summary(weight01)
summary(weight02)
summary(weight01a)

plot(weight01)
plot(weight01a)
morph_long2$residuals<-residuals(weight01)
morph_long2$residuals.log<-residuals(weight01a)
ggplot(morph_long2,aes(x=time_step, y= residuals))+geom_boxplot()
ggplot(morph_long2,aes(x=temp, y= residuals))+geom_boxplot()+facet_grid(.~time_step)
ggplot(morph_long2,aes(x=time_step, y= residuals.log))+geom_boxplot()
ggplot(morph_long2,aes(x=temp, y= residuals.log))+geom_boxplot()+facet_grid(.~time_step)

anova(weight01,  ddf = "Kenward-Roger")
anova(weight02,  ddf = "Kenward-Roger")
anova(weight01a, ddf = "Kenward-Roger")

emm2 = emmeans (weight01, ~ temp | time_step)
contrast(emm2, contrast = TRUE)
pairs(emm2)
pairs(emm2, by = "temp")
plot(emm2)

emm2a = emmeans (weight01a, ~ temp | time_step)
contrast(emm2a, contrast = TRUE)
pairs(emm2a)
pairs(emm2a, by = "temp")
plot(emm2a)

weight01b<-glmmTMB(WEIGHT ~ temp * time_step + (1|TAGNUMBER), data = morph_long2, family = Gamma)
summary(weight01b)

emm2b = emmeans (weight01b, ~ temp | time_step)
contrast(emm2b, contrast = TRUE)
pairs(emm2b)
pairs(emm2b, by = "temp")
plot(emm2b)

anova(weight01b, ddf = "Kenward-Roger")

#### SGR analysis and plotting
sgr<-read.csv(here('data/BT_Morphometrics_SGR_LongForm_Merged_05102023.csv'), stringsAsFactors = T)
str(sgr)
which(sgr$SGR < 0)
sgr <- sgr %>%
  mutate (temp = factor(substr(TANK_Real, 1, 2)))%>%
  mutate (TANK_Real = factor(TANK_Real)) %>%
  mutate (Days_in_Exp2 = factor(Days_in_Exp)) %>%
  rename ("tank" = "TANK_Real") %>%
  rename ("time_step" = "condition3") %>%
  filter (condition1 !="SGR_6") %>%
  filter (SGR > -0.2) #remove mass = 0 fish

ggplot(sgr, aes(x=time_step, y=Days_in_Exp))+geom_point()

#### summarize means
sgr_means <-tapply(sgr$SGR,list(sgr$temp,sgr$time_step),mean)
sgr_std   <-tapply(sgr$SGR,list(sgr$temp,sgr$time_step),sd)
sgr_count <-tapply(sgr$SGR,list(sgr$temp,sgr$time_step),length)
sgr_sem   <-sgr_means/sqrt(sgr_count)

sgr_means<-data.frame(sgr_means)
sgr_means$C_temp<-row.names(sgr_means)
sgr_means <-sgr_means %>%
  pivot_longer(cols = starts_with("T"),
               names_to = "time_step",
               names_prefix = "T",
               values_to = "mean_sgr")

sgr_sem<-data.frame(sgr_sem)
sgr_sem$C_temp<-row.names(sgr_sem)
sgr_sem <-sgr_sem %>%
  pivot_longer(cols = starts_with("T"),
               names_to = "time_step",
               names_prefix = "T",
               values_to = "se_sgr")

sgr_mean_se<- left_join(sgr_means, sgr_sem, by = c("time_step", "C_temp"))
sgr_mean_se<- sgr_mean_se %>%
  mutate (time_step = factor(time_step))%>%
  mutate (Days = case_when (time_step == "1"~ 0,
                            time_step == "2"~ 20,
                            time_step == "3"~ 62,
                            time_step == "4"~ 98,
                            time_step == "5"~ 133,
                            time_step == "6"~ 161,
                            time_step == "8"~ 244))

#### create MS plots, 2B second
A_col <- "#9933FF"
B_col <- "#FF6666"
str(sgr_mean_se)
plot2<-ggplot(sgr_mean_se, aes(x=Days, y=mean_sgr, color = C_temp,group = C_temp))+
  geom_point(size = 3.5, position = position_dodge(width = 0.2))+
  geom_errorbar(data=sgr_mean_se, aes(ymin=mean_sgr-se_sgr, ymax = mean_sgr+se_sgr),
                width=0, size = 1, position = position_dodge(width = 0.2))+
  geom_line(aes(group = C_temp), position = position_dodge(width = 0.2))+
  labs(y=expression(paste("Specific Growth Rate (mean \U00B1SEM)")), x="Days in experiment")+
  theme_bw()+theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
                   axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
                   axis.text.x=element_text(size=16),axis.title.x=element_text(size=18),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   legend.position = "none")+
  scale_fill_manual(values  = c(A_col, B_col)) +
  scale_color_manual(values = c("15" = A_col, "20" = B_col))+
  coord_cartesian(xlim = c(0,250), ylim = c(0,4))
plot2
ppi=300
png('figures/sgr_time_series.png', width=9*ppi, height=6*ppi, res=ppi)
plot2
dev.off()

#### SGR analysis
str(sgr)

sgr01<-lmer(SGR ~ temp * time_step + (1|TAGNUMBER), data = sgr)
sgr02<-lmer(SGR ~ temp * time_step + (1|TAGNUMBER) + (1|tank), data = sgr)
summary(sgr01)
summary(sgr02)

plot(sgr01)
sgr$residuals<-residuals(sgr01)
ggplot(sgr,aes(x=time_step, y= residuals))+geom_boxplot()
ggplot(sgr,aes(x=temp, y= residuals))+geom_boxplot()+facet_grid(.~time_step)

anova(sgr01, ddf = "Kenward-Roger")
anova(sgr02, ddf = "Kenward-Roger")

emm_sgr = emmeans (sgr01, ~ temp | time_step)
contrast(emm_sgr, contrast = TRUE)
pairs(emm_sgr)
pairs(emm_sgr, by = "temp")
plot(emm_sgr)
