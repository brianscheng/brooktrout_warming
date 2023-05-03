### Script for Figure 2 - weight and SGR time series

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)

#### read data
morph_long<-read.csv(here('data/BT_Morphometrics_LongForm_Merged_05032023.csv'), stringsAsFactors = T)
str(morph_long)

morph_long <- morph_long %>%
  mutate (temp = factor(substr(TANK_Real, 1, 2)))%>%
  mutate (TANK_Real = factor(TANK_Real)) %>%
  mutate (rdate = as.POSIXct(x=TIME, format = "%Y-%m-%d")) %>%
  mutate (K = WEIGHT*100*(LENGTH^-3)) %>%
  rename ("tank" = "TANK_Real") %>%
  rename ("time_step" = "condition3")

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
plot1<-ggplot(weight_mean_se, aes(x=time_step, y=mean_weight, color = C_temp,group = C_temp))+
  geom_point(size = 3, position = position_dodge(width = 0.2))+
  geom_errorbar(data=weight_mean_se, aes(ymin=mean_weight-se_weight, ymax = mean_weight+se_weight),
                width=0, size = 1, position = position_dodge(width = 0.2))+
  geom_line(aes(group = C_temp), position = position_dodge(width = 0.2))+
  labs(y=expression(paste("Weight (mean grams \U00B1SEM)")), x="Time Step")+
  theme_bw()+theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
                   axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
                   axis.text.x=element_text(size=16),axis.title.x=element_text(size=18),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(values  = c(A_col, B_col)) +
  scale_color_manual(values = c("15" = A_col, "20" = B_col))
plot1
ppi=300
png('figures/mass_time_series.png', width=9*ppi, height=6*ppi, res=ppi)
plot1
dev.off()