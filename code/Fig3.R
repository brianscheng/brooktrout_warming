#### Fig 3 - GSA and metabolic rate
library(here)
library(tidyverse)
library(scales)
library(car)
data<-read.csv(here('data/BT_Resp_Combined_062823_BSC.csv'), stringsAsFactors = T)
str(data)


data2<-data %>%
  mutate (Temp = factor(Temp)) %>%
  mutate (Time = substring(time_month,1,2)) %>%
  mutate (Time = factor(Time)) %>%
  mutate (Replicate = case_when (Temp == "15" & Replicate == "1"~ "1",
                                 Temp == "15" & Replicate == "2"~ "2",
                                 Temp == "20" & Replicate == "1"~ "3",
                                 Temp == "20" & Replicate == "2"~ "4")) %>%
  mutate (Replicate = factor (Replicate)) %>%
  mutate (log10_Weight = log10(weight_g)) 

str(data2)

A_col <- "#9933FF"
B_col <- "#FF6666"
C_col <- "#66FF66"
fig3<-ggplot(data=data2, aes(x=log10(weight_g), y=log10(Metric_absolute), group = Metric, color = Temp))+
  stat_smooth(method = "lm")+geom_point(aes(shape = Metric), size = 3)+
  facet_grid(.~Temp)+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=16),axis.title.y=element_blank(),
        axis.text.x=element_text(size=16),axis.title.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  scale_fill_manual(values  = c("black","grey","white"))+
  scale_color_manual(values = c(A_col, B_col, C_col))+
  scale_shape_manual(values = c(21,22,23))+
  coord_cartesian(xlim = c(0.5,2.5))+
  scale_y_continuous (sec.axis = dup_axis())
fig3

ppi=300
png("figures/fig3.png", width=9*ppi, height=6*ppi, res=ppi)
fig3
dev.off()

#equations
data2_GSA   <- data2 %>% filter(Metric == "GSA")
data2_GSA15 <- data2 %>% filter(Metric == "GSA" & Temp == 15 )
data2_GSA20 <- data2 %>% filter(Metric == "GSA" & Temp == 20 )

data2_RMR   <- data2 %>% filter(Metric == "RMR")
data2_RMR15 <- data2 %>% filter(Metric == "RMR" & Temp == 15 )
data2_RMR20 <- data2 %>% filter(Metric == "RMR" & Temp == 20 )

data2_MMR   <- data2 %>% filter(Metric == "MMR")
data2_MMR15 <- data2 %>% filter(Metric == "MMR" & Temp == 15 )
data2_MMR20 <- data2 %>% filter(Metric == "MMR" & Temp == 20 )

g00<-lm(data=data2_GSA, log10(Metric_absolute)~log10_Weight*Temp)
g15<-lm(data=data2_GSA15, log10(Metric_absolute)~log10_Weight)
g20<-lm(data=data2_GSA20, log10(Metric_absolute)~log10_Weight)

summary(g00)
confint(g00)
anova(g00)
summary(g15)
confint(g15)
summary(g20)
confint(g20)

r00<-lm(data=data2_RMR, log10(Metric_absolute)~log10_Weight*Temp)
r15<-lm(data=data2_RMR15, log10(Metric_absolute)~log10_Weight)
r20<-lm(data=data2_RMR20, log10(Metric_absolute)~log10_Weight)

summary(r00)
anova(r00)
summary(r15)
confint(r15)
summary(r20)
confint(r20)

m00<-lm(data=data2_MMR, log10(Metric_absolute)~log10_Weight*Temp)
m15<-lm(data=data2_MMR15, log10(Metric_absolute)~log10_Weight)
m20<-lm(data=data2_MMR20, log10(Metric_absolute)~log10_Weight)

summary(m00)
anova(m00)
summary(m15)
confint(m15)
summary(m20)
confint(m20)
