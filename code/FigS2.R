### Script for Figure S2 - supporting gill metrics

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)


####bring data in
data<-read.csv(here('data/GSA_BT_Compiled_03122023_BSC.csv'), stringsAsFactors = T)
str(data)
summary(data)

data2<-data %>%
  mutate (Temp = factor(Temp)) %>%
  mutate (Replicate = case_when (Temp == "15" & Replicate == "1"~ "1",
                                 Temp == "15" & Replicate == "2"~ "2",
                                 Temp == "20" & Replicate == "1"~ "3",
                                 Temp == "20" & Replicate == "2"~ "4")) %>%
  mutate (Replicate = factor (Replicate)) %>%
  mutate (log10_GSA = log10(GSA)) %>%
  mutate (log10_Weight = log10(Weight)) %>%
  rename (Lamellar_area = Lamallae.Area,
          Total_filament_length = total_filament_length_mm,
          Lamellar_frequency = Lamallae.Density,
          Temperature = Temp)

#units for future reference
# Frequency = Lamellae / mm
# Surface Area = mm2
# Filament Length = mm 

####basic EDA
ggplot(data = data2, aes(x= Temp, y = Total_filament_length))+geom_boxplot()+geom_point()
ggplot(data = data2, aes(x= Temp, y = Lamellar_area))+geom_boxplot()+geom_point()
ggplot(data = data2, aes(x= Temp, y = Lamellar_frequency))+geom_boxplot()+geom_point()

####basic models
m1  <-lm(log10(Total_filament_length)~log10_Weight*Temperature, data = data2)
n1  <-lm(log10(Lamellar_area)~log10_Weight*Temperature, data = data2)
o1  <-lm(log10(Lamellar_frequency)~log10_Weight*Temperature, data = data2)

summary(m1)
summary(n1)
summary(o1)

Anova(m1)
Anova(n1)
Anova(o1)

plot(m1)
plot(n1)
plot(o1)

####MS plots
A_col <- "#9933FF"
B_col <- "#FF6666"
dodge <- position_dodge(width=0.5)

figS2a<-ggplot(data = data2, aes(x= log10_Weight, y = log10(Total_filament_length), color = Temperature))+
  theme_bw()+theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
                   axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
                   axis.text.x=element_text(size=16),axis.title.x=element_text(size=18),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   legend.position = c(0.8,0.2))+
  labs(y=expression(paste("Total filament length (log"[10]," mm)")), 
                  x=expression(paste("Body mass (log"[10]," g)"))) +
  scale_fill_manual(values  = c(A_col, B_col))+
  scale_color_manual(values = c(A_col, B_col))+
  stat_smooth(method="lm")+
  geom_point()
ppi=300
png("figures/figS2a.png", width=6*ppi, height=6*ppi, res=ppi)
figS2a
dev.off()

figS2b<-ggplot(data = data2, aes(x= log10_Weight, y = log10(Lamellar_frequency), color = Temperature))+
  theme_bw()+theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
                   axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
                   axis.text.x=element_text(size=16),axis.title.x=element_text(size=18),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   legend.position = 'none')+
  labs(y=expression(paste("Lamellar frequency (log"[10]," ", "count"," ", mm^-2,")")), 
       x=expression(paste("Body mass (log"[10]," g)"))) +
  scale_fill_manual(values  = c(A_col, B_col))+
  scale_color_manual(values = c(A_col, B_col))+
  stat_smooth(method="lm")+
  geom_point()
ppi=300
png("figures/figS2b.png", width=6*ppi, height=6*ppi, res=ppi)
figS2b
dev.off()

figS2c<-ggplot(data = data2, aes(x= log10_Weight, y = log10(Lamellar_area), color = Temperature))+
  theme_bw()+theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
                   axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
                   axis.text.x=element_text(size=16),axis.title.x=element_text(size=18),
                   panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   legend.position = 'none')+
  labs(y=expression(paste("Lamellar area (log"[10]," ", mm^2,")")), 
       x=expression(paste("Body mass (log"[10]," g)"))) +
  scale_fill_manual(values  = c(A_col, B_col))+
  scale_color_manual(values = c(A_col, B_col))+
  stat_smooth(method="lm")+
  geom_point()
ppi=300
png("figures/figS2c.png", width=6*ppi, height=6*ppi, res=ppi)
figS2c
dev.off()

figS2a
figS2b
figS2c
