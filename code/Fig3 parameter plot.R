#figure 3 parameter plot
library(here)
library(tidyverse)
library(ggplot2)
params<-read.csv(here("data/parameter_all_traits.csv"), stringsAsFactors = T, header = T)
A_col <- "#9933FF"
B_col <- "#FF6666"
dodge <- position_dodge(width=0.5)

data.slopes<-params %>%
  mutate (Temp = factor(Temp)) %>%
  mutate (Trait = factor(Trait, levels = c("GSA", "RMR", "MMR") ))
dodge <- position_dodge(width=0.5)

fig3<-ggplot(data.slopes,aes(x=Trait, y = Slope, color = Temp))+
  geom_point(size = 4, alpha = 2/3,position = dodge)+
  geom_errorbar(aes(ymax=Slope_UCI, ymin = Slope_LCI), position = dodge, width = 0)+
  theme_bw()+theme(legend.title=element_text(size=16),legend.text=element_text(size=14),
                         axis.text.y=element_text(size=16),axis.title.y=element_text(size=18, vjust=1.2),
                         axis.text.x=element_text(size=16),axis.title.x=element_text(size=18),
                         panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(values  = c(A_col, B_col))+
  scale_color_manual(values = c(A_col, B_col))+
  geom_hline(yintercept = 1, linetype = "dashed")
fig3
ppi=300
png("figures/fig3 params.png", width=8*ppi, height=6*ppi, res=ppi)
fig3
dev.off()

ggplot(data.slopes,aes(x=Trait, y = Slope))+geom_point()
