#### Fig 3 - GSA and metabolic rate
library(here)

data<-read.csv(here('data/BT_Resp_Combined_071022_BSC.csv'), stringsAsFactors = T)
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

ggplot(data=data, aes(x=log10(weight_g), y=log10(MR_absolute), group = Metric, color = Metric))+geom_point()+
  stat_smooth(method = "lm")+facet_grid(.~Temp)


