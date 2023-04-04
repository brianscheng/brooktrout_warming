library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(broom)
library(stringi)

Growth_1 <- read_excel("Measurements_morphometric_07.29.2021.xlsx")
Growth_2 <- read_excel("Measurements_morphometric_08.30.2021.xlsx")
Growth_3 <- read_excel("Measurements_morphometric_09.29.2021.xlsx")
Growth_4 <- read_excel("Measurements_morphometric_11.04.2021.xlsx")
Growth_5 <- read_excel("Measurements_morphometric_12.09.2021.xlsx")
Growth_6 <- read_excel("Measurements_morphometric_01.06.2022.xlsx")
Growth_7 <- read_excel("Measurements_morphometric_03.30.2022.xlsx")

## Remove information and add date from Growth 1

Growth_1 <- Growth_1 %>%
  rename(LENGTH_1 = LENGTH,
         WEIGHT_1 = WEIGHT)
Growth_1$LAST4 <- stri_sub(Growth_1$TAGNUMBER,-4)

Growth_1 <- data.frame(T1 = "2021-07-29", Growth_1)
Growth_1$T1 = as.Date(Growth_1$T1, "%Y-%m-%d")

Growth_1 <- Growth_1[-c(3,4,7)]
## Remove information and add date from Growth 2

Growth_2 <- Growth_2 %>%
  rename(LENGTH_2 = Length,
         WEIGHT_2 = Weight,
         TAGNUMBER = `Tag Number...3`)
Growth_2$LAST4 <- stri_sub(Growth_2$TAGNUMBER,-4)

Growth_2 <- Growth_2[-c(1,2,3,4,7)]

Growth_2 <- data.frame(T2 = "2021-08-30", Growth_2)
Growth_2$T2 = as.Date(Growth_2$T2, "%Y-%m-%d")

#Merge Files

Growth_combined <- merge(Growth_1,Growth_2, by = "LAST4", all.x = TRUE)

## Remove information and add date from Growth 3

Growth_3 <- Growth_3 %>%
  rename(LENGTH_3 = FORKLEN,
         WEIGHT_3 = WEIGHT)
Growth_3$LAST4 <- stri_sub(Growth_3$TAGNUMBER,-4)

Growth_3 <- Growth_3[-c(1,3,5,6)]

Growth_3 <- data.frame(T3 = "2021-09-29", Growth_3)
Growth_3$T3 = as.Date(Growth_3$T3, "%Y-%m-%d")

#Merge Files

Growth_combined <- merge(Growth_combined,Growth_3, by = "LAST4", all.x = TRUE)

## Remove information and add date from Growth 4

Growth_4 <- Growth_4 %>%
  rename(LENGTH_4 = LENGTH,
         WEIGHT_4 = WEIGHT)
Growth_4$LAST4 <- stri_sub(Growth_4$TAGNUMBER,-4)

Growth_4 <- Growth_4[-c(1,2,3,6,7)]

Growth_4 <- data.frame(T4 = "2021-11-04", Growth_4)
Growth_4$T4 = as.Date(Growth_4$T4, "%Y-%m-%d")

#Merge Files

Growth_combined <- merge(Growth_combined,Growth_4, by = "LAST4", all.x = TRUE)

## Remove information and add date from Growth 5

Constant = 10
Growth_5 <- Growth_5[-c(246), ]
Growth_5 <- Growth_5 %>%
  replace(is.na(.), 0)

Growth_5$WEIGHT_5 <- ifelse(Growth_5$SEX == "1", Growth_5$WEIGHT*Constant, Growth_5$WEIGHT)

Growth_5 <- Growth_5 %>%
  rename(LENGTH_5 = LENGTH)

Growth_5$LAST4 <- stri_sub(Growth_5$TAGNUMBER,-4)

Growth_5 <- Growth_5[-c(1,2,3,5,6,7)]

Growth_5 <- data.frame(T5 = "2021-12-09", Growth_5)
Growth_5$T5 = as.Date(Growth_5$T5, "%Y-%m-%d")

#Merge Files

Growth_combined <- merge(Growth_combined,Growth_5, by = "LAST4", all.x = TRUE)

## Remove information and add date from Growth 6

Growth_6 <- Growth_6 %>%
  rename(LENGTH_6 = `Length (mm)`,
         TAGNUMBER = `Tag Number`,
         WEIGHT_6 = Weight)

Growth_6$LAST4 <- stri_sub(Growth_6$TAGNUMBER,-4)

Growth_6$WEIGHT_6 = as.numeric(Growth_6$WEIGHT_6)
Growth_6$LENGTH_6 = as.numeric(Growth_6$LENGTH_6)
Growth_6 <- Growth_6[-c(1,2,5)]

Growth_6 <- data.frame(T6 = "2022-01-06", Growth_6)
Growth_6$T6 = as.Date(Growth_6$T6, "%Y-%m-%d")

#Merge Files

Growth_combined <- merge(Growth_combined,Growth_6, by = "LAST4", all.x = TRUE)

## Remove information and add date from Growth 7

Growth_7 <- Growth_7 %>%
  rename(LENGTH_7 = LENGTH,
         WEIGHT_7 = WEIGHT)

Growth_7 <- Growth_7 %>%
  rename(LAST4 = TAGNUMBER)

Growth_7 <- Growth_7[-c(3,5,6,7,8)]

Growth_7 <- data.frame(T7 = "2022-03-30", Growth_7)
Growth_7$T7 = as.Date(Growth_7$T7, "%Y-%m-%d")

#Merge Files --> Output

Growth_combined <- merge(Growth_combined,Growth_7, by = "LAST4", all.x = TRUE)

Growth_combined <- Growth_combined[-c(1)]

Growth_combined <- Growth_combined %>%
  relocate(TAGNUMBER, .before = T1)

Growth_combined <- Growth_combined %>%
  relocate(TANK_Real, .before = T1)

write.csv(Growth_combined, "BT_Morphometrics_Merged_04042023.csv")

#Calculate SGR and K

## Calculate Growth Rates Between T1 and T2

Growth_combined_SGR <- Growth_combined[c(1,2)]

Growth_combined_SGR$T2minusT1 <- difftime(Growth_combined$T2,Growth_combined$T1)
Growth_combined_SGR$T2minusT1 <- as.numeric(Growth_combined_SGR$T2minusT1)

Growth_combined_SGR$SGR_1 <- ((exp((log(Growth_combined$WEIGHT_2) - log(Growth_combined$WEIGHT_1))/Growth_combined_SGR$T2minusT1))-1)*100

## Calculate Growth Rates Between T1 and T3

Growth_combined_SGR$T3minusT1 <- difftime(Growth_combined$T3,Growth_combined$T1)
Growth_combined_SGR$T3minusT1 <- as.numeric(Growth_combined_SGR$T3minusT1)

Growth_combined_SGR$SGR_2 <- ((exp((log(Growth_combined$WEIGHT_3) - log(Growth_combined$WEIGHT_1))/Growth_combined_SGR$T3minusT1))-1)*100

## Calculate Growth Rates Between T1 and T4

Growth_combined_SGR$T4minusT1 <- difftime(Growth_combined$T4,Growth_combined$T1)
Growth_combined_SGR$T4minusT1 <- as.numeric(Growth_combined_SGR$T4minusT1)

Growth_combined_SGR$SGR_3 <- ((exp((log(Growth_combined$WEIGHT_4) - log(Growth_combined$WEIGHT_1))/Growth_combined_SGR$T4minusT1))-1)*100

## Calculate Growth Rates Between T1 and T5

Growth_combined_SGR$T5minusT1 <- difftime(Growth_combined$T5,Growth_combined$T1)
Growth_combined_SGR$T5minusT1 <- as.numeric(Growth_combined_SGR$T5minusT1)

Growth_combined_SGR$SGR_4 <- ((exp((log(Growth_combined$WEIGHT_5) - log(Growth_combined$WEIGHT_1))/Growth_combined_SGR$T5minusT1))-1)*100

## Calculate Growth Rates Between T1 and T6

Growth_combined_SGR$T6minusT1 <- difftime(Growth_combined$T6,Growth_combined$T1)
Growth_combined_SGR$T6minusT1 <- as.numeric(Growth_combined_SGR$T6minusT1)

Growth_combined_SGR$SGR_5 <- ((exp((log(Growth_combined$WEIGHT_6) - log(Growth_combined$WEIGHT_1))/Growth_combined_SGR$T6minusT1))-1)*100

## Calculate Growth Rates Between T1 and T7

Growth_combined_SGR$T7minusT1 <- difftime(Growth_combined$T7,Growth_combined$T1)
Growth_combined_SGR$T7minusT1 <- as.numeric(Growth_combined_SGR$T7minusT1)

Growth_combined_SGR$SGR_6 <- ((exp((log(Growth_combined$WEIGHT_7) - log(Growth_combined$WEIGHT_1))/Growth_combined_SGR$T7minusT1))-1)*100

write.csv(Growth_combined_SGR, "BT_Morphometrics_SGR_Output_04042023.csv")

## Calculate K

Growth_combined_K <- Growth_combined[c(1,2)]

## Calculate K at Time Point 1

Growth_combined_K$K1 <- (Growth_combined$WEIGHT_1)*((Growth_combined$LENGTH_1/10)^-3)*100

## Calculate K at Time Point 2

Growth_combined_K$K2 <- (Growth_combined$WEIGHT_2)*((Growth_combined$LENGTH_2/10)^-3)*100

## Calculate K at Time Point 3

Growth_combined_K$K3 <- (Growth_combined$WEIGHT_3)*((Growth_combined$LENGTH_3/10)^-3)*100

## Calculate K at Time Point 4

Growth_combined_K$K4 <- (Growth_combined$WEIGHT_4)*((Growth_combined$LENGTH_4/10)^-3)*100

## Calculate K at Time Point 5

Growth_combined_K$K5 <- (Growth_combined$WEIGHT_5)*((Growth_combined$LENGTH_5/10)^-3)*100

## Calculate K at Time Point 6

Growth_combined_K$K6 <- (Growth_combined$WEIGHT_6)*((Growth_combined$LENGTH_6/10)^-3)*100

## Calculate K at Time Point 7

Growth_combined_K$K7 <- (Growth_combined$WEIGHT_7)*((Growth_combined$LENGTH_7/10)^-3)*100

write.csv(Growth_combined_SGR, "BT_Morphometrics_K_Output_04042023.csv")

#Prepare to Merge K and SGR Files

Growth_combined_K$LAST4 <- stri_sub(Growth_combined_K$TAGNUMBER,-4)
Growth_combined_K <- Growth_combined_K[-c(1,2)]
Growth_combined_SGR$LAST4 <- stri_sub(Growth_combined_SGR$TAGNUMBER,-4)

Growth_combined_Calculations_Output <- merge(Growth_combined_SGR,Growth_combined_K, by = "LAST4", all.x = TRUE)
Growth_combined_Calculations_Output <- Growth_combined_Calculations_Output[-c(1)]

write.csv(Growth_combined_Calculations_Output, "BT_Morphometrics_Calculations_Merged_04042023.csv")