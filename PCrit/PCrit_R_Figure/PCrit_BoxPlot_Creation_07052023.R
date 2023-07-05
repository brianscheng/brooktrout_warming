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
library(tibble)

PCrit_15C_1_initial <- read.csv("15C_1_PCrit_initial_06262023.csv")
PCrit_15C_2_initial <- read.csv("15C_2_PCrit_initial_06262023.csv")
PCrit_15C_1_3month <- read.csv("15C_1_PCrit_3month_06262023.csv")
PCrit_15C_2_3month <- read.csv("15C_2_PCrit_3month_06262023.csv")
PCrit_15C_1_6month <- read.csv("15C_1_PCrit_6month_06262023.csv")
PCrit_15C_2_6month <- read.csv("15C_2_PCrit_6month_06262023.csv")
PCrit_20C_1_initial <- read.csv("20C_1_PCrit_initial_06262023.csv")
PCrit_20C_2_initial <- read.csv("20C_2_PCrit_initial_06262023.csv")
PCrit_20C_1_3month <- read.csv("20C_1_PCrit_3month_06262023.csv")
PCrit_20C_2_3month <- read.csv("20C_2_PCrit_3month_06262023.csv")
PCrit_20C_1_6month <- read.csv("20C_1_PCrit_6month_06262023.csv")
PCrit_20C_2_6month <- read.csv("20C_2_PCrit_6month_06262023.csv")

df_list <- list(
  PCrit_15C_1_initial,
  PCrit_15C_2_initial,
  PCrit_15C_1_3month,
  PCrit_15C_2_3month,
  PCrit_15C_1_6month,
  PCrit_15C_2_6month,
  PCrit_20C_1_initial,
  PCrit_20C_2_initial,
  PCrit_20C_1_3month,
  PCrit_20C_2_3month,
  PCrit_20C_1_6month,
  PCrit_20C_2_6month)

# Get the original file names without the ".csv" extension
file_names <- c("PCrit_15C_1_initial",
                "PCrit_15C_2_initial",
                "PCrit_15C_1_3month",
                "PCrit_15C_2_3month",
                "PCrit_15C_1_6month",
                "PCrit_15C_2_6month",
                "PCrit_20C_1_initial",
                "PCrit_20C_2_initial",
                "PCrit_20C_1_3month",
                "PCrit_20C_2_3month",
                "PCrit_20C_1_6month",
                "PCrit_20C_2_6month"
)

# Set the original file names as names for the data frames
df_list <- setNames(df_list, file_names)

# Function to transpose a data frame
transpose_df <- function(df) {
  return(t(df))
}

# Transpose each data frame in the list
transposed_df_list <- lapply(df_list, transpose_df)

# Save each transposed data frame as individual data frames
for (i in 1:length(transposed_df_list)) {
  df_name <- paste0("transposed_df_", file_names[i])
  assign(df_name, transposed_df_list[[i]])
}

df_list2 <- list(
  transposed_df_PCrit_15C_1_initial,
  transposed_df_PCrit_15C_2_initial,
  transposed_df_PCrit_15C_1_3month,
  transposed_df_PCrit_15C_2_3month,
  transposed_df_PCrit_15C_1_6month,
  transposed_df_PCrit_15C_2_6month,
  transposed_df_PCrit_20C_1_initial,
  transposed_df_PCrit_20C_2_initial,
  transposed_df_PCrit_20C_1_3month,
  transposed_df_PCrit_20C_2_3month,
  transposed_df_PCrit_20C_1_6month,
  transposed_df_PCrit_20C_2_6month)

# Set the original file names as names for the data frames
df_list2 <- setNames(df_list2, file_names)

# Define the new column names

new_column_names <- c("Alpha", "Break", "LLO", "NLR", "Sub-")

# Loop through each data frame
for (i in 1:length(df_list2)) {
  # Rename the columns of the current data frame
  colnames(df_list2[[i]]) <- new_column_names
}

# Save each transposed data frame as individual data frames
for (i in 1:length(df_list2)) {
  df_name <- paste0("edited_", file_names[i])
  assign(df_name, df_list2[[i]])
}

temp_15 <- 15
temp_20 <- 20
replicate_1 <- 1
replicate_2 <- 2
time_20 <- 20
time_98 <- 98
time_190 <- 190

# Add the new columns to the data frame using cbind()
converted_PCrit_15C_1_initial <- cbind(edited_PCrit_15C_1_initial, Time = time_20, Temp = temp_15, Replicate = replicate_1)
converted_PCrit_15C_2_initial <- cbind(edited_PCrit_15C_2_initial, Time = time_20, Temp = temp_15, Replicate = replicate_2)
converted_PCrit_15C_1_3month <- cbind(edited_PCrit_15C_1_3month, Time = time_98, Temp = temp_15, Replicate = replicate_1)
converted_PCrit_15C_2_3month <- cbind(edited_PCrit_15C_2_3month, Time = time_98, Temp = temp_15, Replicate = replicate_2)
converted_PCrit_15C_1_6month <- cbind(edited_PCrit_15C_1_6month, Time = time_190, Temp = temp_15, Replicate = replicate_1)
converted_PCrit_15C_2_6month <- cbind(edited_PCrit_15C_2_6month, Time = time_190, Temp = temp_15, Replicate = replicate_2)

converted_PCrit_20C_1_initial <- cbind(edited_PCrit_20C_1_initial, Time = time_20, Temp = temp_20, Replicate = replicate_1)
converted_PCrit_20C_2_initial <- cbind(edited_PCrit_20C_2_initial, Time = time_20, Temp = temp_20, Replicate = replicate_2)
converted_PCrit_20C_1_3month <- cbind(edited_PCrit_20C_1_3month, Time = time_98, Temp = temp_20, Replicate = replicate_1)
converted_PCrit_20C_2_3month <- cbind(edited_PCrit_20C_2_3month, Time = time_98, Temp = temp_20, Replicate = replicate_2)
converted_PCrit_20C_1_6month <- cbind(edited_PCrit_20C_1_6month, Time = time_190, Temp = temp_20, Replicate = replicate_1)
converted_PCrit_20C_2_6month <- cbind(edited_PCrit_20C_2_6month, Time = time_190, Temp = temp_20, Replicate = replicate_2)

## Converts to data frames and then removes row names

converted_PCrit_15C_1_initial <- as.data.frame(converted_PCrit_15C_1_initial)
converted_PCrit_15C_1_initial <- rownames_to_column(converted_PCrit_15C_1_initial, var = "Tank")

converted_PCrit_15C_2_initial <- as.data.frame(converted_PCrit_15C_2_initial)
converted_PCrit_15C_2_initial <- rownames_to_column(converted_PCrit_15C_2_initial, var = "Tank")

converted_PCrit_15C_1_3month <- as.data.frame(converted_PCrit_15C_1_3month)
converted_PCrit_15C_1_3month <- rownames_to_column(converted_PCrit_15C_1_3month, var = "Tank")

converted_PCrit_15C_2_3month <- as.data.frame(converted_PCrit_15C_2_3month)
converted_PCrit_15C_2_3month <- rownames_to_column(converted_PCrit_15C_2_3month, var = "Tank")

converted_PCrit_15C_1_6month <- as.data.frame(converted_PCrit_15C_1_6month)
converted_PCrit_15C_1_6month <- rownames_to_column(converted_PCrit_15C_1_6month, var = "Tank")

converted_PCrit_15C_2_6month <- as.data.frame(converted_PCrit_15C_2_6month)
converted_PCrit_15C_2_6month <- rownames_to_column(converted_PCrit_15C_2_6month, var = "Tank")

converted_PCrit_20C_1_initial <- as.data.frame(converted_PCrit_20C_1_initial)
converted_PCrit_20C_1_initial <- rownames_to_column(converted_PCrit_20C_1_initial, var = "Tank")

converted_PCrit_20C_2_initial <- as.data.frame(converted_PCrit_20C_2_initial)
converted_PCrit_20C_2_initial <- rownames_to_column(converted_PCrit_20C_2_initial, var = "Tank")

converted_PCrit_20C_1_3month <- as.data.frame(converted_PCrit_20C_1_3month)
converted_PCrit_20C_1_3month <- rownames_to_column(converted_PCrit_20C_1_3month, var = "Tank")

converted_PCrit_20C_2_3month <- as.data.frame(converted_PCrit_20C_2_3month)
converted_PCrit_20C_2_3month <- rownames_to_column(converted_PCrit_20C_2_3month, var = "Tank")

converted_PCrit_20C_1_6month <- as.data.frame(converted_PCrit_20C_1_6month)
converted_PCrit_20C_1_6month <- rownames_to_column(converted_PCrit_20C_1_6month, var = "Tank")

converted_PCrit_20C_2_6month <- as.data.frame(converted_PCrit_20C_2_6month)
converted_PCrit_20C_2_6month <- rownames_to_column(converted_PCrit_20C_2_6month, var = "Tank")

##

A_col <- "#9933FF"
B_col <- "#FF6666"

# Create a list of 12 data frames
df_list3 <- list(
  converted_PCrit_15C_1_initial,
  converted_PCrit_15C_2_initial,
  converted_PCrit_15C_1_3month,
  converted_PCrit_15C_2_3month,
  converted_PCrit_15C_1_6month,
  converted_PCrit_15C_2_6month,
  converted_PCrit_20C_1_initial,
  converted_PCrit_20C_2_initial,
  converted_PCrit_20C_1_3month,
  converted_PCrit_20C_2_3month,
  converted_PCrit_20C_1_6month,
  converted_PCrit_20C_2_6month
)

# Bind the data frames together by row
Pcrit_compiled <- do.call(rbind, df_list3)

Pcrit_compiled <- Pcrit_compiled[, c("Tank", "LLO", "Time", "Temp", "Replicate")]

## Remove NAs

Pcrit_compiled <- Pcrit_compiled[complete.cases(Pcrit_compiled), ]

## Outlier Test

rosner.out_PCrit <- rosnerTest(Pcrit_compiled$LLO, k=10)$all.stats
rosner.out_PCrit <- subset(rosner.out_PCrit, Outlier %in% "TRUE")

Pcrit_compiled <- Pcrit_compiled[-rosner.out_PCrit$Obs.Num, ]

## Make Time, Temp, and LLO numerics

Pcrit_compiled$Time <- as.character(Pcrit_compiled$Time)
Pcrit_compiled$Temp <- as.character(Pcrit_compiled$Temp)
Pcrit_compiled$LLO <- as.numeric(Pcrit_compiled$LLO)

# Create a custom order for the Time variable
time_order <- c("20", "98", "190")

# Convert Time to a factor with the custom order
Pcrit_compiled$Time <- factor(Pcrit_compiled$Time, levels = time_order)

# Rename columns

names(Pcrit_compiled)[names(Pcrit_compiled) == "Tank"] <- "Chamber"
names(Pcrit_compiled)[names(Pcrit_compiled) == "Replicate"] <- "Tank"

# Analysis of PCrit

str(Pcrit_compiled)
Pcrit_compiled01  <-lmer(LLO ~ Temp * Time + (1|Tank), data = Pcrit_compiled)
Pcrit_compiled01a <-lmer(log10(LLO) ~ Temp * Time + (1|Tank), data = Pcrit_compiled)

summary(Pcrit_compiled01)
summary(Pcrit_compiled01a)

plot(Pcrit_compiled01)
plot(Pcrit_compiled01a)

Pcrit_compiled$residuals<-residuals(Pcrit_compiled01a)
ggplot(Pcrit_compiled,aes(x=Time, y= residuals))+geom_boxplot()
ggplot(Pcrit_compiled,aes(x=Temp, y= residuals))+geom_boxplot()+facet_grid(.~Time)

anova(Pcrit_compiled01a, ddf = "Kenward-Roger")

emmPcrit_compiled = emmeans (Pcrit_compiled01a, ~ Temp | Time)
contrast(emmPcrit_compiled, contrast = TRUE)
pairs(emmPcrit_compiled)
pairs(emmPcrit_compiled, by = "Temp")
plot(emmPcrit_compiled)


# Plot with the custom order on the x-axis
ggplot(Pcrit_compiled, aes(x = Time, y = LLO, fill = Temp, group = interaction(Temp, Time))) +
  geom_boxplot() +
  labs(x = "Days in Experiment", y = expression(P[Crit]~"(% air saturation)"), fill = "Treatment") +
  scale_fill_manual(values = c("15" = A_col, "20" = B_col)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), axis.line = element_line(),
        axis.text = element_text(size = 12), axis.title.x = element_text(size = 16), 
        axis.title.y = element_text(size = 16)) +  
  scale_y_continuous(limits = c(15, 40))

ggsave("PCrit_boxplot_06262023.png", width = 6, height = 6, dpi = 600)

# Create CVS of Data

write.csv(Pcrit_compiled, "PCrit_LLOatMR_LongForm_Merged_07052023.csv")
