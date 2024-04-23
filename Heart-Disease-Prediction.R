#Name: Vatsal Sharma, Dhruvi, Dhruv


#-----------------------------------------------------------  

#load all required libraries
library(tidyverse)
library(ggthemes)
library(GGally)
library(ggExtra)
library(caret)
library(glmnet)
library(corrplot)
library(DescTools)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(visdat)
library(leaflet)
library(hrbrthemes)
library(RColorBrewer)
library(ggridges)
library(cowplot)
library(ggplot2)
library(tidyverse)
library(GGally)
library(kableExtra)
library(RColorBrewer)
library(plotly)
library(visdat)
library(glue)



data <- read.csv("UCI_Heart_Disease_Dataset_Combined.csv")
data

head(data)
names(data)
summary(data)
glimpse(data)
dim(data)
names(data)
str(data)
vis_miss(data)


missing_values <- sum(is.na(data))
duplicates <- data[duplicated(data), ]
numeric_data <- data[, sapply(data, is.numeric)]  


data$Sex <- as.factor(data$Sex)
data$HeartDisease <- as.factor(data$HeartDisease)
data$ExerciseAngina <- as.factor(data$ExerciseAngina)

ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Age")

ggplot(data, aes(x = HeartDisease, y = MaxHR)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of MaxHR by Heart Disease Status")

# Define a custom palette of shades of blue
blue_palette <- c("#004c6d", "#005a84", "#00679b", "#0074b2", "#0081c9")

# Age Distribution by Gender
ggplot(data, aes(x = Age, fill = Sex)) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  scale_fill_manual(values = blue_palette) +
  labs(title = "Age Distribution by Gender", x = "Age", y = "Count") +
  theme_minimal()

# Correlation Heatmap
library(reshape2)
cor_data <- melt(cor(data), na.rm = TRUE)
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = blue_palette[1], high = blue_palette[length(blue_palette)]) +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Heart Disease Status by Chest Pain Type
ggplot(data, aes(x = ChestPainType, fill = factor(HeartDisease))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("No" = blue_palette[3], "Yes" = blue_palette[length(blue_palette)])) +
  labs(title = "Heart Disease Status by Chest Pain Type", x = "Chest Pain Type", y = "Proportion") +
  theme_minimal()

# Exercise-Induced Angina by Age Group
ggplot(data, aes(x = AgeGroup, fill = factor(ExerciseAngina))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("No" = blue_palette[3], "Yes" = blue_palette[length(blue_palette)])) +
  labs(title = "Exercise-Induced Angina by Age Group", x = "Age Group", y = "Proportion") +
  theme_minimal()

cor_data <- melt(cor(data[, sapply(data, is.numeric)]), na.rm = TRUE)
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = blue_palette[1], high = blue_palette[length(blue_palette)]) +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert HeartDisease to factor
data$HeartDisease <- as.factor(data$HeartDisease)

# Define a custom palette with shades of blue and salmon
blue_palette <- c("No" = "#4e79a7", "Yes" = "#f28e2b")

# Distribution of Cholesterol Levels by Heart Disease Status
ggplot(data, aes(x = Cholesterol, fill = HeartDisease)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Cholesterol Levels by Heart Disease Status", x = "Cholesterol", y = "Density") +
  scale_fill_manual(values = blue_palette) +
  theme_minimal()




