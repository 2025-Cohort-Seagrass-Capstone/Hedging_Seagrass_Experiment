#Author: Adam Krogmann
#Date: Oct 12, 2026

#Libraries
library(tidyverse)
library(readxl)
library(sjPlot)
library(performance)
library(ggeffects)
library(patchwork)

#Get the data
excel_sheets("C:/Users/adamk/Desktop/GIS Class coding/Seagrass-Stats/Brewers Plots/DataFiles/Capstone Data_9_22.xlsx")
shootraw <- read_excel("C:/Users/adamk/Desktop/GIS Class coding/Seagrass-Stats/Brewers Plots/DataFiles/Capstone Data_10_12.xlsx", sheet = "Weekly Brewers Bay monitoring")

onetime <- read_xlsx("C:/Users/adamk/Desktop/GIS Class coding/Seagrass-Stats/Brewers Plots/DataFiles/Capstone Data_4_20.xlsx", sheet = "One Time Measurements") %>% 
  select(-Date)

shoot_join<- shootraw %>%
  left_join(onetime, by = "Plot ID") %>% 
  select("Date", "Plot ID":"Grazing (Y/N)", "Hedging Type")

#Make all three columns numeric
glimpse(shoot_join)
shoot_join$`Shoot count Pb` <- as.numeric(shoot_join$`Shoot count Pb`)
shoot_join$`Shoot count Pp` <- as.numeric(shoot_join$`Shoot count Pp`)
shoot_join$`Shoot count B` <- as.numeric(shoot_join$`Shoot count B`)

#Plot for shoot count of Plastic Bag
Pb <- ggplot(data = shootraw)+
  geom_boxplot(aes(x = Date, y = `Shoot count Pb`, group = Date))

#Plot for shoot count of Peat Pot
Pp <- ggplot(data = shootraw)+
  geom_boxplot(aes(x = Date, y = `Shoot count Pp`, group = Date))

#Plot for shoot count of Bucket
B <- ggplot(data = shootraw)+
  geom_boxplot(aes(x = Date, y = `Shoot count B`, group = Date))

#Combined Graph
Pb / Pp / B

#Create a new column for the sum of the transplants' shoots
shootsum <- shoot_join %>% 
  mutate(shoots_sum = rowSums(across(c(`Shoot count Pb`, `Shoot count Pp`, `Shoot count B`)))) %>% 
  mutate(
    `Hedging Type`=recode(`Hedging Type`,
                        "W" = "WB",
                        "Control" = "C",
                        "G" = "GB",
                        "M" = "SCR",
                        "R" = "LCR")
  )

#Plot for sum of shoot counts
sum <- ggplot(data = shootsum)+
  geom_boxplot(aes(x = Date, y = shoots_sum, group = Date))+
  geom_jitter(aes(x = Date, y = shoots_sum, group = Date, color = `Grazing (Y/N)`), size = 2)
#Graph it
sum
