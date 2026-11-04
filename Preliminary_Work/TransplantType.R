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
excel_sheets("C:/Users/adamk/Desktop/GIS Class coding/Seagrass-Stats/Brewers Plots/DataFiles/Capstone Data_shootCountAdj.xlsx")
shootraw <- read_excel("C:/Users/adamk/Desktop/GIS Class coding/Seagrass-Stats/Brewers Plots/DataFiles/Capstone Data_shootCountAdj.xlsx", sheet = "Weekly Brewers Bay monitoring")

onetime <- read_xlsx("C:/Users/adamk/Desktop/GIS Class coding/Seagrass-Stats/Brewers Plots/DataFiles/Capstone Data_4_20.xlsx", sheet = "One Time Measurements") %>% 
  select(-Date)

shootraw <- shootraw %>% 
   filter(!shootraw$Date %in% as.Date(c( "2026-02-23", "2026-02-25")))

shoot_join<- shootraw %>%
  left_join(onetime, by = "Plot ID") %>% 
  select("Date", "Plot ID":"Grazing (Y/N)", "Hedging Type") 
unique(shoot_join$Date)

#Make all three columns numeric
glimpse(shoot_join)
shoot_join$`Shoot count Pb` <- as.numeric(shoot_join$`Shoot count Pb`)
shoot_join$`Shoot count Pp` <- as.numeric(shoot_join$`Shoot count Pp`)
shoot_join$`Shoot count B` <- as.numeric(shoot_join$`Shoot count B`)

#Plot for shoot count of Plastic Bag
Pb <- ggplot(data = shoot_join)+
  geom_boxplot(aes(x = Date, y = `Shoot count Pb`, group = Date))

#Plot for shoot count of Peat Pot
Pp <- ggplot(data = shoot_join)+
  geom_boxplot(aes(x = Date, y = `Shoot count Pp`, group = Date))

#Plot for shoot count of Bucket
B <- ggplot(data = shoot_join)+
  geom_boxplot(aes(x = Date, y = `Shoot count B`, group = Date))

#Combined Graph
Pb / Pp / B

#Create a new column for the sum of the transplants' shoots
shootsum <- shoot_join %>% 
  mutate(shoots_sum = rowSums(across(c(`Shoot count Pb`, `Shoot count Pp`, `Shoot count B`)))) %>% 
  mutate(
    `Hedging Type`=dplyr::recode(`Hedging Type`,
                        "B" = "WB",
                        "Control" = "C",
                        "G" = "GB",
                        "M" = "SCR",
                        "R" = "LCR")
  )

#Plot for sum of shoot counts
sum <- ggplot(data = shootsum)+
  geom_boxplot(aes(x = Date, y = shoots_sum, group = Date))+
  geom_jitter(aes(x = Date, y = shoots_sum, group = Date), size = 1
              )
#Graph it
sum

###################################################################################

###########Need to turn this data to long############

shoots_long <- shootsum %>% 
  select(Date, `Plot ID`, `Shoot count Pb`, `Shoot count Pp`, `Shoot count B`, `Grazing (Y/N)`, `Hedging Type`, shoots_sum) %>% 
  filter((!is.na(Date)))
  
shoots_longer <- shoots_long %>% 
  pivot_longer(
    cols = c(`Shoot count Pb`, `Shoot count Pp`, `Shoot count B`), # The columns you want to collapse
    names_to = "Treatment_type",  # Name of the new column for the old column headers
    values_to = "Shoot_Count"   # Name of the new column for the cell values
  )

shoots_longer <- shoots_longer %>% 
  mutate(
    Treatment_type=recode(Treatment_type,
                          "Shoot count Pb" = "Pb",
                          "Shoot count Pp" = "Pp",
                          "Shoot count B" = "B"
  ))
  
unique(shootsum$Date)


# Calulating the days since the start of the data collection
shoots_longer$day <- shoots_longer$Date - shoots_longer$Date[1]
# number of days since starting is now the response variable
shoots_longer$day <- as.numeric(shoots_longer$day)






library(lme4)


# 1. Fit the Negative Binomial GLMM
# (1 | plant_id) specifies a random intercept for each individual plant
glmm_model <- glmer(Shoot_Count ~ Treatment_type * day + (1 | `Plot ID`), 
                       data = shoots_longer)

# 2. Check the model summary
summary(glmm_model)

#####################################################################################
library(glmmTMB)

plant_model <- glmmTMB(Shoot_Count ~ Treatment_type + day + (1 | `Plot ID`), 
                       data = shoots_longer, 
                       family = nbinom2)
summary(plant_model)
check_overdispersion(plant_model)

library(emmeans)

# 1. Get the averages for each treatment type
treatment_means <- emmeans(plant_model, specs = ~ Treatment_type)

# 2. Compare them all against each other
pairs(treatment_means, adjust = "tukey")
