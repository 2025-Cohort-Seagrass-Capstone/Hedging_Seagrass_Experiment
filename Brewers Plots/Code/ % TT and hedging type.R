## % TT and hedging type

library(tidyverse)
library(readxl)
library(sjPlot)
library(performance)
library(ggeffects)
library(patchwork)


# Data across all weeks 
weekly_data <- read_xlsx("Brewers Plots/DataFiles/Capstone Data_10_14.xlsx", sheet = "Weekly Brewers Bay monitoring")
weekly_data$Date <- as.Date(weekly_data$Date)
  
  # Extracting the width, length, height one time measurements
  onetime_data <- read_xlsx("Brewers Plots/DataFiles/Capstone Data_10_14.xlsx", sheet = "One Time Measurements") %>% 
  select(-Date)
  
# filter out the days before data collection stated (started 02-28-2026)
weekly_data <- weekly_data |> 
    filter(!Date %in% as.Date(c("2026-02-23", "2026-02-25")))

# Joining one time data across all weeks and selecting columns
weekly_coverTT<- weekly_data %>%
  left_join(onetime_data, by = "Plot ID") %>% 
  select("Date", "Plot ID", "Hedging Type", "% Cover TT") 

weekly_coverTT <- weekly_coverTT %>%
  rename("Hedging.Type" = "Hedging Type")
weekly_coverTT$Hedging.Type[weekly_coverTT$Hedging.Type == "B"] <- "W"

# Calulating the days since the start of the data collection
weekly_coverTT$day <- weekly_coverTT$Date - weekly_coverTT$Date[1]
# number of days since starting is now the response variable
weekly_coverTT$day <- as.numeric(weekly_coverTT$day)

## change % cover TT to numeric
weekly_coverTT$`% Cover TT` <- as.numeric(weekly_coverTT$`% Cover TT`)

ggplot(weekly_coverTT, aes(day, `% Cover TT`, fill = Hedging.Type))+
  geom_bar(stat="identity")

##Presence absence: 1 if TT present (>0), 0 if TT absent (0)
weekly_coverTT$TTpresent <- ifelse(weekly_coverTT$`% Cover TT` > 0, 1, 0)

## presence of TT with time
presenceTT <- glm(TTpresent ~ Hedging.Type + day, family = "binomial", data = weekly_coverTT)
summary(presenceTT)

check_model(presenceTT)
check_overdispersion(presenceTT)

#***presence of TT prob***
pred_TT <- ggpredict(presenceTT, terms = c("day", "Hedging.Type"))

#shows at what day is TT present in the plot
prob_t <- ggplot(pred_TT, aes(x = x, y = predicted, color = group)) +
    geom_line(size = 1) +
     geom_jitter(data = weekly_coverTT, aes(x = day, y = TTpresent, color = Hedging.Type),
                 alpha = 0.4, width = 0.7, height = 0.01) +
     geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group),
                 alpha = 0.2, color = NA) +
    scale_color_discrete(labels = c(
      "W" = "WB",
      "C" = "Control",
      "G" = "GB",
      "M" = "SCR",
      "R" = "LCR"
    )) +
     labs(color = "Hedging Type") +
     ylab("Predicted presence of TT")+
     xlab(expression(paste("Day Since ", italic("T. testudinum"), " Plug Planting")))+
    scale_fill_discrete(guide = "none") +
    theme_minimal()+
    theme(
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 25),
      axis.title.y = element_text(size = 25),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 22),
      plot.tag = element_text(size = 25, face = "bold"))
prob_t
## do not 