## % TT and hedging type

library(tidyverse)
library(readxl)
library(sjPlot)
library(performance)
library(ggeffects)
library(patchwork)


# Data across all weeks 
weekly <- read_xlsx("Brewers Plots/DataFiles/Capstone Data_9_22.xlsx", sheet = "Weekly Brewers Bay monitoring")
weekly$Date <- as.Date(weekly$Date)
  
  # Extracting the width, length, height one time measurements
  onetime <- read_xlsx("Brewers Plots/DataFiles/Capstone Data_9_22.xlsx", sheet = "One Time Measurements") %>% 
  select(-Date)


# Joining one time data across all weeks and selecting columns
brewers_weekly<- weekly %>%
  left_join(onetime, by = "Plot ID") %>% 
  select("Date", "Plot ID", "Hedging Type", "% Cover TT") 

brewers_weekly <- brewers_weekly %>%
  rename("Hedging.Type" = "Hedging Type")
brewers_weekly$Hedging.Type[brewers_weekly$Hedging.Type == "B"] <- "W"

# Calulating the days since the start of the data collection
brewers_weekly$day <- brewers_weekly$Date - brewers_weekly$Date[1]
# number of days since starting is now the response variable
brewers_weekly$day <- as.numeric(brewers_weekly$day)

## change % cover TT ot numeric
brewers_weekly$`% Cover TT` <- as.numeric(brewers_weekly$`% Cover TT`)

ggplot(brewers_weekly, aes(day, `% Cover TT`, fill = Hedging.Type))+
  geom_bar(stat="identity")

##Presence absence: 1 if TT present (>0), 0 if TT absent (0)
brewers_weekly$TTpresent <- ifelse(brewers_weekly$`% Cover TT` > 0, 1, 0)

## Meets Hedge with time
presenceTT <- glm(TTpresent ~ Hedging.Type + day, family = "binomial", data = brewers_weekly)
summary(presenceTT)

check_model(presenceTT)
check_overdispersion(presenceTT)

#***meeting hedge prob***
pred_TT <- ggpredict(presenceTT, terms = c("day", "Hedging.Type"))

#shows at what day is TT present in the plot
prob_t <- ggplot(pred_TT, aes(x = x, y = predicted, color = group)) +
    geom_line(size = 1) +
     geom_jitter(data = brewers_weekly, aes(x = day, y = TTpresent, color = Hedging.Type),
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