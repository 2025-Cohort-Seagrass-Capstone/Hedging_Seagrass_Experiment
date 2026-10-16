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
  select(-Date) |> 
  rename("Hedging_Type" = "Hedging Type")
  
# filter out the days before data collection stated (started 02-28-2026) and only keep the first 7 weeks (2/28/26 - 4/18/26) of data because the monthly summer data was very inconsistent
weekly_data <- weekly_data |> 
    filter(!Date %in% as.Date(c("2026-02-23", "2026-02-25", "2026-05-16", "2026-06-14", "2026-07-11", "2026-08-08", "2026-09-05")))

# Joining one time data across all weeks and selecting columns
weekly_coverTT<- weekly_data %>%
  left_join(onetime_data, by = "Plot ID") %>% 
  select("Date", "Plot ID", "Hedging_Type", "% Cover TT") 
  
weekly_coverTT$Hedging_Type[weekly_coverTT$Hedging_Type == "B"] <- "W"

# Calulating the days since the start of the data collection
weekly_coverTT$day <- weekly_coverTT$Date - weekly_coverTT$Date[1]
# number of days since starting is now the response variable
weekly_coverTT$day <- as.numeric(weekly_coverTT$day)

## Dealing with <1 values in % Cover TT
# Since it is unknown the exact value <1 (left sensored), best practice is to change to 0.5 (Helsel, D.R. (1990). Less than obvious: Statistical treatment of data below the detection limit. Environmental Science & Technology, 24(12), 1766–1774.) 
# Change all values in % cover TT to numeric
weekly_coverTT <- weekly_coverTT |> 
  mutate(`% Cover TT`= case_when(
    `% Cover TT` == "<1" ~ 0.5,
    TRUE ~ as.numeric(`% Cover TT`)
  )) 

# take the mean of percent TT cover for each hedging type each data collection week
week_coverTT_sum <- weekly_coverTT |> 
  group_by(Date, Hedging_Type, day) |> 
  summarize(
    mean_percent_cover = mean(`% Cover TT`))

#initial look at data
ggplot(week_coverTT_sum, aes(day, mean_percent_cover, fill = Hedging_Type))+
  geom_bar(stat="identity", position = position_dodge()) +
  facet_wrap(~Hedging_Type)




# Not using ---------------------------------------------------------------

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