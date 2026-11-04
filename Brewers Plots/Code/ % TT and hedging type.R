## % TT and hedging type


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(sjPlot)
library(performance)
library(ggeffects)
library(patchwork)
library(car)


# Data Import and Cleaning -------------------------------------------------------------

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
hedge_avg_TTcover <- weekly_coverTT |> 
  group_by(Date, Hedging_Type, day) |> 
  summarize(
    mean_percent_cover = mean(`% Cover TT`)) |>
  ungroup() |>
    rename("No Hedge (Control)" = C)

facet_names <- c("C" = "No Hedge (Control)", "GB" = "GB", "LCR" = "LCR", "SCR" = "SCR", "WB" = "WB")
                 
#Mean percent cover of TT over day (7 weeks)
TTpercent <- ggplot(hedge_avg_TTcover, aes(day, mean_percent_cover, fill = Hedging_Type))+
  geom_bar(stat="identity", position = position_dodge()) +
  facet_wrap(~Hedging_Type,
    labeller = as_labeller(facet_names)) +
  labs(
    y = expression(paste("Mean percent cover of ", italic("T. testudinum"), "(%)")), 
    x = expression(paste("Day Since ", italic("T. testudinum"), " Plug Planting")))+
  scale_fill_manual(
    breaks = c("C", "GB", "LCR", "SCR", "WB"),
    labels = c(
      "WB" = "WB",
      "C" = "No Hedge (Control)",
      "GB" = "GB",
      "M" = "SCR",
      "R" = "LCR"),
    values = c(
      "C" = "#22a884ff",
      "GB" = "#2a788eff",
      "LCR" = "#414487ff",
      "SCR" = "#fde725ff",
      "WB" = "#440154ff"),
    guide = "none")+
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    strip.background = element_rect(fill = "lightgray", color = "black"),
    strip.text = element_text(size = 15))
TTpercent
ggsave("Brewers Plots/Figures/Publication_Ready/Figure_S1.png", plot = TTpercent, dpi = 600, width = 7,height = 4,units = "in") 
  

mean <- mean(hedge_avg_TTcover$mean_percent_cover)
med <- median(hedge_avg_TTcover$mean_percent_cover)

ggplot(hedge_avg_TTcover, aes(x = mean_percent_cover)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_vline(aes(xintercept = mean, color = "Mean"), linewidth = 1.2) +
  geom_vline(aes(xintercept = med, color = "Median"), linetype = "dashed", linewidth = 1.2) +
  labs(y = "frequency")

# Raw TT t-test -----------------------------------------------------------
# t-test on raw TT cover data between first and last week

FL <- weekly_coverTT |> 
  filter(Date %in% as.Date(c("2026-02-28", "2026-04-18"))) 

FL$`% Cover TT` <- FL$`% Cover TT`/100

# did logit bc some values are 0 (if log then the result would be -Inf)
FL <- FL |> 
  mutate(
    week = case_when(
      Date == "2026-02-28" ~ "first",
      Date == "2026-04-18" ~ "last",
      TRUE  ~ "other"),  
    logit_perc_cover = logit(`% Cover TT`, percents = TRUE))

shapiro.test(FL$logit_perc_cover)
# data:  FL$`% Cover TT`
# W = 0.78469, p-value = 3.491e-05

# After Transformation
# data:  FL$logit_perc_cover
# W = 0.84496, p-value = 0.0004871

t.test(logit_perc_cover ~ week, data = FL)
# Welch Two Sample t-test
# data:  logit_perc_cover by week
# t = 6.2639, df = 16.199, p-value = 1.068e-05
# alternative hypothesis: true difference in means between group first and group last is not equal to 0
# 95 percent confidence interval:
#   0.5727255 1.1578142
# sample estimates:
#   mean in group first  mean in group last 
# -2.575413           -3.440682


# Univariate Statistics --------------------------------------------------
# Testing the first vs last week of this 7 week period for differences in % TT cover within each hedging type

first_last <- hedge_avg_TTcover |> 
  filter(Date %in% as.Date(c("2026-02-28", "2026-04-18"))) 

first_last <- first_last |> 
  mutate(
    week = case_when(
      Date == "2026-02-28" ~ "first",
      Date == "2026-04-18" ~ "last",
      TRUE  ~ "other"))
  
## Assumptions:
# Independence = Met
# Normality of residuals = Not Met
mean <- mean(first_last$mean_percent_cover)
med <- median(first_last$mean_percent_cover)

ggplot(first_last, aes(x = mean_percent_cover)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_vline(aes(xintercept = mean, color = "Mean"), linewidth = 1.2) +
  geom_vline(aes(xintercept = med, color = "Median"), linetype = "dashed", linewidth = 1.2) +
  labs(y = "frequency")
#Bi-model, right & positively skewed

shapiro.test(first_last$mean_percent_cover)
# data:  first_last$mean_percent_cover
#W = 0.80626, p-value = 0.01727

##Transform data log because of positive skew
first_last$log_mean_per_cover <- log(first_last$mean_percent_cover)

# Normality of residuals = Met after Log tranformation
mean <- mean(FL$`% Cover TT`)
med <- median(FL$`% Cover TT`)

ggplot(FL, aes(x = `% Cover TT`)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_vline(aes(xintercept = mean, color = "Mean"), linewidth = 1.2) +
  geom_vline(aes(xintercept = med, color = "Median"), linetype = "dashed", linewidth = 1.2) +
  labs(y = "frequency")
#Bi-model, right & positively skewed

# Shapiro-Wilk normality test
shapiro.test(FL$`% Cover TT`)
# data:  first_last$log_mean_per_cover
# W = 0.90622, p-value = 0.256

## Equal variances: Met 
# Bartlett test of homogeneity of variances
bartlett.test(log_mean_per_cover ~ week, data = first_last)
# data:  log_mean_per_cover by week
# Bartlett's K-squared = 0.1312, df = 1, p-value = 0.7172

t.test(log_mean_per_cover ~ week, data = first_last)
# Welch Two Sample t-test
# data:  log_mean_per_cover by week
# t = 6.8407, df = 7.7203, p-value = 0.0001561
# alternative hypothesis: true difference in means between group first and group last is not equal to 0
# 95 percent confidence interval:
#   1.378083 2.793109
# sample estimates:
#   mean in group first  mean in group last 
# 1.6096866          -0.4759092

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