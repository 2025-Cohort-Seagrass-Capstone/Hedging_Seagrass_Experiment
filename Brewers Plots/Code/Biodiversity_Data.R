#Adam Krogmann
#Oct 18, 2026

library(tidyverse)
library(readxl)
library(sjPlot)
library(performance)
library(ggeffects)
library(patchwork)


excel_sheets("C:/Users/adamk/Desktop/GIS Class coding/Seagrass-Stats/Brewers Plots/DataFiles/Capstone Fish Counts.xlsx")
fishraw <- read_excel("C:/Users/adamk/Desktop/GIS Class coding/Seagrass-Stats/Brewers Plots/DataFiles/Capstone Fish Counts.xlsx", sheet = "FishCounts")

excel_sheets("C:/Users/adamk/Desktop/GIS Class coding/Seagrass-Stats/Brewers Plots/DataFiles/Capstone Fish Counts RandC.xlsx")
fishraw2 <- read_excel("C:/Users/adamk/Desktop/GIS Class coding/Seagrass-Stats/Brewers Plots/DataFiles/Capstone Fish Counts RandC.xlsx", sheet = "FishCounts")


###random and control are the same###
# 2. Count how many species/count pairs you have
# (If you have 5 species columns, this will equal 5)
num_pairs <- ncol(fishraw %>% select(starts_with("species")))

# 3. Stack the columns and repeat the hedging type
fish_long <- data.frame(
  # Replace 'hedging_type' with your exact column name
  hedging = rep(fishraw$Hedging_Type, times = num_pairs), 
  species = unlist(fishraw %>% select(starts_with("species")), use.names = FALSE),
  count   = unlist(fishraw %>% select(starts_with("count")), use.names = FALSE)
)


###separating random and control###
# 2. Count how many species/count pairs you have
# (If you have 5 species columns, this will equal 5)
num_pairs2 <- ncol(fishraw2 %>% select(starts_with("species")))

# 3. Stack the columns and repeat the hedging type
fish_long2 <- data.frame(
  # Replace 'hedging_type' with your exact column name
  hedging = rep(fishraw2$Hedging_Type, times = num_pairs2), 
  species = unlist(fishraw2 %>% select(starts_with("species")), use.names = FALSE),
  count   = unlist(fishraw2 %>% select(starts_with("count")), use.names = FALSE)
)

#filter out a third no hedge value
fish_long_cooked <- fish_long2 %>% 
  dplyr::filter(fish_long2$hedging != "no hedge")

unique(fish_long2$hedging)

#Arrange the hedging types to be consistent
fish_long_cooked$hedging <- factor(fish_long_cooked$hedging, levels = c("LCR", "SCR", "WB", "GB", "no hedge (control)", "no hedge (random)"))

#Total counts
ggplot(data = fish_long_cooked)+
  geom_col(aes(x = count, y = hedging, fill = species))

#Average species
fish_average <- fish_long_cooked %>%
  group_by(hedging) %>%                  # Group by your hedging column
  mutate(avg_count = mean(count, na.rm = TRUE)) %>%   # Calculate the average
  mutate(sd_count = sd(count, na.rm = TRUE))

fish_average$hedging <- factor(fish_average$hedging, levels = c("LCR", "SCR", "WB", "GB", "no hedge (control)", "no hedge (random)"))

ggplot(data = fish_average)+
  geom_boxplot(aes(x = count, y = hedging, fill = hedging)) +
  #geom_jitter(aes(x = count, y = hedging), width = 0.15, height = 0) +
  labs(y = "Hedging Type", x = "Average Count (Per Hedging Unit)")

bioglm <- glm(count ~ hedging, family = poisson, data = fish_average)
summary(bioglm)

check_model(bioglm)
check_overdispersion(bioglm)

###########################Look at GLM against control#######################

glimpse(fish_average)
fish_average$hedging <- as.factor(fish_average$hedging)

# Make "Control" the reference/baseline level
fish_average$hedging <- relevel(fish_average$hedging, ref = "no hedge (control)")

fish_average$hedging <- factor(fish_average$hedging, levels = c("no hedge (control)", "no hedge (random)", "GB", "WB", "SCR", "LCR"))

bioglm1 <- glm(count ~ hedging, family = poisson, data = fish_average)
summary(bioglm1)
##All differ significantly except random no hedge##

###########################Look at GLM against random#######################

glimpse(fish_average)
fish_average$hedging <- as.factor(fish_average$hedging)

# Make "Control" the reference/baseline level
fish_average$hedging <- relevel(fish_average$hedging, ref = "no hedge (random)")

fish_average$hedging <- factor(fish_average$hedging, levels = c("no hedge (random)", "no hedge (control)", "GB", "WB", "SCR", "LCR"))

bioglm2 <- glm(count ~ hedging, family = poisson, data = fish_average)
summary(bioglm2)
##All differ significantly except control no hedge##

###########################Look at GLM against LCR#######################

glimpse(fish_average)
fish_average$hedging <- as.factor(fish_average$hedging)

# Make "Control" the reference/baseline level
fish_average$hedging <- relevel(fish_average$hedging, ref = "LCR")

fish_average$hedging <- factor(fish_average$hedging, levels = c("LCR", "no hedge (control)", "no hedge (random)", "GB", "WB", "SCR"))

bioglmLCR <- glm(count ~ hedging, family = poisson, data = fish_average)
summary(bioglmLCR)
##All differ significantly except SCR

#####################################################################################

counts_fish <- fish_long_cooked %>% 
  count(hedging, species, name = "species_count")

# View the result
print(counts_df)
#####################################################################################

unique_counts <- fish_long_cooked %>%
  group_by(hedging) %>%
  summarize(unique_species_count = n_distinct(species))

#Arrange the hedging types to be consistent
unique_counts$hedging <- factor(unique_counts$hedging, levels = c("LCR", "SCR", "WB", "GB", "no hedge (control)", "no hedge (random)"))

#Total counts
ggplot(data = unique_counts)+
  geom_col(aes(x = unique_species_count, y = hedging))
