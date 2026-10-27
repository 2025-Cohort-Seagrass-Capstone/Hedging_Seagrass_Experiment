#Adam Krogmann
#Oct 18, 2026

library(tidyverse)
library(readxl)
library(sjPlot)
library(performance)
library(ggeffects)
library(patchwork)


excel_sheets("Brewers Plots/DataFiles/Capstone Fish Counts.xlsx")
fishraw <- read_excel("Brewers Plots/DataFiles/Capstone Fish Counts.xlsx", sheet = "FishCounts")
classraw <- read_excel("Brewers Plots/DataFiles/Capstone Fish Counts.xlsx", sheet = "FishMetadata")

excel_sheets("Brewers Plots/DataFiles/Capstone Fish Counts RandC.xlsx")
fishraw2 <- read_excel("Brewers Plots/DataFiles/Capstone Fish Counts RandC.xlsx", sheet = "FishCounts")


###random and control are the same###
# 2. Count how many species/count pairs you have
# (If you have 5 species columns, this will equal 5)
num_pairs <- ncol(fishraw %>% select(starts_with("species")))

# 3. Stack the columns and repeat the hedging type
fish_long <- data.frame(
  # Replace 'hedging_type' with your exact column name
  hedging = rep(fishraw$Hedging_Type, times = num_pairs), 
  species = unlist(fishraw %>% select(starts_with("species")), use.names = FALSE),
  count   = unlist(fishraw %>% select(starts_with("count")), use.names = FALSE),
  ID = rep(fishraw$Plot_ID, times = num_pairs)
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
  count   = unlist(fishraw2 %>% select(starts_with("count")), use.names = FALSE),
  ID = rep(fishraw$Plot_ID, times = num_pairs)
)

#filter out a third no hedge value
fish_long_cooked <- fish_long2 %>% 
  dplyr::filter(fish_long2$hedging != "no hedge")

unique(fish_long2$hedging)

#Arrange the hedging types to be consistent
fish_long_cooked$hedging <- factor(fish_long_cooked$hedging, levels = c("LCR", "SCR", "WB", "GB", "no hedge (control)", "no hedge (random)"))

#Total counts
ggplot(data = abundance_summary)+
  geom_col(aes(x = count, y = hedging, fill = Family))

#Average species
fish_average <- fish_long_cooked %>%
  group_by(hedging, species) %>%                  # Group by your hedging column
  mutate(avg_count = mean(count, na.rm = TRUE)) %>%   # Calculate the average
  mutate(sd_count = sd(count, na.rm = TRUE))

fish_average$hedging <- factor(fish_average$hedging, levels = c("LCR", "SCR", "WB", "GB", "no hedge (control)", "no hedge (random)"))

ggplot(data = fish_long_cooked)+
  geom_boxplot(aes(x = count, y = hedging, fill = hedging)) +
  #geom_jitter(aes(x = count, y = hedging), width = 0.15, height = 0) +
  labs(y = "Hedging Type", x = "Average Count (Per Hedging Unit)", title = "Distribution of Fish Counts by Hedging Type")
############
anova_model <- aov(count ~ hedging, data = fish_long_cooked)
summary(anova_model)

tukey_result <- TukeyHSD(anova_model)
print(tukey_result)
##############


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


#####################################################################################

unique_counts <- abundance_summary %>%
  group_by(hedging) %>%
  summarize(unique_family_count = n_distinct(Family))

#Arrange the hedging types to be consistent
unique_counts$hedging <- factor(unique_counts$hedging, levels = c("LCR", "SCR", "WB", "GB", "no hedge (control)", "no hedge (random)"))

#Total counts
ggplot(data = unique_counts)+
  geom_col(aes(x = unique_family_count, y = hedging))

######################################################################################
classraw <- classraw %>% 
  rename(species = Common_Name)

#Left join the class
joined_class <- left_join(fish_long_cooked, classraw, by = "species")

# 2. Calculate Total and Relative Abundance simultaneously
abundance_summary <- joined_class %>%
  group_by(hedging) %>%
  mutate(Total_Site_Abundance = sum(count)) %>%  # Total per site
  group_by(hedging, species) %>%
  mutate(
    Absolute_Abundance = sum(count),
    Relative_Abundance = Absolute_Abundance / first(Total_Site_Abundance)
  )

#############LCR####################
ab_LCR <- abundance_summary %>%
  filter(hedging == "LCR") %>% 
  mutate(ab_per_site = Absolute_Abundance/3)
LCR <- ggplot(data = ab_LCR)+
  geom_boxplot(aes(x = count, y = Class))+
  labs(title = "LCR")
LCR

#############SCR####################
ab_SCR <- abundance_summary %>%
  filter(hedging == "SCR") %>% 
  mutate(ab_per_site = Absolute_Abundance/3)
SCR <- ggplot(data = ab_SCR)+
  geom_boxplot(aes(x = count, y = Class))+
  labs(title = "SCR")
SCR

#############WB####################
ab_WB <- abundance_summary %>%
  filter(hedging == "WB") %>% 
  mutate(ab_per_site = Absolute_Abundance/3)
WB <- ggplot(data = ab_WB)+
  geom_boxplot(aes(x = count, y = Class))+
  labs(title = "WB")
WB

#############GB####################
ab_GB <- abundance_summary %>%
  filter(hedging == "GB") %>% 
  mutate(ab_per_site = Absolute_Abundance/3)
GB <- ggplot(data = ab_GB)+
  geom_boxplot(aes(x = count, y = Class))+
  labs(title = "GB")
GB
#############no hedge (control)####################
ab_Con <- abundance_summary %>%
  filter(hedging == "no hedge (control)") %>% 
  mutate(ab_per_site = Absolute_Abundance/3)
Con <- ggplot(data = ab_Con)+
  geom_boxplot(aes(x = count, y = Class))+
  labs(title = "no hedge (control)")
Con

#############LCR####################
ab_Ran <- abundance_summary %>%
  filter(hedging == "no hedge (random)") %>% 
  mutate(ab_per_site = Absolute_Abundance/3)
Ran <- ggplot(data = ab_Ran)+
  geom_boxplot(aes(x = count, y = Class))+
  labs(title = "no hedge (random)")
Ran

LCR + SCR + GB + WB + Con + Ran 
################################################################
#fish_long_cooked
glimpse(fish_long_cooked)

fishmodel <- aov(count ~ hedging, data = abundance_summary)
summary(fishmodel)


shapiro.test(residuals(fishmodel))
qqnorm(residuals(fishmodel))
qqline(residuals(fishmodel))


# Make "Random" the reference/baseline level
abundance_summary$hedging <- relevel(abundance_summary$hedging, ref = "no hedge (random)")

abundance_summary$hedging <- factor(abundance_summary$hedging, levels = c("no hedge (random)", "no hedge (control)", "GB", "WB", "SCR", "LCR"))

model_poisR <- glm(
  count ~ hedging,
  family = poisson,
  data = abundance_summary
)

summary(model_poisR)

# Make "Control" the reference/baseline level
abundance_summary$hedging <- relevel(abundance_summary$hedging, ref = "no hedge (control)")

abundance_summary$hedging <- factor(abundance_summary$hedging, levels = c("no hedge (control)", "no hedge (random)", "GB", "WB", "SCR", "LCR"))

model_poisC <- glm(
  count ~ hedging,
  family = poisson,
  data = abundance_summary
)

summary(model_poisC)


# Make "LCR" the reference/baseline level
abundance_summary$hedging <- relevel(abundance_summary$hedging, ref = "LCR")

abundance_summary$hedging <- factor(abundance_summary$hedging, levels = c("LCR", "no hedge (control)", "no hedge (random)", "GB", "WB", "SCR"))

model_poisLCR <- glm(
  count ~ hedging,
  family = poisson,
  data = abundance_summary
)

summary(model_poisLCR)

check_overdispersion(model_poisC)
check_overdispersion(model_poisR)
check_overdispersion(model_poisLCR)

##############################################
library(MASS)

# Make "LCR" the reference/baseline level
abundance_summary$hedging <- relevel(abundance_summary$hedging, ref = "LCR")

abundance_summary$hedging <- factor(abundance_summary$hedging, levels = c("LCR", "no hedge (control)", "no hedge (random)", "GB", "WB", "SCR"))

model.nbLCR <- glm.nb(
  count ~ hedging,
  data = abundance_summary
)
summary(model.nbLCR)
###########################
abundance_summary$hedging <- relevel(abundance_summary$hedging, ref = "no hedge (control)")

abundance_summary$hedging <- factor(abundance_summary$hedging, levels = c("no hedge (control)", "no hedge (random)", "GB", "WB", "SCR", "LCR"))

model.nbC <- glm.nb(
  count ~ hedging,
  data = abundance_summary
)
summary(model.nbC)
##############################
abundance_summary$hedging <- relevel(abundance_summary$hedging, ref = "no hedge (random)")

abundance_summary$hedging <- factor(abundance_summary$hedging, levels = c("no hedge (random)", "no hedge (control)", "GB", "WB", "SCR", "LCR"))

model.nbR <- glm.nb(
  count ~ hedging,
  data = abundance_summary
)
summary(model.nbR)

check_overdispersion(model.nbC)
check_overdispersion(model.nbR)
check_overdispersion(model.nbLCR)

model_null <- glm.nb(
  count ~ 1,
  data = abundance_summary
)

anova(model_null, model.nbC, test = "LRT")
library(emmeans)

emmeans(model.nbC, pairwise ~ hedging, type = "response", adjust = "tukey")
#############################################################################
library(dplyr)

richness_fam <- abundance_summary %>%
  group_by(ID, hedging) %>%
  dplyr::summarise(
    family_richness = n_distinct(Family, na.rm = TRUE),
    .groups = "drop"
  )

str(richness_fam)
summary(richness_fam$family_richness)

boxplot(
  family_richness ~ hedging,
  data = richness_fam
)

model_richness <- glm.nb(
  family_richness ~ hedging,
  data = richness_fam
)

summary(model_richness)

check_overdispersion(model_richness)

model_poisfam <- glm(
  family_richness ~ hedging,
  family = poisson(link = "log"),
  data = richness_fam
)
summary(model_poisfam)
check_overdispersion(model_poisfam)


model_richness_null <- glm(
  family_richness ~ 1,
  family = poisson(link = "log"),
  data = richness_fam
)

anova(
  model_richness_null,
  model_richness,
  test = "Chisq"
)

richness_results <- emmeans(
  model_richness,
  pairwise ~ hedging,
  type = "response",
  adjust = "tukey"
)
summary(richness_results)
richness_results

#########################################

install.packages("vegan")  # Run once
library(dplyr)
library(vegan) 
family_counts <- abundance_summary %>%
  count(ID, hedging, Family, name = "countfam")
community <- xtabs(
    countfam ~ ID + Family,
    data = family_counts
  )
shannon <- diversity(community, index = "shannon")

shannon_results <- data.frame(
  ID = rownames(community),
  shannon = as.numeric(shannon)
)
shannon_results <- shannon_results %>%
  left_join(
    abundance_summary %>% distinct(ID, hedging),
    by = "ID"
  )
boxplot(shannon ~ hedging, data = shannon_results)

shapiro.test(shannon_results$shannon)

model_shannon <- aov(
  shannon ~ hedging,
  data = shannon_results
)

shapiro.test(residuals(model_shannon))
qqnorm(residuals(model_shannon))
qqline(residuals(model_shannon), col = "red")

library(car)
leveneTest(shannon ~ hedging, data = shannon_results)
kruskal.test(shannon ~ hedging, data = shannon_results)


pairwise.wilcox.test(
  shannon_results$shannon,
  shannon_results$hedging,
  p.adjust.method = "BH"
)

shannon_results |>
  dplyr::group_by(hedging) |>
  dplyr::summarise(
    median_shannon = median(shannon, na.rm = TRUE),
    mean_shannon = mean(shannon, na.rm = TRUE),
    n = dplyr::n()
  )

#############################################################
#NMDS?
library(vegan)
library(dplyr)

ab_clean <- abundance_summary %>% 
group_by(Family, hedging, ID) %>%
  summarise(across(where(is.numeric), sum)) %>% 
  select(hedging, Family, Absolute_Abundance, ID)#%>% 
  #drop_na()

ab_clean_wide <- ab_clean %>% 
  pivot_wider(
    names_from = Family, 
    values_from = Absolute_Abundance
  )
  

justrich <- ab_clean_wide  %>% 
  ungroup() %>% 
  select("Acanthuridae":"Pomacentridae") 
justrich[is.na(justrich)] <- 0
env.data <- ab_clean_wide %>% 
  ungroup() %>% 
  select(hedging)



nmds_result <- metaMDS(justrich, distance = "bray", k = 2, trymax = 100)
print(nmds_result$stress)
sample_coords <- as.data.frame(scores(nmds_result, display = "sites"))

sample_coords$hedging <- env.data$hedging 

plot(nmds_result, type = "n", main = "NMDS of Family Richness by Hedging Type")
colors <- as.numeric(as.factor(sample_coords$hedging))
points(nmds_result, display = "sites", col = colors, pch = 16, cex = 1.5)

ordiellipse(nmds_result, groups = sample_coords$hedging, 
            kind = "sd", label = TRUE, col = 1:max(colors), lwd = 2)
