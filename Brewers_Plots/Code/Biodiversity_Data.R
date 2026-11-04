## Biodiversity_Data
# Author: Adam Krogmann
# Description: Testing for differences in family richness and diversity between hedging types.

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(sjPlot)
library(performance)
library(ggeffects)
library(patchwork)
library(emmeans)
library(vegan)
library(car)

# Data Import -------------------------------------------------------------

excel_sheets("Brewers_Plots/DataFiles/Capstone Fish Counts RandC.xlsx")
fishraw2 <- read_excel("Brewers_Plots/DataFiles/Capstone Fish Counts RandC.xlsx", sheet = "FishCounts")
classraw <- read_excel("Brewers_Plots/DataFiles/Capstone Fish Counts.xlsx", sheet = "FishMetadata")

# Data Cleaning -------------------------------------------------------------

num_pairs2 <- ncol(fishraw2 %>% select(starts_with("species")))

fish_long2 <- data.frame(
  hedging = rep(fishraw2$Hedging_Type, times = num_pairs2), 
  species = unlist(fishraw2 %>% select(starts_with("species")), use.names = FALSE),
  count   = unlist(fishraw2 %>% select(starts_with("count")), use.names = FALSE),
  ID = rep(fishraw2$Plot_ID, times = num_pairs2)
)

#filter out a third no hedge value
fish_long_cooked <- fish_long2 %>% 
  dplyr::filter(fish_long2$hedging != "no hedge")

unique(fish_long_cooked$hedging)

#Arrange the hedging types to be consistent
fish_long_cooked$hedging <- factor(fish_long_cooked$hedging, levels = c("LCR", "SCR", "WB", "GB", "no hedge (control)", "no hedge (random)"))


#Average species
fish_average <- fish_long_cooked %>%
  group_by(hedging, species) %>%                  
  mutate(avg_count = mean(count, na.rm = TRUE)) %>%
  mutate(sd_count = sd(count, na.rm = TRUE))

fish_average$hedging <- factor(fish_average$hedging, levels = c("LCR", "SCR", "WB", "GB", "no hedge (control)", "no hedge (random)"))

##Join with Class+ family+ Order data###############################
classraw <- classraw %>% 
  rename(species = Common_Name)

#Left join the class
joined_class <- left_join(fish_long_cooked, classraw, by = "species")

# 2. Calculate Total and Relative Abundance simultaneously
abundance_summary <- joined_class %>%
  group_by(hedging) %>%
  mutate(Total_Site_Abundance = sum(count)) %>%  
  group_by(hedging, species) %>%
  mutate(
    Absolute_Abundance = sum(count),
    Relative_Abundance = Absolute_Abundance / first(Total_Site_Abundance))

# Statistics and graphs -------------------------------------------------------------

###Find and graph total number of unique family counts##################################################

#Find total number of unique counts#
unique_counts <- abundance_summary %>%
  group_by(hedging) %>%
  summarize(unique_family_count = n_distinct(Family))

#Arrange the hedging types to be consistent#
unique_counts$hedging <- factor(unique_counts$hedging, levels = c("LCR", "SCR", "WB", "GB", "no hedge (control)", "no hedge (random)"))

#Total counts preliminary graph#
ggplot(data = unique_counts)+
  geom_col(aes(x = unique_family_count, y = hedging))

###Statistics for Family Richness#########################################################

richness_fam <- abundance_summary %>%
  group_by(ID, hedging) %>%
  dplyr::summarise(
    family_richness = n_distinct(Family, na.rm = TRUE),
    .groups = "drop")

str(richness_fam)
summary(richness_fam$family_richness)

#Test Boxplot#
boxplot(
  family_richness ~ hedging,
  data = richness_fam
)

richness_fam$hedging <- as.factor(as.character(richness_fam$hedging))
richness_fam$hedging <- factor(richness_fam$hedging, levels = c("LCR", "WB", "SCR", "GB", "no hedge (control)", "no hedge (random)"))

#Rename for publication#
richness_fam <- richness_fam %>% 
  mutate(
    hedging=dplyr::recode(hedging,
                        "no hedge (control)" = "No Hedge (Control)",
                   "no hedge (random)" = "No Hedge (Random)"))

#Publication Graph#
richness_plot <- ggplot(data = richness_fam)+
  geom_boxplot(aes(x = hedging, y = family_richness, fill = hedging))+
  labs(x = "Hedging Type", y = "Average Number of Families Observed")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  theme(legend.position = "none")+
  scale_fill_manual(
    breaks = c("No Hedge (Control)", "No Hedge (Random)", "GB", "LCR", "SCR", "WB"),
    labels = c(
      "WB" = "WB",
      "No Hedge (Control)" = "No Hedge (Control)",
      "No Hedge (Random)" = "No Hedge (Random)",
      "GB" = "GB",
      "M" = "SCR",
      "R" = "LCR"),
    values = c(
      "No Hedge (Control)" = "#22a884ff",
      "No Hedge (Random)" = "#7ad151ff",
      "GB" = "#2a788eff",
      "LCR" = "#414487ff",
      "SCR" = "#fde725ff",
      "WB" = "#440154ff"))

richness_plot

#Export as a high-resolution TIFF matching journal dimensions#
ggsave( filename = "Brewers_Plots/Figures/Publication_Ready/Figure_6.pdf", plot = richness_plot, device = "pdf", dpi = 600, 
        width = 5, 
        height = 4) 

###Look at statistics for the graph###
model_poisfam <- glm(
  family_richness ~ hedging,
  family = poisson(link = "log"),
  data = richness_fam)

summary(model_poisfam)
check_overdispersion(model_poisfam)

model_richness_null <- glm(
  family_richness ~ 1,
  family = poisson(link = "log"),
  data = richness_fam)

anova(
  model_richness_null,
  model_poisfam,
  test = "Chisq")

richness_results <- emmeans(
  model_poisfam,
  pairwise ~ hedging,
  type = "response",
  adjust = "tukey")

summary(richness_results)
richness_results

###Look at shannon diversity of family#############################

family_counts <- abundance_summary %>%
  count(ID, hedging, Family, name = "countfam")

community <- xtabs(
    countfam ~ ID + Family,
    data = family_counts)

shannon <- diversity(community, index = "shannon")

shannon_results <- data.frame(
  ID = rownames(community),
  shannon = as.numeric(shannon))

shannon_results <- shannon_results %>%
  left_join(
    abundance_summary %>% distinct(ID, hedging),
    by = "ID")
#Preliminary Plot#
boxplot(shannon ~ hedging, data = shannon_results)

#Check Assumptions#
shapiro.test(shannon_results$shannon)

model_shannon <- aov(
  shannon ~ hedging,
  data = shannon_results)

shapiro.test(residuals(model_shannon))
qqnorm(residuals(model_shannon))
qqline(residuals(model_shannon), col = "red")

leveneTest(shannon ~ hedging, data = shannon_results)

#Statistics#
kruskal.test(shannon ~ hedging, data = shannon_results)

pairwise.wilcox.test(
  shannon_results$shannon,
  shannon_results$hedging,
  p.adjust.method = "BH")

shannon_results %>% 
  dplyr::group_by(hedging) %>% 
  dplyr::summarise(
    median_shannon = median(shannon, na.rm = TRUE),
    mean_shannon = mean(shannon, na.rm = TRUE),
    n = dplyr::n())

###NMDS creation and analysis#################################

ab_clean <- abundance_summary %>% 
group_by(Family, hedging, ID) %>%
  summarise(across(where(is.numeric), sum)) %>% 
  select(hedging, Family, Absolute_Abundance, ID)%>% 
  drop_na()

ab_clean_wide <- ab_clean %>% 
  pivot_wider(
    names_from = Family, 
    values_from = Absolute_Abundance)

ab_clean_wide <- ab_clean_wide %>% 
  ungroup() %>% 
  mutate(
    hedging = dplyr::recode(hedging,
                   "no hedge (control)" = "No Hedge (Control)",
                   "no hedge (random)" = "No Hedge (Random)"))

justrich <- ab_clean_wide  %>% 
  ungroup() %>% 
  dplyr::select("Acanthuridae":"Pomacentridae") 
justrich[is.na(justrich)] <- 0
env.data <- ab_clean_wide %>% 
  ungroup() %>% 
  dplyr::select(hedging)

#Results#
nmds_result <- metaMDS(justrich, distance = "bray", k = 2, trymax = 200)
#Stress Test#
print(nmds_result$stress)

sample_coords <- as.data.frame(scores(nmds_result, display = "sites"))

sample_coords$hedging <- env.data$hedging

#Function of the Plot#
plot_nmds <- function() {
  
  custom_order <- c("No Hedge (Control)", "No Hedge (Random)", "GB", "LCR", "SCR", "WB") 
  sample_coords$hedging <- factor(sample_coords$hedging, levels = custom_order)
  
  group_factor <- as.factor(sample_coords$hedging)
  group_levels <- levels(group_factor)
  shapes <- c(1, 2, 17, 18, 15, 16)[as.numeric(group_factor)] 
  plot_color <- "black" 
  
  # Extract coordinates to establish limits
  nmds_scores <- scores(nmds_result, display = "sites")
  x_min <- min(nmds_scores[,1])
  x_max <- max(nmds_scores[,1])
  y_max <- max(nmds_scores[,2])
  
  # Draw the NMDS plot with a 40% expanded X-axis limit on the right
  plot(nmds_result, type = "n", 
       xlim = c(x_min, x_max + (x_max - x_min) * 0.40)) 
  
  # Add sites and ellipses
  points(nmds_result, display = "sites", col = plot_color, pch = shapes, cex = 1.5)
  ordiellipse(nmds_result, groups = sample_coords$hedging, 
              kind = "sd", label = FALSE, col = "grey50", lwd = 1.5, lty = 2)
  
  # Add the Group Legend (Smaller and pushed all the way right)
  legend(x = x_max * 1.45, 
         y = y_max * 1.5, 
         legend = group_levels, 
         pch = c(1, 2, 17, 18, 15, 16)[1:length(group_levels)], 
         col = plot_color, 
         pt.cex = 1.2,       
         cex = 0.8,          
         title.cex = 1.2,    
         xjust = 0,          
         title = "Hedging Type",
         bty = "n")
}
#Run the plot function#
plot_nmds()

#ANOSIM#
ano <- anosim(justrich, env.data$hedging, distance = "bray", permutations = 9999)
summary(ano)
print(ano)

#SIMPER analysis#
simper_result <- simper(comm = justrich, grouping = env.data$hedging, permutations = 999)
summary(simper_result)

#Publication worthy export#
tiff("Brewers_Plots/Figures/Publication_Ready/Figure_7.tiff", width = 6.7, height = 4.5, units = "in", res = 600, compression = "lzw")
plot_nmds()
dev.off()

