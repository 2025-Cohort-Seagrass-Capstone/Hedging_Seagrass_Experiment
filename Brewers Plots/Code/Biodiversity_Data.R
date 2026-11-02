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
#ggplot(data = abundance_summary)+
#  geom_col(aes(x = count, y = hedging, fill = Family))

#Average species
fish_average <- fish_long_cooked %>%
  group_by(hedging, species) %>%                  # Group by your hedging column
  mutate(avg_count = mean(count, na.rm = TRUE)) %>%   # Calculate the average
  mutate(sd_count = sd(count, na.rm = TRUE))

fish_average$hedging <- factor(fish_average$hedging, levels = c("LCR", "SCR", "WB", "GB", "no hedge (control)", "no hedge (random)"))




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


#############################################################################
library(dplyr)
library(emmeans)
#library(MASS)

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

richness_fam$hedging <- as.factor(as.character(richness_fam$hedging))
richness_fam$hedging <- factor(richness_fam$hedging, levels = c("LCR", "WB", "SCR", "GB", "no hedge (control)", "no hedge (random)"))

richness_fam <- richness_fam %>% 
  mutate(
    hedging=recode(hedging,
                        "no hedge (control)" = "No Hedge (Control)",
                   "no hedge (random)" = "No Hedge (Random)")
  )

richness_plot <- ggplot(data = richness_fam)+
  geom_boxplot(aes(x = hedging, y = family_richness))+
  #geom_point(aes(x = hedging, y = family_richness))+
  labs(x = "Hedging Type", y = "Average Number of Families Observed")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

richness_plot

# 2. Export as a high-resolution TIFF matching journal dimensions 
ggsave( filename = "figure_6.pdf", plot = richness_plot, device = "pdf", dpi = 600, # 300 DPI minimum for color, use 600 for pure line plots 
        width = 5, # Single-column width in millimeters 
        height = 4) # Adjust height as needed units = "mm", 
        #compression = "lzw") # LZW compression reduces file size without losing quality )
#dev.off()

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
  model_poisfam,
  test = "Chisq"
)

richness_results <- emmeans(
  model_poisfam,
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
  select(hedging, Family, Absolute_Abundance, ID)%>% 
  drop_na()

ab_clean_wide <- ab_clean %>% 
  pivot_wider(
    names_from = Family, 
    values_from = Absolute_Abundance
  )

ab_clean_wide <- ab_clean_wide %>% 
  mutate(
    hedging=recode(hedging,
                   "no hedge (control)" = "No Hedge (Control)",
                   "no hedge (random)" = "No Hedge (Random)")
  )

justrich <- ab_clean_wide  %>% 
  ungroup() %>% 
  dplyr::select("Acanthuridae":"Pomacentridae") 
justrich[is.na(justrich)] <- 0
env.data <- ab_clean_wide %>% 
  ungroup() %>% 
  dplyr::select(hedging)




nmds_result <- metaMDS(justrich, distance = "bray", k = 2, trymax = 200)
print(nmds_result$stress)
sample_coords <- as.data.frame(scores(nmds_result, display = "sites"))


sample_coords$hedging <- env.data$hedging 

ano <- anosim(justrich, env.data$hedging, distance = "bray", permutations = 9999)
summary(ano)
print(ano)



# Run the SIMPER analysis
simper_result <- simper(comm = justrich, grouping = env.data$hedging, permutations = 999)

# View the summary output
summary(simper_result)


# 1. Setup shapes and labels
group_factor <- as.factor(sample_coords$hedging)
group_levels <- levels(group_factor)
shapes <- c(15, 16, 17, 18, 1, 2)[as.numeric(group_factor)] 
plot_color <- "black" 

# 2. Extract coordinates to establish limits
nmds_scores <- scores(nmds_result, display = "sites")
x_min <- min(nmds_scores[,1])
x_max <- max(nmds_scores[,1])
y_max <- max(nmds_scores[,2])

# 3. Draw the NMDS plot with a 40% expanded X-axis limit on the right
plot(nmds_result, type = "n", 
     xlim = c(x_min, x_max + (x_max - x_min) * 0.40)) 

# 4. Add sites and ellipses
points(nmds_result, display = "sites", col = plot_color, pch = shapes, cex = 1.5)
ordiellipse(nmds_result, groups = sample_coords$hedging, 
            kind = "sd", label = F, col = "grey50", lwd = 1.5, lty = 2)

# 5. Add the Group Legend (Smaller and pushed all the way right)
# Increase the multiplier (e.g., x_max * 1.15) if you want to push it even further out
legend(x = x_max * 1.45, 
       y = y_max * 1.5, 
       legend = group_levels, 
       pch = c(15, 16, 17, 18, 1, 2)[1:length(group_levels)], 
       col = plot_color, 
       pt.cex = 1.2,       
       cex = 0.8,          # Keeps the group names smaller (80% size)
       title.cex = 1.2,    # Bumps the "Hedging Type" title up to 120% size
       xjust = 0,          
       title = "Hedging Type",
       bty = "n")

plot_nmds <- function() {
  # Setup factor and shapes
  group_factor <- as.factor(sample_coords$hedging)
  group_levels <- levels(group_factor)
  shapes <- c(15, 16, 17, 18, 1, 2)[as.numeric(group_factor)] 
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
         pch = c(15, 16, 17, 18, 1, 2)[1:length(group_levels)], 
         col = plot_color, 
         pt.cex = 1.2,       
         cex = 0.8,          
         title.cex = 1.2,    
         xjust = 0,          
         title = "Hedging Type",
         bty = "n")
}

plot_nmds()

tiff("Figure_7.tiff", width = 6.7, height = 4.5, units = "in", res = 600, compression = "lzw")
plot_nmds()
dev.off()

