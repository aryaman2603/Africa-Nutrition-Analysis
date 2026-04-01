# ==============================================================================
# Script Name: 01_clustering_analysis.R
# Purpose: K-Means clustering & PCA of African dietary phenotypes
# ==============================================================================

# 1. LOAD LIBRARIES
library(tidyverse)
library(factoextra) 
library(cluster)    

# 2. LOAD DATA
df <- read_csv("../data/raw/africa_dataset.csv", show_col_types = FALSE)

# ------------------------------------------------------------------------------
# 3. DATA PREPARATION & CLEANING
# ------------------------------------------------------------------------------
clustering_data <- df %>%
  # Create the 'nutrient' column by extracting the keyword from 'source_file'
  mutate(
    nutrient = case_when(
      str_detect(toupper(source_file), "PUFA") ~ "PUFA",
      str_detect(toupper(source_file), "FRUIT") ~ "Fruit",
      str_detect(toupper(source_file), "VEG") ~ "Vegetables",
      str_detect(toupper(source_file), "SODIUM") ~ "Sodium",
      str_detect(toupper(source_file), "SUGAR") ~ "Sugar",
      str_detect(toupper(source_file), "LEGUMES") ~ "Legumes",
      str_detect(toupper(source_file), "NUTS") ~ "Nuts",
      str_detect(toupper(source_file), "MEAT") ~ "Red_Meat",
      # A fallback rule just in case a file name doesn't match the above
      TRUE ~ "Other" 
    )
  ) %>%
  # Filter out any "Other" rows to keep only the specific 8 nutrients you tracked
  filter(nutrient != "Other") %>%
  
  # Filter for the most recent year to capture current phenotypes
  filter(year_id == max(year_id, na.rm = TRUE)) %>% 
  
  # Group by country and nutrient to average out ages and sexes
  group_by(location_name, nutrient) %>%
  summarise(mean_intake = mean(val, na.rm = TRUE), .groups = "drop") %>%
  
  # Pivot to Wide Format (Rows = Countries, Columns = Nutrients)
  pivot_wider(names_from = nutrient, values_from = mean_intake) %>%
  
  # Drop any countries missing data for certain nutrients
  drop_na() %>%
  
  # Move the country names to "row names" for the clustering functions
  column_to_rownames(var = "location_name")

# SCALE THE DATA (Mean = 0, SD = 1)
scaled_data <- scale(clustering_data)

# ------------------------------------------------------------------------------
# 4. PRINCIPAL COMPONENT ANALYSIS (PCA)
# ------------------------------------------------------------------------------
pca_result <- prcomp(scaled_data, center = FALSE, scale. = FALSE)

pca_plot <- fviz_pca_biplot(pca_result,
                            repel = TRUE,                
                            col.var = "#E74C3C",         
                            col.ind = "#2C3E50",         
                            title = "PCA: African Dietary Landscape")
print(pca_plot)

# ------------------------------------------------------------------------------
# 5. K-MEANS CLUSTERING
# ------------------------------------------------------------------------------
set.seed(123) 

# Assuming 4 distinct dietary profiles
k_model <- kmeans(scaled_data, centers = 4, nstart = 25)

cluster_plot <- fviz_cluster(k_model, data = scaled_data,
                             palette = "Set2", 
                             ggtheme = theme_minimal(),
                             main = "Nutritional Clusters of Africa")
print(cluster_plot)

ggsave(filename = "../outputs/figures/PCA_Biplot.png", 
       plot = pca_plot, 
       width = 10, 
       height = 8, 
       dpi = 300, 
       bg = "white")

# 2. Save the K-Means Cluster Plot
ggsave(filename = "../outputs/figures/KMeans_Clusters.png", 
       plot = cluster_plot, 
       width = 10, 
       height = 8, 
       dpi = 300, 
       bg = "white")

clustering_data_with_clusters <- clustering_data %>%
  rownames_to_column(var = "location_name") %>%
  mutate(Cluster = as.factor(k_model$cluster))

cluster_profiles <- clustering_data_with_clusters %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

print(cluster_profiles)

write_csv(cluster_profiles, "../outputs/reports/Cluster_Nutrient_Profiles.csv")

write_csv(clustering_data_with_clusters, "../outputs/reports/Country_Cluster_Assignments.csv")

library(dplyr)
library(tidyr)

scoring_data <- cluster_profiles %>%
  pivot_longer(cols = -Cluster, names_to = "nutrient", values_to = "mean_intake") %>%
  mutate(
    optimization_goal = if_else(nutrient %in% c("Sodium", "Sugar", "Red_Meat"), "Minimize", "Maximize")
  ) %>%
  group_by(nutrient) %>%
  mutate(
    nutrient_rank = if_else(optimization_goal == "Maximize",
                            rank(mean_intake, ties.method = "min"),
                            rank(-mean_intake, ties.method = "min"))
  ) %>%
  ungroup()

cluster_final_scores <- scoring_data %>%
  group_by(Cluster) %>%
  summarise(
    composite_score = sum(nutrient_rank)
  ) %>%
  arrange(desc(composite_score))

print(cluster_final_scores)

install.packages("fmsb")
library(fmsb)
library(tibble)

radar_data <- cluster_profiles %>%
  column_to_rownames(var = "Cluster")

max_min <- data.frame(
  rbind(
    apply(radar_data, 2, max) * 1.1,
    apply(radar_data, 2, min) * 0.9
  )
)
colnames(max_min) <- colnames(radar_data)

df_radar <- rbind(max_min, radar_data)

colors_border <- c("#E74C3C", "#2ECC71", "#3498DB", "#F1C40F")
colors_in <- c(rgb(231,76,60, max=255, alpha=76), 
               rgb(46,204,113, max=255, alpha=76), 
               rgb(52,152,219, max=255, alpha=76), 
               rgb(241,196,15, max=255, alpha=76))

png("outputs/figures/Cluster_Radar_Chart.png", width = 2400, height = 2400, res = 300)

radarchart(df_radar,
           axistype = 0,
           pcol = colors_border,
           pfcol = colors_in,
           plwd = 2,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           vlcex = 1.2,
           title = "Multidimensional Nutritional Profiles by Cluster")

legend(x = "topright", 
       legend = paste("Cluster", rownames(radar_data)), 
       bty = "n", 
       pch = 20, 
       col = colors_border, 
       text.col = "black", 
       cex = 1.2, 
       pt.cex = 3)

dev.off()