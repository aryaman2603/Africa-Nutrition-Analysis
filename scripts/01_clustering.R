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