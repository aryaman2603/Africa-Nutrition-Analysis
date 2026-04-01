library(tidyverse)
library(sf)               
library(rnaturalearth)    
library(rnaturalearthdata)


africa_map <- ne_countries(scale = "medium", continent = "africa", returnclass = "sf")


cluster_to_map <- clustering_data_with_clusters %>%
  select(location_name, Cluster) %>%
  
  mutate(location_name = case_when(
    location_name == "Cote d'Ivoire" ~ "Ivory Coast",
    location_name == "Democratic Republic of the Congo" ~ "Dem. Rep. Congo",
    location_name == "Congo" ~ "Congo",
    location_name == "Eswatini" ~ "eSwatini",
    location_name == "United Republic of Tanzania" ~ "Tanzania",
    TRUE ~ location_name
  ))


africa_joined <- africa_map %>%
  left_join(cluster_to_map, by = c("name" = "location_name"))


cluster_map_plot <- ggplot(data = africa_joined) +
  geom_sf(aes(fill = Cluster), color = "white", size = 0.2) +
  scale_fill_brewer(palette = "Set2", na.value = "grey90", 
                    name = "Dietary Cluster",
                    labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "No Data")) +
  coord_sf(expand = FALSE) + 
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  ) +
  labs(
    title = "Geospatial Distribution of Nutritional Phenotypes",
    subtitle = "Mapping K-Means Clusters across Africa (2021 Data)",
    caption = "Source: GBD 2021 | Grey areas indicate missing data"
  )


print(cluster_map_plot)

ggsave("outputs/figures/African_Nutritional_Map.png", 
       plot = cluster_map_plot, 
       width = 10, height = 10, dpi = 300, bg = "white")