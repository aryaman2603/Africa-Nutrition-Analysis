# ============================================
# Demographic Heatmapping (Africa EDA)
# 2015–2021 | PUFA, Sugar, Sodium
# ============================================

# Load library
library(tidyverse)

# -------------------------------
# 1. Load Dataset
# -------------------------------
df <- read_csv("data/africa_dataset.csv")

# -------------------------------
# 2. Select Nutrients of Interest
# -------------------------------
df_selected <- df %>%
  filter(
    str_detect(source_file, "PUFA") |
      str_detect(source_file, "SODIUM") |
      str_detect(source_file, "SSBS")
  ) %>%
  mutate(
    Nutrient = case_when(
      str_detect(source_file, "PUFA") ~ "PUFA",
      str_detect(source_file, "SODIUM") ~ "Sodium",
      str_detect(source_file, "SSBS") ~ "Sugar"
    )
  )

# -------------------------------
# 3. Filter 2015–2021 & Sex
# -------------------------------
df_selected <- df_selected %>%
  filter(
    year_id >= 2015,
    sex_name %in% c("Male", "Female"),
    !location_name %in% c("North Africa and Middle East",
                          "Sub-Saharan Africa")
  )

# -------------------------------
# 4. Region Classification
# -------------------------------
region_map <- tribble(
  ~location_name, ~Region,
  
  # North Africa
  "Algeria", "North Africa",
  "Egypt", "North Africa",
  "Libya", "North Africa",
  "Morocco", "North Africa",
  "Tunisia", "North Africa",
  "Sudan", "North Africa",
  
  # West Africa
  "Benin", "West Africa",
  "Burkina Faso", "West Africa",
  "Cabo Verde", "West Africa",
  "Gambia", "West Africa",
  "Ghana", "West Africa",
  "Guinea", "West Africa",
  "Guinea-Bissau", "West Africa",
  "Liberia", "West Africa",
  "Mali", "West Africa",
  "Mauritania", "West Africa",
  "Niger", "West Africa",
  "Nigeria", "West Africa",
  "Senegal", "West Africa",
  "Sierra Leone", "West Africa",
  "Togo", "West Africa",
  
  # East Africa
  "Burundi", "East Africa",
  "Comoros", "East Africa",
  "Djibouti", "East Africa",
  "Eritrea", "East Africa",
  "Ethiopia", "East Africa",
  "Kenya", "East Africa",
  "Madagascar", "East Africa",
  "Malawi", "East Africa",
  "Mauritius", "East Africa",
  "Mozambique", "East Africa",
  "Rwanda", "East Africa",
  "Seychelles", "East Africa",
  "Somalia", "East Africa",
  "South Sudan", "East Africa",
  "Uganda", "East Africa",
  "Zambia", "East Africa",
  "Zimbabwe", "East Africa",
  
  # Central Africa
  "Angola", "Central Africa",
  "Cameroon", "Central Africa",
  "Central African Republic", "Central Africa",
  "Chad", "Central Africa",
  "Congo", "Central Africa",
  "Democratic Republic of the Congo", "Central Africa",
  "Equatorial Guinea", "Central Africa",
  "Gabon", "Central Africa",
  "Sao Tome and Principe", "Central Africa",
  
  # Southern Africa
  "Botswana", "Southern Africa",
  "Eswatini", "Southern Africa",
  "Lesotho", "Southern Africa",
  "Namibia", "Southern Africa",
  "South Africa", "Southern Africa"
)

df_selected <- df_selected %>%
  left_join(region_map, by = "location_name")

# -------------------------------
# 5. Region-Level Aggregation
# -------------------------------
df_region <- df_selected %>%
  group_by(Region, age_group_name, sex_name, Nutrient) %>%
  summarise(
    region_mean = mean(val, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------
# 6. Order Factors
# -------------------------------
df_region <- df_region %>%
  mutate(
    age_group_name = factor(
      age_group_name,
      levels = c(
        "25 to 29", "30 to 34", "35 to 39",
        "40 to 44", "45 to 49", "50 to 54",
        "55 to 59", "60 to 64", "65 to 69",
        "70 to 74", "75 to 79", "80 to 84",
        "85 to 89", "90 to 94", "95 plus"
      ),
      ordered = TRUE
    ),
    Region = factor(
      Region,
      levels = c(
        "North Africa",
        "West Africa",
        "Central Africa",
        "East Africa",
        "Southern Africa"
      ),
      ordered = TRUE
    )
  )
# -------------------------------
# 7. PUFA Heatmap
# -------------------------------

df_pufa <- df_region %>%
  filter(Nutrient == "PUFA")

ggplot(df_pufa, aes(x = age_group_name, y = Region, fill = region_mean)) +
  geom_tile(color = "white") +
  facet_wrap(~sex_name) +
  scale_fill_viridis_c(option = "C", name = "PUFA (% energy)") +
  labs(
    title = "PUFA Intake Across Age Groups and African Regions",
    x = "Age Group",
    y = "Region"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# -------------------------------
# 8. Sugar Heatmap
# -------------------------------

df_sugar <- df_region %>%
  filter(Nutrient == "Sugar")

ggplot(df_sugar, aes(x = age_group_name, y = Region, fill = region_mean)) +
  geom_tile(color = "white") +
  facet_wrap(~sex_name) +
  scale_fill_viridis_c(option = "C", name = "Sugar Intake") +
  labs(
    title = "Sugar Intake Across Age Groups and African Regions",
    x = "Age Group",
    y = "Region"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# -------------------------------
# 9. Sodium Heatmap
# -------------------------------

df_sodium <- df_region %>%
  filter(Nutrient == "Sodium")

ggplot(df_sodium, aes(x = age_group_name, y = Region, fill = region_mean)) +
  geom_tile(color = "white") +
  facet_wrap(~sex_name) +
  scale_fill_viridis_c(option = "C", name = "Sodium Intake") +
  labs(
    title = "Sodium Intake Across Age Groups and African Regions",
    x = "Age Group",
    y = "Region"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#10. Combined Nutrient Heatmap
# -------------------------------

ggplot(df_region, aes(x = age_group_name, y = Region, fill = region_mean)) +
  geom_tile(color = "white") +
  facet_grid(Nutrient ~ sex_name) +
  scale_fill_viridis_c(option = "C", name = "Exposure Level") +
  labs(
    title = "Dietary Risk Exposure Across Age Groups and African Regions (2015–2021)",
    subtitle = "Comparison of PUFA, Sugar, and Sodium Intake",
    x = "Age Group",
    y = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold")
  )
# Save heatmap as high-resolution image
ggsave(
  "outputs/figures/Demographic_Heatmaps.png",
  plot = combined_plot,
  width = 12,
  height = 8,
  dpi = 300
)
