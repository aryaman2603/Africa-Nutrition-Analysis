# Purpose: Identify countries facing the double burden of malnutrition
# (High Sodium Intake + Low Fruit & Vegetable Intake as proxy)

# 1. LOAD LIBRARIES
install.packages("tidyverse")
library(tidyverse)

# 2. LOAD DATA

setwd("D:/downloads/Africa-Nutrition-Analysis-main/Africa-Nutrition-Analysis-main/scripts")
df <- read_csv("../data/raw/africa_dataset.csv", show_col_types = FALSE)
View(df)

# 3. DATA PREPARATION & CLEANING
double_burden_data <- df %>%
  
  # Extract relevant nutrients from source_file
  mutate(
    nutrient = case_when(
      str_detect(toupper(source_file), "SODIUM") ~ "Sodium",
      str_detect(toupper(source_file), "FRUIT") ~ "Fruit",
      str_detect(toupper(source_file), "VEG") ~ "Vegetables",
      TRUE ~ "Other"
    )
  ) %>%
  
  # Keep only required nutrients
  filter(nutrient != "Other") %>%
  
  # Use most recent year and both sexes
  filter(
    year_id == max(year_id, na.rm = TRUE),
    sex_name == "Both"
  ) %>%
  
  # Aggregate across age groups
  group_by(location_name, nutrient) %>%
  summarise(mean_value = mean(val, na.rm = TRUE), .groups = "drop") %>%
  
  # Convert to wide format
  pivot_wider(names_from = nutrient, values_from = mean_value) %>%
  
  # Remove incomplete rows
  drop_na()
View(double_burden_data)

# 4. CREATE HEALTHY FOOD PROXY (FRUIT + VEGETABLES)
double_burden_data <- double_burden_data %>%
  mutate(Healthy_Food = Fruit + Vegetables)

View(double_burden_data)

# 5. DEFINE QUADRANT THRESHOLDS
sodium_median <- median(double_burden_data$Sodium, na.rm = TRUE)
healthy_median <- median(double_burden_data$Healthy_Food, na.rm = TRUE)
print(sodium_median)
print(healthy_median)

# 6. CLASSIFY COUNTRIES INTO ZONES
double_burden_data <- double_burden_data %>%
  mutate(
    Zone = case_when(
      Sodium >= sodium_median & Healthy_Food < healthy_median ~ "Danger Zone",
      Sodium < sodium_median & Healthy_Food < healthy_median ~ "Undernutrition Focus",
      Sodium >= sodium_median & Healthy_Food >= healthy_median ~ "Overnutrition Risk",
      TRUE ~ "Better Zone"
    )
  )

View(double_burden_data)

# 7. VISUALIZATION: 4-QUADRANT SCATTER PLOT
plot <- ggplot(double_burden_data,
               aes(x = Sodium, y = Healthy_Food, color = Zone)) +
  geom_point(size = 3) +
  
  # Quadrant divider lines
  geom_vline(xintercept = sodium_median, linetype = "dashed") +
  geom_hline(yintercept = healthy_median, linetype = "dashed") +
  
  # Label danger zone countries
  geom_text(data = subset(double_burden_data, Zone == "Danger Zone"),
            aes(label = location_name),
            vjust = -1,
            size = 3) +
  
  theme_minimal() +
  labs(
    title = "Double Burden of Malnutrition in Africa",
    subtitle = "High Sodium Intake vs Low Fruit & Vegetable Intake",
    x = "Sodium Intake",
    y = "Fruit + Vegetable Intake",
    color = "Zone"
  )

print(plot)

# Save plot
ggsave(filename = "../outputs/figures/Double_Burden_Scatter.png",
       plot = plot,
       width = 10,
       height = 8,
       dpi = 300,
       bg = "white")

# Save full dataset with zones
write_csv(double_burden_data,
          "../outputs/reports/Double_Burden_Analysis.csv")

# Save only danger zone countries
danger_countries <- double_burden_data %>%
  filter(Zone == "Danger Zone")

write_csv(danger_countries,
          "../outputs/reports/Danger_Zone_Countries.csv")

#summary
danger_count <- nrow(danger_countries)

cat("Number of countries in Double Burden Danger Zone:", danger_count, "\n")