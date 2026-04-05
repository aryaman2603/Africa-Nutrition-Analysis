
# Script: 02_nutrition_transition.R


library(tidyverse)
library(scales)
library(ggrepel)

# 1. DATA INGESTION & PREPROCESSING --------------------------------------------
# Load Global Burden of Disease (GBD) 2021 dietary risk dataset
data <- read.csv("africa_dataset.csv")

data_fats <- data %>%
  # Filter out GBD regional aggregates to ensure analysis is strictly country-level
  filter(!location_name %in% c("North Africa and Middle East", "Sub-Saharan Africa")) %>%
  mutate(risk = case_when(
    str_detect(str_to_lower(source_file), "pufa") ~ "PUFA",
    str_detect(str_to_lower(source_file), "sfa|saturated") ~ "Saturated",
    str_detect(str_to_lower(source_file), "trans|tfa") ~ "Trans",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(risk), sex_name == "Both") %>%
  group_by(location_name, year_id, risk) %>%
  summarise(val = mean(val, na.rm = TRUE), .groups = "drop") %>%
  # Fill missing categories with 0 to prevent downstream math errors
  pivot_wider(names_from = risk, values_from = val, values_fill = 0)

# Safely ensure target columns exist without breaking vector data (broadcasting fix)
if (!"Saturated" %in% names(data_fats)) data_fats$Saturated <- 0
if (!"Trans" %in% names(data_fats)) data_fats$Trans <- 0
if (!"PUFA" %in% names(data_fats)) data_fats$PUFA <- 0

# Calculate the final combined metric
data_fats <- data_fats %>%
  mutate(industrial_fats = Saturated + Trans)

# 2. TRANSITION VELOCITY ANALYSIS (CAGR) ---------------------------------------
fat_growth <- data_fats %>%
  group_by(location_name) %>%
  filter(year_id %in% c(1990, 2021)) %>%
  select(location_name, year_id, industrial_fats) %>%
  pivot_wider(names_from = year_id, names_prefix = "yr_", values_from = industrial_fats) %>%
  # Drop missing 2021 data points to prevent anomalous -100% calculations
  filter(!is.na(yr_2021), yr_2021 > 0) %>% 
  mutate(
    abs_change = yr_2021 - yr_1990,
    cagr = ((yr_2021 / yr_1990)^(1/31) - 1) * 100
  ) %>%
  arrange(desc(cagr))

# Output 1: Ranking Report
write.csv(fat_growth, "Fat_Growth_Rates_CAGR.csv", row.names = FALSE)

# 3. IDENTIFICATION OF NUTRITIONAL CROSSOVER POINTS ----------------------------
crossovers <- data_fats %>%
  group_by(location_name) %>%
  filter(industrial_fats > PUFA) %>%
  summarise(crossover_year = min(year_id))

# Output 2: Timeline Report
write.csv(crossovers, "Crossover_Years_Summary.csv", row.names = FALSE)

# 4. TIME-SERIES VISUALIZATION (LONGITUDINAL TRENDS) ---------------------------
top_countries <- head(fat_growth$location_name, 12)

viz_data <- data_fats %>%
  filter(location_name %in% top_countries) %>%
  pivot_longer(cols = c(PUFA, industrial_fats), names_to = "Fat_Type", values_to = "Intake")

p1 <- ggplot(viz_data, aes(x = year_id, y = Intake, color = Fat_Type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~location_name, scales = "free_y") +
  scale_color_manual(values = c("industrial_fats" = "#D55E00", "PUFA" = "#0072B2"),
                     labels = c("industrial_fats" = "Saturated + Trans", "PUFA" = "PUFA (Protective)")) +
  theme_minimal() +
  labs(title = "Longitudinal Fat Quality Shifts in Africa (1990-2021)",
       subtitle = "Comparative analysis of protective PUFAs versus industrial fats",
       y = "Intake (% Total Daily Energy)", x = "Year", color = "Lipid Profile") +
  theme(legend.position = "bottom")

# Output 3: Faceted Plot Image
ggsave("Nutrition_Transition_Faceted.png", plot = p1, width = 12, height = 8, bg = "white")

# 5. MAGNITUDE OF TRANSITION VISUALIZATION (SLOPE CHART) -----------------------
top_15_names <- head(fat_growth$location_name, 15)

slope_data <- data_fats %>%
  filter(location_name %in% top_15_names) %>%
  filter(year_id %in% c(1990, 2021)) %>%
  mutate(year_factor = factor(year_id))

p2 <- ggplot(slope_data, aes(x = year_factor, y = industrial_fats, group = location_name)) +
  geom_line(aes(color = location_name), linewidth = 1.2, alpha = 0.6) +
  geom_point(aes(color = location_name), size = 3) +
  geom_text_repel(data = filter(slope_data, year_id == 2021), 
                  aes(label = location_name, color = location_name), 
                  nudge_x = 0.4,           
                  direction = "y",         
                  hjust = 0, 
                  box.padding = 0.8,        
                  point.padding = 0.5,     
                  segment.color = "grey60",
                  size = 3.5, 
                  fontface = "bold",
                  max.overlaps = Inf) +     
  theme_minimal() +
 
  scale_x_discrete(expand = expansion(mult = c(0.1, 0.7))) + 
  labs(title = "Magnitude of the Nutrition Transition",
       subtitle = "Top 15 Countries by Growth in Industrial Fat Intake (1990-2021)",
       x = "Year", y = "Saturated + Trans Fat Intake (% Energy)") +
  theme(legend.position = "none", 
        panel.grid.minor = element_blank())

ggsave("Fat_Transition_Slope_Chart.png", plot = p2, width = 11, height = 8, bg = "white")