# Load necessary libraries
library(tidyverse)     # Data wrangling + ggplot2
library(readr)         # Reading CSVs
library(skimr)         # Quick data summaries
library(scales)        # Normalising variables
library(factoextra)    # Clustering visualisation
library(cluster)       # Clustering algorithms
library(sf)            # Spatial data handling
library(tmap)          # Mapping
pacman::p_load(spdep)         # Spatial autocorrelation

project<-read.csv("year_msoa_grocery.csv")
project



# Check for missing values
col(is.na(project))

# Optional: Filter out areas with low representativeness
msoa_clean <- project%>%
  filter(representativeness_norm > 0.1)

health_vars <- msoa_clean %>%
  
  select(area_id, 
         f_sweets, 
         f_soft_drinks, 
         f_readymade, 
         energy_sugar, 
         energy_fat, 
         energy_density, 
         f_fruit_veg, 
         h_nutrients_weight_norm)


# Example: Histogram of sweets purchase fraction
ggplot(health_vars, aes(x = f_sweets)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Distribution of Sweets Purchase Fraction", x = "Fraction of Sweets", y = "Count")

ggplot(health_vars, aes(x = f_readymade)) +
  geom_histogram(fill = "red", bins = 30) +
  
  labs(title = "Distribution of Sweets Purchase Fraction", x = "Fraction of Sweets", y = "Count")


# Calculate correlation matrix
cor_matrix <- health_vars %>% 
  
  select(-area_id) %>%
  cor(use = "complete.obs")

# Visualise correlations



library(corrplot)
corrplot::corrplot(cor_matrix, method = "circle")


# Create scaled variables and unhealthiness index
health_index <- health_vars %>%
  mutate(
    # Scale each variable between 0-1
    sweets_scaled = rescale(f_sweets),
    soft_drinks_scaled = rescale(f_soft_drinks),
    readymade_scaled = rescale(f_readymade),
    energy_sugar_scaled = rescale(energy_sugar),
    energy_fat_scaled = rescale(energy_fat),
    energy_density_scaled = rescale(energy_density),
    
    # Protective factors: higher is healthier, so reverse scale
    fruit_veg_scaled = 1 - rescale(f_fruit_veg),
    diet_diversity_scaled = 1 - rescale(h_nutrients_weight_norm),
    
    # Calculate unhealthiness index as mean of all scaled variables
    unhealthiness_index = rowMeans(across(c(sweets_scaled, soft_drinks_scaled, readymade_scaled,
                                            energy_sugar_scaled, energy_fat_scaled, energy_density_scaled,
                                            fruit_veg_scaled, diet_diversity_scaled)))
  )


# View first few rows
head(health_index %>% select(area_id, unhealthiness_index))

# Summary of the index
summary(health_index$unhealthiness_index)


ggplot(health_index, aes(x = unhealthiness_index)) +
  geom_histogram(fill = "firebrick", bins = 30) +
  labs(title = "Distribution of Unhealthiness Index", x = "Unhealthiness Index", y = "Count")


# Select only the scaled variables for clustering
cluster_input <- health_index %>%
  select(sweets_scaled, soft_drinks_scaled, readymade_scaled,
         energy_sugar_scaled, energy_fat_scaled, energy_density_scaled,
         fruit_veg_scaled, diet_diversity_scaled)

tesco_and_msoas_full %>%
  group_by(cluster) %>%
  summarise(mean_UI = mean(unhealthiness_index, na.rm = TRUE))



cluster_means <- tesco_and_msoas_full %>%
  group_by(cluster) %>%
  summarise(mean_UI = mean(unhealthiness_index, na.rm = TRUE)) %>%
  arrange(mean_UI) %>%
  mutate(label = c("Healthiest", "Moderate", "Unhealthiest"))

tesco_and_msoas_full <- tesco_and_msoas_full %>%
  left_join(cluster_means %>% select(cluster, label),
            by = "cluster") %>%
  rename(cluster_label = label)

tesco_and_msoas_full <- tesco_and_msoas_full %>%
  select(-cluster_label)   # remove old one if exists

tesco_and_msoas_full <- tesco_and_msoas_full %>%
  left_join(cluster_means %>% select(cluster, label),
            by = "cluster") %>%
  rename(cluster_label = label)

tesco_and_msoas_full %>%
  st_drop_geometry() %>%
  group_by(cluster_label) %>%
  summarise(mean_UI = mean(unhealthiness_index, na.rm = TRUE))

map1 <- tm_shape(tesco_and_msoas_full) +
  tm_polygons(
    fill = "unhealthiness_index",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      values = "-RdYlGn"     # Red = Unhealthy, Green = Healthy
    ),
    fill.legend = tm_legend(title = "Unhealthiness Index")
  ) +
  tm_borders(col = "grey40", lwd = 0.4) +
  tm_title("Map 1: Unhealthiness Index Across Greater London")

map1


map2 <- tm_shape(tesco_and_msoas_full) +
  tm_polygons(
    fill = "cluster_label",
    fill.scale = tm_scale_categorical(
      values = c(
        "Healthiest"   = "#1a9850",   # green
        "Moderate"     = "#fee08b",   # yellow
        "Unhealthiest" = "#d73027"    # red
      )
    ),
    fill.legend = tm_legend(title = "Dietary Cluster")
  ) +
  tm_borders(col = "grey40", lwd = 0.4) +
  tm_title(" Dietary Clusters Across Greater London")

map2



library(ggplot2)




library(factoextra)

# Plot to decide k


fviz_nbclust(cluster_input, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal K")


set.seed(123)  # for reproducibility

kmeans_result <- kmeans(cluster_input, centers = 3)

# Add cluster labels to your dataset
health_index$cluster <- factor(kmeans_result$cluster)

# View cluster counts
table(health_index$cluster)

# View few rows with cluster labels
head(health_index %>% select(unhealthiness_index, cluster))

# Boxplot of Unhealthiness Index by Cluster
ggplot(health_index, aes(x = cluster, y = unhealthiness_index, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Unhealthiness Index by Cluster",
       x = "Cluster", y = "Unhealthiness Index") +
  theme_minimal()


pacman::p_load(tidyverse,dplyr,readr,here,sf,tmap)

packageVersion("tmap")

year_msoa_grocery<-read_csv("year_msoa_grocery.csv")%>%rename(msoa_code=area_id)

#lets import shapefile

msoas<-st_read(here("C:/Users/bathi/OneDrive/Documents/DATA LITERACY ASSIGNMENT/statistical-gis-boundaries-london/ESRI/MSOA_2011_London_gen_MHW.shp"))%>%rename(msoa_code=MSOA11CD,msoa_name=MSOA11NM)

tesco_and_msoas<- inner_join(msoas,year_msoa_grocery)

#tmap 
tm_shape(tesco_and_msoas)+ tm_polygons(fill='f_fruit_veg',fill.chart=tm_chart_histogram())




# First join health_index to your shapefile data

# Rename area_id to msoa_code in health_index for join
health_index <- health_index %>%
  rename(msoa_code = area_id)
tesco_and_msoas <- tesco_and_msoas %>%
  left_join(
    health_index %>% select(msoa_code, cluster),
    by = "msoa_code"
  )

# 2) Create a labelled factor for plotting
tesco_and_msoas <- tesco_and_msoas %>%
  mutate(
    cluster_label = factor(.data$cluster,      # explicitly use the column
                           levels = c(1, 2, 3), 
                           labels = c("Healthiest",
                                      "Moderate",
                                      "Unhealthiest"))
  )

# Step 2: Map using the labelled variable
tm_shape(tesco_and_msoas) +
  tm_polygons("cluster_label",
              fill.scale = tm_scale(values = "Set2"),
              fill.legend = tm_legend(title = "Diet Clusters")) +
  tm_borders() +
  tm_title("Dietary Clusters Across Greater London")

health_index_clean <- health_index %>%
  rename(
    sweets = f_sweets,
    soft_drinks = f_soft_drinks,
    readymade = f_readymade,
    sugar = energy_sugar,
    fat = energy_fat,
    energy_density = energy_density,
    fruit_veg = f_fruit_veg,
    nutrient_diversity = h_nutrients_weight_norm
  )
# 1️⃣ Join the health_index_clean with the London MSOA shapefile
tesco_and_msoas_full <- msoas %>%
  inner_join(health_index_clean, by = "msoa_code")

# 2️⃣ Map 1: Unhealthiness Index across Greater London
map1 <- tm_shape(tesco_and_msoas_full) +
  tm_polygons("unhealthiness_index",
              palette = "-RdYlGn",  # Red = unhealthy, Green = healthy
              style = "quantile",
              title = "Unhealthiness Index") +
  tm_borders(lwd = 0.5, col = "grey40") +
  tm_layout(main.title = "Map 1: Unhealthiness Index Across Greater London",
            legend.outside = TRUE)

# 3️⃣ Map 2: Cluster Map (Healthiest → Unhealthiest)
tesco_and_msoas_full <- tesco_and_msoas_full %>%
  mutate(cluster_label = factor(cluster,
                                levels = c(1, 2, 3), # adjust if needed
                                labels = c("Healthiest", "Moderate", "Unhealthiest")))

map2 <- tm_shape(tesco_and_msoas_full) +
  tm_polygons("cluster_label",
              palette = "Set2",
              title = "Diet Clusters") +
  tm_borders(lwd = 0.5, col = "grey40") +
  tm_layout(main.title = "Map 2: Dietary Clusters Across Greater London",
            legend.outside = TRUE)

# 4️⃣ Print both maps
tmap_mode("plot")
map1
map2


library(tmap)

# Make sure tmap v4 is active
tmap_mode("plot")

map1 <- tm_shape(tesco_and_msoas_full) +
  tm_polygons(
    fill = "unhealthiness_index",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      values = c("#00A65A", "#C7E9B4", "#FDD49E", "#FC8D59", "#B30000")  # Green→Red
    ),
    fill.legend = tm_legend(title = "Unhealthiness Index")
  ) +
  tm_borders(lwd = 0.4, col = "grey40") +
  tm_title("Map 1: Unhealthiness Index Across Greater London")

map1


# Step A: Recode clusters again to ensure correct labels
tesco_and_msoas_full <- tesco_and_msoas_full %>%
  mutate(cluster_label = factor(
    cluster,
    levels = c(1, 2, 3),
    labels = c("Healthiest", "Moderate", "Unhealthiest")
  ))

# Step B: Map with aligned colours
map2 <- tm_shape(tesco_and_msoas_full) +
  tm_polygons(
    fill = "cluster_label",
    fill.scale = tm_scale_categorical(
      values = c(
        "Healthiest"    = "#00A65A",   # Green
        "Moderate"      = "#FDD49E",   # Orange-yellow
        "Unhealthiest"  = "#B30000"    # Red
      )
    ),
    fill.legend = tm_legend(title = "Diet Clusters")
  ) +
  tm_borders(lwd = 0.4, col = "grey40") +
  tm_title("Map 2: Dietary Clusters Across Greater London")

map2

######

missing_areas <- anti_join(msoas, health_index, by = "msoa_code")
# View how many
nrow(missing_areas)
# Optionally: see which areas
missing_areas %>% select(msoa_code, msoa_name)
# Step 1: Load required library
pacman::p_load(spdep)
# Step 2: Define neighbors and weights
nb <- poly2nb(tesco_and_msoas)
lw <- nb2listw(nb, style = "W")
# Spatial neighbors (Queen’s case)
# Step 1: Identify rows with missing unhealthiness index
msoa_nonmissing <- health_index_clean %>%
  filter(!is.na(unhealthiness_index))


###

library(dplyr)
library(sf)
library(spdep)

# 1) Keep only MSOAs with non-missing unhealthiness_index
msoa_full <- tesco_and_msoas_full%>%
  filter(!is.na(unhealthiness_index))

# 2) Build neighbours (Queen contiguity) – add small snap to avoid slivers
nb <- poly2nb(msoa_full, queen = TRUE, snap = 1e-6)

# 3) Convert to weights list (row-standardised)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# 4) Moran’s I test for spatial autocorrelation
moran_result <- moran.test(msoa_full$unhealthiness_index,
                           listw = lw,
                           zero.policy = TRUE)

print(moran_result)



lisa_result <- localmoran(msoa_full$unhealthiness_index, lw,
                          zero.policy = TRUE)
# Add LISA results to spatial dataframe
msoa_full$lisa_i <- lisa_result[,1]
# Local Moran’s I
msoa_full$lisa_pvalue <- lisa_result[,5] # p-value
# Optional: Categorise significance
msoa_nonmissing <- msoa_full %>%
  mutate(lisa_significance = case_when(
    lisa_pvalue < 0.01 ~ "p < 0.01",
    lisa_pvalue < 0.05 ~ "p < 0.05",
    lisa_pvalue < 0.1 ~ "p < 0.10",
    TRUE ~ "Not Significant"
  ))
# 1) Categorise LISA significance
msoa_nonmissing <- msoa_nonmissing %>%
  mutate(lisa_significance = case_when(
    lisa_pvalue < 0.01 ~ "p < 0.01",
    lisa_pvalue < 0.05 ~ "p < 0.05",
    lisa_pvalue < 0.10 ~ "p < 0.10",
    TRUE
    ~ "Not Significant"
  ))
# 2) INNER JOIN: Keep only MSOAs that have both shapefile & LISA results
msoa_lisa_map <- msoa_full %>%
  inner_join(
    st_drop_geometry(msoa_nonmissing) %>%
      select(msoa_code, lisa_significance),
    by = "msoa_code"
  ) %>%
  mutate(lisa_significance = factor(
    lisa_significance,
    levels = c("Not Significant", "p < 0.10", "p < 0.05", "p < 0.01")
  ))
# 3) Plot LISA significance map
tmap_mode("plot")
tm_shape(msoa_lisa_map) +
  tm_polygons("lisa_significance",
              palette = c("#ffffcc", "#fed98e", "#fe9929", "#d95f0e"),
              title = "LISA Significance") +
  tm_borders(lwd = 0.5, col = "grey40") +
  tm_layout(main.title = "Local Moran’s I
 Significance (LISA) for Unhealthiness Index",
            legend.outside = TRUE)
##B Variable Dictionar



neighbours <- poly2nb(tesco_and_msoas_full, queen=TRUE, snap = 1e-6)
neighbours

weights_matrix <- nb2listw(neighbours, style = "W")
weights_matrix

moran.test(tesco_and_msoas_full$f_fruit_veg, listw = weights_matrix)

library(readr)
library(janitor)

# Try UTF-8 first
atlas_raw <- read_csv("msoa-data.csv", 
                      locale = locale(encoding = "UTF-8"),
                      show_col_types = FALSE)

# If error persists, try Latin-1
# atlas_raw <- read_csv("msoa-data.csv", 
#                       locale = locale(encoding = "latin1"),
#                       show_col_types = FALSE)

# Clean column names (fixes spaces, punctuation, duplicates)
atlas_raw <- clean_names(atlas_raw)

library(readr)
library(dplyr)
library(stringr)
library(janitor)

# 1) Read (force encoding if needed)
atlas_raw <- read_csv("msoa-data.csv",
                      locale = locale(encoding = "UTF-8"),
                      show_col_types = FALSE)
# If you still get encoding errors, swap to:
# atlas_raw <- read_csv("msoa-data.csv",
#                       locale = locale(encoding = "latin1"),
#                       show_col_types = FALSE)

# 2) Clean column names to safe snake_case
atlas_raw <- clean_names(atlas_raw)
names(atlas_raw)[1:10]  # quick peek

# After clean_names(), your two first columns will be:
# - "middle_super_output_area"  (MSOA code)
# - "msoa_name"                 (name)

# 3) Standardise join key
atlas <- atlas_raw %>%
  rename(msoa_code = middle_super_output_area) %>%  # << key from your screenshot
  mutate(msoa_code = str_to_upper(str_trim(msoa_code)))

names(atlas_raw)[1:20]

# Should now be "middle_super_output_area"
grep("msoa|output_area", names(atlas_raw), value = TRUE, ignore.case = TRUE)


atlas <- atlas_raw %>%
  rename(msoa_code = middle_super_output_area) %>%
  mutate(msoa_code = stringr::str_to_upper(stringr::str_trim(msoa_code)))

view(atlas)


msoa_full <- msoas %>%
  left_join(health_index, by = "msoa_code") %>%
  left_join(atlas,   by = "msoa_code")

view(msoa_full)

msoa_model <- msoa_full %>% filter(!is.na(unhealthiness_index))
view(msoa_model)

###
# ---- Libraries ----
library(tidyverse)
library(caret)       # for ML workflow
pacman::p_load(randomForest)

# ---- 1) Prepare data ----
df_ml <- msoa_model %>%
  st_drop_geometry() %>%
  transmute(
    msoa_code,
    adult_obesity = obesity_percentage_of_the_population_aged_16_with_a_bmi_of_30_modelled_estimate_2006_2008,
    unhealthiness_index,hh_income_mean = household_income_estimates_2011_12_total_mean_annual_household_income,
    high_quals_pct = qualifications_2011_census_highest_level_of_qualification_level_4_qualifications_and_above,
    income_deprivation = income_deprivation_2010_percent_living_in_income_deprived_households_reliant_on_means_tested_benefit,
    bame_pct = ethnic_group_2011_census_bame_percent,
    pop_density_2012 = population_density_persons_per_hectare_2012
  ) %>%
  filter(complete.cases(.))   # keep only complete rows

# ---- 2) Train Random Forest ----
set.seed(1906525)
rf_model <- randomForest(
  adult_obesity ~ . - msoa_code,  # predict obesity, exclude msoa_code
  data = df_ml,
  ntree = 500,
  importance = TRUE
)

# ---- 3) Variable Importance ----
importance_df <- as.data.frame(importance(rf_model)) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(IncNodePurity))

# Plot importance
ggplot(importance_df, aes(x = reorder(Variable, IncNodePurity), y = IncNodePurity)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance for Predicting Adult Obesity",
       x = "Predictors",
       y = "Importance (IncNodePurity)")

###

# ================================
# Clean OLS vs RF comparison (no IDs in frame)
# ================================
library(tidyverse)
library(sf)
library(randomForest)

set.seed(2025)

# ---- 1) Pick outcome + predictors explicitly ----
y_col <- "obesity_percentage_of_the_population_aged_16_with_a_bmi_of_30_modelled_estimate_2006_2008"

diet_vars <- c(
  "unhealthiness_index")

ses_vars <- c(
  "household_income_estimates_2011_12_total_mean_annual_household_income",
  "qualifications_2011_census_highest_level_of_qualification_level_4_qualifications_and_above",
  "income_deprivation_2010_percent_living_in_income_deprived_households_reliant_on_means_tested_benefit",
  "ethnic_group_2011_census_bame_percent",
  "population_density_persons_per_hectare_2012"
)

keep <- c(y_col, diet_vars, ses_vars)

# ---- 2) Build modelling dataframe (drop geometry & ID; keep only needed cols) ----
df_ml <- msoa_full %>%
  st_drop_geometry() %>%
  select(all_of(keep)) %>%
  rename(
    adult_obesity = !!y_col,
    hh_income_mean = household_income_estimates_2011_12_total_mean_annual_household_income,
    high_quals_pct = qualifications_2011_census_highest_level_of_qualification_level_4_qualifications_and_above,
    income_deprivation = income_deprivation_2010_percent_living_in_income_deprived_households_reliant_on_means_tested_benefit,
    bame_pct = ethnic_group_2011_census_bame_percent,
    pop_density_2012 = population_density_persons_per_hectare_2012
  ) %>%
  mutate(across(everything(), as.numeric)) %>%  # ensure all numeric
  filter(complete.cases(.))                      # no NAs allowed for models

# Sanity check
stopifnot(!any(names(df_ml) == "msoa_code"))

# ---- 3) Train/Test split ----
n <- nrow(df_ml)
idx_train <- sample.int(n, size = floor(0.8*n))
train <- df_ml[idx_train, ]
test  <- df_ml[-idx_train, ]

# Metric helpers
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))
r2   <- function(y, yhat) 1 - sum((y-yhat)^2)/sum((y-mean(y))^2)

# ---- 4) OLS ----
ols_fit <- lm(adult_obesity ~ ., data = train)
summary(ols_fit)

ols_pred_train <- predict(ols_fit, newdata = train)
ols_pred_test  <- predict(ols_fit, newdata = test)

ols_train_r2  <- r2(train$adult_obesity, ols_pred_train)
ols_test_r2   <- r2(test$adult_obesity,  ols_pred_test)
ols_test_rmse <- rmse(test$adult_obesity, ols_pred_test)
ols_test_mae  <- mae(test$adult_obesity,  ols_pred_test)

# ---- 5) Random Forest ----
rf_fit <- randomForest(
  adult_obesity ~ ., data = train,
  ntree = 800,
  mtry  = floor(sqrt(ncol(train) - 1)),  # predictors only
  importance = TRUE
)

rf_pred_train <- predict(rf_fit, newdata = train)
rf_pred_test  <- predict(rf_fit, newdata = test)

rf_train_r2  <- r2(train$adult_obesity, rf_pred_train)
rf_test_r2   <- r2(test$adult_obesity,  rf_pred_test)
rf_test_rmse <- rmse(test$adult_obesity, rf_pred_test)
rf_test_mae  <- mae(test$adult_obesity,  rf_pred_test)

# ---- 6) Results table + quick chart ----
results_tbl <- tibble(
  Model = c("OLS","Random Forest"),
  `Test R²` = c(ols_test_r2, rf_test_r2),
  `Test RMSE` = c(ols_test_rmse, rf_test_rmse),
  `Test MAE`  = c(ols_test_mae,  rf_test_mae),
  `Train R²`  = c(ols_train_r2, rf_train_r2)
)
print(results_tbl %>% mutate(across(-Model, ~round(.x, 3))))

results_long <- results_tbl %>%
  select(Model, `Test R²`, `Test RMSE`, `Test MAE`) %>%
  pivot_longer(-Model, names_to = "Metric", values_to = "Value")

ggplot(results_long, aes(Metric, Value, fill = Model)) +
  geom_col(position = position_dodge(width = 0.7)) +
  geom_text(aes(label = round(Value,3)),
            position = position_dodge(width = 0.7), vjust = -0.2, size = 3) +
  labs(title = "Adult Obesity — Test-set Performance: OLS vs Random Forest",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# ---- 7) (Optional) RF importance plot ----
imp <- as.data.frame(importance(rf_fit)) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(IncNodePurity))

ggplot(imp, aes(reorder(Variable, IncNodePurity), IncNodePurity)) +
  geom_col() + coord_flip() +
  labs(title = "RF Variable Importance (Adult Obesity)",
       x = "Predictor", y = "IncNodePurity") +
  theme_minimal()


####
library(tidyverse)

# 1) Build a long df with observed + predicted for each model
calib_df <- bind_rows(
  tibble(Model = "OLS",
         observed = test$adult_obesity,
         predicted = ols_pred_test),
  tibble(Model = "Random Forest",
         observed = test$adult_obesity,
         predicted = rf_pred_test)
)

# 2) Compute R^2 by model for annotation
r2_by_model <- calib_df %>%
  group_by(Model) %>%
  summarise(
    R2 = 1 - sum((observed - predicted)^2) / sum((observed - mean(observed))^2)
  )

# 3) Plot: observed vs predicted with 45° line + smooth fit + R² label
p_calib <- ggplot(calib_df, aes(x = predicted, y = observed)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.7, color = "grey40") +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  facet_wrap(~ Model, ncol = 2) +
  geom_text(data = r2_by_model,
            aes(x = -Inf, y = Inf,
                label = paste0("R² = ", round(R2, 3))),
            hjust = -0.1, vjust = 1.2, size = 4.2) +
  labs(
    title = "Observed vs Predicted (Test Set): Adult Obesity (%)",
    subtitle = "Dashed line = perfect predictions; solid curve = model calibration",
    x = "Predicted (%)", y = "Observed (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

print(p_calib)

# Optional: save a high-res image for your report
ggsave("adult_obesity_calibration_ols_vs_rf.png", p_calib, width = 10, height = 5, dpi = 300)




###
# --- 0) Load required packages ---
pacman::p_load(dplyr, ggplot2, randomForest, caret, sf, tmap)

# --- 1) Prepare dataset (drop geometry for RF training) ---
df_rf <- msoa_full %>%
  st_drop_geometry() %>%
  select(msoa_code,
         adult_obesity = obesity_percentage_of_the_population_aged_16_with_a_bmi_of_30_modelled_estimate_2006_2008,
         unhealthiness_index,
         hh_income_mean = household_income_estimates_2011_12_total_mean_annual_household_income,
         high_quals_pct = qualifications_2011_census_highest_level_of_qualification_level_4_qualifications_and_above,
         pop_density_2012 = population_density_persons_per_hectare_2012) %>%
  filter(complete.cases(.))

# --- 2) Train/Test split ---
set.seed(123)
train_idx <- createDataPartition(df_rf$adult_obesity, p = 0.7, list = FALSE)
train <- df_rf[train_idx, ]
test  <- df_rf[-train_idx, ]

# --- 3) Train Random Forest ---
rf_model <- randomForest(
  adult_obesity ~ unhealthiness_index + hh_income_mean + high_quals_pct + pop_density_2012,
  data = train,
  ntree = 500, importance = TRUE
)

# --- 4) Predict on ALL MSOAs (not only test) ---
df_rf$rf_pred <- predict(rf_model, newdata = df_rf)

# --- 5) Join predictions back into spatial object ---
msoa_rfmap <- msoa_full %>%
  left_join(df_rf %>% select(msoa_code, adult_obesity, rf_pred), by = "msoa_code") %>%
  mutate(residuals = adult_obesity - rf_pred)

# --- 6) Make tmap plots ---
tmap_mode("plot")

tm_obs <- tm_shape(msoa_rfmap) +
  tm_polygons("adult_obesity",
              palette = "Reds",
              title = "Observed Adult Obesity (%)") +
  tm_borders(lwd = 0.2, col = "grey40")

tm_pred <- tm_shape(msoa_rfmap) +
  tm_polygons("rf_pred",
              palette = "Blues",
              title = "Predicted Adult Obesity (%)") +
  tm_borders(lwd = 0.2, col = "grey40")

tm_resid <- tm_shape(msoa_rfmap) +
  tm_polygons("residuals",
              palette = "-RdBu",
              title = "Residuals (Obs - Pred)") +
  tm_borders(lwd = 0.2, col = "grey40")

# --- 7) Display side by side
tmap_arrange(tm_obs, tm_pred, tm_resid, ncol = 3)

###
LISA

# --- LISA HOT/COLD SPOTS FOR ACTUAL ADULT OBESITY ---
library(dplyr)
library(sf)
library(spdep)
library(tmap)

# 1) Pick the obesity variable (observed)
y_col <- "obesity_percentage_of_the_population_aged_16_with_a_bmi_of_30_modelled_estimate_2006_2008"

# 2) Build an sf with non-missing outcome
sf_obs <- msoa_full %>%
  select(msoa_code, geometry, !!sym(y_col)) %>%
  filter(!is.na(!!sym(y_col))) %>%
  rename(adult_obesity = !!sym(y_col))

# 3) Neighbours + weights (Queen contiguity)
nb <- poly2nb(sf_obs)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# 4) Local Moran's I on observed adult obesity
lisa <- localmoran(sf_obs$adult_obesity, lw, zero.policy = TRUE)

# 5) Standardise variable and compute spatial lag (for quadrant classification)
z      <- as.numeric(scale(sf_obs$adult_obesity))                    # z-score of obesity
lag_z  <- lag.listw(lw, z, zero.policy = TRUE)                       # spatially lagged z
pval   <- lisa[, 5]                                                  # p-values

# 6) Classify LISA clusters (p < 0.05 by default)
sf_obs <- sf_obs %>%
  mutate(
    z = z,
    lag_z = lag_z,
    lisa_i = lisa[, 1],
    lisa_p = pval,
    lisa_cluster = case_when(
      z >  0 & lag_z >  0 & lisa_p < 0.05 ~ "High-High (Hotspot)",
      z <  0 & lag_z <  0 & lisa_p < 0.05 ~ "Low-Low (Coldspot)",
      z >  0 & lag_z <  0 & lisa_p < 0.05 ~ "High-Low (Outlier)",
      z <  0 & lag_z >  0 & lisa_p < 0.05 ~ "Low-High (Outlier)",
      TRUE                                ~ "Not significant"
    )
  ) %>%
  mutate(
    lisa_cluster = factor(
      lisa_cluster,
      levels = c("High-High (Hotspot)", "Low-Low (Coldspot)",
                 "High-Low (Outlier)", "Low-High (Outlier)", "Not significant")
    )
  )

# 7) Map the clusters
tmap_mode("plot")
tm_shape(sf_obs) +
  tm_polygons(
    "lisa_cluster",
    palette = c("#d7191c", "#2c7bb6", "#fdae61", "#abd9e9", "#f0f0f0"),
    title = "LISA clusters (Adult Obesity, p < 0.05)"
  ) +
  tm_borders(col = "grey40", lwd = 0.5) +
  tm_layout(
    main.title = "Adult Obesity Hot/Cold Spots (Local Moran’s I)",
    legend.outside = TRUE
  )





## DEPRIVATION 

# === Policy-brief style plot: Diet effect by deprivation decile ===
library(dplyr)
library(ggplot2)
library(broom)

# 1) Build modelling frame (adjust the deprivation column name if needed)
df_eq <- msoa_full %>%
  sf::st_drop_geometry() %>%
  transmute(
    adult_obesity = obesity_percentage_of_the_population_aged_16_with_a_bmi_of_30_modelled_estimate_2006_2008,
    unhealthiness_index,
    income_dep = income_deprivation_2010_percent_living_in_income_deprived_households_reliant_on_means_tested_benefit
  ) %>%
  filter(complete.cases(.)) %>%
  mutate(dep_decile = dplyr::ntile(income_dep, 10))   # 1 = least deprived, 10 = most deprived

# 2) Fit OLS per decile and extract β + 95% CI for unhealthiness_index
by_decile <- df_eq %>%
  group_by(dep_decile) %>%
  do({
    fit <- lm(adult_obesity ~ unhealthiness_index, data = .)
    tidy_fit <- broom::tidy(fit)
    tidy_fit
  }) %>%
  ungroup() %>%
  filter(term == "unhealthiness_index") %>%
  mutate(
    ci_low  = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error
  )

# 3) Policy-brief style figure
p_equity <- ggplot(by_decile, aes(x = dep_decile, y = estimate, group = 1)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Unequal Burdens: Diet–Obesity Link Across Income Deprivation",
    subtitle = "β = effect of dietary unhealthiness on adult obesity within each decile (95% CI)",
    x = "Income deprivation decile (1 = least deprived(highest income), 10 = most deprived(least income))",
    y = "β (diet effect on adult obesity)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey30")
  )

print(p_equity)

# 4) Save high-resolution image for the report


ggsave("diet_effect_by_deprivation_decile.png", p_equity, width = 9, height = 5, dpi = 300)



w## WHAT IF 


# --- Create modelling dataset with raw diet vars ---

df_policy <- msoa_full %>%
  st_drop_geometry() %>%
  transmute(
    msoa_code,
    adult_obesity =
      obesity_percentage_of_the_population_aged_16_with_a_bmi_of_30_modelled_estimate_2006_2008,
    
    # FULL dietary profile
    soft_drinks   = f_soft_drinks,
    fruit_veg     = f_fruit_veg,
    readymade     = f_readymade,
    energy_density = energy_density,
    energy_fat     = energy_fat,
    energy_sugar   = energy_sugar,
    
    # SES
    income        = household_income_estimates_2011_12_total_mean_annual_household_income,
    quals         = qualifications_2011_census_highest_level_of_qualification_level_4_qualifications_and_above,
    density       = population_density_persons_per_hectare_2012
  ) %>%
  filter(complete.cases(.))

ols_policy <- lm(adult_obesity ~ soft_drinks + fruit_veg + readymade +
                   energy_density + energy_fat + energy_sugar +
                   income + quals + density,
                 data = df_policy)

summary(ols_policy)





# --- Baseline predictions ---
df_policy$pred_baseline <- predict(ols_policy, newdata = df_policy)



# --- Scenario A: increase fruit/veg by 10% ---
df_policy_fruit <- df_policy
df_policy_fruit$fruit_veg <- df_policy_fruit$fruit_veg * 1.2
df_policy$pred_fruit_plus10 <- predict(ols_policy, newdata = df_policy_fruit)

## -- scenario B: 

df_policy_readymade <- df_policy
df_policy_readymade$readymade <- df_policy_readymade$readymade * 0.8

df_policy$pred_readymade_minus20 <- predict(ols_policy, newdata = df_policy_readymade)


###
# --- Calculate average differences ---
scenario_summary <- data.frame(
  Scenario = c(
    "Baseline",
    "Ready-made -20%",
    "Fruit/veg +20%"
  ),
  Mean_Obesity = c(
    mean(df_policy$pred_baseline),
    
    mean(df_policy$pred_readymade_minus20),
    mean(df_policy$pred_fruit_plus10)
  )
) %>%
  mutate(Change_vs_Baseline = Mean_Obesity - Mean_Obesity[1])

print(scenario_summary)


# --- Calculate average differences ---
scenario_summary <- data.frame(
  Scenario = c("Baseline", "Ready-made -20%","Fruit/veg +20%"),
  Mean_Obesity = c(mean(df_policy$pred_baseline),
                   
                   mean(df_policy$pred_readymade_minus20),
                   mean(df_policy$pred_fruit_plus10))
) %>%
  mutate(Change_vs_Baseline = Mean_Obesity - Mean_Obesity[1])

print(scenario_summary)


##
library(ggplot2)

ggplot(scenario_summary, aes(x = Scenario, y = Mean_Obesity, fill = Scenario)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(Mean_Obesity, 2)), 
            vjust = -0.5, size = 5) +
  ylim(0, 23) +          # <<< ADD THIS LINE
  labs(
    title = "Policy Scenario Simulation (OLS)",
    subtitle = "Effect of diet changes on predicted adult obesity (%)",
    y = "Mean predicted adult obesity (%)",
    x = ""
  ) +
  theme_minimal(base_size = 14)








