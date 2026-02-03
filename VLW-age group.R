#!/usr/bin/env Rscript
# ==================================================================================
# Age-specific VLW calculation - fixed version (bug resolved)
# Issue: HALE was dropped in group_by, which broke VSLY and DF
# Fix: compute individual VLW first, then aggregate
# ==================================================================================

library(tidyverse)

# Parameter settings
VSL_peak_USA <- 11.8e6
GDP_pc_USA <- 71773.237
IE <- 1.0
r <- 0.03

cat("\n" %>% paste0(strrep("=", 100)) %>% paste0("\n"))
cat("Age-specific VLW calculation - fixed version\n")
cat("=" %>% strrep(100) %>% paste0("\n\n"))

# Data validation
required_cols <- c("location_id", "location_name", "sex_id", "age_id", "age_name", "DALY", 
                   "upper", "lower", "HALE", "GDP_PPP_total", "GDP2021")
missing_cols <- setdiff(required_cols, names(df_age_specific))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")))
}

cat("✓ Data validation complete\n")
cat(sprintf("  Total rows: %d | Unique countries: %d | Unique age groups: %d\n\n", 
            nrow(df_age_specific), 
            n_distinct(df_age_specific$location_id),
            n_distinct(df_age_specific$age_name)))

# Add sex labels
df_age_specific <- df_age_specific %>%
  mutate(
    sex_name = case_when(
      sex_id == 1 ~ "Male",
      sex_id == 2 ~ "Female",
      sex_id == 3 ~ "Both",
      TRUE ~ "Unknown"
    )
  )

# ==================================================================================
# Step 1: Compute country-specific VSL
# ==================================================================================

df_age_specific <- df_age_specific %>%
  mutate(
    VSL_i = VSL_peak_USA * (GDP2021 / GDP_pc_USA)^IE,
    VSL_i = round(VSL_i, 0)
  )

cat("Formula 1.1: VSL_i = VSL_USA × (GDP_pc_i / GDP_pc_USA)^IE\n")
cat(sprintf("✓ VSL_i computed (range: %.0f - %.0fM)\n\n", 
            min(df_age_specific$VSL_i, na.rm=T)/1e6,
            max(df_age_specific$VSL_i, na.rm=T)/1e6))

# ==================================================================================
# Step 2: Compute age-specific VSLY
# ==================================================================================

df_age_specific <- df_age_specific %>%
  mutate(
    HALE_check = !is.na(HALE) & HALE > 0,
    VSLY_ia = ifelse(HALE_check, VSL_i / HALE, 0),
    VSLY_ia = round(VSLY_ia, 0)
  )

cat("Formula 2.2: VSLY_ia = VSL_i / HALE_ia\n")
cat(sprintf("✓ VSLY_ia computed\n"))
cat(sprintf("  Valid HALE rows: %d / %d\n", 
            sum(df_age_specific$HALE_check, na.rm=T),
            nrow(df_age_specific)))
cat(sprintf("  VSLY_ia range: %.0f - %.0fk per year\n\n", 
            min(df_age_specific$VSLY_ia, na.rm=T)/1000,
            max(df_age_specific$VSLY_ia, na.rm=T)/1000))

# ==================================================================================
# Step 3: Compute discount factor
# ==================================================================================

calculate_discount_factor_correct <- function(HALE, discount_rate = 0.03) {
  if (is.na(HALE) || HALE <= 0) return(0)
  numerator <- 1 - (1 + discount_rate)^(-HALE)
  denominator <- discount_rate * HALE
  if (denominator <= 0) return(0)
  return(numerator / denominator)
}

df_age_specific <- df_age_specific %>%
  mutate(
    DF_ia = map_dbl(HALE, ~calculate_discount_factor_correct(.x, discount_rate = r)),
    DF_ia = round(DF_ia, 4)
  )

cat("Formula 3.2: DF_ia = [1-(1+r)^(-HALE_ia)] / (r × HALE_ia)\n")
cat(sprintf("✓ DF_ia computed\n"))
cat(sprintf("  DF_ia range: %.4f - %.4f\n\n",
            min(df_age_specific$DF_ia, na.rm=T),
            max(df_age_specific$DF_ia, na.rm=T)))

# ==================================================================================
# Step 4: Compute individual VLW (undiscounted) [key: before group_by]
# ==================================================================================

df_age_specific <- df_age_specific %>%
  mutate(
    VLW_no_discount = VSLY_ia * DALY / 1e9,
    VLW_no_discount_lower = VSLY_ia * lower / 1e9,
    VLW_no_discount_upper = VSLY_ia * upper / 1e9
  )

cat("Formula 4.1: VLW = VSLY_ia × DALY / 10^9 (undiscounted)\n")
cat(sprintf("✓ VLW_no_discount computed\n"))
cat(sprintf("  Max: %.2f billion\n", max(df_age_specific$VLW_no_discount, na.rm=T)))


# ==================================================================================
# Step 5: Compute individual VLW (discounted) [key: before group_by]
# ==================================================================================

df_age_specific <- df_age_specific %>%
  mutate(
    VLW_discounted = VSLY_ia * DALY * DF_ia / 1e9,
    VLW_discounted_lower = VSLY_ia * lower * DF_ia / 1e9,
    VLW_discounted_upper = VSLY_ia * upper * DF_ia / 1e9
  )

cat("Formula 4.2: VLW = VSLY_ia × DALY × DF_ia / 10^9 (discounted)\n")
cat(sprintf("✓ VLW_discounted computed\n"))
cat(sprintf("  Max: %.2f billion\n", max(df_age_specific$VLW_discounted, na.rm=T)))


# ==================================================================================
# Step 6: Compute GDP share
# ==================================================================================

df_age_specific <- df_age_specific %>%
  mutate(
    VLW_no_discount_vs_GDP_pct = 
      (VLW_no_discount * 1e9 / GDP_PPP_total) * 100,
    VLW_no_discount_vs_GDP_lower_pct = 
      (VLW_no_discount_lower * 1e9 / GDP_PPP_total) * 100,
    VLW_no_discount_vs_GDP_upper_pct = 
      (VLW_no_discount_upper * 1e9 / GDP_PPP_total) * 100,
    VLW_discounted_vs_GDP_pct = 
      (VLW_discounted * 1e9 / GDP_PPP_total) * 100,
    VLW_discounted_vs_GDP_lower_pct = 
      (VLW_discounted_lower * 1e9 / GDP_PPP_total) * 100,
    VLW_discounted_vs_GDP_upper_pct = 
      (VLW_discounted_upper * 1e9 / GDP_PPP_total) * 100
  )

cat("Formula 5.1: GDP share = (VLW × 10^9 / GDP_PPP) × 100%\n\n")

# ==================================================================================
# Step 7: Country-age-sex summary (undiscounted)
# ==================================================================================

cat("=" %>% strrep(100) %>% paste0("\n"))
cat("Generating output files...\n")
cat("=" %>% strrep(100) %>% paste0("\n\n"))

# Key change: sum precomputed VLW values directly (no recomputation)
country_age_sex_no_discount <- df_age_specific %>%
  group_by(location_id, location_name, age_id, age_name, sex_id, sex_name) %>%
  summarise(
    # DALY sums
    DALY = sum(DALY, na.rm = TRUE),
    DALY_lower = sum(lower, na.rm = TRUE),
    DALY_upper = sum(upper, na.rm = TRUE),
    
    # Sum VLW directly (computed before group_by)
    VLW_no_discount_point = sum(VLW_no_discount, na.rm = TRUE),
    VLW_no_discount_lower = sum(VLW_no_discount_lower, na.rm = TRUE),
    VLW_no_discount_upper = sum(VLW_no_discount_upper, na.rm = TRUE),
    
    # Sum GDP share directly
    VLW_no_discount_vs_GDP_pct_point = sum(VLW_no_discount_vs_GDP_pct, na.rm = TRUE),
    VLW_no_discount_vs_GDP_pct_lower = sum(VLW_no_discount_vs_GDP_lower_pct, na.rm = TRUE),
    VLW_no_discount_vs_GDP_pct_upper = sum(VLW_no_discount_vs_GDP_upper_pct, na.rm = TRUE),
    
    .groups = 'drop'
  )

# ==================================================================================
# Step 8: Country-age-sex summary (discounted)
# ==================================================================================

country_age_sex_discounted <- df_age_specific %>%
  group_by(location_id, location_name, age_id, age_name, sex_id, sex_name) %>%
  summarise(
    # DALY sums
    DALY = sum(DALY, na.rm = TRUE),
    DALY_lower = sum(lower, na.rm = TRUE),
    DALY_upper = sum(upper, na.rm = TRUE),
    
    # Sum VLW directly (computed before group_by)
    VLW_discounted_point = sum(VLW_discounted, na.rm = TRUE),
    VLW_discounted_lower = sum(VLW_discounted_lower, na.rm = TRUE),
    VLW_discounted_upper = sum(VLW_discounted_upper, na.rm = TRUE),
    
    # Sum GDP share directly
    VLW_discounted_vs_GDP_pct_point = sum(VLW_discounted_vs_GDP_pct, na.rm = TRUE),
    VLW_discounted_vs_GDP_pct_lower = sum(VLW_discounted_vs_GDP_lower_pct, na.rm = TRUE),
    VLW_discounted_vs_GDP_pct_upper = sum(VLW_discounted_vs_GDP_upper_pct, na.rm = TRUE),
    
    .groups = 'drop'
  )

# ==================================================================================
# Export country-age summaries by sex
# ==================================================================================

for (sex in c(1, 2, 3)) {
  sex_label <- case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    sex == 3 ~ "both"
  )
  
  # Undiscounted
  data_no_discount <- country_age_sex_no_discount %>%
    filter(sex_id == sex) %>%
    select(-sex_id, -sex_name)
  
  filename_no_discount <- sprintf("country_age_%s_no_discount_summary.csv", sex_label)
  write.csv(data_no_discount, filename_no_discount, row.names = FALSE)
  cat(sprintf("✓ %s (%d rows)\n", filename_no_discount, nrow(data_no_discount)))
  
  # Discounted
  data_discounted <- country_age_sex_discounted %>%
    filter(sex_id == sex) %>%
    select(-sex_id, -sex_name)
  
  filename_discounted <- sprintf("country_age_%s_discounted_summary.csv", sex_label)
  write.csv(data_discounted, filename_discounted, row.names = FALSE)
  cat(sprintf("✓ %s (%d rows)\n", filename_discounted, nrow(data_discounted)))
  cat("\n")
}

# ==================================================================================
# Global-age-sex summary (undiscounted)
# ==================================================================================

global_age_sex_no_discount <- country_age_sex_no_discount %>%
  group_by(age_id, age_name, sex_id, sex_name) %>%
  summarise(
    DALY_total = sum(DALY, na.rm = TRUE),
    DALY_lower = sum(DALY_lower, na.rm = TRUE),
    DALY_upper = sum(DALY_upper, na.rm = TRUE),
    VLW_no_discount_total = sum(VLW_no_discount_point, na.rm = TRUE),
    VLW_no_discount_lower = sum(VLW_no_discount_lower, na.rm = TRUE),
    VLW_no_discount_upper = sum(VLW_no_discount_upper, na.rm = TRUE),
    .groups = 'drop'
  )

# ==================================================================================
# Global-age-sex summary (discounted)
# ==================================================================================

global_age_sex_discounted <- country_age_sex_discounted %>%
  group_by(age_id, age_name, sex_id, sex_name) %>%
  summarise(
    DALY_total = sum(DALY, na.rm = TRUE),
    DALY_lower = sum(DALY_lower, na.rm = TRUE),
    DALY_upper = sum(DALY_upper, na.rm = TRUE),
    VLW_discounted_total = sum(VLW_discounted_point, na.rm = TRUE),
    VLW_discounted_lower = sum(VLW_discounted_lower, na.rm = TRUE),
    VLW_discounted_upper = sum(VLW_discounted_upper, na.rm = TRUE),
    .groups = 'drop'
  )

# ==================================================================================
# Export global-age summaries by sex
# ==================================================================================

for (sex in c(1, 2, 3)) {
  sex_label <- case_when(
    sex == 1 ~ "male",
    sex == 2 ~ "female",
    sex == 3 ~ "both"
  )
  
  # Undiscounted
  data_no_discount <- global_age_sex_no_discount %>%
    filter(sex_id == sex) %>%
    select(-sex_id, -sex_name, -age_id) %>%
    rename(Age_Group = age_name)
  
  filename_no_discount <- sprintf("global_age_%s_no_discount_summary.csv", sex_label)
  write.csv(data_no_discount, filename_no_discount, row.names = FALSE)
  cat(sprintf("✓ %s (%d rows)\n", filename_no_discount, nrow(data_no_discount)))
  
  # Discounted
  data_discounted <- global_age_sex_discounted %>%
    filter(sex_id == sex) %>%
    select(-sex_id, -sex_name, -age_id) %>%
    rename(Age_Group = age_name)
  
  filename_discounted <- sprintf("global_age_%s_discounted_summary.csv", sex_label)
  write.csv(data_discounted, filename_discounted, row.names = FALSE)
  cat(sprintf("✓ %s (%d rows)\n", filename_discounted, nrow(data_discounted)))
  cat("\n")
}

# ==================================================================================
# Display results summary
# ==================================================================================

cat("=" %>% strrep(100) %>% paste0("\n"))
cat("Summary - global age group (discounted)\n")
cat("=" %>% strrep(100) %>% paste0("\n\n"))

for (sex in c(1, 2, 3)) {
  sex_name_label <- case_when(
    sex == 1 ~ "Male(sex_id=1)",
    sex == 2 ~ "Female(sex_id=2)",
    sex == 3 ~ "Both(sex_id=3)"
  )
  
  results <- global_age_sex_discounted %>%
    filter(sex_id == sex) %>%
    arrange(age_id)
  
  cat(sprintf("\n[%s discounted]\n", sex_name_label))
  cat("Age_Group          DALY_total      VLW_total(B)     Range_Lower(B)   Range_Upper(B)\n")
  cat("-" %>% strrep(100) %>% paste0("\n"))
  
  for (i in 1:nrow(results)) {
    row <- results[i, ]
    cat(sprintf("%-15s   %12.0f   %12.2f     %12.2f     %12.2f\n",
                row$age_name, 
                row$DALY_total,
                row$VLW_discounted_total,
                row$VLW_discounted_lower,
                row$VLW_discounted_upper))
  }
}

cat("\n" %>% paste0(strrep("=", 100)) %>% paste0("\n"))
cat("✓ Done! 12 CSV files generated\n")
cat("=" %>% strrep(100) %>% paste0("\n\n"))
