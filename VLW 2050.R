library(tidyverse)
library(patchwork)
setwd("/Users/anderson/Desktop/VSL-GBD/Gas")
df=readxl::read_excel("20250625极其重要 整理的 DALY 和各种经济指标 表1.xlsx",1 )
dfx=df %>% select(1,2,7,16,17,52) %>% 
  filter(`Series Name` %in% c("GDP per capita, PPP (constant 2021 international $)","GDP, PPP (constant 2021 international $)")) %>% 
  select(2,3,5,6) %>% rename("location_id"=1)

# Assume the original data is df_wide
df_wide <- dfx %>%
  select(location_id,location_name, `Series Name`, `2021 [YR2021]`) %>%
  pivot_wider(
    names_from = `Series Name`,
    values_from = `2021 [YR2021]`
  ) %>% set_names("location_id","location_name","GDP_PPP_total","GDP2021") %>% 
  mutate(GDP_PPP_total=as.numeric(GDP_PPP_total),GDP2021=as.numeric(GDP2021))

a1=df_wide %>% filter(location_id==102)
a2=df_wide %>% filter(!location_id==102)
dfx=rbind(a1,a2)


# Calculate VSL_peak_i
dfx <- dfx %>%
  mutate(
    VSL_peak_i = VSL_peak_USA * (GDP2021 / GDP_pc_USA)^IE
  )

# Inspect results
dfx %>%  head()

########
#load("GBDpop.Rdata")
dfa=read.csv("2050.csv",header = T)
dfa1=dfa %>%   filter(year==2050) %>%
  select(1,5,8,9) %>% set_names("location_id","DALY","lower","upper") %>% 
  mutate(lower=ifelse(lower<0,0,lower)) %>% 
  mutate(upper=ifelse(upper<0,0,upper)) %>% 
  mutate(DALY=ifelse(DALY<0,0,DALY))





HALE=read.csv("/Users/anderson/Desktop/VSL-GBD/HALE.csv",header = T)
HALEdata=HALE %>% filter(age_name=="0-6 days" ) %>% filter(sex_name=="Both") %>% 
  filter(year==2021) %>% 
  select(-location_name) %>% mutate(HALE=val) %>% select(location_id,HALE)

dfa1=left_join(dfa1,HALEdata) 

dfy=left_join(dfx,dfa1,by="location_id")
library(dplyr)

dfx=dfy


# Gastric cancer VLW calculation (with confidence intervals)

# Load required libraries
library(dplyr)

# Set parameters
VSL_peak_USA <- 11.8e6      # US peak VSL
GDP_pc_USA <- 71773.237     # US GDP per capita
IE <- 1.0                   # VSL income elasticity
r <- 0.03                   # Discount rate

# Discount factor function
calculate_discount_factor <- function(life_expectancy, age_ratio = 0.6, discount_rate = 0.03) {
  # Assume current age is 60% of life expectancy
  current_age <- life_expectancy * age_ratio
  remaining_years <- life_expectancy - current_age
  
  # Present value of remaining years after discounting
  if (remaining_years <= 0) {
    return(0)
  } else {
    discounted_years <- (1 - (1/(1+discount_rate))^remaining_years) / discount_rate
    return(discounted_years / remaining_years)
  }
}

# Main calculation function
calculate_vlw_gastric_cancer <- function(data) {
  result <- data %>%
    mutate(
      # Step 1: Compute FLE_Average (average future life expectancy)
      FLE_Average = HALE / 2,
      
      # Step 2: Compute age-specific discount factors
      discount_factor = map_dbl(HALE, calculate_discount_factor),
      
      # Step 3: Compute VSLY
      VSLY = VSL_peak_i / FLE_Average,
      
      # Step 4: Compute VLW - main estimate (use DALY median)
      VLW_no_discount = VSLY * DALY / 1e9,  # Convert to billions
      VLW_discounted = VSLY * DALY * discount_factor / 1e9,
      
      # Step 5: Compute VLW - lower bound (use lower)
      VLW_no_discount_lower = VSLY * lower / 1e9,
      VLW_discounted_lower = VSLY * lower * discount_factor / 1e9,
      
      # Step 6: Compute VLW - upper bound (use upper)
      VLW_no_discount_upper = VSLY * upper / 1e9,
      VLW_discounted_upper = VSLY * upper * discount_factor / 1e9,
      
      # Step 7: Compute GDP share - main estimate
      VLW_no_discount_vs_GDP_pct = (VLW_no_discount * 1e9 / GDP_PPP_total) * 100,
      VLW_discounted_vs_GDP_pct = (VLW_discounted * 1e9 / GDP_PPP_total) * 100,
      
      # Step 8: Compute GDP share - confidence interval
      VLW_no_discount_vs_GDP_lower_pct = (VLW_no_discount_lower * 1e9 / GDP_PPP_total) * 100,
      VLW_no_discount_vs_GDP_upper_pct = (VLW_no_discount_upper * 1e9 / GDP_PPP_total) * 100,
      VLW_discounted_vs_GDP_lower_pct = (VLW_discounted_lower * 1e9 / GDP_PPP_total) * 100,
      VLW_discounted_vs_GDP_upper_pct = (VLW_discounted_upper * 1e9 / GDP_PPP_total) * 100,
      
      # Step 9: Compute discount effect
      discount_effect_pct = (VLW_discounted - VLW_no_discount) / VLW_no_discount * 100
    )
  
  return(result)
}

# Run calculation
results <- calculate_vlw_gastric_cancer(dfx)

# Save to CSV (no row names)
write.csv(results, file = "results.csv", row.names = FALSE)

# Generate main results table
print("=== Gastric cancer VLW results (with CI and GDP share) ===")
main_results <- results %>%
  select(location_id,
         location_name,
         DALY, lower, upper,
         VSL_peak_i, HALE, FLE_Average, VSLY, discount_factor,
         # VLW results
         VLW_no_discount,
         VLW_no_discount_lower,
         VLW_no_discount_upper,
         VLW_discounted,
         VLW_discounted_lower,
         VLW_discounted_upper,
         # GDP share
         VLW_no_discount_vs_GDP_pct,
         VLW_no_discount_vs_GDP_lower_pct,
         VLW_no_discount_vs_GDP_upper_pct,
         VLW_discounted_vs_GDP_pct,
         VLW_discounted_vs_GDP_lower_pct,
         VLW_discounted_vs_GDP_upper_pct,
         discount_effect_pct
  ) %>%
  mutate(
    # Format values for display
    DALY = round(DALY, 0),
    lower = round(lower, 0),
    upper = round(upper, 0),
    VSL_peak_i = round(VSL_peak_i, 0),
    HALE = round(HALE, 1),
    FLE_Average = round(FLE_Average, 1),
    VSLY = round(VSLY, 0),
    discount_factor = round(discount_factor, 3),
    
    # Create formatted VLW string: xxx(xxx-xxx) in billions
    VLW_no_discount_formatted = sprintf("%.2f(%.2f-%.2f)", 
                                        VLW_no_discount, 
                                        VLW_no_discount_lower, 
                                        VLW_no_discount_upper),
    VLW_discounted_formatted = sprintf("%.2f(%.2f-%.2f)", 
                                       VLW_discounted, 
                                       VLW_discounted_lower, 
                                       VLW_discounted_upper),
    
    # Create formatted GDP share string
    GDP_ratio_no_discount_formatted = sprintf("%.2f%%(%.2f%%-%.2f%%)", 
                                              VLW_no_discount_vs_GDP_pct,
                                              VLW_no_discount_vs_GDP_lower_pct,
                                              VLW_no_discount_vs_GDP_upper_pct),
    GDP_ratio_discounted_formatted = sprintf("%.2f%%(%.2f%%-%.2f%%)", 
                                             VLW_discounted_vs_GDP_pct,
                                             VLW_discounted_vs_GDP_lower_pct,
                                             VLW_discounted_vs_GDP_upper_pct),
    
    discount_effect_pct = round(discount_effect_pct, 2)
  )

print(main_results)

# Show formatted summary table
print("\n=== Formatted summary (units: billion USD) ===")
formatted_summary <- main_results %>%
  select(
    location_id,
    location_name,
    DALY,
    VSLY,
    discount_factor,
    VLW_no_discount_formatted,
    VLW_discounted_formatted,
    GDP_ratio_no_discount_formatted,
    GDP_ratio_discounted_formatted,
    discount_effect_pct
  ) %>%
  rename(
    location_id=    location_id,
    Country = location_name,
    DALY = DALY,
    VSLY = VSLY,
    Discount_Factor = discount_factor,
    `VLW_No_Discount(billion)` = VLW_no_discount_formatted,
    `VLW_Discounted(billion)` = VLW_discounted_formatted,
    `GDP_Ratio_No_Discount(%)` = GDP_ratio_no_discount_formatted,
    `GDP_Ratio_Discounted(%)` = GDP_ratio_discounted_formatted,
    `Discount_Effect(%)` = discount_effect_pct
  ) %>% select(-Discount_Factor,-`Discount_Effect(%)`)

print(formatted_summary)
# Save to CSV (no row names)
write.csv(formatted_summary, file = "Country 204.csv", row.names = FALSE)

# Compute overall summary statistics
print("\n=== Overall summary statistics ===")
total_summary <- results %>%
  summarise(
    Total_DALY = sprintf("%.0f(%.0f-%.0f)", 
                         sum(DALY), sum(lower), sum(upper)),
    
    Total_VLW_No_Discount_Billion = sprintf("%.2f(%.2f-%.2f)", 
                                            sum(VLW_no_discount), 
                                            sum(VLW_no_discount_lower), 
                                            sum(VLW_no_discount_upper)),
    
    Total_VLW_Discounted_Billion = sprintf("%.2f(%.2f-%.2f)", 
                                           sum(VLW_discounted), 
                                           sum(VLW_discounted_lower), 
                                           sum(VLW_discounted_upper)),
    
    Weighted_Avg_GDP_Ratio_No_Discount = sprintf("%.3f%%", 
                                                 weighted.mean(VLW_no_discount_vs_GDP_pct, GDP_PPP_total)),
    
    Weighted_Avg_GDP_Ratio_Discounted = sprintf("%.3f%%", 
                                                weighted.mean(VLW_discounted_vs_GDP_pct, GDP_PPP_total)),
    
    Average_Discount_Effect = sprintf("%.2f%%", 
                                      mean(discount_effect_pct))
  ) %>% select(-"Average_Discount_Effect" )

print(total_summary)
# Save to CSV (no row names)
write.csv(total_summary, file = "total_summary.csv", row.names = FALSE)


dfregion=read.csv("21.csv",header = T) %>% select(2,5,6)
resultsX= dfregion %>% left_join(results)

library(dplyr)

sdi_summary <- resultsX %>%
  group_by(SDI) %>%
  summarise(
    Total_DALY = sprintf("%.0f(%.0f-%.0f)", 
                         sum(DALY), sum(lower), sum(upper)),
    
    Total_VLW_No_Discount_Billion = sprintf("%.2f(%.2f-%.2f)", 
                                            sum(VLW_no_discount), 
                                            sum(VLW_no_discount_lower), 
                                            sum(VLW_no_discount_upper)),
    
    Total_VLW_Discounted_Billion = sprintf("%.2f(%.2f-%.2f)", 
                                           sum(VLW_discounted), 
                                           sum(VLW_discounted_lower), 
                                           sum(VLW_discounted_upper)),
    
    # Weighted average (undiscounted)
    w_avg_no_discount = weighted.mean(VLW_no_discount_vs_GDP_pct, GDP_PPP_total, na.rm = TRUE),
    w_var_no_discount = sum(GDP_PPP_total^2 * (VLW_no_discount_vs_GDP_pct - weighted.mean(VLW_no_discount_vs_GDP_pct, GDP_PPP_total, na.rm = TRUE))^2, na.rm = TRUE) / sum(GDP_PPP_total)^2,
    w_se_no_discount = sqrt(w_var_no_discount),
    w_lci_no_discount = w_avg_no_discount - 1.96 * w_se_no_discount,
    w_uci_no_discount = w_avg_no_discount + 1.96 * w_se_no_discount,
    Weighted_Avg_GDP_Ratio_No_Discount = sprintf("%.3f%%(%.3f–%.3f%%)", w_avg_no_discount, w_lci_no_discount, w_uci_no_discount),
    
    # Weighted average (discounted)
    w_avg_discounted = weighted.mean(VLW_discounted_vs_GDP_pct, GDP_PPP_total, na.rm = TRUE),
    w_var_discounted = sum(GDP_PPP_total^2 * (VLW_discounted_vs_GDP_pct - weighted.mean(VLW_discounted_vs_GDP_pct, GDP_PPP_total, na.rm = TRUE))^2, na.rm = TRUE) / sum(GDP_PPP_total)^2,
    w_se_discounted = sqrt(w_var_discounted),
    w_lci_discounted = w_avg_discounted - 1.96 * w_se_discounted,
    w_uci_discounted = w_avg_discounted + 1.96 * w_se_discounted,
    Weighted_Avg_GDP_Ratio_Discounted = sprintf("%.3f%%(%.3f–%.3f%%)", w_avg_discounted, w_lci_discounted, w_uci_discounted),
    
    # Average discount effect
    Average_Discount_Effect = sprintf("%.2f%%", mean(discount_effect_pct, na.rm = TRUE))
  ) %>%
  select(SDI, Total_DALY, Total_VLW_No_Discount_Billion, Total_VLW_Discounted_Billion,
         Weighted_Avg_GDP_Ratio_No_Discount, Weighted_Avg_GDP_Ratio_Discounted) %>%
  ungroup() 

# Save to CSV (no row names)
write.csv(sdi_summary, file = "sdi_summary.csv", row.names = FALSE)



Region21_summary <- resultsX %>%
  group_by(region) %>%
  summarise(
    Total_DALY = sprintf("%.0f(%.0f-%.0f)", 
                         sum(DALY), sum(lower), sum(upper)),
    
    Total_VLW_No_Discount_Billion = sprintf("%.2f(%.2f-%.2f)", 
                                            sum(VLW_no_discount), 
                                            sum(VLW_no_discount_lower), 
                                            sum(VLW_no_discount_upper)),
    
    Total_VLW_Discounted_Billion = sprintf("%.2f(%.2f-%.2f)", 
                                           sum(VLW_discounted), 
                                           sum(VLW_discounted_lower), 
                                           sum(VLW_discounted_upper)),
    
    # Weighted average (undiscounted)
    w_avg_no_discount = weighted.mean(VLW_no_discount_vs_GDP_pct, GDP_PPP_total, na.rm = TRUE),
    w_var_no_discount = sum(GDP_PPP_total^2 * (VLW_no_discount_vs_GDP_pct - weighted.mean(VLW_no_discount_vs_GDP_pct, GDP_PPP_total, na.rm = TRUE))^2, na.rm = TRUE) / sum(GDP_PPP_total)^2,
    w_se_no_discount = sqrt(w_var_no_discount),
    w_lci_no_discount = w_avg_no_discount - 1.96 * w_se_no_discount,
    w_uci_no_discount = w_avg_no_discount + 1.96 * w_se_no_discount,
    Weighted_Avg_GDP_Ratio_No_Discount = sprintf("%.3f%%(%.3f–%.3f%%)", w_avg_no_discount, w_lci_no_discount, w_uci_no_discount),
    
    # Weighted average (discounted)
    w_avg_discounted = weighted.mean(VLW_discounted_vs_GDP_pct, GDP_PPP_total, na.rm = TRUE),
    w_var_discounted = sum(GDP_PPP_total^2 * (VLW_discounted_vs_GDP_pct - weighted.mean(VLW_discounted_vs_GDP_pct, GDP_PPP_total, na.rm = TRUE))^2, na.rm = TRUE) / sum(GDP_PPP_total)^2,
    w_se_discounted = sqrt(w_var_discounted),
    w_lci_discounted = w_avg_discounted - 1.96 * w_se_discounted,
    w_uci_discounted = w_avg_discounted + 1.96 * w_se_discounted,
    Weighted_Avg_GDP_Ratio_Discounted = sprintf("%.3f%%(%.3f–%.3f%%)", w_avg_discounted, w_lci_discounted, w_uci_discounted),
    
    # Average discount effect
    Average_Discount_Effect = sprintf("%.2f%%", mean(discount_effect_pct, na.rm = TRUE))
  ) %>%
  select(region, Total_DALY, Total_VLW_No_Discount_Billion, Total_VLW_Discounted_Billion,
         Weighted_Avg_GDP_Ratio_No_Discount, Weighted_Avg_GDP_Ratio_Discounted) %>%
  ungroup() 


print(Region21_summary)
# Save to CSV (no row names)
write.csv(Region21_summary, file = "Region21_summary.csv", row.names = FALSE)




Region21_summary <- resultsX %>% mutate(region="Global") %>% 
  group_by(region) %>%
  summarise(
    Total_DALY = sprintf("%.0f(%.0f-%.0f)", 
                         sum(DALY), sum(lower), sum(upper)),
    
    Total_VLW_No_Discount_Billion = sprintf("%.2f(%.2f-%.2f)", 
                                            sum(VLW_no_discount), 
                                            sum(VLW_no_discount_lower), 
                                            sum(VLW_no_discount_upper)),
    
    Total_VLW_Discounted_Billion = sprintf("%.2f(%.2f-%.2f)", 
                                           sum(VLW_discounted), 
                                           sum(VLW_discounted_lower), 
                                           sum(VLW_discounted_upper)),
    
    # Weighted average (undiscounted)
    w_avg_no_discount = weighted.mean(VLW_no_discount_vs_GDP_pct, GDP_PPP_total, na.rm = TRUE),
    w_var_no_discount = sum(GDP_PPP_total^2 * (VLW_no_discount_vs_GDP_pct - weighted.mean(VLW_no_discount_vs_GDP_pct, GDP_PPP_total, na.rm = TRUE))^2, na.rm = TRUE) / sum(GDP_PPP_total)^2,
    w_se_no_discount = sqrt(w_var_no_discount),
    w_lci_no_discount = w_avg_no_discount - 1.96 * w_se_no_discount,
    w_uci_no_discount = w_avg_no_discount + 1.96 * w_se_no_discount,
    Weighted_Avg_GDP_Ratio_No_Discount = sprintf("%.3f%%(%.3f–%.3f%%)", w_avg_no_discount, w_lci_no_discount, w_uci_no_discount),
    
    # Weighted average (discounted)
    w_avg_discounted = weighted.mean(VLW_discounted_vs_GDP_pct, GDP_PPP_total, na.rm = TRUE),
    w_var_discounted = sum(GDP_PPP_total^2 * (VLW_discounted_vs_GDP_pct - weighted.mean(VLW_discounted_vs_GDP_pct, GDP_PPP_total, na.rm = TRUE))^2, na.rm = TRUE) / sum(GDP_PPP_total)^2,
    w_se_discounted = sqrt(w_var_discounted),
    w_lci_discounted = w_avg_discounted - 1.96 * w_se_discounted,
    w_uci_discounted = w_avg_discounted + 1.96 * w_se_discounted,
    Weighted_Avg_GDP_Ratio_Discounted = sprintf("%.3f%%(%.3f–%.3f%%)", w_avg_discounted, w_lci_discounted, w_uci_discounted),
    
    # Average discount effect
    Average_Discount_Effect = sprintf("%.2f%%", mean(discount_effect_pct, na.rm = TRUE))
  ) %>%
  select(region, Total_DALY, Total_VLW_No_Discount_Billion, Total_VLW_Discounted_Billion,
         Weighted_Avg_GDP_Ratio_No_Discount, Weighted_Avg_GDP_Ratio_Discounted) %>%
  ungroup() 

# Save to CSV (no row names)
write.csv(Region21_summary, file = "Global_summary.csv", row.names = FALSE)
