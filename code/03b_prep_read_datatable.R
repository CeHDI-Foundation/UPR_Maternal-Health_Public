# Read in the pre-processed files for analysis ----

# The full dataset of recommendations
sdg_data <- readRDS(here("output", "UHRI_UPR_enhanced.rds")) |> 
  mutate(
    # make sure the maternal health variable doesn't include recommendations related to abortion
    Maternal_health = case_when(abortion != "Other" ~ "Other", .default = maternal_health),
  ) |> 
  relocate(Maternal_health, abortion, .after = maternal_health)

# If you regenerate the datasets from the `03_prep_datable_code.R` file, 
# update the date of the filename when reading it in
dat_table <- readRDS(file = here("output", "dat_table_2026-05-26.rds"))
dat_table_long <- readRDS(file = here("output", "dat_table_long_2026-05-26.rds"))

# Pull the list of countries analyzed
analysis_countries <- dat_table_long |> select(COUNTRY) |> distinct() |> pull(COUNTRY)

# Get the median of the counts for each theme
theme_medians <- dat_table |> 
  select(starts_with("n_tot_")) |> 
  select(-starts_with("n_tot_median")) |> 
  summarise(across(everything(), median))

# Specify the exposure variables for downstream analysis ----

# Read in the pretty labels
source(here("code", "theme_labels.R"))

vars_included <- c(
  "GBV",
  "Maternal_health",
  "abortion",
  "contraception",
  "health_related",
  "health_systems",
  "sexual_education",
  "sexual_health",
  "women"
)

prefix <- "n_tot_" ; theme_vars <- dat_table %>%
  select(all_of(paste0(prefix, vars_included))) %>%
  names()

## Variable name cleaning ----
clean_labels <- enframe(c(
  "Human Development Index, 2023" = "HDI_2023",
  "WHO region" = "WHO_region",
  
  "GDP PPP, 2005" = "GDP_2005",
  "GDP PPP, 2023" = "GDP_2023",
  "Health expenditure in 2005 (% of GDP)" = "CHEGDP_2005",
  "Health expenditure in 2023 (% of GDP)" = "CHEGDP_2023",
  "WB Income Group, 2023" = "WBIncome_2023",
  "WB Income Group, 2005" = "WBIncome_2005",
  "FCS status, 2025" = "FCS_status",
  "FCS status, 2023" = "FCS_2023",
  "Years on FCS list, 2006-2023" = "FCS_count",
  "Years on FCS list, 2006-2023" = "FCS_count_cat",
  
  "Political Stability, 2005" = "political_stability_baseline_cat",
  "Government Effectiveness, 2005" = "government_effectiveness_baseline_cat",
  "UHC sub-index on RMNCH, 2023" = "UHC_RMNCH",
  
  "Skilled birth attendance (%)" = "skilled_birth",
  "Institutional births (%)" = "institutional_birth",
  "ANC coverage (4+ visits, %)" = "anc4",
  "Met need for family planning (%)" = "family_planning",
  
  "UHC sub-index on RMNCH, 2023" = "UHC_RMNCH_cat_2023",
  "UHC sub-index on RMNCH, 2005" = "UHC_RMNCH_cat_2005",
  "UHC sub-index on RMNCH, 2005" = "UHC_RMNCH_2005",
  "UHC sub-index on RMNCH, 2023" = "UHC_RMNCH_cat",
  "Skilled birth attendance (%)" = "skilled_birth_cat",
  "Institutional births (%)" = "institutional_birth_cat",
  "ANC coverage (4+ visits, %)" = "anc4_cat",
  "Met need for family planning (%)" = "family_planning_cat",
  
  "Contraceptive prevalence, any (%)" = "contraceptive_any",
  "Unintended pregnancy (per 1,000)" = "unintended_pregnancy",
  "Maternal deaths attributed to abortion and miscarriage (per 100,000)" = "abortion_deaths",
  "Abortion laws" = "abortion_law",
  "MMR category, 2023" = "mmr_cat3_2023",
  "MMR category, 2023" = "mmr_cat6_2023",
  "MMR category, 2005" = "mmr_cat5_2005",
  "MMR in 2023" = "MMR_2023",
  "MMR in 2007" = "MMR_2007",
  "MMR in 2005" = "MMR_2005"
)) |> 
  rename(clean_name = name, var_label = value)