# =============================================================================
# 12_revision_R2.7_missing_data.R
#
# QUESTION  Reviewer 2, point 7: how much outcome data is missing, and does it affect the results?
#
# ANALYSIS  1. Observations per outcome, 2005-2023 (outcome and GDP both available).
#           2. SBA (survey data, often few points per country): models re-run on countries
#              with at least 3 observations.
#
# OUTPUT    output/revision/12_coverage.csv
#           output/revision/12_sba_min3.csv
# =============================================================================

library(here)
source(here("code", "06_revision_setup.R"))

# 1. Coverage -------------------------------------------------------------------------------------
coverage <- dat_main |>
  group_by(country_name) |>
  summarise(across(all_of(unname(outcome_vars)), \(x) sum(!is.na(x) & !is.na(GDP_long_c))), .groups = "drop") |>
  pivot_longer(-country_name, names_to = "variable", values_to = "n_obs") |>
  mutate(outcome = names(outcome_vars)[match(variable, outcome_vars)]) |>
  group_by(outcome) |>
  summarise(
    countries_with_data = sum(n_obs > 0),
    observations        = sum(n_obs),
    pct_missing         = round(100 * (1 - sum(n_obs) / (n() * n_distinct(dat_main$YEAR))), 1),
    median_per_country  = median(n_obs),
    q1 = quantile(n_obs, 0.25), q3 = quantile(n_obs, 0.75),
    countries_lt3       = sum(n_obs < 3)
  )

print(coverage)
write_csv(coverage, file.path(out_dir, "12_coverage.csv"))

# 2. SBA, countries with >= 3 observations ---------------------------------------------------
sba_countries <- dat_main |>
  group_by(country_name) |>
  summarise(n_sba = sum(!is.na(skilled_birth_long_p_squeezed) & !is.na(GDP_long_c))) |>
  filter(n_sba >= 3) |>
  pull(country_name)

r27 <- bind_rows(
  run_over_themes(dat_main, themes = themes_sens, outcomes = outcome_vars["SBA"]) |>
    mutate(analysis = "All countries"),
  run_over_themes(dat_main |> filter(country_name %in% sba_countries), themes = themes_sens,
                  outcomes = outcome_vars["SBA"]) |>
    mutate(analysis = paste0(">= 3 observations (", length(sba_countries), " countries)"))
)

write_csv(r27, file.path(out_dir, "12_sba_min3.csv"))
show_wide(r27, "analysis")
