# =============================================================================
# 08_revision_R2.2_supported.R
#
# QUESTION  Reviewer 2, point 2: should results be stratified by whether the State
#           supported or noted the recommendations?
#
# ANALYSIS  1. Share of supported recommendations per theme (if almost all are supported,
#              stratifying is not informative).
#           2. Main model re-run counting supported recommendations only (n_supported_<theme>),
#              compared with all recommendations.
#
# OUTPUT    output/revision/08_supported_share.csv
#           output/revision/08_supported_only.csv
# =============================================================================

library(here)
source(here("code", "06_revision_setup.R"))

# 1. Share of supported recommendations (one row per country) ----------------------------
country_level <- dat_main |> filter(YEAR == 2005)

supported_share <- tibble(theme = theme_vars) |>
  mutate(
    total       = map_dbl(theme, \(v) sum(country_level[[v]], na.rm = TRUE)),
    supported   = map_dbl(theme, \(v) sum(country_level[[str_replace(v, "n_tot_", "n_supported_")]], na.rm = TRUE)),
    pct_supported = round(100 * supported / total, 1),
    outcome = "MMR"   # only needed by add_labels
  ) |>
  add_labels() |>
  select(theme_label, total, supported, pct_supported)

print(supported_share)
write_csv(supported_share, file.path(out_dir, "08_supported_share.csv"))

# 2. All vs supported recommendations ------------------------------------------------------
all_recs  <- run_over_themes(dat_main, themes = themes_sens) |>
  mutate(analysis = "All recommendations")
supported <- run_over_themes(dat_main, themes = str_replace(themes_sens, "n_tot_", "n_supported_")) |>
  mutate(analysis = "Supported only")

r22 <- bind_rows(all_recs, supported)
write_csv(r22, file.path(out_dir, "08_supported_only.csv"))

show_wide(r22, "analysis")
plot_compare(r22, "analysis")
